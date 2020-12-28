{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Data.Functor                   ( (<&>) )
import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( replicateM
                                                , void
                                                )
import           Control.Monad.Trans.State.Strict
                                                ( State
                                                , evalState
                                                , get
                                                , put
                                                )
import           Data.Functor                   ( ($>) )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Void                      ( Void )

-- | CLI
import           System.Environment             ( getArgs )
import           System.Exit                    ( ExitCode(..)
                                                , exitWith
                                                )

-- | Parsing and printing
import           Text.Megaparsec         hiding ( State )
import           Text.Megaparsec.Char
import           Text.Pretty.Simple             ( pPrint )

-- | Web server
import qualified Network.Wai                   as Wai
import qualified Network.Wai.Handler.Warp      as Warp
import           Network.HTTP.Types.Status      ( status400
                                                , status200
                                                )
import           Data.ByteString.Lazy.Char8     ( unpack
                                                , pack
                                                )

import           Types
import           Compile                        ( compileSC
                                                , r0
                                                , argReg
                                                )

main :: IO ()
main = do
  args <- getArgs
  let multi     = parseInput <&> multiLambda
      lift_     = multi <&> lift
      eta'd     = lift_ <&> Map.map eta
      necessary = eta'd <&> removeRedundant
      asm       = necessary <&> printProgram
  case args of
    ["parse"]     -> parseInput >>= pPrint
    ["multi"]     -> multi >>= pPrint
    ["lift" ]     -> lift_ >>= pPrint
    ["asm"  ]     -> asm >>= putStrLn
    []            -> asm >>= putStrLn
    ["web", port] -> do
      putStrLn $ "Launching HTTP server on port " <> port
      launchWebServer port
    a -> error $ "Bad argument: " <> show a

launchWebServer :: String -> IO ()
launchWebServer portStr = Warp.run (read portStr) app
 where
  app req respond = do
    body <- unpack <$> Wai.strictRequestBody req
    respond $ case parse (parseExp <* eof) "<request>" body of
      Left err -> do
        let errStr = pack $ errorBundlePretty err
        Wai.responseLBS status400 mempty errStr
      Right expr -> do
        let supers = removeRedundant $ Map.map eta $ lift $ multiLambda expr
            asm    = printProgram supers
        Wai.responseLBS status200 mempty (pack asm)

parseInput = do
  input <- getContents
  case parse (parseExp <* eof) "<stdin>" input of
    Left  err  -> putStrLn (errorBundlePretty err) >> exitWith (ExitFailure 1)
    Right expr -> pure expr

-- Convert lambda calculus with just integers into assembly.
--
-- Pipeline
-- --------
-- 1. MultiLambda: Merge nested lambdas
-- 2. DeBruijn: Replace variables with de Bruijn indices (TODO - this allows better parameter ordering during lambda-lifting)
-- 3. Lift: Lift lambdas to top-level supercombinators
-- 5. Eta: eta-reduce supercombinators
-- 6. RemoveRedundant: Remove supercombinators which are aliases for other supercombinators
-- 7. Compile: Convert each supercombinator to a sequence of assembly instructions

-- Multi-lambda pass: convert \w x. \y z. into \w x y z.
multiLambda :: Exp -> Exp
multiLambda = \case
  Lam xs e -> case multiLambda e of
    Lam ys e -> Lam (xs <> ys) e
    e        -> Lam xs e
  App e1 e2    -> App (multiLambda e1) (multiLambda e2)
  Prim p e1 e2 -> Prim p (multiLambda e1) (multiLambda e2)
  e            -> e

-- Simple algorithm:
-- 1. If there are no lambda abstractions, finish
-- 2. Choose any lambda which has no inner lambdas in its body
-- 3. Abstract its free variables as extra parameters
-- 4. Name it and put it in the environment
-- 5. Replace the occurrence of the lambda by the name applied to the free variables

lift :: Exp -> Map String SC
lift = snd . lift' (0, mempty)
 where
    -- The Int is a counter for naming new definitions
  lift' :: (Int, Map String SC) -> Exp -> (Int, Map String SC)
  lift' (i, scs) expr = case findLambda expr of
    Just (lam@(Lam xs body), hole) ->
      -- get the free variables of the lambda body
      let fvs  = fv lam
          -- abstract each as a parameter
          sc   = mkSC (fvs <> xs) body
          -- insert the resulting supercombinator into the environment
          scs' = Map.insert ("_" <> show i) sc scs
          lam' = foldr (flip App . Var) (Global i) fvs
      in            -- replace the lambda with a variable applied to the free vars and repeat
          lift' (i + 1, scs') (hole lam')
    Nothing -> (i, Map.insert "_main" (mkSC [] expr) scs)

-- Make a supercombinator from a list of bound variable and an expression body
-- The body must not contain any lambdas
mkSC :: [String] -> Exp -> SC
mkSC vars body = (vars, convert body)
 where
  convert = \case
    App e1 e2 -> case convert e1 of
      SApp f args -> SApp f (args <> [convert e2])
      se1         -> SApp se1 [convert e2]
    Var    x     -> SVar x
    Global i     -> SGlobal i
    Int    i     -> SInt i
    Prim p e1 e2 -> SPrim p (convert e1) (convert e2)
    e            -> error $ "Unexpected: " <> show e

-- Convert
--   $1 ... y = $2 ... y
-- into
--   $1 ... = $2 ...
eta :: SC -> SC
eta ([]  , e) = ([], e)
eta (vars, e) = case e of
  SApp f args -> case (unsnoc vars, unsnoc args) of
    -- We can eta-reduce a definition f x1 ... xN = g y1 ... yM xN iff
    -- xN does not appear in y1 ... yM
    (Just (vs, v), Just (as, a))
      | a == SVar v, all (v `notElem`) (map sfv as) -> eta (vs, SApp f as)
    _ -> (vars, e)
  _ -> (vars, e)

removeRedundant :: Map String SC -> Map String SC
removeRedundant scs =
  let scs' = Map.filter (not . isRedundant) scs
  in  if Map.size scs' /= Map.size scs then removeRedundant scs' else scs

-- True if the supercombinator is just an alias for another supercombinator, e.g.
--   $1 = $2
isRedundant :: SC -> Bool
isRedundant ([], e) = case e of
  SApp (SGlobal i) [] -> True
  _                   -> False
isRedundant _ = False

test :: IO ()
test =
  let e = App
        (Lam ["x"] (App (Lam ["y"] (Prim Add (Var "x") (Var "y"))) (Var "x")))
        (Int 4)
      expected = Map.fromList
        [ ("_main", ([], SApp (SGlobal 1) [SInt 4]))
        , ("_0"   , (["x", "y"], SPrim Add (SVar "x") (SVar "y")))
        , ("_1"   , (["x"], SApp (SGlobal 0) [SVar "x", SVar "x"]))
        ]
      actual = removeRedundant $ Map.map eta $ lift $ multiLambda e
  in  if expected == actual
        then putStrLn "Pass."
        else do
          putStrLn "Fail."
          putStrLn "Expected:"
          pPrint expected
          putStrLn "Actual:"
          pPrint actual

-- Find the first lambda which has no lambdas in its body
findLambda :: Exp -> Maybe (Exp, Exp -> Exp)
findLambda = go id
 where
  go :: (Exp -> Exp) -> Exp -> Maybe (Exp, Exp -> Exp)
  go hole = \case
    Int    _ -> Nothing
    Var    _ -> Nothing
    Global _ -> Nothing
    Prim p e1 e2 ->
      go (hole . flip (Prim p) e2) e1 <|> go (hole . Prim p e1) e2
    App e1 e2 -> go (hole . flip App e2) e1 <|> go (hole . App e1) e2
    Lam x  e  -> go (hole . Lam x) e <|> Just (Lam x e, hole)

-- Calculate the free variable of an expression
fv :: Exp -> [String]
fv = go []
 where
  go :: [String] -> Exp -> [String]
  go bound = \case
    Var v | v `elem` bound -> []
          | otherwise      -> [v]
    Lam xs e     -> go (xs <> bound) e
    Int    _     -> []
    Global _     -> []
    App e1 e2    -> go bound e1 <> go bound e2
    Prim _ e1 e2 -> go bound e1 <> go bound e2

-- Calculate the free variables of a SExp
sfv :: SExp -> [String]
sfv = go
 where
  go :: SExp -> [String]
  go = \case
    SVar    v     -> [v]
    SInt    _     -> []
    SGlobal _     -> []
    SApp _ args   -> concatMap go args
    SPrim _ e1 e2 -> go e1 <> go e2

printAsm :: Asm -> String
printAsm = \case
  Mov  o1 o2 -> "mov " <> printOp o1 <> ", " <> printOp o2
  IAdd o1 o2 -> "add " <> printOp o1 <> ", " <> printOp o2
  ISub o1 o2 -> "sub " <> printOp o1 <> ", " <> printOp o2
  IMul o1 o2 -> "imul " <> printOp o1 <> ", " <> printOp o2
  Push r     -> "push " <> printReg r
  Pop  r     -> "pop " <> printReg r
  Call o     -> "call " <> printOp o
  Ret        -> "ret"

printOp :: Op -> String
printOp = \case
  R r -> printPseudoReg r
  L i -> "_" <> show i
  I i -> show i

printReg :: Register -> String
printReg (Register r) = r

printPseudoReg :: PseudoReg -> String
printPseudoReg (Reg   r) = printReg r
printPseudoReg (Stack i) = "[rsp+" <> show i <> "]"

printProgram :: Map String SC -> String
printProgram m =
  let defs = map (\(l, f) -> (l, compileSC f)) $ Map.toList m
  in  foldMap (\(l, e) -> unlines $ (l <> ":") : map (("  " <>) . printAsm) e)
              defs

-- Parsing

type Parser = Parsec Void String

parseExp :: Parser Exp
parseExp = try parseApp <|> parseLam <|> parseVar <|> parseInt

parseNonApp = parseLam <|> parseVar <|> parseInt <|> parens parseExp

parseApp = do
  appOrPrim <- (Left <$> parseNonApp) <|> (Right <$> parsePrim)
  case appOrPrim of
    Left f -> do
      args <- some parseNonApp
      pure (foldl App f args)
    Right p -> do
      Prim p <$> parseNonApp <*> parseNonApp

parseLam = do
  _ <- string "\\"
  void space
  xs <- some parseVarString
  void space
  _ <- string "."
  void space
  Lam xs <$> parseExp

parseVar = Var <$> parseVarString

parseVarString = do
  a  <- letterChar
  as <- many alphaNumChar
  void space
  pure (a : as)

parsePrim :: Parser Prim
parsePrim =
  ((string "+" <* space) $> Add)
    <|> ((string "-" <* space) $> Sub)
    <|> ((string "*" <* space) $> Mul)

parseInt = Int . read <$> some digitChar <* space

parens :: Parser a -> Parser a
parens = between (string "(" >> space) (string ")" >> space)

-- | If the list is empty returns 'Nothing', otherwise returns the 'init' and the 'last'.
--
-- > unsnoc "test" == Just ("tes",'t')
-- > unsnoc ""     == Nothing
-- > \xs -> unsnoc xs == if null xs then Nothing else Just (init xs, last xs)
unsnoc :: [a] -> Maybe ([a], a)
unsnoc []       = Nothing
unsnoc [x     ] = Just ([], x)
unsnoc (x : xs) = Just (x : a, b) where Just (a, b) = unsnoc xs
