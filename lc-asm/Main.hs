{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Data.Functor                   ( (<&>) )
import           Data.List                      ( nub
                                                , (\\)
                                                )
import           Control.Monad                  ( void
                                                , guard
                                                )
import           Data.Functor                   ( ($>) )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Void                      ( Void )
import           Control.Monad.Trans.State.Strict
                                                ( runState )

-- | CLI
import           System.Environment             ( getArgs )
import           System.Exit                    ( ExitCode(..)
                                                , exitWith
                                                )

-- | Parsing and printing
import           Text.Megaparsec         hiding ( State
                                                , Label
                                                )
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
import           Compile                        ( compileSC )

main :: IO ()
main = do
  args <- getArgs
  let multi        = getContents >>= parseInput <&> multiLambda
      lift_        = multi <&> lift
      eta'd        = lift_ <&> Map.map eta
      necessary    = eta'd <&> removeRedundant
      asm          = necessary <&> compileSupercombinators
      optimisedAsm = asm <&> Map.map removeRedundantInstructions
  case args of
    ["parse"    ] -> getContents >>= parseInput >>= pPrint
    ["multi"    ] -> multi >>= pPrint
    ["lift"     ] -> lift_ >>= pPrint
    ["asm"      ] -> asm <&> printProgram >>= putStrLn
    ["optimised"] -> optimisedAsm <&> printProgram >>= putStrLn
    []            -> optimisedAsm <&> printProgram >>= putStrLn
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
            asm    = printProgram $ compileSupercombinators supers
        Wai.responseLBS status200 mempty (pack asm)

parseInput :: String -> IO Exp
parseInput input = case parse (parseExp <* eof) "<stdin>" input of
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
-- 8. Simplify: Remove redundant instructions, like mov r0, r0

-- Multi-lambda pass: convert \w x. \y z. into \w x y z.
multiLambda :: Exp -> Exp
multiLambda = \case
  Lam xs e -> case multiLambda e of
    Lam ys e' -> Lam (xs <> ys) e'
    e'        -> Lam xs e'
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
    -- We've found a lambda
    Just (Left (xs, body, hole)) ->
      -- get the free variables of the lambda body
      let fvs  = fv body \\ xs
          -- abstract each as a parameter
          sc   = mkSC (fvs <> xs) body
          -- insert the resulting supercombinator into the environment
          scs' = Map.insert ("_" <> show i) sc scs
          lam' = foldr (flip App . Var) (Global i) fvs
      in                                                                                                                                                                            -- replace the lambda with a variable applied to the free vars and repeat
          lift' (i + 1, scs') (hole lam')
    -- We've found a fixpoint
    Just (Right (x, body, hole)) ->
      let
          -- name this supercombinator
          scName = "_" <> show i
          -- rename any references to x in the body to refer to the supercombinator
          body'  = rename x i body
          -- get the free variables of the body
          fvs    = fv body' \\ [scName]
          -- abstract each as a parameter
          sc     = mkSC fvs body'
          -- insert the resulting supercombinator into the environment
          scs'   = Map.insert scName sc scs
          lam'   = foldr (flip App . Var) (Global i) fvs
      in                                                                                                                                                                            -- replace the lambda with a variable applied to the free vars and repeat
          lift' (i + 1, scs') (hole lam')
    Nothing -> (i, Map.insert "_main" (mkSC [] expr) scs)

rename :: String -> Int -> Exp -> Exp
rename x y = go
 where
  go = \case
    Var z | z == x    -> Global y
          | otherwise -> Var z
    Lam zs e | x `elem` zs -> Lam zs e
             | otherwise   -> Lam zs (go e)
    Fix z e | z == x    -> Fix z e
            | otherwise -> Fix x (go e)
    App e1 e2    -> App (go e1) (go e2)
    If   b t  e  -> If (go b) (go t) (go e)
    Prim p e1 e2 -> Prim p (go e1) (go e2)
    Not    e     -> Not (go e)
    Global l     -> Global l
    Int    n     -> Int n
    Bool   b     -> Bool b

-- Make a supercombinator from a list of bound variable and an expression body
-- The body must not contain any lambdas or lets
mkSC :: [String] -> Exp -> SC
mkSC vars body = (vars, convert body)
 where
  convert = \case
    App e1 e2 -> case convert e1 of
      SApp f args -> SApp f (args <> [convert e2])
      se1         -> SApp se1 [convert e2]
    Var    x     -> SVar x
    Global i     -> SGlobal ("_" <> show i)
    Int    i     -> SInt i
    Bool   b     -> SBool b
    Prim p e1 e2 -> SPrim p (convert e1) (convert e2)
    If   b t  e  -> SIf (convert b) (convert t) (convert e)
    Not e        -> SNot (convert e)
    e@(Fix _ _) ->
      error $ "Unexpected fixpoint in supercombinator body: " <> show e
    e@(Lam _ _) ->
      error $ "Unexpected lambda in supercombinator body: " <> show e

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
  SApp (SGlobal _) [] -> True
  _                   -> False
isRedundant _ = False

compileSupercombinators :: Map String SC -> Map String [Asm]
compileSupercombinators = snd . Map.mapAccum
  (\n sc -> let (asm, n') = runState (compileSC sc) n in (n', asm))
  0

removeRedundantInstructions :: [Asm] -> [Asm]
removeRedundantInstructions = filter (not . redundant)
 where
  redundant = \case
    Mov r1 r2 | r1 == r2 -> True
    _                    -> False

test :: IO ()
test =
  let e = App
        (Lam ["x"] (App (Lam ["y"] (Prim Add (Var "x") (Var "y"))) (Var "x")))
        (Int 4)
      expected = Map.fromList
        [ ("_main", ([], SApp (SGlobal "_1") [SInt 4]))
        , ("_0"   , (["x", "y"], SPrim Add (SVar "x") (SVar "y")))
        , ("_1"   , (["x"], SApp (SGlobal "_0") [SVar "x", SVar "x"]))
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

-- Find the first lambda or fixpoint which has no lambdas (or fixpoints) in its body
-- Left is Lambda, right is fixpoint
findLambda
  :: Exp -> Maybe (Either ([String], Exp, Exp -> Exp) (String, Exp, Exp -> Exp))
findLambda = go id
 where
  go
    :: (Exp -> Exp)
    -> Exp
    -> Maybe
         (Either ([String], Exp, Exp -> Exp) (String, Exp, Exp -> Exp))
  go hole = \case
    Int    _ -> Nothing
    Bool   _ -> Nothing
    Var    _ -> Nothing
    Global _ -> Nothing
    Not    e -> go (hole . Not) e
    Prim p e1 e2 ->
      go (hole . flip (Prim p) e2) e1 <|> go (hole . Prim p e1) e2
    App e1 e2 -> go (hole . flip App e2) e1 <|> go (hole . App e1) e2
    If b t e ->
      go (hole . (\b' -> If b' t e)) b
        <|> go (hole . (\t' -> If b t' e)) t
        <|> go (hole . If b t)             e
    Lam x e -> go (hole . Lam x) e <|> Just (Left (x, e, hole))
    Fix x e -> go (hole . Fix x) e <|> Just (Right (x, e, hole))

-- Calculate the free variable of an expression
fv :: Exp -> [String]
fv = nub . go []
 where
  go :: [String] -> Exp -> [String]
  go bound = \case
    Var v | v `elem` bound -> []
          | otherwise      -> [v]
    Lam xs e     -> go (xs <> bound) e
    Fix x  e     -> go (x : bound) e
    Int    _     -> []
    Bool   _     -> []
    Global _     -> []
    Not    e     -> go bound e
    App e1 e2    -> go bound e1 <> go bound e2
    Prim _ e1 e2 -> go bound e1 <> go bound e2
    If   b t  e  -> go bound b <> go bound t <> go bound e

-- Calculate the free variables of a SExp
sfv :: SExp -> [String]
sfv = nub . go
 where
  go :: SExp -> [String]
  go = \case
    SVar    v     -> [v]
    SInt    _     -> []
    SBool   _     -> []
    SGlobal _     -> []
    SNot    e     -> go e
    SApp _ args   -> concatMap go args
    SPrim _ e1 e2 -> go e1 <> go e2
    SIf   b t  e  -> go b <> go t <> go e

printAsm :: Asm -> String
printAsm = \case
  Mov  o1 o2   -> "    mov " <> printOp o1 <> ", " <> printOp o2
  IAdd o1 o2   -> "    add " <> printOp o1 <> ", " <> printOp o2
  ISub o1 o2   -> "    sub " <> printOp o1 <> ", " <> printOp o2
  IMul o1 o2   -> "    imul " <> printOp o1 <> ", " <> printOp o2
  Push r       -> "    push " <> printReg r
  Pop  r       -> "    pop " <> printReg r
  Call o       -> "    call " <> printOp o
  Ret          -> "    ret"
  Label l      -> l <> ":"
  Cmp o1 o2    -> "    cmp " <> printOp o1 <> ", " <> printOp o2
  Jmp   o      -> "    jmp " <> printOp o
  JmpEq o      -> "    je " <> printOp o
  JmpGt o      -> "    jg " <> printOp o
  JmpLt o      -> "    jl " <> printOp o
  IAnd o1 o2   -> "    and " <> printOp o1 <> ", " <> printOp o2
  IOr  o1 o2   -> "    or " <> printOp o1 <> ", " <> printOp o2
  INot o       -> "    not " <> printOp o
  ShiftR o1 o2 -> "    shr " <> printOp o1 <> ", " <> printOp o2

printOp :: Op -> String
printOp = \case
  R r -> printPseudoReg r
  L l -> l
  I i -> show i

printReg :: Register -> String
printReg (Register r) = r

printPseudoReg :: PseudoReg -> String
printPseudoReg (Reg   r) = printReg r
printPseudoReg (Stack i) = "[rsp+" <> show i <> "]"

printProgram :: Map String [Asm] -> String
printProgram defs =
  foldMap (\(l, e) -> unlines $ (l <> ":") : map (printAsm) e) $ Map.toList defs

-- Parsing

type Parser = Parsec Void String

parseExp :: Parser Exp
parseExp = try parseApp <|> parseIf <|> parseLet <|> parseNonApp

parseNonApp :: Parser Exp
parseNonApp =
  parseLam
    <|> parseInt
    <|> parseBool
    <|> parseNot
    <|> parseVar
    <|> parens parseExp

parseApp :: Parser Exp
parseApp = do
  appOrPrim <- (Left <$> parseNonApp) <|> (Right <$> parsePrim)
  case appOrPrim of
    Left f -> do
      args <- some parseNonApp
      pure (foldl App f args)
    Right p -> do
      Prim p <$> parseNonApp <*> parseNonApp

parseLam :: Parser Exp
parseLam = do
  _ <- string "\\"
  void space
  xs <- some parseVarString
  void space
  _ <- string "."
  void space
  Lam xs <$> parseExp

parseVar :: Parser Exp
parseVar = Var <$> parseVarString

parseVarString :: Parser String
parseVarString = try $ do
  a  <- letterChar
  as <- many alphaNumChar
  void space
  guard (a : as `notElem` keywords)
  pure (a : as)

keywords :: [String]
keywords =
  [ "if"
  , "then"
  , "else"
  , "True"
  , "False"
  , "not"
  , "and"
  , "or"
  , "fix"
  , "let"
  , "in"
  ]

parsePrim :: Parser Prim
parsePrim =
  ((string "+" <* space) $> Add)
    <|> ((string "-" <* space) $> Sub)
    <|> ((string "*" <* space) $> Mul)
    <|> ((string "=" <* space) $> Eq)
    <|> ((string ">" <* space) $> Gt)
    <|> ((string "<" <* space) $> Lt)
    <|> ((string "and" <* space) $> And)
    <|> ((string "or" <* space) $> Or)

parseInt :: Parser Exp
parseInt = Int . read <$> some digitChar <* space

parseBool :: Parser Exp
parseBool = parseTrue <|> parseFalse
 where
  parseTrue  = string "True" >> void space $> Bool True
  parseFalse = string "False" >> void space $> Bool True

parseNot :: Parser Exp
parseNot = do
  _ <- string "not"
  void space
  e <- parseNonApp
  pure $ Not e

parseIf :: Parser Exp
parseIf = do
  _ <- string "if"
  void space
  b <- parseExp
  _ <- string "then"
  void space
  t <- parseExp
  _ <- string "else"
  void space
  e <- parseExp
  pure $ If b t e

-- Lets are recursive and are converted into a combination of lambda and fixpoint:
-- let x = e1 in e2 ==> (\x. e2) (fix x. e1)
parseLet :: Parser Exp
parseLet = do
  _ <- string "let"
  void space
  x <- parseVarString
  _ <- string "="
  void space
  e1 <- parseExp
  _  <- string "in"
  void space
  e2 <- parseExp
  pure $ App (Lam [x] e2) (Fix x e1)

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
