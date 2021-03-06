{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

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


import           Types
import           Compile                        ( compileSC )
import           Print                          ( printExp
                                                , printSExp
                                                , printDefinitions
                                                )
import qualified Server                         ( run )
import           GHC.Generics
import           Data.Aeson                     ( ToJSON )

data Result = Result { _parse :: Exp
                     , _merge_lambdas :: Exp
                     , _merge_lambdas_str :: String
                     , _lift :: Map String SC
                     , _lift_str :: String
                     , _eta_reduce :: Map String SC
                     , _eta_reduce_str :: String
                     , _remove_redundant_supercombinators :: Map String SC
                     , _remove_redundant_supercombinators_str :: String
                     , _compile :: Map String [Asm]
                     , _compile_str :: String
                     , _optimise :: Map String [Asm]
                     , _optimise_str :: String
                     }
                     deriving (Generic)
instance ToJSON Result

main :: IO ()
main = do
  args <- getArgs
  let printOrError :: (a -> IO ()) -> Either String a -> IO ()
      printOrError _ (Left  err) = putStrLn $ "Error: " <> err
      printOrError p (Right r  ) = p r
      runStage :: (a -> IO ()) -> (Result -> a) -> IO ()
      runStage printer stage =
        getContents >>= (printOrError printer . fmap stage . generateResult)
  case args of
    ["parse"    ] -> runStage pPrint _parse
    ["multi"    ] -> runStage pPrint _merge_lambdas
    ["lift"     ] -> runStage pPrint _lift
    ["asm"      ] -> runStage (print . printProgram) _compile
    ["optimised"] -> runStage (print . printProgram) _optimise
    []            -> runStage (print . printProgram) _optimise
    ["web", port] -> do
      Server.run port generateResult
    a -> error $ "Bad argument: " <> show a

generateResult :: String -> Either String Result
generateResult input = case parse (parseExp <* eof) "<input>" input of
  Left err -> Left (errorBundlePretty err)
  Right expr ->
    let multi        = multiLambda expr
        lifted       = lift multi
        eta'd        = Map.map eta lifted
        necessary    = removeRedundant eta'd
        asm          = compileSupercombinators necessary
        optimisedAsm = Map.map removeRedundantInstructions asm
    in  Right $ Result expr
                       multi
                       (show (printExp multi))
                       lifted
                       (show (printDefinitions printSExp lifted))
                       eta'd
                       (show (printDefinitions printSExp eta'd))
                       necessary
                       (show (printDefinitions printSExp necessary))
                       asm
                       (printProgram asm)
                       optimisedAsm
                       (printProgram optimisedAsm)

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
       -- replace the lambda with a variable applied to the free vars and repeat
      in
          lift' (i + 1, scs') (hole lam')
    -- We've found a let f = \xs. e in body
    Just (Right (f, xs, e, body, hole)) ->
      -- get the free variables of the let
      let fvs   = fv $ Let f (Lam xs e) body
          -- this is an application of the sc to its free vars
          self  = foldr (flip App . Var) (Global i) fvs
          -- rename any occurrences of 'f' in 'e' to 'self'
          e'    = rename f self e
          -- rename any occurrences of 'f' in 'body' to 'self'
          body' = rename f self body
          -- abstract each free variable as a parameter
          sc    = mkSC (fvs <> xs) e'
          -- insert the resulting supercombinator into the environment
          scs'  = Map.insert ("_" <> show i) sc scs
      -- replace the lambda with a variable applied to the free vars and repeat
      in
          lift' (i + 1, scs') (hole body')

    Nothing -> (i, Map.insert "_main" (mkSC [] expr) scs)

rename :: String -> Exp -> Exp -> Exp
rename x y = go
 where
  go = \case
    Var z | z == x    -> y
          | otherwise -> Var z
    Lam zs e | x `elem` zs -> Lam zs e
             | otherwise   -> Lam zs (go e)
    App e1 e2    -> App (go e1) (go e2)
    If   b t  e  -> If (go b) (go t) (go e)
    Prim p e1 e2 -> Prim p (go e1) (go e2)
    Not    e     -> Not (go e)
    Global l     -> Global l
    Int    n     -> Int n
    Bool   b     -> Bool b
    Let z e1 e2 | z == x    -> Let z e1 e2
                | otherwise -> Let z (go e1) (go e2)

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
    e@(Let _ _ _) ->
      error $ "Unexpected let in supercombinator body: " <> show e
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

-- TODO: this may leave dangling references to the removed supers
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

-- Find the first lambda or let which has no lambdas or lets in its body
-- Left is Lambda, Right is let
findLambda
  :: Exp
  -> Maybe
       ( Either
           ([String], Exp, Exp -> Exp)
           (String, [String], Exp, Exp, Exp -> Exp)
       )
findLambda = go id
 where
  go
    :: (Exp -> Exp)
    -> Exp
    -> Maybe
         ( Either
             ([String], Exp, Exp -> Exp)
             (String, [String], Exp, Exp, Exp -> Exp)
         )
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
    Lam x e     -> go (hole . Lam x) e <|> Just (Left (x, e, hole))
    Let x e1 e2 -> go (hole . Let x e1) e2 <|> case e1 of
      Lam ys e3 -> go (hole . (\e3' -> Let x (Lam ys e3') e2)) e3
        <|> Just (Right (x, ys, e3, e2, hole))
      _ -> go (hole . flip (Let x) e2) e1

-- Calculate the free variable of an expression
fv :: Exp -> [String]
fv = nub . go []
 where
  go :: [String] -> Exp -> [String]
  go bound = \case
    Var v | v `elem` bound -> []
          | otherwise      -> [v]
    Lam xs e     -> go (xs <> bound) e
    Int    _     -> []
    Bool   _     -> []
    Global _     -> []
    Not    e     -> go bound e
    App e1 e2    -> go bound e1 <> go bound e2
    Prim _ e1 e2 -> go bound e1 <> go bound e2
    If   b t  e  -> go bound b <> go bound t <> go bound e
    Let  x e1 e2 -> go (x : bound) e1 <> go (x : bound) e2

-- Calculate the free variables of a SExp
sfv :: SExp -> [String]
sfv = nub . go []
 where
  go :: [String] -> SExp -> [String]
  go bound = \case
    SVar    v     -> [v]
    SInt    _     -> []
    SBool   _     -> []
    SGlobal _     -> []
    SNot    e     -> go bound e
    SApp _ args   -> concatMap (go bound) args
    SPrim _ e1 e2 -> go bound e1 <> go bound e2
    SIf   b t  e  -> go bound b <> go bound t <> go bound e

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
  ["if", "then", "else", "True", "False", "not", "and", "or", "let", "in"]

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

-- Lets are recursive
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
  pure $ Let x e1 e2

parens :: Parser a -> Parser a
parens = between (string "(" >> space) (string ")" >> space)

-- | If the list is empty returns 'Nothing', otherwise returns the 'init' and the 'last'.
--
-- > unsnoc "test" == Just ("tes",'t')
-- > unsnoc ""     == Nothing
-- > \xs -> unsnoc xs == if null xs then Nothing else Just (init xs, last xs)
unsnoc :: [a] -> Maybe ([a], a)
unsnoc []       = Nothing
unsnoc (x : xs) = case unsnoc xs of
  Just (a, b) -> Just (x : a, b)
  Nothing     -> Just ([], x)
