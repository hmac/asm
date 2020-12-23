{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad (replicateM, void)
import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Data.Functor (($>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Void (Void)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

main :: IO ()
main = do
  input <- getContents
  case parse (parseExp <* eof) "<stdin>" input of
    Left err -> putStrLn (errorBundlePretty err)
    Right expr ->
      let defs = lift expr
          scs = Map.map supercombinate defs
       in putStrLn $ printProgram scs

-- Idea:
-- Convert lambda calculus with just integers into assembly
-- 1. Fully lambda lift
-- 2. Convert each (now top-level) function to an assembly function

data Exp
  = Lam String Exp
  | App Exp Exp
  | Var String
  | Global Int
  | Int Int
  | Prim Prim Exp Exp
  deriving (Eq, Show)

data Prim = Add | Sub | Mul
  deriving (Eq, Show)

-- The Int is a counter for naming new definitions
type Env = (Int, Map Int Exp)

-- Simple algorithm:
-- 1. If there are no lambda abstractions, finish
-- 2. Choose any lambda which has no inner lambdas in its body
-- 3. Abstract its free variables as extra parameters
-- 4. Name it and put it in the environment
-- 5. Replace the occurrence of the lambda by the name applied ot the free variables

lift :: Exp -> Map Int Exp
lift = snd . lift' (0, mempty)
  where
    lift' :: Env -> Exp -> Env
    lift' (i, scs) expr = case findLambda expr of
      Just (lam, hole) ->
        -- get the free variables of the lambda body
        let vars = fv lam
            -- abstract each as a parameter
            sc = foldr Lam lam vars
            -- insert the resulting supercombinator into the environment
            scs' = Map.insert i sc scs
            lam' = foldr (flip App . Var) (Global i) vars
         in -- replace the lambda with a variable applied to the free vars and repeat
            lift' (i + 1, scs') (hole lam')
      Nothing -> (i, Map.insert (negate 1) expr scs)

test :: Bool
test =
  let e =
        App
          ( Lam
              "x"
              (App (Lam "y" (Prim Add (Var "x") (Var "y"))) (Var "x"))
          )
          (Int 4)
   in Map.map
        supercombinate
        (lift e)
        == Map.fromList
          [ (-1, ([], SApp (SGlobal 1) [SInt 4])),
            (0, (["x", "y"], SPrim Add (SVar "x") (SVar "y"))),
            (1, (["x"], SApp (SGlobal 0) [SVar "x", SVar "x"]))
          ]

-- Find the first lambda which has no lambdas in its body
findLambda :: Exp -> Maybe (Exp, Exp -> Exp)
findLambda = go id
  where
    go :: (Exp -> Exp) -> Exp -> Maybe (Exp, Exp -> Exp)
    go hole = \case
      Int _ -> Nothing
      Var _ -> Nothing
      Global _ -> Nothing
      Prim p e1 e2 -> go (hole . flip (Prim p) e2) e1 <|> go (hole . Prim p e1) e2
      App e1 e2 -> go (hole . flip App e2) e1 <|> go (hole . App e1) e2
      Lam x e -> go (hole . Lam x) e <|> Just (Lam x e, hole)

fv :: Exp -> [String]
fv = go []
  where
    go :: [String] -> Exp -> [String]
    go bound = \case
      Var v
        | v `elem` bound -> []
        | otherwise -> [v]
      Lam x e -> go (x : bound) e
      Int _ -> []
      Global _ -> []
      App e1 e2 -> go bound e1 <> go bound e2
      Prim _ e1 e2 -> go bound e1 <> go bound e2

-- Supercombinator body
-- Like Exp but with no lambda
-- and applications are saturated
data SExp
  = SApp SExp [SExp]
  | SVar String
  | SGlobal Int
  | SInt Int
  | SPrim Prim SExp SExp
  deriving (Eq, Show)

type SC = ([String], SExp)

supercombinate :: Exp -> SC
supercombinate = go []
  where
    go vars = \case
      Lam x e -> go (vars <> [x]) e
      e -> (vars, convert e)
    convert = \case
      App e1 e2 -> case convert e1 of
        SApp f args -> SApp f (args <> [convert e2])
        se1 -> SApp se1 [convert e2]
      Var x -> SVar x
      Global i -> SGlobal i
      Int i -> SInt i
      Prim p e1 e2 -> SPrim p (convert e1) (convert e2)

data Asm = Mov Op Op | IAdd Op Op | ISub Op Op | IMul Op Op | Call Op | Ret
  deriving (Eq, Show)

printAsm :: Asm -> String
printAsm = \case
  Mov o1 o2 -> "mov " <> printOp o1 <> ", " <> printOp o2
  IAdd o1 o2 -> "add " <> printOp o1 <> ", " <> printOp o2
  ISub o1 o2 -> "sub " <> printOp o1 <> ", " <> printOp o2
  IMul o1 o2 -> "mul " <> printOp o1 <> ", " <> printOp o2
  Call o -> "call " <> printOp o
  Ret -> "ret"

-- Operands are registers, labels or literal integers
data Op = R Int | L Int | I Int
  deriving (Eq, Show)

printOp :: Op -> String
printOp = \case
  R i -> "r" <> show i
  L i -> "_" <> show i
  I i -> show i

-- Compile a supercombinator to assembly instructions
-- Firstly, the register r0..rN will contain vars
-- we need to save these if they're needed
-- For each arg at index i:
-- - if it's a variable in vars, we just move it to ri
-- - if it's a global, we just move it to ri
compile :: SC -> Reg [Asm]
compile (variables, e) = do
  -- Allocate registers for each argument to the function
  vs <- mapM (\v -> (v,) <$> mkReg) variables
  go vs e
  where
    go :: [(String, Int)] -> SExp -> Reg [Asm]
    go vars = \case
      SVar x -> pure [Mov (R 0) (R (lookupVar x vars)), Ret]
      SGlobal i -> pure [Call (L i)]
      SApp f args -> case f of
        SGlobal i -> do
          is <- mkCall vars (zip args [1 ..])
          pure $ is <> [Call (L i)]
        SVar x -> do
          is <- mkCall vars (zip args [1 ..])
          pure $ is <> [Call (R (lookupVar x vars))]
      SPrim p a1 a2 -> do
        r1 <- mkReg
        r2 <- mkReg
        is <- mkCall vars [(a1, r1), (a2, r2)]
        let rest = case p of
              Add -> [Mov (R 0) (R r1), IAdd (R 0) (R r2), Ret]
              Sub -> [Mov (R 0) (R r1), ISub (R 0) (R r2), Ret]
              Mul -> [Mov (R 0) (R r1), IMul (R 0) (R r2), Ret]
        pure $ is <> rest
    -- Convert an application to a call instruction
    -- Before we can do this we need to ensure all arguments are evaluated
    -- Because applications are in spine form, any well-typed program will have a variable as the
    -- application head, so we don't need to evaluate f.
    mkCall :: [(String, Int)] -> [(SExp, Int)] -> Reg [Asm]
    mkCall _ [] = pure []
    mkCall vars ((a, i) : args) = case a of
      SGlobal j -> do
        is <- mkCall vars args
        pure $ [Mov (R i) (L j)] <> is
      SVar x ->
        let j = lookupVar x vars
         in if i == j
              then mkCall vars args
              else do
                is <- mkCall vars args
                pure $ [Mov (R i) (R (lookupVar x vars))] <> is
      SInt n -> do
        is <- mkCall vars args
        pure $ [Mov (R i) (I n)] <> is
      -- We need to compile g_args, call g, then put the result in ri
      -- But at the same time we need to preserve any rN for 0 < N < i
      SApp g g_args -> do
        -- First, copy r1..r(i-1) to new registers
        newRegs <- zip [1 ..] <$> replicateM (i -1) mkReg
        let save = map (\(i, j) -> Mov (R j) (R i)) newRegs
        -- Then compile (g g_args)
        is <- go vars (SApp g g_args)
        -- Then put the registers back
        let restore = map (\(i, j) -> Mov (R i) (R j)) newRegs
        -- Then put the result of the call (r0) into ri
        let finally = [Mov (R i) (R 0)]
        pure $ concat [save, is, restore, finally]
    lookupVar :: String -> [(String, Int)] -> Int
    lookupVar x vars = case lookup x vars of
      Just j -> j
      Nothing -> error $ "undefined variable: " <> x

-- A monad for generating unique registers
type Reg a = State Int a

-- Generate a unique register
mkReg :: Reg Int
mkReg = do
  i <- get
  put (i + 1)
  pure i

runReg :: Reg a -> a
runReg = flip evalState 1

printProgram :: Map Int SC -> String
printProgram m =
  case Map.lookup (-1) m of
    Just main ->
      let m' = Map.delete (-1) m
          mainStr = unlines $ "main:" : map (("  " <>) . printAsm) (runReg (compile main))
       in mainStr <> Map.foldMapWithKey (\l e -> unlines $ ("_" <> show l <> ":") : map (("  " <>) . printAsm) (runReg (compile e))) m'

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
  x <- many alphaNumChar
  void space
  _ <- string "."
  void space
  Lam x <$> parseExp

parseVar = do
  a <- letterChar
  as <- many alphaNumChar
  void space
  pure $ Var (a : as)

parsePrim :: Parser Prim
parsePrim =
  ((string "+" <* space) $> Add)
    <|> ((string "-" <* space) $> Sub)
    <|> ((string "*" <* space) $> Mul)

parseInt = Int . read <$> some digitChar <* space

parens :: Parser a -> Parser a
parens = between (string "(" >> space) (string ")" >> space)
