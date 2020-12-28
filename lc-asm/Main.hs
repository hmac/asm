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

-- | CLI
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)

-- | Parsing and printing
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Pretty.Simple (pPrint)

-- | Web server
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.HTTP.Types.Status (status400, status200)
import Data.ByteString.Lazy.Char8 (unpack, pack)

import Types
import Compile (compileSC, r0, argReg)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> asm
    ["parse"] -> parseInput >>= pPrint
    ["asm"] -> asm
    ["lift"] -> lambdaLift >>= pPrint
    ["super"] -> super >>= pPrint
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
         Left err -> do let errStr = pack $ errorBundlePretty err
                        Wai.responseLBS status400 mempty errStr
         Right expr -> do let lifted = lift expr
                              scs = Map.map supercombinate lifted
                              asm = printProgram scs
                          Wai.responseLBS status200 mempty (pack asm)


parseInput = do
  input <- getContents
  case parse (parseExp <* eof) "<stdin>" input of
    Left err -> putStrLn (errorBundlePretty err) >> exitWith (ExitFailure 1)
    Right expr -> pure expr

lambdaLift = do
  expr <- parseInput
  pure (lift expr)

super = do
  defs <- lambdaLift
  pure $ Map.map supercombinate defs

asm = do
  scs <- super
  putStrLn $ printProgram scs

-- Idea:
-- Convert lambda calculus with just integers into assembly
-- 1. Fully lambda lift
-- 2. Convert each (now top-level) function to an assembly function

-- The Int is a counter for naming new definitions
type Env = (Int, Map String Exp)

-- Simple algorithm:
-- 1. If there are no lambda abstractions, finish
-- 2. Choose any lambda which has no inner lambdas in its body
-- 3. Abstract its free variables as extra parameters
-- 4. Name it and put it in the environment
-- 5. Replace the occurrence of the lambda by the name applied ot the free variables

-- TODO: lift multi-lambdas into multi-supercombinators

lift :: Exp -> Map String Exp
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
            scs' = Map.insert ("_" <> show i) sc scs
            lam' = foldr (flip App . Var) (Global i) vars
         in -- replace the lambda with a variable applied to the free vars and repeat
            lift' (i + 1, scs') (hole lam')
      Nothing -> (i, Map.insert "_main" expr scs)

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
          [ ("main", ([], SApp (SGlobal 1) [SInt 4])),
            ("0", (["x", "y"], SPrim Add (SVar "x") (SVar "y"))),
            ("1", (["x"], SApp (SGlobal 0) [SVar "x", SVar "x"]))
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

printAsm :: Asm -> String
printAsm = \case
  Mov o1 o2 -> "mov " <> printOp o1 <> ", " <> printOp o2
  IAdd o1 o2 -> "add " <> printOp o1 <> ", " <> printOp o2
  ISub o1 o2 -> "sub " <> printOp o1 <> ", " <> printOp o2
  IMul o1 o2 -> "imul " <> printOp o1 <> ", " <> printOp o2
  Push r -> "push " <> printReg r
  Pop r -> "pop " <> printReg r
  Call o -> "call " <> printOp o
  Ret -> "ret"

printOp :: Op -> String
printOp = \case
  R r -> printPseudoReg r
  L i -> "_" <> show i
  I i -> show i

printReg :: Register -> String
printReg (Register r) = r

printPseudoReg :: PseudoReg -> String
printPseudoReg (Reg r) = printReg r
printPseudoReg (Stack i) = "[rsp+" <> show i <> "]"

printProgram :: Map String SC -> String
printProgram m =
  let defs = map (\(l, f) -> (l, compileSC f)) $ Map.toList m
   in foldMap (\(l, e) -> unlines $ (l <> ":") : map (("  " <>) . printAsm) e) defs

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
