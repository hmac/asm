{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Print where

import           Types
import           Data.Text.Prettyprint.Doc
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map

printDefinitions :: (a -> Doc b) -> Map String ([String], a) -> Doc b
printDefinitions printDef =
  vsep
    . map
        (\(n, (xs, d)) ->
          pretty n <+> hsep (map pretty xs) <+> equals <+> printDef d
        )
    . Map.toList

printExp :: Exp -> Doc a
printExp = go 0
 where
  go :: Int -> Exp -> Doc a
  go p = \case
    Var    x        -> pretty x
    Global g        -> pretty g
    Int    n        -> pretty n
    Bool   b        -> pretty b
    Lam xs e -> paren (p > 2) $ "\\" <> hsep (map pretty xs) <> "." <+> go 2 e
    App e1 e2       -> paren (p > 3) $ (go 3 e1) <+> go 4 e2
    Prim prim e1 e2 -> paren (p > 3) $ hsep [printPrim prim, go 4 e1, go 4 e2]
    Not e           -> paren (p > 3) $ hsep ["not", go 4 e]
    If b t e ->
      paren (p > 2) $ hsep ["if", go 0 b, "then", go 0 t, "else", go 0 e]
    Let x e1 e2 ->
      paren (p > 2) $ hsep ["let", pretty x, equals, go 0 e1, "in", go 0 e2]

printSExp :: SExp -> Doc a
printSExp = go 0
 where
  go :: Int -> SExp -> Doc a
  go p = \case
    SVar    x        -> pretty x
    SGlobal g        -> pretty g
    SInt    n        -> pretty n
    SBool   b        -> pretty b
    SApp e1 es       -> paren (p > 3) $ (go 3 e1) <+> hsep (map (go 4) es)
    SPrim prim e1 e2 -> paren (p > 3) $ hsep [printPrim prim, go 4 e1, go 4 e2]
    SNot e           -> paren (p > 3) $ hsep ["not", go 4 e]
    SIf b t e ->
      paren (p > 2) $ hsep ["if", go 0 b, "then", go 0 t, "else", go 0 e]

paren :: Bool -> Doc a -> Doc a
paren False = id
paren True  = parens

printPrim :: Prim -> Doc a
printPrim = \case
  Add -> "+"
  Sub -> "-"
  Mul -> "*"
  Eq  -> "="
  Gt  -> ">"
  Lt  -> "<"
  And -> "and"
  Or  -> "or"
