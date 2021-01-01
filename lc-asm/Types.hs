{-# LANGUAGE DeriveGeneric #-}
module Types where

import           GHC.Generics
import           Data.Aeson                     ( ToJSON )

data Exp
  = Lam [String] Exp
  | App Exp Exp
  | Var String
  | Global Int
  | Int Int
  | Bool Bool
  | Prim Prim Exp Exp
  | If Exp Exp Exp
  | Not Exp
  | Let String Exp Exp
  deriving (Generic, Eq, Show)

instance ToJSON Exp

data Prim = Add | Sub | Mul | Eq | Gt | Lt | And | Or
  deriving (Eq, Show, Generic)

instance ToJSON Prim

-- Supercombinator
type SC = ([String], SExp)

-- Supercombinator body
-- Like Exp but with no lambda
-- and applications are saturated
data SExp
  = SApp SExp [SExp] -- Note: the application head is either a SVar or SGlobal
  | SVar String
  | SGlobal String
  | SInt Int
  | SBool Bool
  | SNot SExp
  | SPrim Prim SExp SExp
  | SIf SExp SExp SExp
  deriving (Eq, Show, Generic)

instance ToJSON SExp

data Asm
  = Mov Op Op
  | IAdd Op Op
  | ISub Op Op
  | IMul Op Op
  | Call Op
  | Ret
  | Push Register
  | Pop Register
  | Cmp Op Op
  | Jmp Op
  | JmpEq Op
  | JmpGt Op
  | JmpLt Op
  | Label String
  | IAnd Op Op
  | IOr Op Op
  | INot Op
  | ShiftR Op Op
  deriving (Eq, Show, Generic)

instance ToJSON Asm

-- Operands are registers, labels, or literal integers
data Op = R PseudoReg | L String | I Int
  deriving (Eq, Show, Generic)

instance ToJSON Op

data Register = Register String
  deriving (Eq, Show, Generic)

instance ToJSON Register

data PseudoReg = Reg Register | Stack Int
  deriving (Eq, Show, Generic)

instance ToJSON PseudoReg
