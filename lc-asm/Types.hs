module Types where

data Exp
  = Lam [String] Exp
  | App Exp Exp
  | Var String
  | Global Int
  | Int Int
  | Bool Bool
  | Prim Prim Exp Exp
  deriving (Eq, Show)

data Prim = Add | Sub | Mul
  deriving (Eq, Show)

-- Supercombinator
type SC = ([String], SExp)

-- Supercombinator body
-- Like Exp but with no lambda
-- and applications are saturated
data SExp
  = SApp SExp [SExp] -- Note: the application head is either a SVar or SGlobal
  | SVar String
  | SGlobal Int
  | SInt Int
  | SBool Bool
  | SPrim Prim SExp SExp
  deriving (Eq, Show)

data Asm = Mov Op Op | IAdd Op Op | ISub Op Op | IMul Op Op | Call Op | Ret | Push Register | Pop Register
  deriving (Eq, Show)

-- Operands are registers, labels, or literal integers
data Op = R PseudoReg | L Int | I Int
  deriving (Eq, Show)

data Register = Register String
  deriving (Eq, Show)

data PseudoReg = Reg Register | Stack Int
  deriving (Eq, Show)
