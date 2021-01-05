{-# LANGUAGE LambdaCase #-}
module Compile where

import           Types
import           Data.Functor                   ( (<&>) )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.Reader     ( ReaderT
                                                , asks
                                                , local
                                                , runReaderT
                                                )
import           Control.Monad.Trans.State.Strict
                                                ( State
                                                , get
                                                , put
                                                )
import qualified Data.Stream                   as S

-- Compiling supercombinators to assembly

data Env = Env
  { context :: Ctx
  , scratchRegisters :: ScratchRegs
  , renaming :: Renaming
  , stackCounter :: StackCounter
  }

type Ctx = [(String, Register)]

type ScratchRegs = S.Stream Register

type Renaming = [(Register, PseudoReg)]

type StackCounter = Int

type Gen a = ReaderT Env (State Int) a

runGen :: Env -> Gen a -> State Int a
runGen env g = runReaderT g env

-- To compile a supercombinator we map each argument to a register and then set up a circular
-- sequence of scratch registers to use.
-- For now we assume there are 4 scratch registers available.
compileSC :: SC -> State Int [Asm]
compileSC (vars, e) =
  let gamma = zipWith (\v i -> (v, argReg i)) vars [1 ..]
      delta = S.cycle
        [Register "r10", Register "r11", Register "r12", Register "r13"]
      rbx = Register "rbx"
  in  do
        is <- runGen (Env gamma delta mempty 0) (compile rbx e)
        pure
          $  [Push rbx]
          <> is
          <> [Mov (R (Reg r0)) (R (Reg rbx)), Pop rbx, Ret]

-- This depends on the target architecture
-- and should really be configurable
r0 :: Register
r0 = Register "rax"

-- The x86-64 registers used for arguments.
-- TODO: handle functions of more than 6 arguments
argReg :: Int -> Register
argReg i = Register $ ["rdi", "rsi", "rdx", "rcx", "r8", "r9"] !! i

compile :: Register -> SExp -> Gen [Asm]
-- Variables
-- ---------
-- Look up the variable in the context
-- If it's been pushed onto the stack, look up the correct stack address
compile r (SVar x) = do
  xi       <- asks stackCounter
  register <- lookupVar x <&> \case
    Stack i ->
      -- if the register is in stack position i, we want the offset
      -- from the stack pointer, which will be at stack position
      -- <length of stack - 1>.
      Stack (xi - i - 1)
    r' -> r'
  pure $ if register == Reg r then [] else [Mov (R (Reg r)) (R register)]

-- Global labels
-- -------------
compile r (SGlobal i    ) = pure [Mov (R (Reg r)) (L i)]

-- Integers
-- --------
compile r (SInt    i    ) = pure [Mov (R (Reg r)) (I i)]

-- Booleans
-- --------
compile r (SBool   True ) = pure [Mov (R (Reg r)) (I 1)]
compile r (SBool   False) = pure [Mov (R (Reg r)) (I 0)]

-- Binary primitives
-- -----------------
-- Compile e1 to the given register
-- Get a fresh scratch register and compile e2 to it.
-- Add the scratch register to the given register
compile r (SPrim p e1 e2) = do
  i1   <- compile r e1
  rest <- withScratchRegister $ \s -> do
    i2 <- compile s e2
    ip <- binaryPrim p r s
    pure $ i2 <> ip
  pure $ i1 <> rest

-- Not
-- ---
-- This is special-cased because it's the only unary primitive.
compile r (SNot e) = do
  i <- compile r e
  pure $ i <> [INot (R (Reg r)), ShiftR (R (Reg r)) (I 63)]

-- Applications
-- ------------
-- Compile each argument to the corresponding arg register (pushing it first)
-- TODO: if the argument is already in the right register, don't push it
-- Call the function
-- Move the the result to the given register
-- Pop the arg registers
compile r (SApp f args) = do
  let call = case f of
        SVar x ->
          lookupVar x <&> \xr -> [Call (R xr), Mov (R (Reg r)) (R (Reg r0))]
        SGlobal l -> pure [Call (L l), Mov (R (Reg r)) (R (Reg r0))]
        _         -> error $ "Unexpected application head: " <> show f
  compileArguments (zip [1 ..] args) call

-- If expressions
-- --------------
-- Compile to: [condition]
--             compare <reg> 0
--             jump-if-equal else
--             [then]
--             jump end
--       else: [else]
--       end:
--
-- We can re-use the same register for all three sections
compile r (SIf b t e) = do
  elseLabel <- newLabel
  endLabel  <- newLabel
  condition <- compile r b
  then_     <- compile r t
  else_     <- compile r e
  pure
    $  condition
    <> [Cmp (R (Reg r)) (I 0), JmpEq (L elseLabel)]
    <> then_
    <> [Jmp (L endLabel), Label elseLabel]
    <> else_
    <> [Label endLabel]

compileArguments :: [(Int, SExp)] -> Gen [Asm] -> Gen [Asm]
compileArguments []                   call = call
-- if a is a variable already in argReg i, don't compile bother compiling it
-- if it's a variable in another register, just move it to argReg i
compileArguments ((i, SVar x) : args) call = do
  xr <- lookupVar x
  if xr == Reg (argReg i)
    then compileArguments args call
    else (Mov (R (Reg (argReg i))) (R xr) :) <$> compileArguments args call
compileArguments ((i, a) : args) call = withRegister (argReg i) $ do
  a_is <- compile (argReg i) a
  rest <- compileArguments args call
  pure $ a_is <> rest

-- Save the original contents of the register for the duration of the computation:
-- push r
-- <computation>
-- pop r
withRegister :: Register -> Gen [Asm] -> Gen [Asm]
withRegister r g = do
  m  <- asks renaming
  xi <- asks stackCounter
  let m'  = (r, Stack xi) : m
      xi' = succ xi
  is <- local (\env -> env { renaming = m', stackCounter = xi' }) g
  pure $ [Push r] <> is <> [Pop r]

withScratchRegister :: (Register -> Gen [Asm]) -> Gen [Asm]
withScratchRegister g = do
  S.Cons s delta <- asks scratchRegisters
  local (\env -> env { scratchRegisters = delta }) $ withRegister s (g s)


lookupVar :: String -> Gen PseudoReg
lookupVar x = do
  gamma <- asks context
  m     <- asks renaming
  pure $ case lookup x gamma of
    Nothing -> error $ "Unknown variable " <> show x
    Just r  -> case lookup r m of
      Just r' -> r'
      Nothing -> Reg r

newLabel :: Gen String
newLabel = do
  i <- lift get
  lift $ put (i + 1)
  pure $ "__" <> show i

binaryPrim :: Prim -> Register -> Register -> Gen [Asm]
binaryPrim Add r1 r2 = pure [IAdd (R (Reg r1)) (R (Reg r2))]
binaryPrim Sub r1 r2 = pure [ISub (R (Reg r1)) (R (Reg r2))]
binaryPrim Mul r1 r2 = pure [IMul (R (Reg r1)) (R (Reg r2))]
binaryPrim Eq  r1 r2 = binaryBooleanPrim JmpEq r1 r2
binaryPrim Gt  r1 r2 = binaryBooleanPrim JmpGt r1 r2
binaryPrim Lt  r1 r2 = binaryBooleanPrim JmpLt r1 r2
-- TODO: we can probably optimise this using a sub-register
binaryPrim And r1 r2 = pure [IAnd (R (Reg r1)) (R (Reg r2))]
binaryPrim Or  r1 r2 = pure [IOr (R (Reg r1)) (R (Reg r2))]

-- Primitives like =, >, < all have the same form, differing only in the kind
-- of jump instruction they use
binaryBooleanPrim :: (Op -> Asm) -> Register -> Register -> Gen [Asm]
binaryBooleanPrim p r1 r2 = do
  true <- newLabel
  end  <- newLabel
  pure
    [ Cmp (R (Reg r1)) (R (Reg r2))
    , p (L true)
    , Mov (R (Reg r1)) (I 0)
    , Jmp (L end)
    , Label true
    , Mov (R (Reg r1)) (I 1)
    , Label end
    ]
