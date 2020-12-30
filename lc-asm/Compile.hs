module Compile where

import           Types
import           Control.Monad                  ( foldM )
import           Control.Monad.Trans.State.Strict
                                                ( State
                                                , get
                                                , put
                                                )
import qualified Data.Stream                   as S

-- Compiling supercombinators to assembly

type Env = (Ctx, ScratchRegs, Renaming, StackCounter)

type Ctx = [(String, Register)]

type ScratchRegs = S.Stream Register

type Renaming = [(Register, PseudoReg)]

type StackCounter = Int

type Gen a = State Int a

-- To compile a supercombinator we map each argument to a register and then set up a circular
-- sequence of scratch registers to use.
-- For now we assume there are 4 scratch registers available.
compileSC :: SC -> Gen [Asm]
compileSC (vars, e) =
  let gamma = zipWith (\v i -> (v, argReg i)) vars [1 ..]
      delta = S.cycle
        [Register "r10", Register "r11", Register "r12", Register "r13"]
  in  do
        is <- compile (gamma, delta, mempty, 0) r0 e
        pure $ is <> [Ret]

-- This depends on the target architecture
-- and should really be configurable
r0 :: Register
r0 = Register "rax"

-- The x86-64 registers used for arguments.
-- TODO: handle functions of more than 6 arguments
argReg :: Int -> Register
argReg i = Register $ ["rdi", "rsi", "rdx", "rcx", "r8", "r9"] !! (i - 1)

compile :: Env -> Register -> SExp -> Gen [Asm]
-- Variables
-- ---------
-- Look up the variable in the context
-- If it's been pushed onto the stack, look up the correct stack address
compile (gamma, _delta, m, _xi) r (SVar x) =
  pure
    $ let register :: PseudoReg
          register = case lookup x gamma of
            Nothing  -> error $ "Unknown variable " <> show x
            Just reg -> case lookup reg m of
              Nothing -> Reg reg
              Just r' -> r'
      in  if register == Reg r then [] else [Mov (R (Reg r)) (R register)]

-- Global labels
-- -------------
compile _ r (SGlobal i    ) = pure [Mov (R (Reg r)) (L i)]

-- Integers
-- --------
compile _ r (SInt    i    ) = pure [Mov (R (Reg r)) (I i)]

-- Booleans
-- --------
compile _ r (SBool   True ) = pure [Mov (R (Reg r)) (I 1)]
compile _ r (SBool   False) = pure [Mov (R (Reg r)) (I 0)]

-- Binary primitives
-- -----------------
-- Compile e1 to the given register
-- Get a fresh scratch register and compile e2 to it.
-- Add the scratch register to the given register
compile env@(gamma, (S.Cons s delta), m, xi) r (SPrim p e1 e2) = do
  i1 <- compile env r e1
  i2 <- compile (gamma, delta, ((s, Stack xi) : m), xi + 1) s e2
  ip <- binaryPrim p r s
  pure $ i1 <> [Push s] <> i2 <> ip <> [Pop s]

-- Not
-- ---
-- This is special-cased because it's the only unary primitive.
compile env r (SNot e) = do
  i <- compile env r e
  pure $ i <> [INot (R (Reg r)), ShiftR (R (Reg r)) (I 63)]

-- Applications
-- ------------
-- Compile each argument to the corresponding arg register (pushing it first)
-- If the argument is already in the right register, don't push it
-- Call the function
-- Move the the result to the given register
-- Pop the arg registers
-- TODO: clean this up
compile (gamma, delta, m, xi) r (SApp f args) = do
  (prelude, postlude, m') <- foldM
    (\(pre, post, m') (e, i) -> case e of
      -- If the arg is a variable which happens to be in the right register, just leave it as-is
      SVar x | Just xr <- lookup x gamma, xr == argReg i -> pure ([], [], m')
      -- Otherwise, push the register, put the argument in it, then pop it after the call
      _ ->
        let ri  = argReg i
            m'' = ((ri, Stack (xi + i - 1)) : m') :: Renaming
        in  do
              is <- compile (gamma, delta, m'', xi + i) ri e
              pure (pre <> (Push ri : is), Pop ri : post, m'')
    )
    ([], [], m)
    (zip args [1 ..])
  let call = case f of
        SVar x -> case lookup x gamma of
          Nothing -> error $ "Unknown variable " <> show x
          Just xr -> case lookup xr m' of
            Just xr' -> [Call (R xr'), Mov (R (Reg r)) (R (Reg r0))]
            Nothing  -> [Call (R (Reg xr)), Mov (R (Reg r)) (R (Reg r0))]
        SGlobal l -> [Call (L l), Mov (R (Reg r)) (R (Reg r0))]
        _         -> error $ "Unexpected application head: " <> show f
  pure $ prelude <> call <> postlude

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
compile env r (SIf b t e) = do
  elseLabel <- newLabel
  endLabel  <- newLabel
  condition <- compile env r b
  then_     <- compile env r t
  else_     <- compile env r e
  pure
    $  condition
    <> [Cmp (R (Reg r)) (I 0), JmpEq (L elseLabel)]
    <> then_
    <> [Jmp (L endLabel), Label elseLabel]
    <> else_
    <> [Label endLabel]

newLabel :: State Int String
newLabel = do
  i <- get
  put (i + 1)
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
