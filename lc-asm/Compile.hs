module Compile where

import Types

-- Compiling supercombinators to assembly

type Env = (Ctx, ScratchRegs, Renaming, StackCounter)

type Ctx = [(String, Register)]

type ScratchRegs = [Register]

type Renaming = [(Register, PseudoReg)]

type StackCounter = Int

-- To compile a supercombinator we map each argument to a register and then set up a circular
-- sequence of scratch registers to use.
-- For now we assume there are 4 scratch registers available.
compileSC :: SC -> [Asm]
compileSC (vars, e) =
  let gamma = zipWith (\v i -> (v, argReg i)) vars [1 ..]
      delta = Register "r10" : Register "r11" : Register "r12" : Register "r13" : delta
   in compile (gamma, delta, mempty, 0) r0 e <> [Ret]

compile :: Env -> Register -> SExp -> [Asm]
-- Variables
-- ---------
-- Look up the variable in the context
-- If it's been pushed onto the stack, look up the correct stack address
compile (gamma, delta, m, xi) r (SVar x) =
  let register :: PseudoReg
      register = case lookup x gamma of
        Nothing -> error $ "Unknown variable " <> show x
        Just reg -> case lookup reg m of
          Nothing -> Reg reg
          Just r' -> r'
   in if register == Reg r then [] else [Mov (R (Reg r)) (R register)]
-- Global labels
-- -------------
-- This is just an application with no arguments.
compile env r (SGlobal i) = compile env r (SApp (SGlobal i) [])
-- Integers
-- --------
compile _ r (SInt i) = [Mov (R (Reg r)) (I i)]
-- Binary primitives
-- -----------------
-- Compile e1 to the given register
-- Get a fresh scratch register and compile e2 to it.
-- Add the scratch register to the given register
compile env@(gamma, (s : delta), m, xi) r (SPrim p e1 e2) =
  let i1 = compile env r e1
      i2 = compile (gamma, delta, ((s, Stack xi) : m), xi + 1) s e2
   in i1 <> [Push s] <> i2 <> binaryPrim p r s <> [Pop s]
-- Applications
-- ------------
-- Compile each argument to the corresponding arg register (pushing it first)
-- Call the function
-- Move the the result to the given register
-- Pop the arg registers
-- TODO: clean this up
compile (gamma, delta, m, xi) r (SApp f args) =
  let (prelude, _) =
        foldl
          ( \(is, m') (e, i) ->
              let ri = argReg i
                  m'' :: Renaming
                  m'' = ((ri, Stack (xi + i -1)) : m')
               in (Push ri : compile (gamma, delta, m'', xi + i) ri e, m'')
          )
          ([], m)
          (zip args [1 ..])
      postlude = map (\(_, i) -> Pop (argReg i)) $ reverse $ zip args [1 ..]
      call = case f of
        SVar x -> case lookup x gamma of
          Nothing -> error $ "Unknown variable " <> show x
          Just xr -> [Call (R (Reg xr)), Mov (R (Reg r)) (R (Reg r0))]
        SGlobal i -> [Call (L i), Mov (R (Reg r)) (R (Reg r0))]
   in prelude <> call <> postlude

-- This depends on the target architecture
-- and should really be configurable
r0 :: Register
r0 = Register "rax"

-- The x86-64 registers used for arguments.
-- TODO: handle functions of more than 6 arguments
argReg :: Int -> Register
argReg i = Register $ ["rdi", "rsi", "rdx", "rcx", "r8", "r9"] !! (i -1)

binaryPrim :: Prim -> Register -> Register -> [Asm]
binaryPrim Add r1 r2 = [IAdd (R (Reg r1)) (R (Reg r2))]
binaryPrim Sub r1 r2 = [ISub (R (Reg r1)) (R (Reg r2))]
binaryPrim Mul r1 r2 = [IMul (R (Reg r1)) (R (Reg r2))]
