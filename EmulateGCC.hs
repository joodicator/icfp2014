module EmulateGCC where

import Prelude hiding ((!!), length)
import Data.List hiding ((!!), length, splitAt)
import Control.Applicative
import Control.Monad

import GCC

(!!)    = genericIndex
length  = genericLength
splitAt = genericSplitAt

--------------------------------------------------------------------------------
-- The state of an emulated GCC machine.
data GCC = GCC{
    gCode   :: [Code],   -- Program code.
    gPC     :: IAdr,     -- Program Counter register.
    gData   :: [Data],   -- Data stack.
    gFrame  :: [Frame],  -- Environment frame chain.
    gReturn :: [Return]} -- Control stack.
  deriving Show

-- An entry in a data stack, environment frame, or cons cell.
data Data
  = DAtom Atom
  | DCons Data Data
  | DClos IAdr Frame
  deriving Show

-- An (immutable) environment frame, excluding the parent frame pointer.
data Frame
  = Frame{ fData::[Data] }
  | DummyFrame{ fSize::Size }
  deriving Show

-- An entry in the control stack.
data Return
  = ReturnClos{ rPC::IAdr, rFrame::[Frame] }
  | ReturnJoin{ rPC::IAdr }
  | ReturnStop
  deriving Show

-- Reason for halting the machine.
data Halt
  = HStop | HFault String
  deriving Show

--------------------------------------------------------------------------------
-- Monadic computation within a GCC, potentially halting or producing a value.
newtype RunGCC a = RunGCC{ unRunGCC :: GCC -> (Either Halt a, GCC) }

instance Monad RunGCC where
    RunGCC f >>= g
      = RunGCC $ \gcc -> case f gcc of
            (Right x, gcc')   -> let RunGCC h = g x in h gcc'
            (Left halt, gcc') -> (Left halt, gcc')
    return x
      = RunGCC $ \gcc -> (Right x, gcc)

instance Functor RunGCC where
    fmap f m = do x <- m; return (f x)

instance Applicative RunGCC where
    pure     = return
    mf <*> m = do f <- mf; x <- m; return (f x)

-- The following are as Control.Monad.State.{get,gets,put,modify}:
getGCC :: RunGCC GCC
getGCC = RunGCC $ \gcc -> (Right gcc, gcc)

getsGCC :: (GCC -> a) -> RunGCC a
getsGCC f = RunGCC $ \gcc -> (Right $ f gcc, gcc)

putGCC :: GCC -> RunGCC ()
putGCC gcc = RunGCC $ const (Right (), gcc)

modifyGCC :: (GCC -> GCC) -> RunGCC ()
modifyGCC f = RunGCC $ \gcc -> (Right (), f gcc)

-- Halt the computation normally.
stop :: RunGCC a
stop = RunGCC $ \gcc -> (Left $ HStop, gcc)

-- Halt the computation erroneously.
fault :: String -> RunGCC a
fault str = RunGCC $ \gcc -> (Left $ HFault str, gcc)

--------------------------------------------------------------------------------
-- Run the machine until it halts, returning the final machine state and the
-- reason for halting.
runGCC :: GCC -> (Halt, GCC)
runGCC gcc = case unRunGCC stepGCC gcc of
    (Right _, gcc')   -> runGCC gcc'
    (Left halt, gcc') -> (halt, gcc')

-- Execute a single instruction.
stepGCC :: RunGCC ()
stepGCC = do
    gcc@GCC{ gCode=code, gPC=IAdr pc } <- getGCC
    unless (pc < length code) (fault "PC out of bounds")
    putGCC gcc{ gPC=IAdr(pc+1) }
    stepGCC' (code !! pc)

--------------------------------------------------------------------------------
-- Execute the given instruction, assuming the PC has already been incremented.
stepGCC' :: Code -> RunGCC ()

stepGCC' (LDC atom) = do
    push (DAtom atom)

stepGCC' (LD (FAdr fAdr) (EAdr eAdr)) = do
    gcc@GCC{ gData=ds, gFrame=fs } <- getGCC
    unless (fAdr < length fs) (fault "frame number out of bounds")
    case fs !! fAdr of
        Frame{ fData=es } -> do
            unless (eAdr < length es) (fault "environment index out of bounds")
            push (es !! eAdr)
        _ -> fault "frame mismatch"

stepGCC' (CArith op) = do
    [x, y] <- popN 2
    case (x,y) of
        (DAtom x', DAtom y') -> push (DAtom $ arith op x' y')
        _                    -> fault "type mismatch"

stepGCC' ATOM = do
    d <- pop
    let bool = case d of { DAtom _ -> 1; _ -> 0 }
    push $ DAtom (Atom bool)

stepGCC' CONS = do
    [x, y] <- popN 2
    push $ DCons x y

stepGCC' CAR = do
    d <- pop
    case d of
        DCons x _ -> push x
        _         -> fault "type mismatch"

stepGCC' (SEL (IAdr trueAddr) (IAdr falseAddr)) = do
    d <- pop
    case d of
        DAtom bool -> modifyGCC $ \gcc@GCC{ gReturn=rs, gPC=pc } ->
            gcc{ gPC = if bool /= 0 then trueAddr else falseAddr,
                 gReturn = ReturnJoin{ rPC=pc } }
        _ -> fault "type mismatch"

stepGCC' JOIN = do
    r <- popReturn
    case r of
        ReturnJoin{ rPC=pc } -> modifyGCC $ \gcc -> gcc{ gPC=pc }
        _                    -> fault "control mismatch"

stepGCC' (LDF (IAdr iAdr)) = do
    fs <- getsGCC gFrame
    push $ DClos iAdr fs

stepGCC' (AP (Size argSize)) = do
    gcc@GCC{ gData=ds, gFrame=fs, gReturn=rs, gPC=pc } <- getGCC
    d <- pop
    case d of
        DClos iAdr fs' -> do
            args <- popN argSize
            putGCC gcc{ gPC=iAdr,
                        gFrame=Frame{ fData=args }:fs',
                        gReturn=ReturnClos{ rPC=pc, rFrame=fs }:rs }
        _ -> fault "type mismatch"

stepGCC' RTN = do
    r <- popReturn
    case r of
        ReturnClos{ rPC=pc, rFrame=fs } ->
            modifyGCC $ \gcc -> gcc{ gPC=pc, gFrame=fs }
        ReturnStop -> stop
        _          -> fault "control mismatch"

stepGCC' (DUM dumSize) = do
    modifyGCC $ \gcc@GCC{ gFrame=fs } ->
        gcc{ gFrame=DummyFrame{ fSize=dumSize }:fs }

stepGCC' (RAP (Size argSize)) = do
    d <- pop
    gcc@GCC{ gFrame=fs }
    case (d,fs) of
        (DClos iAdr fs', :) ->
            
        _ -> fault "type mismatch"

-- Remove a value from atop the data stack and return it.
pop :: RunGCC Data
pop = head <$> popN 1

-- Remove n values from atop the data stack and return them in reverse order.
popN :: Integral i => i -> RunGCC [Data]
popN n = do
    gcc@GCC{ gData=ds } <- getGCC
    let (ds',ds'') = splitAt n ds
    putGCC gcc{ gData=ds'' }
    unless (length ds' == n) (fault "data stack underflow")
    return (reverse ds')

-- Insert a value at the top of the data stack.
push :: Data -> RunGCC ()
push d = do
    modifyGCC $ \gcc@GCC{ gData=ds } -> gcc{ gData=d:ds }

-- Remove the top entry of the control stack and return it.
popReturn :: RunGCC Return
popReturn = do
    gcc@GCC{ gReturn=rs } <- getGCC
    case rs of
        r:rs' -> putGCC gcc{ gReturn=rs' } >> return r
        _     -> fault "control stack underflow"

--------------------------------------------------------------------------------
-- Perform a binary arithmetic operation.
arith :: Arith -> Atom -> Atom -> Atom
arith op (Atom x) (Atom y) = case op of
    ADD  -> Atom $ x + y
    SUB  -> Atom $ x - y
    MUL  -> Atom $ x * y
    DIV  -> Atom $ x `div` y
    CEQ  -> Atom $ if x == y then 1 else 0
    CGT  -> Atom $ if x > y  then 1 else 0
    CGTE -> Atom $ if x >= y then 1 else 0
