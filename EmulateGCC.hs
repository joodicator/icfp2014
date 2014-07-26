module EmulateGCC where

import Prelude hiding (!!)
import Data.List hiding (!!)
import Control.Applicative

import GCC(Size(..), EAdr(..), IAdr(..), Atom(..), Code(..), Arith(..))

(!!) = genericIndex -- Needed since we are indexing on Word values.

--------------------------------------------------------------------------------
-- The state of an emulated GCC machine.
data GCC = GCC{
    gCode   :: [Code],   -- Program code.
    gPC     :: IAdr,     -- Program Counter register.
    gData   :: [Data],   -- Data stack.
    gFrame  :: [Frame],  -- Environment frame chain.
    gReturn :: [Return]} -- Control stack.

-- An entry in a data stack, environment frame, or cons cell.
data Data
  = DAtom Atom
  | DCons Data Data
  | DClos IAdr Frame

-- An (immutable) environment frame, excluding the parent frame pointer.
data Frame = Frame{
    fDummy  :: Boolean,
    fData   :: [Data]}

-- An entry in the control stack.
data Return
  = Return{ rPC::IAdr, rFrame::[Frame] }
  | ReturnStop

-- Reason for halting the machine.
data Halt = HStop | HFault String

--------------------------------------------------------------------------------
-- Monadic computation within a GCC, potentially halting or producing a value.
newtype RunGCC a = RunGCC (GCC -> (Either Halt a, GCC))

instance Monad RunGCC where
    RunGCC f >>= g
      = RunGCC $ \gcc -> case f gcc of
            (Right x, gcc') -> let RunGCC h = g x in h gcc'
            halted          -> halted
    return x
      = RunGCC $ \gcc -> (Right x, gcc)

instance Functor RunGCC where
    fmap f m = do x <- m; return (f x)

instance Applicative RunGCC where
    pure     = return
    mf <*> m = do f <- mf; x <- m; return (f x)

-- The following are as Control.Monad.State.{get,gets,put,modify}:
getGCC :: RunGCC GCC
getGCC = RunGCC $ \gcc -> (Left gcc, gcc)

getsGCC :: (GCC -> a) -> RunGCC a
getsGCC f = RunGCC $ \gcc -> (Left $ f gcc, gcc)

putGCC :: GCC -> RunGCC ()
putGCC gcc = RunGCC $ const (Right (), gcc)

modifyGCC :: (GCC -> GCC) -> RunGCC ()
modifyGCC f = RunGCC $ \gcc -> (Left (), f gcc)

-- Halt the computation erroneously.
fault :: String -> RunGCC a
fault str = RunGCC $ \gcc -> (Left $ Fault str, gcc)

--------------------------------------------------------------------------------
-- Run the machine until it halts, returning the final machine state and the
-- reason for halting.
runGCC :: GCC -> (Halt, GCC)
runGCC gcc = case stepGCC gcc of
    (Just halt, gcc') -> (halt, gcc')
    (Nothing,   gcc') -> runGCC gcc'

-- Execute a single instruction, returning the new machine state, and, if the
-- machine halted during this instruction, the reason for halting.
stepGCC :: GCC -> (Maybe Halt, GCC)
stepGCC gcc@GCC{ gCode=code, gPC=IAdr pc }
  | pc >= length code = (Just $ HFault "PC out of bounds", gcc)
  | otherwise         = stepGCC' (code!!pc) gcc{ gPC=IAdr(pc+1) }

--------------------------------------------------------------------------------
-- Execute the given instruction, assuming the PC has already been incremented.
stepGCC' :: Code -> GCC -> (Maybe Halt, GCC)

stepGCC' (LDC atom) gcc@GCC{ gData=ds }
  = (Nothing, gcc{ gData=(DAtom atom):ds })

stepGCC' (LD (FAdr fAdr) (EAdr eAdr)) gcc@GCC{ gData=ds, gFrame=fs }
  | fAdr >= length fs = (Just $ HFault "frame number out of bounds", gcc)
  | dummy             = (Just $ HFault "frame mismatch", gcc)
  | eAdr >= length es = (Just $ HFault "environment index out of bounds", gcc)
  | otherwise         = (Nothing, gcc{ gData=(es !! eAdr):ds })
  where Frame{ fData=es, fDummy=dummy } = fs !! fAdr

stepGCC' (CArith op) gcc = case gData gcc of
    DAtom x : DAtom y : ds -> (Nothing, gcc{ gData=(arith op x y):ds })
    _ : _ : ds             -> (Just $ HFault "type mismatch", gcc{ gData=ds })
    _                      -> (Just $ HFault "stack underflow", gcc)

stepGCC' ATOM gcc = case gData gcc of
    DAtom _ : ds -> (Nothing, gcc{ gData=DAtom(Atom 1):ds })
    _ : ds       -> (Nothing, gcc{ gData=DAtom(Atom 0):ds })
    _            -> (Just $ HFault "stack underflow", gcc)

stepGCC' CONS gcc = case gData gcc of
    x : y : ds -> (Nothing, gcc{ gData=(DCons x y):ds })
    _          -> (Just $ HFault "stack underflow", gcc)

stepGCC' CAR gcc = case gData gcc of
    DCons x _ : ds  -> (Nothing, gcc{ gData=x:ds })
    _ : ds          -> (Just $ HFault "type mismatch", gcc{ gData=ds })
    _               -> (Just $ HFault "stack underflow", gcc)

stepGCC' CDR gcc = case gData gcc of
    DCons _ y : ds  -> (Nothing, gcc{ gData=y:ds })
    _ : ds          -> (Just $ HFault "type mismatch", gcc{ gData=ds })
    _               -> (Just $ HFault "stack underflow", gcc)

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
