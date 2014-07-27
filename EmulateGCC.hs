{-# LANGUAGE NoMonomorphismRestriction #-}

module EmulateGCC where

import Prelude   hiding ((!!), length)
import Data.List hiding ((!!), length)
import Data.IORef
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import System.Mem.Weak

import Halt
import GCC

--------------------------------------------------------------------------------
(!!)   = genericIndex
length = genericLength

--------------------------------------------------------------------------------
data Word
  = WAtom Atom
  | WCons (IORef Cons)

data Cons
  = Cons{ car::Word, cdr::Word }

-- A data stack entry.
data Stack = Stack{
    sParent :: Maybe (IORef Stack),
    sData   :: Word }

-- An environment frame.
data Frame = Frame{
    fParent :: Maybe (IORef Frame),
    fData   :: [Word],
    fDummy  :: Bool }

-- A control stack frame.
data Return
  = ReturnCall{
        rParent::Maybe(IORef Return), rPC::InsAdr, rFrame::Maybe(IORef Frame) }
  | ReturnJoin{
        rParent::Maybe(IORef Return), rPC::InsAdr }
  | ReturnExit

-- The memory manager's record of an object allocated on the heap.
data Heap
  = HCons   (Weak (IORef Cons))
  | HFrame  (Weak (IORef Frame))
  | HReturn (Weak (IORef Return))
  | HStack  (Weak (IORef Stack))

-- The resource limits of a GCC instance.
data Limits = Limits{   
    lHeapSize::Size,  -- Heap memory size, where a CONS cell occupies 2 units.
    lCodeSize::Size } -- Instruction memory size, in single instructions.

stdLimits = Limits{
    lHeapSize=20000000,
    lCodeSize=1048576 }

-- The state of a GCC computation.
data GCCState = GCCState{
    gLimits :: Limits,
    gCode   :: [Code],                -- Currently executing program.
    gHeap   :: [Heap],                -- Heap records for allocated objects.
    gPC     :: InsAdr,                -- Program counter.
    gData   :: Maybe (IORef Stack),   -- Data stack.
    gFrame  :: Maybe (IORef Frame),   -- Environment frame.
    gReturn :: Maybe (IORef Return) } -- Control stack.

stdEmptyState = GCCState{
    gLimits = stdLimits,
    gCode   = [],
    gHeap   = [],
    gPC     = 0,
    gData   = Nothing,
    gFrame  = Nothing,
    gReturn = Nothing }

--------------------------------------------------------------------------------
data HaltReason
  = Stop | Fault Fault
  deriving Show

data Fault
  = TypeMismatch  | FrameMismatch    | ControlMismatch
  | DataUnderflow | FrameUnderflow   | ControlUnderflow
  | PCOutOfBounds | FrameOutOfBounds | DataOutOfBounds
  | HeapOverflow  | CodeOverflow
  deriving Show

type GCC a
  = StateT GCCState (HaltT HaltReason IO) a

--------------------------------------------------------------------------------
-- Given the Heap constructor for an object, and the object itself, allocate
-- the object on the heap and return the corresponding IORef.
alloc :: (Weak (IORef a) -> Heap) -> a -> GCC (IORef a)
alloc con obj = do
    ioRef <- liftIO $ newIORef obj
    wkRef <- liftIO $ mkWeakIORef ioRef (return ())
    let heapObj = con wkRef

    s@GCCState{ gHeap=hs, gLimits=Limits{ lHeapSize=hSizeMax } } <- get
    hSize <- gc; oSize <- size heapObj
    when (hSize + oSize > hSizeMax) (lift . halt $ Fault HeapOverflow)

    put s{ gHeap=heapObj:hs }
    return ioRef

-- Perform garbage collection, reporting the amount of heap space in use.
gc :: GCC Size
gc = do
    s@GCCState{ gHeap=heap } <- get
    sizes <- mapM size heap
    put s{ gHeap=[h | (h,sz) <- zip heap sizes, sz > 0] }
    return (sum sizes)

-- The space occupied in the heap by the given object,
-- or 0 if it has been deallocated.
size :: Heap -> GCC Size
size (HCons   w) = size' w $ const 2
size (HFrame  w) = size' w $ \Frame{ fData=xs } -> 2 + length xs
size (HReturn w) = size' w $ const 2
size (HStack  w) = size' w $ const 1

size' :: (Weak (IORef a)) -> (a -> Size) -> GCC Size
size' wkRef size
  = liftIO $ deRefWeak wkRef >>= \mbRef -> case mbRef of
        Nothing    -> return 0
        Just ioRef -> size <$> readIORef ioRef
