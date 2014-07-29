{-# LANGUAGE NoMonomorphismRestriction #-}

module EmulateGCC where

import Prelude   hiding ((!!), length, replicate, sequence)
import Data.List hiding ((!!), length, replicate)

import Data.Traversable (sequence)
import Data.IORef
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.State.Strict hiding (sequence)
import System.Mem.Weak

import Halt
import GCC

--------------------------------------------------------------------------------
(!!)      = genericIndex
length    = genericLength
replicate = genericReplicate

--------------------------------------------------------------------------------
-- The data held in a data stack entry or an environment frame address.
data Word
  = WAtom Atom
  | WCons (IORef Cons)
  | WClos (IORef Closure)

instance Show Word where
    show (WAtom x) = "WAtom " ++ show (fromIntegral x)
    show (WCons _) = "<WCons>"
    show (WClos _) = "<WClos>"

-- A CONS cell.
data Cons
  = Cons{ car::Word, cdr::Word }

-- A function closure cell.
data Closure = Closure{
    cCode  :: InsAdr,
    cFrame :: Maybe (IORef Frame)}

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
  | ReturnStop{
        rParent::Maybe(IORef Return) }

-- The memory manager's record of an object allocated on the heap.
data Heap
  = HCons   (Weak (IORef Cons))
  | HClos   (Weak (IORef Closure))
  | HFrame  (Weak (IORef Frame))
  | HReturn (Weak (IORef Return))
  | HStack  (Weak (IORef Stack))

instance Show Heap where
    show (HCons   _) = "<HCons>"
    show (HClos   _) = "<HClos>"
    show (HFrame  _) = "<HFrame>"
    show (HReturn _) = "<HReturn>"
    show (HStack  _) = "<HStack>"

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

instance Show GCCState where
    show _ = "<GCCState>"

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
  = HaltT HaltReason (StateT GCCState IO) a

-- Run a GCC computation in the IO monad.
runGCC :: GCC a -> GCCState -> IO (Either HaltReason a, GCCState)
runGCC = runStateT . runHaltT

-- Run a GCC non-halting GCC compution in the IO monad.
runGCC' :: GCC a -> GCCState -> IO (a, GCCState)
runGCC' gcc s
  = runGCC gcc s >>= \e -> case e of
        (Left h, _)  -> error $ "runGCC': " ++ show h
        (Right x, s) -> return (x, s)

-- Execute instructions until the machine halts,
-- intercepting the halt and returning its description.
runUntilHalt :: GCC HaltReason
runUntilHalt = do
    Left h <- catchHalt $ forever step
    return h

-- Execute instructions until the machine stops normally.
runUntilStop :: GCC ()
runUntilStop = do
    runUntilHalt >>= \h -> case h of
        Stop  -> return ()
        fault -> halt fault

-- Execute a single instruction.
step :: GCC ()
step = do
    GCCState{ gPC=pc, gCode=code } <- lift get
    unless (pc < length code) (halt $ Fault PCOutOfBounds)
    lift . modify $ \s -> s{ gPC=(pc+1) }
    instr (code !! pc)

-- Load a program into code memory, replacing any previous program.
loadProgram :: [Code] -> GCC ()
loadProgram code = do
    GCCState{ gLimits=Limits{ lCodeSize=maxSize } } <- lift get
    unless (length code <= maxSize) (halt $ Fault CodeOverflow)
    lift . modify $ \s -> s{ gCode=code }

--------------------------------------------------------------------------------
-- Execute the given instruction, assuming the PC has already been incremented.
instr :: Code -> GCC ()

instr (LDC x)
  = pushStack (WAtom x)

instr (LD frmAdr envAdr) = do
    Frame{ fData=xs, fDummy=dummy } <- liftIO . readIORef =<< getFrame frmAdr
    when dummy (halt $ Fault FrameMismatch)
    unless (envAdr < length xs) (halt $ Fault DataOutOfBounds)
    pushStack (xs !! envAdr)

instr (CArith op)
  = popStack 2 >>= \p -> case p of
        [WAtom x, WAtom y] -> pushStack $ WAtom (arith op x y)
        [_, _]             -> halt $ Fault TypeMismatch

instr ATOM = do
    popStack 1 >>= \p -> case p of
        [WAtom _]   -> pushStack $ WAtom 1
        [_]         -> pushStack $ WAtom 0

instr CONS = do
    [x, y] <- popStack 2
    ref <- alloc HCons (Cons x y)
    pushStack $ WCons ref

instr CAR = do
    popStack 1 >>= \p -> case p of
        [WCons ref] -> pushStack . car =<< liftIO (readIORef ref)
        [_]         -> halt $ Fault TypeMismatch

instr CDR = do
    popStack 1 >>= \p -> case p of
        [WCons ref] -> pushStack . cdr =<< liftIO (readIORef ref)
        [_]         -> halt $ Fault TypeMismatch

instr (SEL trueAdr falseAdr) = do
    popStack 1 >>= \p -> case p of
        [WAtom x] -> do
            pc <- lift $ gets gPC
            pushReturn ReturnJoin{ rPC=pc, rParent=Nothing }
            let pc' = if x /= 0 then trueAdr else falseAdr
            lift . modify $ \s -> s{ gPC=pc' }
        [_] -> halt $ Fault TypeMismatch

instr JOIN = do
    popReturn >>= \ret -> case ret of
        ReturnJoin{ rPC=pc' } -> lift . modify $ \s -> s{ gPC=pc' }
        _                     -> halt $ Fault ControlMismatch

instr (LDF insAdr) = do
    GCCState{ gFrame=frame } <- lift get
    ref <- alloc HClos Closure{ cCode=insAdr, cFrame=frame }
    pushStack (WClos ref)

instr (AP size) = do
    GCCState{ gPC=oldPC, gFrame=oldFrm, gReturn=oldRet } <- lift get
    popStack 1 >>= \p -> case p of
        [WClos cloRef] -> do
            Closure{ cCode=newPC, cFrame=cloFrm } <- liftIO $ readIORef cloRef
            args <- popStack size
            newFrm <- alloc HFrame Frame{
                fParent=cloFrm, fData=args, fDummy=False }
            pushReturn ReturnCall{ rPC=oldPC, rFrame=oldFrm, rParent=Nothing }
            lift . modify $ \s -> s{ gPC=newPC, gFrame=(Just newFrm) }
        [_] -> halt $ Fault TypeMismatch

instr RTN = do
    popReturn >>= \ret -> case ret of
        ReturnCall{ rPC=pc', rFrame=frame' } ->
            lift . modify $ \s -> s{ gPC=pc', gFrame=frame' }
        ReturnStop{} -> halt Stop
        _            -> halt $ Fault ControlMismatch

instr (DUM size) = do
    s@GCCState{ gFrame=oldFrm } <- lift get
    let args = replicate size (WAtom 0)
    newFrm <- alloc HFrame Frame{ fParent=oldFrm, fDummy=True, fData=args }
    lift . modify $ \s -> s{ gFrame=(Just newFrm) }

instr (RAP size) = do
    [word] <- popStack 1
    s@GCCState{ gPC=oldPC, gFrame=curFrm } <- lift get
    case (word, curFrm) of
        (WClos cloRef, Just frmRef) -> do
            Closure{ cCode=cloPC, cFrame=cloFrm } <- liftIO $ readIORef cloRef
            frm@Frame{ fParent=retFrm, fData=args } <- liftIO $ readIORef frmRef
            unless (fDummy frm)          (halt $ Fault FrameMismatch)
            unless (length args == size) (halt $ Fault FrameMismatch)
            unless (curFrm == cloFrm)    (halt $ Fault FrameMismatch)
            pushReturn ReturnCall{ rPC=oldPC, rFrame=retFrm, rParent=Nothing }
            args' <- popStack size
            liftIO $ writeIORef frmRef frm{ fData=args', fDummy=False }
            lift . modify $ \s -> s{ gPC=cloPC }
        (_, Just _)  -> halt $ Fault TypeMismatch
        (_, Nothing) -> halt $ Fault ControlUnderflow

instr STOP
  = halt $ Stop

--------------------------------------------------------------------------------
-- Calculate the given arithmetic operation.
arith :: Arith -> Atom -> Atom -> Atom
arith op x y = case op of
    ADD  -> x + y
    SUB  -> x - y
    MUL  -> x * y
    DIV  -> x `div` y
    CEQ  -> if x == y then 1 else 0
    CGT  -> if x > y  then 1 else 0
    CGTE -> if x >= y then 1 else 0

--------------------------------------------------------------------------------
-- Push an element onto the data stack.
pushStack :: Word -> GCC ()
pushStack x = do
    top <- lift $ gets gData
    ref <- alloc HStack Stack{ sParent=top, sData=x }
    lift . modify $ \s -> s{ gData=(Just ref) }

-- Pop n elements from the data stack and return them in the order in which
-- they were pushed.
popStack :: Size -> GCC [Word]
popStack n = do
  top <- lift $ gets gData
  (ws, top') <- popStack' n ([], top)
  lift . modify $ \s -> s{ gData=top' }
  return (reverse ws)
  where
    popStack' 0 args = do
        return args
    popStack' _ (_, Nothing) = do
        halt $ Fault DataUnderflow
    popStack' n (ws, Just r) = do
        Stack{ sParent=mr', sData=w } <- liftIO $ readIORef r
        popStack' (n-1) (w:ws, mr')

-- Return a list representing the data stack.
listStack :: GCC [Word]
listStack = do
    top <- lift $ gets gData
    listStack' top
  where
    listStack' (Just ref) = do
        Stack{ sParent=par, sData=word } <- liftIO $ readIORef ref
        (word :) <$> listStack' par
    listStack' Nothing = return []

-- Pop an entry from the control stack and return it.
popReturn :: GCC Return
popReturn = do
    mref <- lift $ gets gReturn
    mret <- sequence $ liftIO . readIORef <$> mref
    case mret of
        Just ret -> do
            lift . modify $ \s -> s{ gReturn=(rParent ret) }
            return ret
        Nothing -> halt $ Fault ControlUnderflow

-- Push an entry onto the control stack, changing its rParent field.
pushReturn :: Return -> GCC ()
pushReturn ret = do
    oldRet <- lift $ gets gReturn
    ref <- alloc HReturn ret{ rParent=oldRet }
    lift . modify $ \s -> s{ gReturn=(Just ref) }

-- Return an IORef to the frame at the given address.
getFrame :: FrmAdr -> GCC (IORef Frame)
getFrame frmAdr = do
    unless (frmAdr >= 0) (halt $ Fault FrameOutOfBounds)
    getFrame' frmAdr =<< lift (gets gFrame)
  where
    getFrame' :: FrmAdr -> Maybe (IORef Frame) -> GCC (IORef Frame)
    getFrame' _ Nothing  = halt $ Fault FrameOutOfBounds
    getFrame' 0 (Just r) = return r
    getFrame' n (Just r) = getFrame' (n-1) . fParent =<< liftIO (readIORef r)

--------------------------------------------------------------------------------
-- Given the Heap constructor for an object, and the object itself, allocate
-- the object on the heap and return the corresponding IORef.
alloc :: (Weak (IORef a) -> Heap) -> a -> GCC (IORef a)
alloc con obj = do
    ioRef <- liftIO $ newIORef obj
    wkRef <- liftIO $ mkWeakIORef ioRef (return ())
    let heapObj = con wkRef

    GCCState{ gHeap=hs, gLimits=Limits{ lHeapSize=hSizeMax } } <- lift get
    hSize <- gc; oSize <- size heapObj
    when (hSize + oSize > hSizeMax) (halt $ Fault HeapOverflow)

    lift . modify $ \s -> s{ gHeap=heapObj:hs }
    return ioRef

-- Perform garbage collection, reporting the amount of heap space in use.
gc :: GCC Size
gc = do
    heap <- lift $ gets gHeap
    sizes <- mapM size heap
    lift . modify $ \s -> s{ gHeap=[h | (h,sz) <- zip heap sizes, sz > 0] }
    return (sum sizes)

-- The space occupied in the heap by the given object,
-- or 0 if it has been deallocated.
size :: Heap -> GCC Size
size heap = case heap of
    HCons   w -> size' w $ const 2
    HFrame  w -> size' w $ \Frame{ fData=xs } -> 2 + length xs
    HReturn w -> size' w $ const 2
    HStack  w -> size' w $ const 1
    HClos   w -> size' w $ const 2
  where
    size' :: (Weak (IORef a)) -> (a -> Size) -> GCC Size
    size' wkRef sizeF
      = liftIO (deRefWeak wkRef) >>= \mbRef -> case mbRef of
            Nothing    -> return 0
            Just ioRef -> liftIO $ sizeF <$> readIORef ioRef
