module GCC where

import Int(Int32)   -- Signed 32-bit integer.
import Word(Word32) -- Unsigned 32-bit integer.

newtype Size = Size Word32
newtype FAdr = FAdr Word32  -- Frame address within a frame chain.
newtype EAdr = EAdr Word32  -- Data address within an environment frame.
newtype IAdr = IAdr Word32  -- Instruction address.
newtype Atom = Atom Int32   -- Atomic data value.

data Code
  = LDC Word | LD FAdr EAdr
  | CArith Arith
  | ATOM | CONS | CAR | CDR
  | SEL | JOIN
  | LDF IAdr | AP Size | RTN
  | DUM Size | RAP Size
  | STOP

data Arith
  = ADD | SUB | MUL | DIV
  | CEQ | CGT | CGTE
