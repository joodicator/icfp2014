{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GCC where

import Data.Int(Int32)   -- Signed 32-bit integer.
import Data.Word(Word32) -- Unsigned 32-bit integer.

newtype Size = Size Word32 deriving (Num, Show)
newtype FAdr = FAdr Word32 deriving (Num, Show) -- Frame address within a chain.
newtype EAdr = EAdr Word32 deriving (Num, Show) -- Data address within frame.
newtype IAdr = IAdr Word32 deriving (Num, Show) -- Instruction address.
newtype Atom = Atom Int32  deriving (Num, Show) -- Atomic data value.

data Code
  = LDC Atom | LD FAdr EAdr
  | CArith Arith
  | ATOM | CONS | CAR | CDR
  | SEL | JOIN
  | LDF IAdr | AP Size | RTN
  | DUM Size | RAP Size
  | STOP
  deriving Show

data Arith
  = ADD | SUB | MUL | DIV
  | CEQ | CGT | CGTE
  deriving Show
