{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GCC where

import Data.Int(Int32)
import Data.Word(Word32)

newtype Size
  = Size Word32
  deriving (Eq, Ord, Enum, Num, Real, Integral, Show)

newtype FrmAdr -- Frame chain address
  = FrmAdr Word32
  deriving (Eq, Ord, Enum, Num, Real, Integral, Show)

newtype EnvAdr -- Environment address.
  = EnvAdr Word32
  deriving (Eq, Ord, Enum, Num, Real, Integral, Show)

newtype InsAdr -- Instruction address.
  = InsAdr Word32
  deriving (Eq, Ord, Enum, Num, Real, Integral, Show)

newtype Atom -- Atomic machine-sized integer.
  = Atom Int32
  deriving (Eq, Ord, Enum, Num, Real, Integral, Show)

data Code
  = LDC Atom | LD FrmAdr EnvAdr
  | CArith Arith
  | ATOM | CONS | CAR | CDR
  | SEL | JOIN
  | LDF InsAdr | AP Size | RTN
  | DUM Size | RAP Size
  | STOP
  deriving Show

data Arith
  = ADD | SUB | MUL | DIV
  | CEQ | CGT | CGTE
  deriving Show
