{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GCC where

import Data.Int(Int32)
import Data.Word(Word32)

newtype Size
  = Size Word32
  deriving (Eq, Ord, Enum, Num, Real, Integral)

newtype FrmAdr -- Frame chain address.
  = FrmAdr Word32
  deriving (Eq, Ord, Enum, Num, Real, Integral)

newtype EnvAdr -- Environment address.
  = EnvAdr Word32
  deriving (Eq, Ord, Enum, Num, Real, Integral)

newtype InsAdr -- Instruction address.
  = InsAdr Word32
  deriving (Eq, Ord, Enum, Num, Real, Integral)

newtype Atom -- Atomic machine-sized integer.
  = Atom Int32
  deriving (Eq, Ord, Enum, Num, Real, Integral)

data Code
  = LDC Atom
  | LD FrmAdr EnvAdr
  | Arith Arith
  | ATOM
  | CONS
  | CAR
  | CDR
  | SEL InsAdr InsAdr
  | JOIN
  | LDF InsAdr
  | AP Size
  | RTN
  | DUM Size
  | RAP Size
  | STOP

data Arith
  = ADD
  | SUB
  | MUL
  | DIV
  | CEQ
  | CGT
  | CGTE
