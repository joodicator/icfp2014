{-# LANGUAGE DoRec #-}

module TextGCC where

import Data.List
import GCC

--------------------------------------------------------------------------------
-- Parse a GCC program, allowing labels in addition to line numbers.
readProgram :: String -> Either String [Code]
readProgram = undefined
{-
readProgram string = do rec
    (code, labels, _) <- foldl' f (Right ([], [], 0)) (words string)
    let f 
-}

--------------------------------------------------------------------------------
instance Read Code where
    readsPrec _ s = case words (map toUpper s) of {
        ["LDC",x]   -> pure LDC <*> reads x;
        ["LD",f,e]  -> pure LD <*> reads f <*> reads e;
        ["ATOM"]    -> pure ATOM;
        ["CONS"]    -> pure CONS;
        ["CAR"]     -> pure CAR;
        ["CDR"]     -> pure CDR;
        ["SEL",t,f] -> pure SEL <*> reads t <*> reads f;
        ["JOIN"]    -> pure JOIN;
        ["LDF",f]   -> pure LDF <*> reads f;
        ["AP",s]    -> pure AP <*> reads s;
        ["RTN"]     -> pure RTN;
        ["DUM",s]   -> pure DUM <*> reads s;
        ["RAP",s]   -> pure RAP <*> reads s;
        ["STOP"]    -> pure STOP;
        _           -> pure Arith <*> reads s }

instance Read Arith where
    readsPrec _ s = case words (map toUpper s) of {
        ["ADD"]  -> [ADD];
        ["SUB"]  -> [SUB];
        ["DIV"]  -> [DIV];
        ["MUL"]  -> [MUL];
        ["CEQ"]  -> [CEQ];
        ["CGT"]  -> [CGT];
        ["CGTE"] -> [CGTE] }

instance Read Size   where readsPrec _ = fmap Size   . reads
instance Read FrmAdr where readsPrec _ = fmap FrmAdr . reads
instance Read EnvAdr where readsPrec _ = fmap EnvAdr . reads
instance Read InsAdr where readsPrec _ = fmap InsAdr . reads
instance Read Atom   where readsPrec _ = fmap Atom   . reads

--------------------------------------------------------------------------------
-- Show a GCC program with relevant line number annotations.
showProgram :: [Code] -> String
showProgram code = unlines (reverse lines) where
    (lines, nums, _) = foldl' f ([], [], 0) code
    f (ls, ns, n) c = (l:ls, ns'++ns, n+1) where
        l = show c ++ if n `elem` nums then "\t; line " ++ show n else "" 
        ns' = case c of {
            SEL a b -> [a,b];
            LDF a   -> [a];
            _       -> [] }

--------------------------------------------------------------------------------
instance Show Code where show c = case c of {
    LDC x   -> "LDC " ++ show x;
    LD f e  -> "LD " ++ show f ++ " " ++ show e;
    Arith o -> show o;
    ATOM    -> "ATOM";
    CONS    -> "CONS";
    CAR     -> "CAR";
    CDR     -> "CDR";
    SEL t f -> "SEL " ++ show t ++ show f;
    JOIN    -> "JOIN";
    LDF f   -> "LDF " ++ show f;
    AP s    -> "AP " ++ show s;
    RTN     -> "RTN";
    DUM s   -> "SUM " ++ show s;
    RAP s   -> "RAP " ++ show s;
    STOP    -> "STOP" }

instance Show Arith where show o = case o of {
    ADD     -> "ADD";
    SUB     -> "SUB";
    MUL     -> "MUL";
    DIV     -> "DIV";
    CEQ     -> "CEQ";
    CGT     -> "CGT";
    CGTE    -> "CGTE" }

instance Show Size   where show (Size   x) = show x
instance Show FrmAdr where show (FrmAdr x) = show x
instance Show EnvAdr where show (EnvAdr x) = show x
instance Show InsAdr where show (InsAdr x) = show x
instance Show Atom   where show (Atom   x) = show x
