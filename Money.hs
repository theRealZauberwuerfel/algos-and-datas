{-# LANGUAGE EmptyDataDecls #-}

module Money where

newtype Amount a = Amount { getAmount :: Double }

data Euro
data Dollar
data Pound
