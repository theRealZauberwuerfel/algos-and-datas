{-# LANGUAGE EmptyDataDecls #-}

module Units where

newtype Unit a b c d = Unit { getQuantity :: Double }

data Meter
data Kilogram
data Second
data Ampere

type MKSA = Unit Meter Kilogram Second Ampere
