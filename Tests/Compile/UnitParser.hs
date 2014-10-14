{- Test units package quasiquoter mechanism
   Copyright (c) 2014 Richard Eisenberg
-}

{-# LANGUAGE QuasiQuotes, CPP, DataKinds #-}

module Tests.Compile.UnitParser where

import Tests.Compile.UnitParser.Quoters ( ms )
import Data.Metrology.SI
import Data.Metrology

len1, len2 :: Length
len1 = 5 % [ms| m |]
len2 = redim $ 10 % [ms| s km / ms |]

vel1, vel2, vel3, vel4 :: Velocity
vel1 = 5 % [ms| m/s |]
vel2 = redim $ 10 % [ms| m s/s^2 |]
vel3 = redim $ 15 % [ms| m 1 / s 1 |]
vel4 = redim $ 20 % [ms| m /(1*1*s) |]

acc1, acc2 :: Acceleration
acc1 = 5 % [ms| m/s^2 |]
acc2 = redim $ 10 % [ms| m/s s |]

#if __GLASGOW_HASKELL >= 709
len3 :: Length
len3 = 15 % [unit| μm |]

freq :: Frequency
freq = 100 % [unit| MHz |]
#endif

lenty :: MkQu_U [ms| m |]
lenty = 5 % Meter

velty :: MkQu_U [ms| m/s |]
velty = 5 % (Meter :/ Second)

freqty :: MkQu_U [ms| s^-1 |]
freqty = 5 % Hertz

num1, num2 :: Count
num1 = 3 % [ms|  |]
num2 = 4 % [ms| 1 |]
