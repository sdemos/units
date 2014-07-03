{-# LANGUAGE TypeOperators, TypeFamilies, ImplicitParams, NoMonomorphismRestriction #-}

module Tests.Imperial where

import Data.Metrology
import Data.Metrology.SI

import Test.Tasty
import Test.Tasty.HUnit
import Test.HUnit.Approx

data Mile = Mile
instance Unit Mile where
  type BaseUnit Mile = Meter
  conversionRatio _ = 1609.34

data Pound = Pound
instance Unit Pound where
  type BaseUnit Pound = Kilo :@ Gram
  conversionRatio _ = 0.453592

tests =
  testGroup "Imperial" 
  [ testCase "Mile" ((1 % Mile :: Length) # Meter @?~ 1609.34) 
  , testCase "Pound"  ((1 % Pound :: Mass) # kilo Gram @?~ 0.453592) ] 
  
