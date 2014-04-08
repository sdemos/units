{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Dimensions
import Data.Dimensions.Types
import Data.Dimensions.SI (SI)
import Data.Dimensions.Imperial.Units
import Data.Dimensions.Show
import qualified Data.Dimensions.SI.Dims as D

fromPUtoWH :: Length SI Double
fromPUtoWH = 137 % Mile

fuelEfficiency :: MkGenDim (D.Area :^ MOne) SI Double
fuelEfficiency = 40 % (Mile :/ Gallon)

gasolineDensity :: Density SI Double
gasolineDensity = 7.29 % (Pound :/ Gallon)


main :: IO ()
main = do
  putStrLn $ fromPUtoWH `showIn` Mile
  putStrLn $ fuelEfficiency `showIn` Mile :/ Gallon
  putStrLn $ gasolineDensity `showIn` Pound :/ Gallon

{-
module Main where

import UnitTyped
import UnitTyped.SI
import UnitTyped.SI.Constants hiding (pi)
import UnitTyped.SI.Meta
import UnitTyped.SI.Derived.Length
import UnitTyped.SI.Derived.Mass
import UnitTyped.SI.Derived.Time
import qualified UnitTyped.NoPrelude as D

-- The distance
fromPUtoWH :: Double :| Mile
fromPUtoWH = 137 *| mile

fuelEfficiency :: Value '[ '(Length, NTwo) ] '[  '(Gallon, NOne), '(Mile, POne) ] Double
fuelEfficiency = autoc $ 40 *| mile |/| gallon

gasolineDensity :: Value '[ '(Mass, POne), '(Length, NThree) ] '[  '(Gallon, NOne), '(Pound, POne) ] Double
gasolineDensity = autoc $ 7.29 *| pound |/| gallon

fromPUtoWHMetric :: Double :| Kilo Meter
fromPUtoWHMetric = autoc $ fromPUtoWH

fuelEfficiencyMetric :: Value '[ '(Length, NTwo) ] '[  '(Liter, NOne), '(Kilo Meter, POne) ] Double 
fuelEfficiencyMetric = autoc $ fuelEfficiency

gasolineDensityMetric :: Value '[ '(Mass, POne), '(Length, NThree) ] '[  '(Liter, NOne), '(Kilo Gram, POne) ] Double
gasolineDensityMetric = autoc $ gasolineDensity



gasolineWeight :: 
  ( Fractional val
  , Convertible' '[ '(Length, POne) ] uniLen
  , Convertible' '[ '(Length, NTwo) ] uniEf
  , Convertible' '[ '(Length, PThree) ]  uniVol
  , Convertible' '[ '(Mass, POne), '(Length, NThree) ] uniDen
  , Convertible' '[ '(Mass, POne) ] uniMass

  , MapNeg uniEf negUniEf
  , MapMerge uniLen negUniEf uniVol
  , MapMerge uniVol uniDen uniMass
  ) =>
     Value '[ '(Length, POne) ] uniLen val
  -> Value '[ '(Length, NTwo) ] uniEf val
  -> Value '[ '(Mass, POne), '(Length, NThree) ] uniDen val 
  -> Value '[ '(Mass, POne) ] uniMass val 

gasolineWeight len0 ef0 den0 = len0 |/| ef0 |*| den0

main :: IO ()
main = do
  putStrLn $ "The distance between University of Pennsylvania and The White House is " ++ show fromPUtoWH
  putStrLn $ "The fuel economy of our car is " ++ show fuelEfficiency
  putStrLn $ "The density of gasoline is " ++ show gasolineDensity
  putStrLn $ "The mass of gasoline we need is " ++ show (gasolineWeight fromPUtoWH fuelEfficiency gasolineDensity)

  putStrLn ""

  putStrLn $ "The distance between University of Pennsylvania and The White House is " ++ show fromPUtoWHMetric
  putStrLn $ "The fuel economy of our car is " ++ show fuelEfficiencyMetric
  putStrLn $ "The density of gasoline is " ++ show gasolineDensityMetric
  putStrLn $ "The mass of gasoline we need is " ++ 
    show (gasolineWeight fromPUtoWHMetric fuelEfficiencyMetric gasolineDensityMetric)
-}