{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Dimensions
import Data.Dimensions.Types
import Data.Dimensions.SI.Prefixes
import Data.Dimensions.SI.Units
import Data.Dimensions.SI (SI)
import Data.Dimensions.Imperial.Types (Imperial)
import Data.Dimensions.Imperial.Units
import Data.Dimensions.Show
import qualified Data.Dimensions.SI.Dims as D

type PerArea = MkGenDim (D.Area :^ MOne)

fromPUtoWH :: Length Imperial Double
fromPUtoWH = 137 % Mile

fuelEfficiency :: PerArea Imperial Double
fuelEfficiency = 40 % (Mile :/ Gallon)

gasolineDensity :: Density Imperial Double
gasolineDensity = 7.29 % (Pound :/ Gallon)

gasolineWeight :: (Fractional f) 
  => Length su f -> PerArea su f -> Density su f -> Mass su f
gasolineWeight len0 ef0 den0 = len0 ./ ef0 .* den0


main :: IO ()
main = do
  putStrLn $ fromPUtoWH `showIn` Mile
  putStrLn $ fuelEfficiency `showIn` Mile :/ Gallon
  putStrLn $ gasolineDensity `showIn` Pound :/ Gallon
  putStrLn $ show $ gasolineWeight fromPUtoWH fuelEfficiency gasolineDensity 

  putStrLn $ fromPUtoWH `showIn` (kilo Meter)
  putStrLn $ fuelEfficiency `showIn`  kilo Meter :/ Liter
  putStrLn $ gasolineDensity `showIn` kilo Gram :/ Liter
  putStrLn $ show $ (gasolineWeight 
    (convert fromPUtoWH) (convert fuelEfficiency) (convert gasolineDensity) :: Mass SI Double)

