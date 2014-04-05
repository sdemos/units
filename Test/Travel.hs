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

fromGLtoED :: Length Imperial Float
fromGLtoED = 46.5 % Mile

fuelEfficiency :: PerArea Imperial Float
fuelEfficiency = 40 % (Mile :/ Gallon)

gasolineDensity :: Density Imperial Float
gasolineDensity = 7.29 % (Pound :/ Gallon)

gasolineWeight :: (Fractional f) 
  => Length su f -> PerArea su f -> Density su f -> Mass su f
gasolineWeight len0 ef0 den0 = len0 ./ ef0 .* den0


main :: IO ()
main = do
  putStrLn $ fromGLtoED `showIn` Mile
  putStrLn $ fuelEfficiency `showIn` Mile :/ Gallon
  putStrLn $ gasolineDensity `showIn` Pound :/ Gallon
  putStrLn $ show $ gasolineWeight fromGLtoED fuelEfficiency gasolineDensity 

  putStrLn ""
  putStrLn $ fromGLtoED `showIn` (kilo Meter)
  putStrLn $ fuelEfficiency `showIn`  kilo Meter :/ Liter
  putStrLn $ gasolineDensity `showIn` kilo Gram :/ Liter
  putStrLn $ show $ (gasolineWeight 
    (convert fromGLtoED) (convert fuelEfficiency) (convert gasolineDensity) :: Mass SI Float)

{---- Execution result ---

46.5 mi
39.999996 mi/gal
7.29 lb/gal
8.474626 lb

74.834496 km
14.160248 km/l
0.7273698 kg/l
3.8440251 kg


-}