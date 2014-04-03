{-# LANGUAGE TypeFamilies #-}

module Test.OffSystemAdd where

import Data.Dimensions
import Data.Dimensions.SI

data Foot = Foot
instance Unit Foot where
  type BaseUnit Foot = Meter
  conversionRatio _ = 0.3048

data Year = Year
instance Unit Year where
  type BaseUnit Year = Second
  conversionRatio _ = 60 * 60 * 24 * 365.242

vel1 :: Velocity
vel1 = 1e6 % (Foot :/ Year)

vel2 :: Velocity
vel2 = 0.01 % (Meter :/ Second)


vel1InMS :: Double
vel1InMS = vel1 # (Meter :/ Second)

vel2InMS :: Double
vel2InMS = vel2 # (Meter :/ Second)

vel12InMS :: Double
vel12InMS = (vel1 .+ vel2) # (Meter :/ Second)


len1 :: Length
len1 = 3 % Foot

len2 :: Length
len2 = 1 % Meter

len12InM :: Double
len12InM = (len1 .+ len2) # Meter

-- The following expression does not typecheck,
-- because the system is not designed to infer the dimension from the unit.
-- 
len12InM' :: Double
len12InM' = (len2 .+ (3 % Foot)) # Meter

{-
test/OffSystemAdd.hs:48:25:
    Couldn't match type Data.Dimensions.DimSpec.Normalize
                           (Data.Dimensions.DimSpec.Reorder
                              '['Data.Dimensions.DimSpec.D Meter ('S 'Zero)]
                              (Data.Dimensions.Units.Canonicalize
                                 (Data.Dimensions.Map.LookupList
                                    d20
                                    ('Data.Dimensions.Map.MkM
                                       '[(Data.Dimensions.SI.Dims.Length, Meter),
                                         (Data.Dimensions.SI.Dims.Mass, Kilo :@ Gram),
                                         (Data.Dimensions.SI.Dims.Time, Second),
                                         (Data.Dimensions.SI.Dims.Current, Ampere),
                                         (Data.Dimensions.SI.Dims.Temperature, Kelvin),
                                         (Data.Dimensions.SI.Dims.Quantity, Mole),
                                         (Data.Dimensions.SI.Dims.Luminosity, Lumen)]))))
                  with Data.Dimensions.DimSpec.Normalize
                          (Data.Dimensions.Units.Canonicalize
                             (Data.Dimensions.Map.LookupList
                                d20
                                ('Data.Dimensions.Map.MkM
                                   '[(Data.Dimensions.SI.Dims.Length, Meter),
                                     (Data.Dimensions.SI.Dims.Mass, Kilo :@ Gram),
                                     (Data.Dimensions.SI.Dims.Time, Second),
                                     (Data.Dimensions.SI.Dims.Current, Ampere),
                                     (Data.Dimensions.SI.Dims.Temperature, Kelvin),
                                     (Data.Dimensions.SI.Dims.Quantity, Mole),
                                     (Data.Dimensions.SI.Dims.Luminosity, Lumen)])))
    NB: Data.Dimensions.DimSpec.Normalize is a type function, and may not be injective
    The type variable d20 is ambiguous
    In the second argument of (.+), namely (3 % Foot)
    In the first argument of (#), namely (len2 .+ (3 % Foot))
    In the expression: (len2 .+ (3 % Foot)) # Meter


-}