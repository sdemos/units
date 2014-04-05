{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Dimensions.SI.Dims
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines SI's seven Base Dimensions and Units.
-----------------------------------------------------------------------------

module Data.Dimensions.SI.Base where

import Data.Dimensions.Z
import Data.Dimensions.DimSpec
import Data.Dimensions.Units
import Data.Dimensions.UnitCombinators
import Data.Dimensions.SI.Prefixes (Kilo)

data Length = Length
instance Dimension Length where
  type DimSpecsOf Length = '[ D Length One ]

data Mass = Mass
instance Dimension Mass where
  type DimSpecsOf Mass = '[ D Mass One ]

data Time = Time
instance Dimension Time where
  type DimSpecsOf Time = '[ D Time One ]

data Current = Current
instance Dimension Current where
  type DimSpecsOf Current = '[ D Current One ]

data Temperature = Temperature
instance Dimension Temperature where
  type DimSpecsOf Temperature = '[ D Temperature One ]

data AmountOfSubstance = AmountOfSubstance
instance Dimension AmountOfSubstance where
  type DimSpecsOf AmountOfSubstance = '[ D AmountOfSubstance One ]

data LuminousIntensity = LuminousIntensity
instance Dimension LuminousIntensity where
  type DimSpecsOf LuminousIntensity = '[ D LuminousIntensity One ]

data Meter = Meter
instance Unit Meter where
  type DimOfUnit Meter = '[D Length One ]
  conversionRatio _ = 1
instance Show Meter where
  show _ = "m"

data Gram = Gram
instance Unit Gram where
  type DimOfUnit Gram = '[D Mass One]
  conversionRatio _ = 0.001
instance Show Gram where
  show _ = "g"

data Second = Second
instance Unit Second where
  type DimOfUnit Second = '[D Time One]
instance Show Second where
  show _ = "s"

data Ampere = Ampere
instance Unit Ampere where
  type DimOfUnit Ampere = '[D Current One]
instance Show Ampere where
  show _ = "A"

data Kelvin = Kelvin
instance Unit Kelvin where
  type DimOfUnit Kelvin = '[D Temperature One]
instance Show Kelvin where
  show _ = "K"

data Mole = Mole
instance Unit Mole where
  type DimOfUnit Mole = '[D AmountOfSubstance One]
instance Show Mole where
  show _ = "mol"

data Candela = Candela
instance Unit Candela where
  type DimOfUnit Candela = '[D LuminousIntensity One]
instance Show Candela where
  show _ = "cd"


