{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Dimensions.Imperial.Units
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module exports unit definitions according to the SI system of units.
-- The definitions were taken from here: <http://www.bipm.org/en/si/>.
--
-- There is one deviation from the definition at that site: To work better
-- with prefixes, the unit of mass is 'Gram'.
-----------------------------------------------------------------------------

module Data.Dimensions.Imperial.Units where

import Data.Dimensions
import Data.Dimensions.DimSpec
import Data.Dimensions.Units
import Data.Dimensions.SI.Dims
import Data.Dimensions.SI.Units (Meter(..), Gram(..))
import Data.Proxy

data Thou = Thou
instance Unit Thou where
  type DimOfUnit Thou = '[D Length One ]
  conversionRatio _ = 1/1000 * conversionRatio Inch
instance Show Thou where
  show _ = "th"

data Inch = Inch
instance Unit Inch where
  type DimOfUnit Inch = '[D Length One ]
  conversionRatio _ = 1/12 * conversionRatio Foot
instance Show Inch where
  show _ = "in"

data Foot = Foot
instance Unit Foot where
  type DimOfUnit Foot = '[D Length One ]
  conversionRatio _ = 1/3 * conversionRatio Yard
instance Show Foot where
  show _ = "ft"

data Yard = Yard
instance Unit Yard where
  type DimOfUnit Yard = '[D Length One ]
  conversionRatio _ = 0.9144 * conversionRatio Meter
instance Show Yard where
  show _ = "yd"

data Chain = Chain
instance Unit Chain where
  type DimOfUnit Chain = '[D Length One ]
  conversionRatio _ = 22 * conversionRatio Yard
instance Show Chain where
  show _ = "ch"

data Furlong = Furlong
instance Unit Furlong where
  type DimOfUnit Furlong = '[D Length One ]
  conversionRatio _ = 10 * conversionRatio Chain
instance Show Furlong where
  show _ = "fur"

data Mile = Mile
instance Unit Mile where
  type DimOfUnit Mile = '[D Length One ]
  conversionRatio _ = 8 * conversionRatio Furlong
instance Show Mile where
  show _ = "mi"

data League = League
instance Unit League where
  type DimOfUnit League = '[D Length One ]
  conversionRatio _ = 3 * conversionRatio Mile
instance Show League where
  show _ = "lea"


data Ounce = Ounce
instance Unit Ounce where
  type DimOfUnit Ounce = '[D Mass One]
  conversionRatio _ = 1/16 * conversionRatio Pound
instance Show Ounce where
  show _ = "oz"

data Pound = Pound
instance Unit Pound where
  type DimOfUnit Pound = '[D Mass One]
  conversionRatio _ = 453.59237 * conversionRatio Gram
instance Show Pound where
  show _ = "lb"



