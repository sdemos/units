{- Data/Dimensions/Units.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   This file defines the class Unit, which is needed for
   user-defined units.
-}

{-# LANGUAGE TypeFamilies, DataKinds, DefaultSignatures, MultiParamTypeClasses,
             ConstraintKinds, UndecidableInstances, FlexibleContexts,
             FlexibleInstances, ScopedTypeVariables, TypeOperators #-}

module Data.Dimensions.Units where

import Data.Dimensions.Z
import Data.Dimensions.DimSpec
import Data.Dimensions.Dim
import Data.Dimensions.Map
import Data.Type.Bool
import Data.Proxy
import Data.Singletons

-- | Class of dimensions.
class Dimension dim where
  -- | The representation of the dimension as producs of integer powers of the base dimensions.
  type DimSpecsOf dim :: [DimSpec *]

-- | Class of units. Make an instance of this class to define a new unit.
class Unit unit where
  -- | The dimension of this unit.
  type DimOfUnit unit :: [DimSpec *]

  -- | The conversion ratio /from/ this unit /to/ the global coherent
  -- unit of the same dimension. The conversion ratio of a unit is never
  -- used alone; the relative ratio between two units are only things that
  -- matters. Therefore, the conversion ratio of a unit is usually defined
  -- relative to that of other unit's conversion ratio (probably that of the global
  -- coherent unit.)            
  --
  -- For example:
  --
  -- > instance Unit Foot where
  -- >   type DimOfUnit Foot = Length
  -- >   conversionRatio _ = 0.3048 * conversionRatio Meter
  -- The global base unit of length is meter, and 1 Foot = 0.3048 meter. 
  --       
  -- Implementations should /never/ examine their argument!       
  conversionRatio :: Fractional f => unit -> f


-- Abbreviation for creating a Dim (defined here to avoid a module cycle)

-- | Make a dimensioned quantity type capable of storing a value of a given
-- unit. This uses a 'Double' for storage of the value. For example:
--
-- > type Length = MkDim Meter
type MkDim dim lcsu = Dim (DimSpecsOf dim) lcsu Double

-- | Make a dimensioned quantity with a custom numerical type.
type MkGenDim dim lcsu n = Dim (DimSpecsOf dim) lcsu n




class UnitSpec (units :: [DimSpec *]) where
  conversionRatioSpec :: Fractional f => Proxy units -> f

instance UnitSpec '[] where
  conversionRatioSpec _ = 1

instance (UnitSpec rest, Unit unit, SingI n) => UnitSpec (D unit n ': rest) where
  conversionRatioSpec _ =
    (conversionRatio (undefined :: unit) ^^ szToInt (sing :: Sing n)) *
     conversionRatioSpec (Proxy :: Proxy rest)
