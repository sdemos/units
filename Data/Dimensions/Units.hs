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
  -- | A unit of this dimension that belongs to the global coherent system
  -- of units. 
  -- We use the global coherent system of units as the standard for unit conversion.
  -- i.e. globally coherent units have conversion factors of 1.
  type GlobalBaseUnit dim :: *
  

-- | Class of units. Make an instance of this class to define a new unit.
class Unit unit where
  -- | The dimension of this unit.
  type DimOfUnit unit :: [DimSpec *]

  -- | The conversion ratio /from/ this unit /to/ the global coherent
  -- unit of the same dimension.
  -- If left out, a conversion ratio of 1 is assumed.
  --
  -- For example:
  --
  -- > instance Unit Foot where
  -- >   type DimOfUnit Foot = Length
  -- >   conversionRatio _ = 0.3048 
  -- The global base unit of length is meter, and 1 Foot = 0.3048 meter. 
  --       
  -- Implementations should /never/ examine their argument!
  conversionRatio :: Fractional f => unit -> f

{- COMCANO
  -- | The internal list of canonical units corresponding to this unit.
  type UnitSpecsOf unit :: [DimSpec *]
  type UnitSpecsOf unit = If (IsCanonical unit)
                            '[D unit One]
                            (UnitSpecsOf (BaseUnit unit))

  -- | Compute the conversion from the underlying canonical unit to
  -- this one. A default is provided that multiplies together the ratios
  -- of all units between this one and the canonical one.
  canonicalConvRatio :: Fractional f => unit -> f
  default canonicalConvRatio :: (BaseHasConvRatio unit, Fractional f)
                             => unit -> f
  canonicalConvRatio u = conversionRatio u * baseUnitRatio u
-}


-- Abbreviation for creating a Dim (defined here to avoid a module cycle)

-- | Make a dimensioned quantity type capable of storing a value of a given
-- unit. This uses a 'Double' for storage of the value. For example:
--
-- > type Length = MkDim Meter
type MkDim dim lcsu = Dim (DimSpecsOf dim) lcsu Double

-- | Make a dimensioned quantity with a custom numerical type.
type MkGenDim dim lcsu n = Dim (DimSpecsOf dim) lcsu n



-- The functions related to canonical units are temporallily commented out.

--  -- | Is this unit a canonical unit?
--  type IsCanonical (unit :: *) = CheckCanonical (BaseUnit unit)
--  
--  -- | Is the argument the special datatype 'Canonical'?
--  type family CheckCanonical (base_unit :: *) :: Bool where
--    CheckCanonical Canonical = True
--    CheckCanonical unit      = False
--  
--  
--  {- I want to say this. But type families are *eager* so I have to write
--     it another way.
--  type family CanonicalUnit (unit :: *) where
--    CanonicalUnit unit
--      = If (IsCanonical unit) unit (CanonicalUnit (BaseUnit unit))
--  -}
--  
--  
--  
--  
--  -- | Get the canonical unit from a given unit.
--  -- For example: @CanonicalUnit Foot = Meter@
--  type CanonicalUnit (unit :: *) = CanonicalUnit' (BaseUnit unit) unit
--  
--  -- | Helper function in 'CanonicalUnit'
--  type family CanonicalUnit' (base_unit :: *) (unit :: *) :: * where
--    CanonicalUnit' Canonical unit = unit
--    CanonicalUnit' base      unit = CanonicalUnit' (BaseUnit base) base
--  
--  -- | Essentially, a constraint that checks if a conversion ratio can be
--  -- calculated for a @BaseUnit@ of a unit.
--  type BaseHasConvRatio unit = HasConvRatio (IsCanonical unit) unit
--  
--  -- | This is like 'Unit', but deals with 'Canonical'. It is necessary
--  -- to be able to define 'canonicalConvRatio' in the right way.
--  class is_canonical ~ IsCanonical unit
--        => HasConvRatio (is_canonical :: Bool) (unit :: *) where
--    baseUnitRatio :: Fractional f => unit -> f
--  instance True ~ IsCanonical canonical_unit
--           => HasConvRatio True canonical_unit where
--    baseUnitRatio _ = 1
--  instance ( False ~ IsCanonical noncanonical_unit
--           , Unit (BaseUnit noncanonical_unit) )
--           => HasConvRatio False noncanonical_unit where
--    baseUnitRatio _ = canonicalConvRatio (undefined :: BaseUnit noncanonical_unit)
 

-- | Calculates the conversion factor of @[DimSpec *]@s representing /units/ to the
-- global coherent system of units.

class UnitSpec (units :: [DimSpec *]) where
  conversionRatioSpec :: Fractional f => Proxy units -> f

instance UnitSpec '[] where
  conversionRatioSpec _ = 1

instance (UnitSpec rest, Unit unit, SingI n) => UnitSpec (D unit n ': rest) where
  conversionRatioSpec _ =
    (conversionRatio (undefined :: unit) ^^ szToInt (sing :: Sing n)) *
     conversionRatioSpec (Proxy :: Proxy rest)
--  
--  infix 4 *~
--  -- | Check if two @[DimSpec *]@s, representing /units/, should be
--  -- considered to be equal
--  type units1 *~ units2 = (Canonicalize units1 @~ Canonicalize units2)

--  
--  type family Canonicalize (units :: [DimSpec *]) :: [DimSpec *] where
--    Canonicalize '[] = '[]
--    Canonicalize (D unit n ': rest) = D (CanonicalUnit unit) n ': Canonicalize rest
--  
--  type Compatible (dim :: *) (lcsu :: Map *) (unit :: *) =
--    ( CanonicalUnit (Lookup dim lcsu) ~ CanonicalUnit unit
--    , Unit (Lookup dim lcsu))