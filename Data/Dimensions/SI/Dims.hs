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
-- This module defines SI dimensions.
-----------------------------------------------------------------------------

module Data.Dimensions.SI.Dims where

import Data.Dimensions
import Data.Dimensions.DimSpec
import Data.Dimensions.Units
import Data.Dimensions.Z

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


type Area                = Length            :^ Two
type Volume              = Length            :^ Three
type Velocity            = Length            :/ Time
type Acceleration        = Velocity          :/ Time
type Wavenumber          = Length            :^ MOne
type Density             = Mass              :/ Volume
type SurfaceDensity      = Mass              :/ Area
type SpecificVolume      = Volume            :/ Mass
type CurrentDensity      = Current           :/ Area
type MagneticStrength    = Current           :/ Length
type Concentration       = AmountOfSubstance :/ Volume
type Luminance           = LuminousIntensity :/ Area

type Frequency           = Time              :^ MOne
type Force               = Mass              :* Acceleration
type Pressure            = Force             :/ Area
type Energy              = Force             :* Length
type Power               = Energy            :/ Time
type Charge              = Current           :* Time
type ElectricPotential   = Power             :/ Current
type Capacitance         = Charge            :/ ElectricPotential
type Resistance          = ElectricPotential :/ Current
type Conductance         = Current           :/ ElectricPotential
type MagneticFlux        = ElectricPotential :* Time
type MagneticFluxDensity = MagneticFlux      :/ Area
type Inductance          = MagneticFlux      :/ Current
type Illuminance         = LuminousIntensity :/ Area
type Kerma               = Area              :/ (Time :^ Two)
type CatalyticActivity   = AmountOfSubstance :/ Time

type Momentum            = Mass              :* Velocity