{-# LANGUAGE TypeOperators, DataKinds #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Dimensions.SI.Types
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines type synonyms for British Imperial units.
-----------------------------------------------------------------------------
module Data.Dimensions.Imperial.Types where

import Data.Dimensions
import Data.Dimensions.SI.Units
import Data.Dimensions.Imperial.Units (Yard, Pound)
import qualified Data.Dimensions.SI.Dims as D


type Imperial = 
  MkLCSU 
   '[ (D.Length, Yard)
    , (D.Mass, Pound)
    , (D.Time, Second)
    , (D.Current, Ampere)
    , (D.Temperature, Kelvin)
    , (D.AmountOfSubstance, Mole)
    , (D.LuminousIntensity, Candela)
    ]

type Length              = MkDim D.Length              Imperial
type Mass                = MkDim D.Mass                Imperial
type Time                = MkDim D.Time                Imperial
type Current             = MkDim D.Current             Imperial
type Temperature         = MkDim D.Temperature         Imperial
type Quantity            = MkDim D.AmountOfSubstance   Imperial
type Luminosity          = MkDim D.LuminousIntensity   Imperial

type Area                = MkDim D.Area                Imperial
type Volume              = MkDim D.Volume              Imperial
type Velocity            = MkDim D.Velocity            Imperial
type Acceleration        = MkDim D.Acceleration        Imperial
type Wavenumber          = MkDim D.Wavenumber          Imperial
type Density             = MkDim D.Density             Imperial
type SurfaceDensity      = MkDim D.SurfaceDensity      Imperial
type SpecificVolume      = MkDim D.SpecificVolume      Imperial
type CurrentDensity      = MkDim D.CurrentDensity      Imperial
type MagneticStrength    = MkDim D.MagneticStrength    Imperial
type Concentration       = MkDim D.Concentration       Imperial
type Luminance           = MkDim D.Luminance           Imperial
type Frequency           = MkDim D.Frequency           Imperial
type Force               = MkDim D.Force               Imperial
type Pressure            = MkDim D.Pressure            Imperial
type Energy              = MkDim D.Energy              Imperial
type Power               = MkDim D.Power               Imperial
type Charge              = MkDim D.Charge              Imperial
type ElectricPotential   = MkDim D.ElectricPotential   Imperial
type Capacitance         = MkDim D.Capacitance         Imperial
type Resistance          = MkDim D.Resistance          Imperial
type Conductance         = MkDim D.Conductance         Imperial
type MagneticFlux        = MkDim D.MagneticFlux        Imperial
type MagneticFluxDensity = MkDim D.MagneticFluxDensity Imperial
type Inductance          = MkDim D.Inductance          Imperial
type Illuminance         = MkDim D.Illuminance         Imperial
type Kerma               = MkDim D.Kerma               Imperial
type CatalyticActivity   = MkDim D.CatalyticActivity   Imperial
type Momentum            = MkDim D.Momentum            Imperial