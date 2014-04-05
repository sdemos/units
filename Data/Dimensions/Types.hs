{-# LANGUAGE TypeOperators, DataKinds #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Dimensions.Types
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines type synonyms for dimensions that are general over
-- system of units and the numerical value types.
-----------------------------------------------------------------------------

module Data.Dimensions.Types where

import Data.Dimensions
import qualified Data.Dimensions.SI.Dims as D

type Length              = MkDim D.Length              
type Mass                = MkDim D.Mass                
type Time                = MkDim D.Time                
type Current             = MkDim D.Current             
type Temperature         = MkDim D.Temperature         
type Quantity            = MkDim D.AmountOfSubstance   
type Luminosity          = MkDim D.LuminousIntensity   

type Area                = MkDim D.Area                
type Volume              = MkDim D.Volume              
type Velocity            = MkDim D.Velocity            
type Acceleration        = MkDim D.Acceleration        
type Wavenumber          = MkDim D.Wavenumber          
type Density             = MkDim D.Density             
type SurfaceDensity      = MkDim D.SurfaceDensity      
type SpecificVolume      = MkDim D.SpecificVolume      
type CurrentDensity      = MkDim D.CurrentDensity      
type MagneticStrength    = MkDim D.MagneticStrength    
type Concentration       = MkDim D.Concentration       
type Luminance           = MkDim D.Luminance           
type Frequency           = MkDim D.Frequency           
type Force               = MkDim D.Force               
type Pressure            = MkDim D.Pressure            
type Energy              = MkDim D.Energy              
type Power               = MkDim D.Power               
type Charge              = MkDim D.Charge              
type ElectricPotential   = MkDim D.ElectricPotential   
type Capacitance         = MkDim D.Capacitance         
type Resistance          = MkDim D.Resistance          
type Conductance         = MkDim D.Conductance         
type MagneticFlux        = MkDim D.MagneticFlux        
type MagneticFluxDensity = MkDim D.MagneticFluxDensity 
type Inductance          = MkDim D.Inductance          
type Illuminance         = MkDim D.Illuminance         
type Kerma               = MkDim D.Kerma               
type CatalyticActivity   = MkDim D.CatalyticActivity   
type Momentum            = MkDim D.Momentum            