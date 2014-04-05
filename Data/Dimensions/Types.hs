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

type Length              = MkGenDim D.Length              
type Mass                = MkGenDim D.Mass                
type Time                = MkGenDim D.Time                
type Current             = MkGenDim D.Current             
type Temperature         = MkGenDim D.Temperature         
type Quantity            = MkGenDim D.AmountOfSubstance   
type Luminosity          = MkGenDim D.LuminousIntensity   

type Area                = MkGenDim D.Area                
type Volume              = MkGenDim D.Volume              
type Velocity            = MkGenDim D.Velocity            
type Acceleration        = MkGenDim D.Acceleration        
type Wavenumber          = MkGenDim D.Wavenumber          
type Density             = MkGenDim D.Density             
type SurfaceDensity      = MkGenDim D.SurfaceDensity      
type SpecificVolume      = MkGenDim D.SpecificVolume      
type CurrentDensity      = MkGenDim D.CurrentDensity      
type MagneticStrength    = MkGenDim D.MagneticStrength    
type Concentration       = MkGenDim D.Concentration       
type Luminance           = MkGenDim D.Luminance           
type Frequency           = MkGenDim D.Frequency           
type Force               = MkGenDim D.Force               
type Pressure            = MkGenDim D.Pressure            
type Energy              = MkGenDim D.Energy              
type Power               = MkGenDim D.Power               
type Charge              = MkGenDim D.Charge              
type ElectricPotential   = MkGenDim D.ElectricPotential   
type Capacitance         = MkGenDim D.Capacitance         
type Resistance          = MkGenDim D.Resistance          
type Conductance         = MkGenDim D.Conductance         
type MagneticFlux        = MkGenDim D.MagneticFlux        
type MagneticFluxDensity = MkGenDim D.MagneticFluxDensity 
type Inductance          = MkGenDim D.Inductance          
type Illuminance         = MkGenDim D.Illuminance         
type Kerma               = MkGenDim D.Kerma               
type CatalyticActivity   = MkGenDim D.CatalyticActivity   
type Momentum            = MkGenDim D.Momentum            