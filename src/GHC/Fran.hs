----------------------------------------------------------------
-- Redirection module so that you don't have to put 
-- "./src" on your Path.  Done this way for Hugs's benefit.
-- Does it accomplish the same thing for GHC?  I don't think
-- I have the comilations set up right to do so.  Work it out.
--
-- GHC version, suitable for use with GHC 2.09 on Win32.
----------------------------------------------------------------
module Fran(module FranLoader) where
import FranLoader
----------------------------------------------------------------
