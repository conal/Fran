----------------------------------------------------------------
-- Redirection module so that you don't have to put 
-- "./src" on your Path.
--
-- Suitable for use with Hugs 98 on Win32.
----------------------------------------------------------------
module Fran(module FranLoader) where
import "src/FranLoader" as FranLoader
----------------------------------------------------------------
