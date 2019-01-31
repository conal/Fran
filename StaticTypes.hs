----------------------------------------------------------------
-- Redirection module so that you don't have to put 
-- "./src" on your Path.
--
-- Suitable for use with Hugs 1.4 on Win32.
----------------------------------------------------------------
module StaticTypes(module StaticTypesLoader) where
import "src/StaticTypesLoader" as StaticTypesLoader
----------------------------------------------------------------
