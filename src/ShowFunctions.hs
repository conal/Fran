-- From QuickCheck distribution.

module ShowFunctions where

instance Show (a->b) where
  show f = "<function>"
