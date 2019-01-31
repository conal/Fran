-- Experiments with runST-based state machines, thanks to suggestions from
-- Erik M and Simon PJ.
--
-- These machines seem fudgetty.  Compare.  One can compose them
-- sequentially, make stateless machines, and map over them.  Check for
-- monadic formulation.

module Machine (runMachine, seqMachine) where

-- import LazyST
-- import GHC

-- Sigbjorn says: The 2.10 distribution for cygwin32 re-exports
-- STBase.runST through the LazyST interface, which is not right.  Here's
-- a temporary workaround, based on hacking the LazyST hi files to export
-- the ST newtype non-abstractly.

import LazyST hiding ( runST )
import GHC
import STBase ( State(..) )

runST :: (All s => ST s a) -> a
runST st = case st of ST st -> let (v,_) = st (S# realWorld#) in v

-- Another Sigbjorn suggestion: an alternative solution is to use the
-- strict version of ST and employ unsafeInterleaveST inside
-- runMachine. This has the merit that the stepper function isn't
-- expressed using the lazy ST monad.

-- End of temp workaround


-- Introduces encapsulated state and a transition function.  But sadly,
-- one cannot yet use All in a type synonym in GHC.
-- 
-- type Machine i o = All s => ST s (i -> ST s o)

-- Run a machine, generating an output stream from an input stream
runMachine :: (All s => ST s (i -> ST s o)) -> [i] -> [o]
runMachine machine is = runST ( do
  next <- machine
  let loop (i : is) = do
        o  <- next i
        os <- loop is
        return (o : os)
  loop is)

-- Machine composition.
-- I don't know which ordering to use here.
--seqMachine :: Machine b c -> Machine a b -> Machine a c
bc `seqMachine` ab = do
  abNext <- ab
  bcNext <- bc
  return (abNext >>. bcNext)

-- More compositional version of >>= (bind)
(>>.) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
ab >>. bc = \ a -> ab a >>= bc

