
{-
 Some stubs to get around this silly idea of renaming 
 MutVar operations for IO
-}
module MutVar where

import Prelude hiding (writeVar,readVar,newVar,MutVar)


type MutVar a = Ref a

newVar v = newRef v >>= \ x -> return x
writeVar  r = assignRef r
readVar   r = derefRef r
updateVar r = updateRef r
