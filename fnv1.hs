import Data.Char (ord)
import Data.Bits (xor,(.&.))

fnv1a x = let initHash = 2166136261
          in  foldl fnv1a_Iter initHash x
fnv1a_Iter hash x = let fnvPrime = 16777619
                    in  (hash `xor` (ord x .&. 0xff)) * fnvPrime
