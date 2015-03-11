import Data.Char (ord)
import Data.Bits (xor,(.&.))

fnv1a :: [Int] -> Int
fnv1a x = let initHash = 0x811C9DC5
          in  foldl fnv1a_Iter initHash x
		  
fnv1a_Iter :: Int -> Int -> Int
fnv1a_Iter hash x = let fnvPrime = 0x01000193
                    in  ((((x .&. 0xff) `xor` hash ) * fnvPrime) .&. 0xffffffff)
