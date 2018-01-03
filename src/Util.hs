
module Util (
    hadamard
) where

import Data.Matrix

hadamard :: Num a => Matrix a -> Matrix a -> Matrix a
hadamard m1 m2 =
    fromList rows cols $ zipWith (*) (toList m1) (toList m2)
    where   rows = nrows m1
            cols = ncols m1
