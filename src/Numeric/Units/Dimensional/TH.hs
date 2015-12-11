{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Numeric.Units.Dimensional.TH
(
  -- * Fixed-Point Quantities
  fixedPointType
  -- * Dimensions
, dimensionType
  -- * Raw ExactPi Splices
, exactPiType
)
where

import Data.ExactPi
import qualified Data.ExactPi.TypeLevel as E
import Data.Ratio
import Language.Haskell.TH
import Numeric.Units.Dimensional.Dimensions
import Numeric.Units.Dimensional.Dynamic
import qualified Numeric.NumType.DK.Integers as Z

-- placeholder until we can add the correct import
type SQuantity (q :: E.ExactPi') (d :: Dimension) = String

fixedPointType :: AnyQuantity ExactPi -> Q Type
fixedPointType q = [t| SQuantity $(v') $(d') |]
  where
    d' = dimensionType . dimension $ q
    v' = exactPiType 0 -- placeholder until we can add /~ (demoteUnit siUnit)

-- | Encodes a term-level 'Dimension'' as a type-level 'Dimension'.
dimensionType :: Dimension' -> Q Type
dimensionType (Dim' l m t i th n j) = [t| 'Dim $(l') $(m') $(t') $(i') $(th') $(n') $(j') |]
  where
    l'  = integerType l
    m'  = integerType m
    t'  = integerType t
    i'  = integerType i
    th' = integerType th
    n'  = integerType n
    j'  = integerType j

-- | Encodes a term-level 'ExactPi' as a type-level 'ExactPi''.
--
-- The 'ExactPi' value must be exact and strictly positive.
exactPiType :: ExactPi -> Q Type
exactPiType (Exact z q) | q > 0 = [t| 'E.ExactPi' $(z') $(p') $(q') |]
  where
    z' = integerType z
    p' = litT . numTyLit $ numerator q
    q' = litT . numTyLit $ denominator q
exactPiType _ = fail "Only exact positive values can be encoded as exact pi types."

integerType :: (Integral a) => a -> Q Type
integerType = f . fromIntegral
  where    
    f (-9) = [t| 'Z.Neg9 |]
    f (-8) = [t| 'Z.Neg8 |]
    f (-7) = [t| 'Z.Neg7 |]
    f (-6) = [t| 'Z.Neg6 |]
    f (-5) = [t| 'Z.Neg5 |]
    f (-4) = [t| 'Z.Neg4 |]
    f (-3) = [t| 'Z.Neg3 |]
    f (-2) = [t| 'Z.Neg2 |]
    f (-1) = [t| 'Z.Neg1 |]
    f 0    = [t| 'Z.Zero |]
    f 1    = [t| 'Z.Pos1 |]
    f 2    = [t| 'Z.Pos2 |]
    f 3    = [t| 'Z.Pos3 |]
    f 4    = [t| 'Z.Pos4 |]
    f 5    = [t| 'Z.Pos5 |]
    f 6    = [t| 'Z.Pos6 |]
    f 7    = [t| 'Z.Pos7 |]
    f 8    = [t| 'Z.Pos8 |]
    f 9    = [t| 'Z.Pos9 |]
    f x | x > 9    = [t| 'Z.Pos10Plus $(litT . numTyLit $ (x - 10)) |]
        | x < (-9) = [t| 'Z.Neg10Minus $(litT . numTyLit $ (-10 - x)) |]
        | otherwise = error "Should be unreachable."
