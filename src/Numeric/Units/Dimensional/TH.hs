{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Numeric.Units.Dimensional.TH
(
  -- * Quantities
  fixedPointType
, quantityType
  -- * Dimensions
, dimensionType
  -- * Quasi-Quotes
, fixed
  -- * Raw Type-Level Number Splices
, exactPiType
, integerType
)
where

import Data.Attoparsec.Text
import Data.ExactPi
import qualified Data.ExactPi.TypeLevel as E
import Data.Ratio
import Data.Text (pack)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Numeric.Units.Dimensional (Quantity)
import Numeric.Units.Dimensional.Dimensions
import Numeric.Units.Dimensional.Dynamic
import Numeric.Units.Dimensional.FixedPoint (SQuantity)
import Numeric.Units.Dimensional.Parsing.UCUM (allUcumUnits)
import Numeric.Units.Dimensional.Parsing.Units (expr, whiteSpace)
import qualified Numeric.NumType.DK.Integers as Z

-- | Encodes a term-level 'AnyQuantity' with an 'ExactPi' value as a fixed point type.
-- The fixed point type will define the scale factor and dimension but leave the representation type open.
--
-- The 'ExactPi' value must be exact and strictly positive to ensure that it has a type-level
-- representation as produced by 'exactPiType'.
fixedPointType :: AnyQuantity ExactPi -> Q Type
fixedPointType q | Just v <- q /~ (siUnit d) = [t| SQuantity $(exactPiType v) $(dimensionType d) |]
                 | otherwise = fail "Should be unreachable. Unable to divide AnyQuantity by the SI unit of its dimension."
  where
    d = dimension q

-- | Encodes a term-level 'Dimension'' as a type-level 'Quantity' of that dimension.
-- The representation type parameter is left open, so the result has kind @* -> *@.
quantityType :: Dimension' -> Q Type
quantityType d = [t| Quantity $(dimensionType d) |]

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

-- | Encodes a term-level integer as a type-level 'Z.TypeInt'.
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

fixed :: QuasiQuoter
fixed = QuasiQuoter
        {
          quoteType = \s -> do
                              let q = parseQuantity s
                              case q of
                                Nothing -> fail "Unable to parse quantity in fixed point type definition."
                                Just q' -> fixedPointType q'
        , quoteExp = fail "fixed: Invalid quasiquotation in expression context."
        , quotePat = fail "fixed: Invalid quasiquotation in pattern context."
        , quoteDec = fail "fixed: Invalid quasiquotation in declaration context."
        }
  where
    parseQuantity :: String -> Maybe (AnyQuantity ExactPi)
    parseQuantity = either (const Nothing) promoteQuantity . parseOnly (whiteSpace *> expr allUcumUnits <* endOfInput) . pack
