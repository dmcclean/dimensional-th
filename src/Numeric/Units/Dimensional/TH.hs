{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Numeric.Units.Dimensional.TH where

import Data.Attoparsec.Text
import Data.ExactPi
import qualified Data.ExactPi.TypeLevel as E
import Data.Ratio
import Language.Haskell.TH
import qualified Numeric.NumType.DK.Integers as Z

exactPiType :: ExactPi -> Q Type
exactPiType (Exact z q) | q > 0 = [t| ExactPi $(z') $(p') $(q') |]
  where
    z' = integerType z
    p' = litT . numTyLit $ numerator q
    q' = litT . numTyLit $ denominator q
exactPiType _ = fail "Only exact positive values can be encoded as exact pi types."

integerType :: Integer -> Q Type
integerType (-9) = [t| 'Z.Neg9 |]
integerType (-8) = [t| 'Z.Neg8 |]
integerType (-7) = [t| 'Z.Neg7 |]
integerType (-6) = [t| 'Z.Neg6 |]
integerType (-5) = [t| 'Z.Neg5 |]
integerType (-4) = [t| 'Z.Neg4 |]
integerType (-3) = [t| 'Z.Neg3 |]
integerType (-2) = [t| 'Z.Neg2 |]
integerType (-1) = [t| 'Z.Neg1 |]
integerType 0    = [t| 'Z.Zero |]
integerType 1    = [t| 'Z.Pos1 |]
integerType 2    = [t| 'Z.Pos2 |]
integerType 3    = [t| 'Z.Pos3 |]
integerType 4    = [t| 'Z.Pos4 |]
integerType 5    = [t| 'Z.Pos5 |]
integerType 6    = [t| 'Z.Pos6 |]
integerType 7    = [t| 'Z.Pos7 |]
integerType 8    = [t| 'Z.Pos8 |]
integerType 9    = [t| 'Z.Pos9 |]
integerType x | x > 9    = [t| 'Z.Pos10Plus $(litT . numTyLit $ (x - 10)) |]
              | x < (-9) = [t| 'Z.Neg10Minus $(litT . numTyLit $ (-10 - x)) |]
              | otherwise = error "Should be unreachable."
