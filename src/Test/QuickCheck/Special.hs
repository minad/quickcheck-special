-----------------------------------------------------------------------------
-- |
-- Module      :  Test.QuickCheck.Special
-- Copyright   :  Daniel Mendler (c) 2016,
-- License     :  MIT (see the file LICENSE)
--
-- Maintainer  :  mail@daniel-mendler.de
-- Stability   :  experimental
-- Portability :  portable
--
-- The standard 'Arbitrary' instances don't generate special values.
-- This is fixed by this package which provides the newtype 'Special' with an 'Arbitrary' instance.
-- The special values are given by the 'SpecialValues' typeclass.
-----------------------------------------------------------

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.QuickCheck.Special (
  Special(..),
  SpecialValues(..),
) where

import Data.Int
import Data.Word
import Numeric.Natural (Natural)
import Numeric.IEEE
import Test.QuickCheck
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Scientific as Scientific
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL

-- | Additionally to the standard Arbitrary instances,
-- this generates special values with a small probability.
newtype Special a = Special { getSpecial :: a }
  deriving (Show, Read, Functor, Bounded, Enum, Eq, Ord, Num, Real, Integral)

-- | Provides a list of special values or edge cases
class SpecialValues a where
  -- | Finite list of special values
  specialValues :: [a]

instance SpecialValues Int    where specialValues = specialInt
instance SpecialValues Int8   where specialValues = specialInt
instance SpecialValues Int16  where specialValues = specialInt
instance SpecialValues Int32  where specialValues = specialInt
instance SpecialValues Int64  where specialValues = specialInt
instance SpecialValues Word   where specialValues = specialBoundedEnum
instance SpecialValues Word8  where specialValues = specialBoundedEnum
instance SpecialValues Word16 where specialValues = specialBoundedEnum
instance SpecialValues Word32 where specialValues = specialBoundedEnum
instance SpecialValues Word64 where specialValues = specialBoundedEnum
instance SpecialValues Bool   where specialValues = [True, False]
instance SpecialValues ()     where specialValues = [()]
instance SpecialValues Float  where specialValues = specialIEEE
instance SpecialValues Double where specialValues = specialIEEE

instance SpecialValues Integer where
  specialValues = [ 0, 1, -1
                  , fromIntegral (minBound :: Int64)  - 1
                  , fromIntegral (maxBound :: Int64)  + 1
                  , fromIntegral (maxBound :: Word64) + 1
                  ]

instance SpecialValues Natural where
  specialValues = [ 0, 1, fromIntegral (maxBound :: Word64) + 1 ]

instance SpecialValues Char where
  specialValues = specialBoundedEnum ++ "\0\a\b\f\n\r\t\v\'\"\\aä "

instance SpecialValues TS.Text where
  specialValues = fmap TS.pack specialValues

instance SpecialValues TL.Text where
  specialValues = fmap TL.pack specialValues

instance SpecialValues BS.ByteString where
  specialValues = fmap BS.pack specialValues

instance SpecialValues BL.ByteString where
  specialValues = fmap BL.pack specialValues

instance SpecialValues Scientific.Scientific where
  specialValues = [ 0
                  , 1, negate 1
                  , Scientific.scientific 1 (-1000), negate $ Scientific.scientific 1 (-1000)
                  , Scientific.scientific 1 1000, negate $ Scientific.scientific 1 1000
                  ]

instance SpecialValues a => SpecialValues [a] where
  specialValues = [[], specialValues]

instance SpecialValues a => SpecialValues (Maybe a) where
  specialValues = Nothing : fmap Just specialValues

instance (SpecialValues a, SpecialValues b) => SpecialValues (Either a b) where
  specialValues = fmap Left specialValues ++ fmap Right specialValues

instance (SpecialValues a, SpecialValues b) => SpecialValues (a, b) where
  specialValues = zip specialValues specialValues

instance (Arbitrary a, SpecialValues a) => Arbitrary (Special a) where
  shrink = fmap Special . shrink . getSpecial
  arbitrary = fmap Special $ frequency $ list specialValues
    where list s = (10 * length s, arbitrary) : fmap (\t -> (1, return t)) s

instance CoArbitrary a => CoArbitrary (Special a) where
  coarbitrary = coarbitrary . getSpecial

specialIEEE :: IEEE a => [a]
specialIEEE = list ++ map negate list
  where list = [nan, 0, 1, epsilon, infinity, minDenormal, minNormal, maxFinite]

specialInt :: (Num a, Bounded a) => [a]
specialInt = [0, 1, -1, minBound, maxBound, minBound + 1, maxBound - 1]

specialBoundedEnum :: (Enum a, Bounded a) => [a]
specialBoundedEnum = [minBound, maxBound, succ minBound, pred maxBound]
