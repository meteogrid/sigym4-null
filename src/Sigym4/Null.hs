{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
module Sigym4.Null (
  HasNull (..)
) where

import           Data.Functor.Identity
import           Data.Maybe (isNothing)

-- | A type (a newtype is highly recommended) that is
-- inhabited by a value which represents the absence
-- of a value.
--
-- Instances must satisfy
--
--   >>> isNull nullValue
--   True
class HasNull a where
  nullValue :: a
  default nullValue :: Bounded a => a
  nullValue = minBound

  isNull    :: a -> Bool
  default isNull :: Eq a => a -> Bool
  isNull = (==nullValue)

instance HasNull a => HasNull (Identity a) where
  nullValue = pure nullValue
  isNull    = isNull . runIdentity

instance HasNull (Maybe a) where
  nullValue = Nothing
  isNull    = isNothing

instance HasNull () where
  nullValue = ()
  isNull () = True
