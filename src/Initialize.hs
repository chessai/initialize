-----------------------------------------------------------------------------------

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-----------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}

module Initialize
  ( Initialize(initialize, initializeElemOff, initializeElems)
  , Deinitialize(deinitialize, deinitializeElemOff, deinitializeElems)
  , Uninitialized(..)
  ) where

-----------------------------------------------------------------------------------

import Data.Int (Int, Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Data.Ord (Ord((<)))
import Data.Eq (Eq)
import Control.Monad (return)
import Data.Char (Char)
import Foreign.Storable (Storable(sizeOf))
import GHC.Ptr (Ptr, plusPtr)
import Foreign.Marshal.Alloc ()
import GHC.IO (IO)
import GHC.Err (undefined)
import GHC.Num (Num((*),(+)))

-----------------------------------------------------------------------------------

-- | The class for initializing memory at a pointer
--   representing 'Storable' values.
class Storable a => Initialize a where
#if __GLASGOW_HASKELL__ >= 708  
  {-# MINIMAL initialize #-}
#endif
  initialize :: Ptr a -> IO ()
  -- ^ Initialize the memory at a pointer. An implementation
  --   of this function may do nothing, or if the data contains
  --   more pointers, 'initialize' may allocate additional memory.
  initializeElemOff :: Ptr a -> Int -> IO ()
  -- ^ Initialize the memory at an offset from the pointer.
  --   This has a default implementation but may be overriden for
  --   efficiency.
  initializeElemOff ptr ix = do
    initialize (plusPtr ptr (ix * sizeOf (undefined :: a)) :: Ptr a)
  initializeElems :: Ptr a -> Int -> IO ()
  -- ^ Initialize a pointer representing an array with
  --   a given number of elements.
  --   This has a default implementation but may be overriden for
  --   efficiency.
  initializeElems ptr n = go 0 where
    go !i = if i < n
      then do
        initialize (plusPtr ptr (i * sizeOf (undefined :: a)) :: Ptr a)
        go (i + 1)
      else return ()

-----------------------------------------------------------------------------------

-- | The class for freeing memory at a pointer
--   representing 'Storable' values.
class Storable a => Deinitialize a where
#if __GLASGOW_HASKELL__ >= 708  
  {-# MINIMAL deinitialize #-}
#endif
  deinitialize :: Ptr a -> IO ()
  -- ^ Free the memory at a pointer. 
  deinitializeElemOff :: Ptr a -> Int -> IO ()
  -- ^ Free the memory at an offset from the pointer.
  --   This has a default implementation but may be overriden for
  --   efficiency.
  deinitializeElemOff ptr ix =
    deinitialize (plusPtr ptr (ix * sizeOf (undefined :: a)) :: Ptr a)
  deinitializeElems :: Ptr a -> Int -> IO ()
  -- ^ Free any memory pointed to by elements of the array.
  --   This has a default implementation but may be overriden for
  --   efficiency.
  deinitializeElems ptr n = go 0 where
    go !i = if i < n
      then do
        deinitialize (plusPtr ptr (i * sizeOf (undefined :: a)) :: Ptr a)
        go (i + 1)
      else return ()

-----------------------------------------------------------------------------------

-- | A type which shares a representation with @a@, but for which
--   all underlying memory remains uninitialized - i.e., all typeclass
--   methods of 'Initialize' and 'Deinitialize' do nothing.
newtype Uninitialized a = Uninitialized a
  deriving (Eq, Storable)

instance Storable a => Initialize (Uninitialized a) where
  initialize _ = return ()
  initializeElemOff _ _ = return ()
  initializeElems _ _ = return ()

instance Storable a => Deinitialize (Uninitialized a) where
  deinitialize _ = return ()
  deinitializeElemOff _ _ = return ()
  deinitializeElems _ _ = return ()

-----------------------------------------------------------------------------------

#define deriveInit(ty)               \
instance Initialize (ty) where {     \
   initialize _ = return ()          \
;  initializeElemOff _ _ = return () \
;  initializeElems _ _ = return ()   \
;  {-# INLINE initialize #-}         \
;  {-# INLINE initializeElemOff #-}  \
;  {-# INLINE initializeElems #-}    \
}

deriveInit(Word)
deriveInit(Word8)
deriveInit(Word16)
deriveInit(Word32)
deriveInit(Word64)
deriveInit(Int)
deriveInit(Int8)
deriveInit(Int16)
deriveInit(Int32)
deriveInit(Int64)
deriveInit(Char)

-----------------------------------------------------------------------------------
