{-# LANGUAGE ForeignFunctionInterface #-}

-- | This module exposes the raw FFI interface to zlib C functions.
-- It is intended for internal use only, and should not be imported by
-- code outside the IterIO library.
module Data.IterIO.ZlibInt where

import Data.ByteString.Internal (memset)
import Foreign
import Foreign.C

import Data.IterIO

#include "zlib.h"

data ZStream = ZStream

foreign import ccall unsafe "zlib.h deflateInit_"
    c_deflateInit :: Ptr ZStream -> CInt -> CString -> CInt -> IO ()
foreign import ccall unsafe "zlib.h deflateInit2_"
    c_deflateInit2 :: Ptr ZStream -> CInt -> ZMethod -> CInt -> CInt
                   -> ZStrategy -> CString -> CInt -> IO ()
foreign import ccall unsafe "zlib.h deflate"
    c_deflate :: Ptr ZStream -> ZFlush -> IO CInt
foreign import ccall unsafe "zlib.h deflateEnd"
    c_deflateEnd :: Ptr ZStream -> IO ()

foreign import ccall unsafe "zlib.h inflateInit_"
    c_inflateInit :: Ptr ZStream -> CString -> CInt -> IO CInt
foreign import ccall unsafe "zlib.h inflateInit2_"
    c_inflateInit2 :: Ptr ZStream -> CInt -> CString -> CInt -> IO CInt
foreign import ccall unsafe "zlib.h inflate"
    c_inflate :: Ptr ZStream -> ZFlush -> IO CInt
foreign import ccall unsafe "zlib.h inflateEnd"
    c_inflateEnd :: Ptr ZStream -> IO CInt

zlib_version :: CString
zlib_version = unsafePerformIO $ newCAString #const_str ZLIB_VERSION

z_stream_size :: (Num a) => a
z_stream_size = #size z_stream

newZStream :: IO (Ptr ZStream)
newZStream = do
  ptr <- mallocBytes z_stream_size
  memset (castPtr ptr) 0 z_stream_size
  return ptr

#{let zoffdef type, field =
          #field " :: Ptr ZStream -> Ptr (" #type ")\n"
          #field " zptr = zptr `plusPtr` %ld"
          , (long) offsetof (z_stream, field)}
#zoffdef Ptr a, next_in
#zoffdef CUInt, avail_in
#zoffdef CULong, total_in
#zoffdef Ptr a, next_out
#zoffdef CUInt, avail_out
#zoffdef CULong, total_out
#zoffdef CString, msg
#zoffdef FunPtr (Ptr a -> CUInt -> CUInt -> Ptr b), zalloc
#zoffdef FunPtr (Ptr a -> Ptr b -> ()), zfree
#zoffdef Ptr a, opaque
#zoffdef ZDataType, data_type
#zoffdef CULong, adler

newtype ZMethod = ZMethod CInt
z_DEFLATED :: ZMethod
z_DEFLATED = ZMethod #const Z_DEFLATED

max_wbits :: CInt
max_wbits = #const MAX_WBITS

newtype ZStrategy = ZStrategy CInt
#{enum ZStrategy, ZStrategy
 , z_FILTERED = Z_FILTERED
 , z_HUFFMAN_ONLY = Z_HUFFMAN_ONLY
 , z_RLE = Z_RLE
 , z_FIXED = Z_FIXED
 , z_DEFAULT_STRATEGY = Z_DEFAULT_STRATEGY
 }

newtype ZDataType = ZDataType CInt
#{enum ZDataType, ZDataType
 , z_BINARY = Z_BINARY
 , z_TEXT = Z_TEXT
 , z_UNKNOWN = Z_UNKNOWN
 }

newtype ZFlush = ZFlush CInt
#{enum ZFlush, ZFlush
 , z_NO_FLUSH = Z_NO_FLUSH
 , z_SYNC_FLUSH = Z_SYNC_FLUSH
 , z_FULL_FLUSH = Z_FULL_FLUSH
 , z_FINISH = Z_FINISH
 , z_BLOCK = Z_BLOCK
 }
