{-# LANGUAGE ForeignFunctionInterface #-}

-- | This module exposes the raw FFI interface to zlib C functions.
-- It is intended for internal use only, and should not be imported by
-- code outside the IterIO library.
module Data.IterIO.ZlibInt where

import Foreign
import Foreign.C

import Data.IterIO

#include "zlib.h"

data ZStream = ZStream

foreign import ccall unsafe "zlib.h deflateInit_"
    c_deflateInit :: Ptr ZStream -> CInt -> CString -> CInt -> IO ()
foreign import ccall unsafe "zlib.h deflateInit2_"
    c_deflateInit2 :: Ptr ZStream -> CInt -> CInt -> CInt -> CInt -> CInt
                   -> CString -> CInt -> IO ()
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

z_stream_size :: Int
z_stream_size = #size z_stream

#{let zoffdef field = #field "_offset :: Int\n" #field "_offset = %ld"
                      , (long) offsetof (z_stream, field)}
#zoffdef next_in
#zoffdef avail_in
#zoffdef total_in
#zoffdef next_out
#zoffdef avail_out
#zoffdef total_out
#zoffdef msg
#zoffdef state
#zoffdef zalloc
#zoffdef zfree
#zoffdef opaque
#zoffdef data_type
#zoffdef adler

newtype ZFlush = ZFlush CInt
#{enum ZFlush, ZFlush
 , z_NO_FLUSH = Z_NO_FLUSH
 , z_SYNC_FLUSH = Z_SYNC_FLUSH
 , z_FULL_FLUSH = Z_FULL_FLUSH
 , z_FINISH = Z_FINISH
 , z_BLOCK = Z_BLOCK
 }

 -- , z_PARTIAL_FLUSH = Z_PARTIAL_FLUSH
 -- , z_TREES = Z_TREES
