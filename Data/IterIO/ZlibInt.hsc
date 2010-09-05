{-# OPTIONS_GHC -fno-warn-deprecated-flags #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | This module exposes the raw FFI interface to zlib C functions.
-- It is intended for internal use only, and should not be imported by
-- code outside the IterIO library.
module Data.IterIO.ZlibInt where

import Data.Word
import Foreign
import Foreign.C

#include "zlib.h"

foreign import ccall unsafe "zlib.h deflateInit_"
    c_deflateInit :: Ptr ZStream -> CInt -> CString -> CInt -> IO CInt
foreign import ccall unsafe "zlib.h deflateInit2_"
    c_deflateInit2 :: Ptr ZStream -> CInt -> ZMethod -> CInt -> CInt
                   -> ZStrategy -> CString -> CInt -> IO CInt
foreign import ccall unsafe "zlib.h deflate"
    c_deflate :: Ptr ZStream -> ZFlush -> IO CInt
foreign import ccall unsafe "zlib.h &deflateEnd"
    c_deflateEnd :: FunPtr (Ptr ZStream -> IO ())

foreign import ccall unsafe "zlib.h inflateInit_"
    c_inflateInit :: Ptr ZStream -> CString -> CInt -> IO CInt
foreign import ccall unsafe "zlib.h inflateInit2_"
    c_inflateInit2 :: Ptr ZStream -> CInt -> CString -> CInt -> IO CInt
foreign import ccall unsafe "zlib.h inflate"
    c_inflate :: Ptr ZStream -> ZFlush -> IO CInt
foreign import ccall unsafe "zlib.h &inflateEnd"
    c_inflateEnd :: FunPtr (Ptr ZStream -> IO ())

-- | Use this value for zlib format.  Add 16 for gzip format.  Negate
-- for raw zlib format.  When uncompressing, add 32 to determine
-- zlib/gzip format automatically.
max_wbits :: CInt
max_wbits = #const MAX_WBITS

max_mem_level :: CInt
max_mem_level = #const MAX_MEM_LEVEL

def_mem_level :: CInt
def_mem_level = #const MAX_MEM_LEVEL > 8 ? 8 : MAX_MEM_LEVEL

zlib_version :: CString
zlib_version = unsafePerformIO $ newCAString #const_str ZLIB_VERSION

z_stream_size :: (Num a) => a
z_stream_size = #size z_stream

#def struct zssz { z_stream z; char c; };
z_stream_alignment :: Int
z_stream_alignment = #const sizeof (struct zssz) - sizeof (z_stream)

data ZStream = ZStream

#{let zoffdef type, field =
          #field " :: Ptr ZStream -> Ptr (" #type ")\n"
          #field " zptr = zptr `plusPtr` %ld"
          , (long) offsetof (z_stream, field)}
#zoffdef Ptr Word8, next_in
#zoffdef CUInt, avail_in
#zoffdef CULong, total_in
#zoffdef Ptr Word8, next_out
#zoffdef CUInt, avail_out
#zoffdef CULong, total_out
#zoffdef CString, msg
#zoffdef FunPtr (Ptr a -> CUInt -> CUInt -> Ptr b), zalloc
#zoffdef FunPtr (Ptr a -> Ptr b -> ()), zfree
#zoffdef Ptr a, opaque
#zoffdef ZDataType, data_type
#zoffdef CULong, adler

newtype ZFlush = ZFlush CInt
#{enum ZFlush, ZFlush
 , z_NO_FLUSH = Z_NO_FLUSH
 , z_SYNC_FLUSH = Z_SYNC_FLUSH
 , z_FULL_FLUSH = Z_FULL_FLUSH
 , z_FINISH = Z_FINISH
 , z_BLOCK = Z_BLOCK
 }

#{enum CInt,
 , z_OK = Z_OK
 , z_STREAM_END = Z_STREAM_END
 , z_NEED_DICT = Z_NEED_DICT
 , z_ERRNO = Z_ERRNO
 , z_STREAM_ERROR = Z_STREAM_ERROR
 , z_DATA_ERROR = Z_DATA_ERROR
 , z_MEM_ERROR = Z_MEM_ERROR
 , z_BUF_ERROR = Z_BUF_ERROR
 , z_VERSION_ERROR = Z_VERSION_ERROR
 }

#{enum CInt,
 , z_DEFAULT_COMPRESSION = Z_DEFAULT_COMPRESSION
 }

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

newtype ZMethod = ZMethod CInt
#{enum ZMethod, ZMethod
 , z_DEFLATED = Z_DEFLATED
 }


-- Local Variables:
-- haskell-program-name: "ghci -lz"
-- End:
