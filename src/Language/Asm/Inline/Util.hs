module Language.Asm.Inline.Util where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr
import GHC.Word

getBSAddr :: BS.ByteString -> Ptr Word8
getBSAddr bs = unsafeForeignPtrToPtr ptr `plusPtr` offset
  where
    (ptr, offset, _) = BS.toForeignPtr bs
