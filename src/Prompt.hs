{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Prompt
    ( prompt
    , promptIO
    ) where

import System.IO.Unsafe

import Foreign.C.String
import GHC.Base

import Data.List

promptIO :: String -> IO ()
promptIO msg = do
  withCString "%s" $ \cfmt -> do
     -- NB: debugBelch can't deal with null bytes, so filter them
     -- out so we don't accidentally truncate the message.  See Trac #9395
     let (nulls, msg') = partition (=='\0') msg
     withCString msg' $ \cmsg ->
      debugBelch cfmt cmsg
     when (not (null nulls)) $
       withCString "WARNING: previous trace message had null bytes" $ \cmsg ->
         debugBelch cfmt cmsg

foreign import ccall unsafe "HsBase.h debugBelch2"
   debugBelch :: CString -> CString -> IO ()

{-# NOINLINE prompt #-}
prompt :: String -> a -> a
prompt string expr = unsafePerformIO $ do
  { promptIO string
  ; return expr
  }
