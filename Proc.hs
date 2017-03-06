-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP
           , NoImplicitPrelude
           , RecordWildCards
           , BangPatterns
           , NondecreasingIndentation
           , MagicHash
  #-}
import Prelude (replicate, (++))
import qualified System.IO as Sys

import GHC.IO
import GHC.IO.FD
import GHC.IO.Buffer
import qualified GHC.IO.BufferedIO as Buffered
import GHC.IO.Exception
import GHC.Exception
import GHC.IO.Handle.Types
import GHC.IO.Handle.Internals
import qualified GHC.IO.Device as IODevice
import qualified GHC.IO.Device as RawIO

import Foreign
import Foreign.C

import qualified Control.Exception as Exception
import Data.Typeable
import System.IO.Error
import Data.Maybe

import GHC.IORef
import GHC.Base
import GHC.Real
import GHC.Num
import GHC.Show
import GHC.List

import System.Posix.Internals (puts)

debug :: String -> IO ()
debug s = puts s

main = do
    let str = concat (replicate 130 (replicate 100 'a' ++ "\n")) ++ "bbbbb\n"
    --  OK!
    --  withCStringLen str $ \(cstr, l) ->
    --      hPutBuf stderr cstr l
    -- BUGGY
    -- hSetBuffering stderr LineBuffering
    -- hSetNewlineMode stderr noNewlineTranslation
    -- hSetBinaryMode stderr True
    hPutStr Sys.stderr str
    -- hPutStrLn stderr "THAT IS ALL FOLKS"


hPutStr :: Handle -> String -> IO ()
hPutStr handle str = hPutStr' handle str

hPutStr' :: Handle -> String -> IO ()
hPutStr' handle str = hPutChars handle str        -- v. slow, but we don't care

hPutChars :: Handle -> [Char] -> IO ()
hPutChars _      [] = return ()
hPutChars handle (c:cs) = hPutChar handle c >> hPutChars handle cs

hPutChar :: Handle -> Char -> IO ()
hPutChar handle c = do
    c `seq` return ()
    wantWritableHandle "hPutChar" handle $ \ handle_  -> do
     hPutcBuffered handle_ c

hPutcBuffered :: Handle__ -> Char -> IO ()
hPutcBuffered handle_@Handle__{..} c = do
  buf <- readIORef haCharBuffer
  if c == '\n'
     then do buf1 <- if haOutputNL == CRLF
                        then do
                          buf1 <- putc buf '\r'
                          putc buf1 '\n'
                        else do
                          putc buf '\n'
             writeCharBuffer handle_ buf1
             when is_line $ flushByteWriteBuffer handle_
      else do
          buf1 <- putc buf c
          writeCharBuffer handle_ buf1
          return ()
  where
    is_line = case haBufferMode of
                LineBuffering -> True
                _             -> False

    putc buf@Buffer{ bufRaw=raw, bufR=w } c = do
       debug ("putc: " ++ summaryBuffer buf)
       w'  <- writeCharBuf raw w c
       return buf{ bufR = w' }
