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

getSpareBuffer :: Handle__ -> IO (BufferMode, CharBuffer)
getSpareBuffer Handle__{haCharBuffer=ref,
                        haBuffers=spare_ref,
                        haBufferMode=mode}
 = do
   case mode of
     NoBuffering -> return (mode, error "no buffer!")
     _ -> do
          bufs <- readIORef spare_ref
          buf  <- readIORef ref
          case bufs of
            BufferListCons b rest -> do
                writeIORef spare_ref rest
                return ( mode, emptyBuffer b (bufSize buf) WriteBuffer)
            BufferListNil -> do
                new_buf <- newCharBuffer (bufSize buf) WriteBuffer
                return (mode, new_buf)

hPutChars :: Handle -> [Char] -> IO ()
hPutChars _      [] = return ()
hPutChars handle (c:cs) = hPutChar handle c >> hPutChars handle cs

writeBlocks :: Handle -> Bool -> Bool -> Newline -> Buffer CharBufElem -> String -> IO ()
writeBlocks hdl line_buffered add_nl nl
            buf@Buffer{ bufRaw=raw, bufSize=len } s =
  let
   shoveString :: Int -> [Char] -> [Char] -> IO ()
   shoveString !n [] [] = do
        commitBuffer hdl raw len n False{-no flush-} True{-release-}
   shoveString !n [] rest = do
        shoveString n rest []
   shoveString !n (c:cs) rest
     -- n+1 so we have enough room to write '\r\n' if necessary
     | n + 1 >= len = do
        commitBuffer hdl raw len n False{-flush-} False
        shoveString 0 (c:cs) rest
     | c == '\n'  =  do
        n' <- if nl == CRLF
                 then do
                    n1 <- writeCharBuf raw n  '\r'
                    writeCharBuf raw n1 '\n'
                 else do
                    writeCharBuf raw n c
        if line_buffered
           then do
                -- end of line, so write and flush
               commitBuffer hdl raw len n' True{-flush-} False
               shoveString 0 cs rest
           else do
               shoveString n' cs rest
     | otherwise = do
        n' <- writeCharBuf raw n c
        shoveString n' cs rest
  in
  shoveString 0 s (if add_nl then "\n" else "")

hPutChar :: Handle -> Char -> IO ()
hPutChar handle c = do
    c `seq` return ()
    wantWritableHandle "hPutChar" handle $ \ handle_  -> do
     hPutcBuffered handle_ c

commitBuffer
        :: Handle                       -- handle to commit to
        -> RawCharBuffer -> Int         -- address and size (in bytes) of buffer
        -> Int                          -- number of bytes of data in buffer
        -> Bool                         -- True <=> flush the handle afterward
        -> Bool                         -- release the buffer?
        -> IO ()

commitBuffer hdl !raw !sz !count flush release =
  wantWritableHandle "commitBuffer" hdl $ \h_@Handle__{..} -> do
      debugIO ("commitBuffer: sz=" ++ show sz ++ ", count=" ++ show count
            ++ ", flush=" ++ show flush ++ ", release=" ++ show release)

      writeCharBuffer h_ Buffer{ bufRaw=raw, bufState=WriteBuffer,
                                 bufL=0, bufR=count, bufSize=sz }

      when flush $ flushByteWriteBuffer h_

      -- release the buffer if necessary
      when release $ do
          -- find size of current buffer
          old_buf@Buffer{ bufSize=size } <- readIORef haCharBuffer
          when (sz == size) $ do
               spare_bufs <- readIORef haBuffers
               writeIORef haBuffers (BufferListCons raw spare_bufs)

      return ()

-- backwards compatibility; the text package uses this
commitBuffer' :: RawCharBuffer -> Int -> Int -> Bool -> Bool -> Handle__
              -> IO CharBuffer
commitBuffer' raw sz@(I# _) count@(I# _) flush release h_@Handle__{..}
   = do
      debugIO ("commitBuffer: sz=" ++ show sz ++ ", count=" ++ show count
            ++ ", flush=" ++ show flush ++ ", release=" ++ show release)

      let this_buf = Buffer{ bufRaw=raw, bufState=WriteBuffer,
                             bufL=0, bufR=count, bufSize=sz }

      writeCharBuffer h_ this_buf

      when flush $ flushByteWriteBuffer h_

      -- release the buffer if necessary
      when release $ do
          -- find size of current buffer
          old_buf@Buffer{ bufSize=size } <- readIORef haCharBuffer
          when (sz == size) $ do
               spare_bufs <- readIORef haBuffers
               writeIORef haBuffers (BufferListCons raw spare_bufs)

      return this_buf

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
       debugIO ("putc: " ++ summaryBuffer buf)
       w'  <- writeCharBuf raw w c
       return buf{ bufR = w' }
