-- {-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent
import System.Process
import System.IO
import System.Exit
import qualified Data.ByteString.Char8 as S
import System.Timeout
import Foreign.C.String

main = do
    let str = concat (replicate 130 (replicate 100 'a' ++ "\n")) ++ "bbbbb\n"
    withCStringLen str $ \(cstr, l) ->
        hPutBuf stderr cstr l
    -- BUGGY
    -- hPutStr stderr str
    hPutStrLn stderr "THAT IS ALL FOLKS"

{-
main = do
    hSetBuffering stderr NoBuffering
    hSetBuffering stdout NoBuffering

    putStrLn "creating pipe"
    (readOut, writeOut) <- createPipe

    putStrLn "calling createProcess"
    (Just writeIn,_,_,proch) <- createProcess (proc "ghci" []) {
            std_in = CreatePipe,
            std_out = UseHandle writeOut,
            std_err = UseHandle writeOut,
            close_fds = True
        }
    hSetBuffering writeIn NoBuffering

    putStrLn "forking thread to interactively dump Python output"
    baton <- newEmptyMVar
    _ <- forkIO $ do
        hPutStr stderr =<< hGetContents readOut
        hClose readOut
        putMVar baton ()

    putStrLn "closing writeOut"
    hClose writeOut

    putStrLn "sending print 42"
    hPutStrLn writeIn "print 42"

    putStrLn "sending print 42"
    hPutStrLn writeIn "print 42"

    putStrLn "sending print 42"
    hPutStrLn writeIn "print 42"

    putStrLn "sending :quit"
    hPutStrLn writeIn ":quit"

    putStrLn "closing writeInt"
    hClose writeIn

    let timeout10s = timeout (1000 * 1000 * 10) -- 10 seconds

    putStrLn "waiting for process"
    mexit <- timeout10s $ waitForProcess proch
    exit <- case mexit of
        Nothing -> error "Waiting for process timed out"
        Just exit -> return exit

    putStrLn "waiting for reader thread to exit"
    readMVar baton

    putStrLn "exiting"
    exitWith exit
    -}
