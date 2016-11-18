{-# LANGUAGE OverloadedStrings #-}
import System.Process
import System.IO
import System.Exit
import qualified Data.ByteString.Char8 as S
import System.Timeout
main = do
    hSetBuffering stderr NoBuffering
    hSetBuffering stdout NoBuffering

    putStrLn "creating pipe"
    (readOut, writeOut) <- createPipe

    putStrLn "calling createProcess"
    (Just writeIn,_,_,proch) <- createProcess (proc "python" ["-i"]) {
            std_in = CreatePipe,
            std_out = UseHandle writeOut,
            std_err = UseHandle writeOut,
            close_fds = True
        }

    putStrLn "closing writeOut"
    hClose writeOut

    putStrLn "sending print(42)"
    S.hPutStrLn writeIn "print(42)"

    putStrLn "sending quit()"
    S.hPutStrLn writeIn "quit()"

    putStrLn "closing writeInt"
    hClose writeIn

    let timeout10s = timeout (1000 * 1000 * 10) -- 10 seconds

    putStrLn "waiting for process"
    mexit <- timeout10s $ waitForProcess proch
    exit <- case mexit of
        Nothing -> error "Waiting for process timed out"
        Just exit -> return exit

    putStrLn "calling hGetContents"
    mres <- timeout10s $ S.hGetContents readOut

    putStrLn "closing readOut"
    hClose readOut

    case mres of
        Nothing -> error "Did not complete"
        Just bs -> do
            putStrLn "sending output to stderr"
            S.hPutStrLn stderr bs
            putStrLn "exiting"
            exitWith exit
