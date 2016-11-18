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
    putStrLn "waiting for process"
    exit <- waitForProcess proch
    putStrLn "calling timeout"
    mres <- timeout (1000 * 1000 * 10) $ do -- 10 seconds
        putStrLn "calling hGetContents"
        S.hGetContents readOut
    putStrLn "closing readOut"
    hClose readOut
    case mres of
        Nothing -> error "Did not complete"
        Just bs -> do
            putStrLn "sending output to stderr"
            S.hPutStrLn stderr bs
            putStrLn "exiting"
            exitWith exit
