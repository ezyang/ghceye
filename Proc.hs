{-# LANGUAGE OverloadedStrings #-}
import System.Process
import System.IO
import System.Exit
import qualified Data.ByteString.Char8 as S
import System.Timeout
main = do
    hSetBuffering stderr NoBuffering
    hSetBuffering stdout NoBuffering
    (readOut, writeOut) <- createPipe
    (Just writeIn,_,_,proch) <- createProcess (proc "python" ["-i"]) {
            std_in = CreatePipe,
            std_out = UseHandle writeOut,
            std_err = UseHandle writeOut,
            close_fds = True
        }
    hClose writeOut
    S.hPutStrLn writeIn "print(42)"
    S.hPutStrLn writeIn "quit()"
    hClose writeIn
    exit <- waitForProcess proch
    mres <- timeout
        (1000 * 1000 * 10) -- 10 seconds
        (S.hPutStrLn stderr =<< S.hGetContents readOut)
    hClose readOut
    case mres of
        Nothing -> error "Did not complete"
        Just () -> exitWith exit
