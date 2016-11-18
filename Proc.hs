import System.Process
import System.IO
import System.Exit
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
    hPutStrLn writeIn "print(42)"
    hPutStrLn writeIn "quit()"
    hClose writeIn
    exit <- waitForProcess proch
    hPutStrLn stderr =<< hGetContents readOut
    hClose readOut
    exitWith exit
