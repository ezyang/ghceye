import System.Process
import System.IO
import System.Exit
main = do
    hSetBuffering stderr NoBuffering
    hSetBuffering stdout NoBuffering
    (readIn, writeIn) <- createPipe
    (readOut, writeOut) <- createPipe
    (_,_,_,proch) <- createProcess (proc "python" ["-i"]) {
            std_in = UseHandle readIn,
            std_out = UseHandle writeOut,
            std_err = UseHandle writeOut,
            create_group = True
        }
    hPutStrLn writeIn "print(42)"
    hPutStrLn writeIn "quit()"
    hClose writeIn
    exit <- waitForProcess proch
    hPutStrLn stderr =<< hGetContents readOut
    hClose readOut
    exitWith exit
