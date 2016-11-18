import System.Process
import System.IO
import System.Exit
main = do
    (readEnd, writeEnd) <- createPipe
    (_,_,_,proch) <- createProcess (proc "ghci" []) {
            std_in = UseHandle readEnd,
            close_fds = True,
            create_group = True
        }
    hPutStrLn writeEnd ":set prompt \"\""
    hPutStrLn writeEnd "print 42"
    hPutStrLn writeEnd ":quit"
    hClose writeEnd
    exit <- waitForProcess proch
    exitWith exit
