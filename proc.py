import subprocess
import os

(rin, win) = os.pipe()
(rout, wout) = os.pipe()
p = subprocess.Popen(["python", "-i"], stdin=rin, stdout=wout, stderr=wout, close_fds=True)
os.write(win, "print(2)")
os.write(win, os.linesep)
os.write(win, "quit()")
os.write(win, os.linesep)
p.wait()
