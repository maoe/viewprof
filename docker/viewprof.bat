@echo off

rem NOTE: This script assumes docker-machine.exe and cygpath are in the PATH!

for /f "usebackq delims=" %%e in (`docker-machine.exe env --shell cmd`) do %%e
for /f "usebackq delims=" %%v in (`cygpath %~f1`) do set viewprof_arg=%%v
docker run --rm -it -v %viewprof_arg%:/home/viewprof/%viewprof_arg% viewprof /home/viewprof%viewprof_arg%
