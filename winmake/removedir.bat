@echo off
if "%~1" == "" goto argument
setlocal
set DIRNAME=%1
set DIRNAME=%DIRNAME:/=\%
if exist %DIRNAME% rd /s /q %DIRNAME%
exit /b 0

:argument
echo Removes a directory and all its content if it exists.
echo Usage: removedir [DIRNAME]
exit /b 1


