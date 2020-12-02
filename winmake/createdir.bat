@echo off
if "%~1" == "" goto argument
setlocal
set DIRNAME=%~1
set DIRNAME=%DIRNAME:/=\%
if not exist "%DIRNAME%\" mkdir "%DIRNAME%"
exit /b 0

:argument
echo Creates a directory if it doesn't exist.
echo Usage: createdir [DIRNAME]
exit /b 1