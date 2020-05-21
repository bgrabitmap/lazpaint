@echo off
if "%~1" == "" goto argument
setlocal
set FILENAME=%1
set FILENAME=%FILENAME:/=\%
if exist %FILENAME% del /f /q %FILENAME%
exit /b 0

:argument
echo Removes a file if it exists.
echo Usage: remove [FILENAME]
exit /b 1


