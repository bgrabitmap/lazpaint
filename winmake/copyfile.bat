@echo off
if "%~1" == "" goto argument
if "%~2" == "" goto argument
setlocal
set FILENAME=%1
set FILENAME=%FILENAME:/=\%
set DESTINATION=%2
set DESTINATION=%DESTINATION:/=\%
copy /y %FILENAME% %DESTINATION%
exit /b 0

:argument
echo Copies a file to the specified destination.
echo Usage: copyfile [FILENAME] [DESTINATION]
exit /b 1


