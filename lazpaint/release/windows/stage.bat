@echo off
if not exist lazpaint.iss goto :baddir

echo Cleaning previous staging files...
if exist lazpaint32 del /s /q lazpaint32 >nul
if exist lazpaint32\i18n rmdir lazpaint32\i18n
if exist lazpaint32\models rmdir lazpaint32\models
if exist lazpaint32\scripts\lazpaint rmdir lazpaint32\scripts\lazpaint
if exist lazpaint32\scripts rmdir lazpaint32\scripts
if exist lazpaint32 rmdir lazpaint32
if exist lazpaint64 del /s /q lazpaint64 >nul
if exist lazpaint64\i18n rmdir lazpaint64\i18n
if exist lazpaint64\models rmdir lazpaint64\models
if exist lazpaint64\scripts\lazpaint rmdir lazpaint64\scripts\lazpaint
if exist lazpaint64\scripts rmdir lazpaint64\scripts
if exist lazpaint64 rmdir lazpaint64

echo Binary found:
dir /b ..\bin\lazpaint*.exe
echo.
if not exist ..\bin\lazpaint32.exe goto :missingbin32
echo Staging 32-bit version...
if not exist lazpaint32 mkdir lazpaint32
copy ..\bin\lazpaint32.exe lazpaint32\lazpaint.exe >nul
copy dcraw\dcraw32.exe lazpaint32\dcraw.exe >nul
copy libwebp\libwebp32.dll lazpaint32 >nul
if exist libavif\win32 copy libavif\win32\*.dll lazpaint32 >nul
copy ..\bin\readme.txt lazpaint32 >nul
copy ..\bin\*.ini lazpaint32 >nul
if not exist lazpaint32\i18n mkdir lazpaint32\i18n
copy ..\bin\i18n\lazpaint.* lazpaint32\i18n >nul
copy ..\bin\i18n\lcresourcestring.* lazpaint32\i18n >nul
copy ..\bin\i18n\lclstrconsts.* lazpaint32\i18n >nul
if not exist lazpaint32\models mkdir lazpaint32\models
copy ..\bin\models lazpaint32\models >nul
if not exist lazpaint32\scripts mkdir lazpaint32\scripts
copy ..\..\..\resources\scripts lazpaint32\scripts >nul
if not exist lazpaint32\scripts\lazpaint mkdir lazpaint32\scripts\lazpaint
copy ..\..\..\resources\scripts\lazpaint lazpaint32\scripts\lazpaint >nul
goto donebin32
:missingbin32
echo Error: 32-bit binary not found
:donebin32

if not exist ..\bin\lazpaint64.exe goto :missingbin64
echo Staging 64-bit version...
if not exist lazpaint64 mkdir lazpaint64
copy ..\bin\lazpaint64.exe lazpaint64\lazpaint.exe >nul
copy dcraw\dcraw64.exe lazpaint64\dcraw.exe >nul
copy libwebp\libwebp64.dll lazpaint64 >nul
if exist libavif\win64 copy libavif\win64\*.dll lazpaint64 >nul
copy ..\bin\readme.txt lazpaint64 >nul
copy ..\bin\*.ini lazpaint64 >nul
if not exist lazpaint64\i18n mkdir lazpaint64\i18n
copy ..\bin\i18n\lazpaint.* lazpaint64\i18n >nul
copy ..\bin\i18n\lcresourcestring.* lazpaint64\i18n >nul
copy ..\bin\i18n\lclstrconsts.* lazpaint64\i18n >nul
if not exist lazpaint64\models mkdir lazpaint64\models
copy ..\bin\models lazpaint64\models >nul
if not exist lazpaint64\scripts mkdir lazpaint64\scripts
copy ..\..\..\resources\scripts lazpaint64\scripts >nul
if not exist lazpaint64\scripts\lazpaint mkdir lazpaint64\scripts\lazpaint
copy ..\..\..\resources\scripts\lazpaint lazpaint64\scripts\lazpaint >nul
goto donebin64
:missingbin64
echo Error: 64-bit binary not found
:donebin64

goto end
:baddir
echo Error: must be launch from its directory
:end
pause
