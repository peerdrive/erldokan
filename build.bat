@ECHO OFF

IF DEFINED ERL GOTO KnowErl
FOR /D %%D IN ("%ProgramFiles%\erl*") DO IF EXIST "%%D\bin\erl.exe" SET ERL=%%D
IF DEFINED ERL GOTO KnowErl
ECHO Could not find your erlang installation. Please set the ERL environment
ECHO variable to the erlang executables directory, e.g. ERL=C:\Erlang.
PAUSE
GOTO :EOF

:KnowErl


IF DEFINED MINGW GOTO KnowMingw
FOR /D %%D IN ("%ProgramFiles%\MinGW" C:\MinGW) DO IF EXIST "%%D\bin\gcc.exe" SET MINGW=%%D
IF DEFINED MINGW GOTO KnowMingw
ECHO Could not find your MinGW installation.
PAUSE
GOTO :EOF

:KnowMingw


IF DEFINED DOKAN GOTO KnowDokan
FOR /D %%D IN ("%ProgramFiles%\Dokan\DokanLibrary") DO IF EXIST "%%D\dokan.h" SET DOKAN=%%D
IF DEFINED DOKAN GOTO KnowDokan
ECHO Could not find your Dokan installation.
PAUSE
GOTO :EOF

:KnowDokan

SET PATH=%PATH%;%ERL%\bin;%MINGW%\bin

SET CFLAGS=-Wall -DUNICODE -DWINVER=0x0500
SET CFLAGS=%CFLAGS% -I "%DOKAN%" -I "%ERL%\usr\include" -I "%ERL%\lib\erl_interface-3.7.2\include"
SET LDFLAGS=-L %DOKAN% -ldokan -L%ERL%\lib\erl_interface-3.7.2\lib -lei -lkernel32

gcc -shared -o priv\erldokan_drv.dll %CFLAGS% c_src\dokan.def c_src\dokan.c %LDFLAGS%
erlc -o ebin -I include src\erldokan.erl src\hello.erl

