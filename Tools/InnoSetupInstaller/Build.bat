@echo off
  echo 1 rsvars.bat:  %1
  echo 2 .dproj file: %2
  echo 3 config:      %3
  echo 4 platform:    %4
  echo 5 dcu path:    %5
  echo 6 define:      %6
  echo 7 bpl path:    %7
  echo 8 pause:       "%8"
  call %1
setlocal
  set DprojDirectory=%~dp2
  set DprojDirectory=%DprojDirectory:~0,-1%
  for %%f in (%DprojDirectory%) do set DprojDelphi=%%~nf
  if not exist Logs\ mkdir Logs
  for /f "tokens=2,4" %%c in ('echo %~4 %~3') do set buildlog="Logs\%DprojDelphi%.%%c.%%d.MSBuildLog.txt"
  echo   build log:   %buildlog%
  %FrameworkDir%\msbuild.exe /nologo %2 /target:build /p:DCC_BuildAllUnits=true /p:%3 /p:%4 /p:%5 /p:%6 /p:%7 /l:FileLogger,Microsoft.Build.Engine;logfile=%buildlog%
endlocal
  if NOT "%8"=="" pause
  if %errorlevel% neq 0 exit /b %errorlevel%
