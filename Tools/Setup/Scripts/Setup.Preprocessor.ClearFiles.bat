@echo off

rem #
rem # Copyright (c) 2021-2024 Skia4Delphi Project.
rem #
rem # Use of this source code is governed by the MIT license that can be
rem # found in the LICENSE file.
rem #

echo Cleaning Library...
for /f "tokens=* delims=" %%i in ('dir "..\..\..\Library\" /s /b /a:-d ^| find /v "\Win32\Release\" ^| find /v "\Win64\Release\"') do (
  echo Deleting "%%i"
  del "%%i"
)
