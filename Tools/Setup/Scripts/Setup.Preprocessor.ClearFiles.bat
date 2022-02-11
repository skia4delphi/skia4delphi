@echo off

rem #
rem # Copyright (c) 2011-2022 Google LLC.
rem # Copyright (c) 2021-2022 Skia4Delphi Project.
rem #
rem # Use of this source code is governed by a BSD-style license that can be
rem # found in the LICENSE file.
rem #

echo Cleaning Library...
for /f "tokens=* delims=" %%i in ('dir "..\..\..\Library\" /s /b /a:-d ^| find /v "\Win32\Release\" ^| find /v "\Win64\Release\"') do (
  echo Deleting "%%i"
  del "%%i"
)
