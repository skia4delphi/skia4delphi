@echo off

rem #
rem # Copyright (c) 2011-2022 Google LLC.
rem # Copyright (c) 2021-2022 Skia4Delphi Project.
rem #
rem # Use of this source code is governed by a BSD-style license that can be
rem # found in the LICENSE file.
rem #

setlocal
pushd "%~dp0"

call ..\Common\Skia.Build.Delphi.bat 15.0 %1

popd
endlocal
