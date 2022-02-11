@echo off

rem #
rem # Copyright (c) 2011-2022 Google LLC.
rem # Copyright (c) 2021-2022 Skia4Delphi Project.
rem #
rem # Use of this source code is governed by a BSD-style license that can be
rem # found in the LICENSE file.
rem #

if "%1" == "" (
    echo Invalid parameter.
    exit 1
)

if "%~2" == "" (
    if exist "%programfiles(x86)%" (
        set "rsvars=%programfiles(x86)%\Embarcadero\Studio\%1\bin\rsvars.bat"
    ) else (
        set "rsvars=%programfiles%\Embarcadero\Studio\%1\bin\rsvars.bat"
    )
) else (
    set "rsvars=%~2\bin\rsvars.bat"
)

if not exist "%rsvars%" (
    echo Could not automatically detect BDS %1 directory.
    echo Run the command: "Build.bat <BDS directory>"
    exit 1
)

call "%rsvars%"

echo Building RTL package for BDS %1...
MSBuild Skia.Package.RTL.dproj /p:Config=Release /p:Platform=Win32 >Build.RTL.log 2>&1 || goto :BUILD_ERROR

echo Building VCL package for BDS %1...
MSBuild Skia.Package.VCL.dproj /p:Config=Release /p:Platform=Win32 >Build.VCL.log 2>&1 || goto :BUILD_ERROR

echo Building VCL Designtime package for BDS %1...
MSBuild Skia.Package.VCL.Designtime.dproj /p:Config=Release /p:Platform=Win32 >Build.VCL.Designtime.log 2>&1 || goto :BUILD_ERROR

echo Building FMX package for BDS %1...
MSBuild Skia.Package.FMX.dproj /p:Config=Release /p:Platform=Win32 >Build.FMX.log 2>&1 || goto :BUILD_ERROR

echo Building FMX Designtime package for BDS %1...
MSBuild Skia.Package.FMX.Designtime.dproj /p:Config=Release /p:Platform=Win32 >Build.FMX.Designtime.log 2>&1 || goto :BUILD_ERROR

echo Finished.
goto :EOF

:BUILD_ERROR
echo An error occurred during build.
exit 1
