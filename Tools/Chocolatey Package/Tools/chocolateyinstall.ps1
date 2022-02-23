$ErrorActionPreference = 'Stop';
$toolsDir = "$(Split-Path -parent $MyInvocation.MyCommand.Definition)"
#$file     = Join-Path $toolsDir 'Skia4Delphi_3.1.0_Setup.exe'
$url     = 'https://github.com/skia4delphi/skia4delphi/releases/download/v3.1.0/Skia4Delphi_3.1.0_Setup.exe'

$packageArgs = @{
  packageName    = $env:ChocolateyPackageName
  unzipLocation  = $toolsDir
  fileType       = 'exe'
  url           = $url
  #file           = $file
  softwareName   = 'Skia4Delphi*'
  checksum       = '5D4327559949B0B878FB3600A9680CFB3CCACD03175E3E6B3ABFF7AF87CD0F6B'
  checksumType   = 'sha256'
  silentArgs     = "/VERYSILENT /LOG=`"$env:TEMP\$env:ChocolateyPackageName.$env:ChocolateyPackageVersion.log`""
  validExitCodes = @(0,3010,1641)
}

$pp = Get-PackageParameters

if ($pp.DIR) {
  $packageArgs['silentArgs'] = "/DIR=`"$($pp.DIR)`" $($packageArgs['silentArgs'])"
}

if ($pp.RADStudioVersions) {
  $packageArgs['silentArgs'] = "/RADStudioVersions=$($pp.RADStudioVersions) $($packageArgs['silentArgs'])"
} else {
  $packageArgs['silentArgs'] = "/RADStudioVersions=all $($packageArgs['silentArgs'])"
}

Install-ChocolateyPackage @packageArgs
