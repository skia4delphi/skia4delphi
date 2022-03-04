$ErrorActionPreference = 'Stop';
$toolsDir = "$(Split-Path -parent $MyInvocation.MyCommand.Definition)"
#$file     = Join-Path $toolsDir 'Skia4Delphi_3.2.0_Setup.exe'
$url     = 'https://github.com/skia4delphi/skia4delphi/releases/download/v3.2.0/Skia4Delphi_3.2.0_Setup.exe'

$packageArgs = @{
  packageName    = $env:ChocolateyPackageName
  unzipLocation  = $toolsDir
  fileType       = 'exe'
  url            = $url
  #file          = $file
  softwareName   = 'Skia4Delphi*'
  checksum       = '577A1FC24F0E90EC8FFBDA79609D8618EB16B6BBFD36DA3B1F218ADBFA4F1130'
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
