$ErrorActionPreference = 'Stop';
$toolsDir = "$(Split-Path -parent $MyInvocation.MyCommand.Definition)"
$file     = Join-Path $toolsDir 'Skia4Delphi_3.0.1_Setup.exe'
#$url     = 'https://github.com/skia4delphi/skia4delphi/releases/download/v3.0.1/Skia4Delphi_3.0.1_Setup.exe'

$packageArgs = @{
  packageName    = $env:ChocolateyPackageName
  unzipLocation  = $toolsDir
  fileType       = 'exe'
  #url           = $url
  file           = $file
  softwareName   = 'Skia4Delphi*'
  checksum       = '1DEFBE78BEE7BCEA153A271087B4706E6AF82CFFA1A645AE9A29E75E309B3919'
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
