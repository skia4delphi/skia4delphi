$ErrorActionPreference = 'Stop';
$toolsDir = "$(Split-Path -parent $MyInvocation.MyCommand.Definition)"
$file     = Join-Path $toolsDir 'Skia4Delphi_3.0.0_Setup.exe'
#$url     = 'https://github.com/viniciusfbb/skia4delphi/releases/download/v3.0.0/Skia4Delphi_3.0.0_Setup.exe'

$packageArgs = @{
  packageName    = $env:ChocolateyPackageName
  unzipLocation  = $toolsDir
  fileType       = 'exe'
  #url           = $url
  file           = $file
  softwareName   = 'Skia4Delphi*'
  checksum       = '4BFFC6214A8807671A7605EEC1C0BCB24C44808C8986E04DA5A371C479890AA5'
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
