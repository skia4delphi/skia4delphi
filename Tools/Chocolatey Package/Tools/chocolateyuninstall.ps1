$ErrorActionPreference = 'Stop';
$packageArgs = @{
  packageName    = $env:ChocolateyPackageName
  softwareName   = 'Skia4Delphi*'
  fileType       = 'exe'
  silentArgs     = "/VERYSILENT /RADStudioVersions=all /SUPPRESSMSGBOXES /SP- /LOG=`"$env:TEMP\$env:ChocolateyPackageName.$env:ChocolateyPackageVersion.Uninstall.log`""
  validExitCodes = @(0)
}

$pp = Get-PackageParameters
if ($pp.RADStudioVersions) {
  $packageArgs['silentArgs'] = "/RADStudioVersions=$($pp.RADStudioVersions) $($packageArgs['silentArgs'])"
} else {
  $packageArgs['silentArgs'] = "/RADStudioVersions=all $($packageArgs['silentArgs'])"
}

[array]$key = Get-UninstallRegistryKey -SoftwareName $packageArgs['softwareName']

if ($key.Count -eq 1) {
  $key | % {
    $packageArgs['file'] = "$($_.UninstallString.Trim('"'))"

    if ($packageArgs['fileType'] -eq 'MSI') {
      $packageArgs['silentArgs'] = "$($_.PSChildName) $($packageArgs['silentArgs'])"
      $packageArgs['file'] = ''
    }

    Uninstall-ChocolateyPackage @packageArgs
  }
} elseif ($key.Count -eq 0) {
  Write-Warning "$packageName has already been uninstalled by other means."
} elseif ($key.Count -gt 1) {
  Write-Warning "$($key.Count) matches found!"
  Write-Warning "To prevent accidental data loss, no programs will be uninstalled."
  Write-Warning "Please alert package maintainer the following keys were matched:"
  $key | % {Write-Warning "- $($_.DisplayName)"}
}
