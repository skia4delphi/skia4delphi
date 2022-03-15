<p><a href="https://www.skia4delphi.org"><img src="../Assets/Artwork/logo-gradient.svg" alt="Logo" height="300" width="360" /></a></p>

#  Installation

## Prerequisites

### RAD Studio XE7 or newer

  - Console / VCL / FMX application platforms:
      - Windows 32-bit
      - Windows 64-bit


### RAD Studio 10.3 or newer

  - Console / VCL aplication platforms:
      - Windows 32-bit
      - Windows 64-bit
  - FMX aplication platforms:
      - Windows 32-bit
      - Windows 64-bit
      - Android 32-bit
      - Android 64-bit


### RAD Studio 11.0 Alexandria

  - Console aplication platforms:
      - Windows 32-bit
      - Windows 64-bit
      - MacOS 64-bit
      - MacOS ARM 64-bit
      - Linux 64-bit
  - VCL aplication platforms:
      - Windows 32-bit
      - Windows 64-bit
  - FMX aplication platforms:
      - Windows 32-bit
      - Windows 64-bit
      - Android 32-bit
      - Android 64-bit
      - iOS Device 64-bit
      - MacOS 64-bit
      - MacOS ARM 64-bit
      - Linux 64-bit


## Steps

Installation can be done automatically or manually.

**Remark: To use Skia after installation it is necessary to [enable](../README.md#enable-skia) it in your project.**

  

### Automatic

1. Automatic installation can be done in 3 ways:
   - Setup (recommended)
     Download the [latest release](../../../releases/latest) and install it.
     
     ![Skia4Delphi Installation](../Assets/Documents/installation.png)
     
   - Embarcadero's GetIt *(RAD Studio > Tools > GetIt Package Manager...)*
     
     <p><img src="https://user-images.githubusercontent.com/11139086/158433099-2ecbd6d1-4d1e-425a-93b4-4d6d5b95b40a.png#gh-light-mode-only" width="510" alt="GetIt" /></p>
     <p><img src="https://user-images.githubusercontent.com/11139086/158433116-89d5f148-fb2f-43b1-9afa-191c0c33b2b6.png#gh-dark-mode-only" width="510" alt="GetIt" /></p>
     
   - Chocolatey package manager
     
     To install run the following command from the command line or from PowerShell:
     
     ```batch
     choco install skia4delphi
     ```

  

2. Finished!

  

### Manual

1. Adopted nomenclatures:

   For `[rad_version_name]`:

   - RAD Studio XE7
   - RAD Studio XE8
   - RAD Studio 10.0 Seattle
   - RAD Studio 10.1 Berlin
   - RAD Studio 10.2 Tokyo
   - RAD Studio 10.3 Rio
   - RAD Studio 10.4 Sydney
   - RAD Studio 11.0 Alexandria

   For `[rad_package_suffix]`:

   - RAD Studio XE7: 210
   - RAD Studio XE8: 220
   - RAD Studio 10.0 Seattle: 230
   - RAD Studio 10.1 Berlin: 240
   - RAD Studio 10.2 Tokyo: 250
   - RAD Studio 10.3 Rio: 260
   - RAD Studio 10.4 Sydney: 270
   - RAD Studio 11.0 Alexandria: 280

   For `[platform_name]`:
   - Win32
   - Win64
   - Android
   - Android64
   - iOSDevice64
   - OSX64
   - OSXARM64
   - Linux64

   For `[skia4delphi_root]` the full path of our library after download and extract files.
2. Considerations:
   - Even if you are only interested in **FMX** applications, or even **Console**, it is essential to install the **VCL** packages, as there is a plugin for the IDE in the package `Skia.Package.VCL.Designtime.dproj` that is necessary for [enable](../README.md#enable-skia) it in your project;
   - If you are cloning the repository for installation, be aware that it is not necessary to download submodules (the Externals folder), because we provide the pre-built binaries (located in the Binary folder) for all platforms. (although you can also [compile your own binary files](BUILD.md))
3. If you have another version of **Skia4Delphi**, uninstall and remove all files and settings that you made;
4. [Download our repository](../../archive/refs/heads/main.zip);
5. Open RAD Studio, go to *Tools > Options... > IDE > Environment Variables*, and:
   
   5.1. Click *New...*, and add the variable name `SKIADIR`, and in *variable value* put your **Skia4Delphi** full path *(eg C:\skia4delphi)*, and click *OK*. From here, we'll call this path as `[skia4delphi_root]` in this documentation. But remember: in the step that is written `[skia4delphi_root]`, it must be filled with full path (without environment variables);
   
   5.2. Still in the *Environment Variables* page, in the *User System Overrides* section, find the `PATH` variable and double click to change it. You need to add the these three folders (full path) at the beginning:
   
   - `[skia4delphi_root]\Binary\Win32\Release;`
     *eg C:\skia4delphi\Binary\Win32\Release;*
   - `[skia4delphi_root]\Library\[rad_version_name]\Win32\Release\Bpl;`
     *eg C:\skia4delphi\Library\RAD Studio 11.0 Alexandria\Win32\Release\Bpl;*
   - `[skia4delphi_root]\Library\[rad_version_name]\Win64\Release\Bpl;`
     *eg C:\skia4delphi\Library\RAD Studio 11.0 Alexandria\Win64\Release\Bpl;*
   
   5.3. Click *OK* and *Save*;
   
6. Open the project group of your RAD Studio version, file `[skia4delphi_root]\Packages\[rad_version_name]\Skia4Delphi.groupproj`; *(eg C:\skia4delphi\Packages\RAD Studio 11.0 Alexandria\Skia4Delphi.groupproj)*
7. Click *Show Build Groups Panel*;
8. All platforms are selected by default in `Skia.Package.RTL` and `Skia.Package.FMX`, but you should select just the platforms that you want.;
9. Click *Build the current build group*;
10. Install the two installable packages: `Skia.Package.FMX.Designtime[rad_package_suffix].bpl` and `Skia.Package.VCL.Designtime[rad_package_suffix].bpl`; (right-click than *Install*);
11. In your RAD Studio, go to menu *Tools > Options... > Language > Delphi > Library*, and add for each platform that you want have compiled, the paths:
    
    11.1. Library path:
    - `$(SKIADIR)\Library\[rad_version_name]\[platform_name]\Release`
      *eg $(SKIADIR)\Library\RAD Studio 11.0 Alexandria\Win32\Release*
    
    11.2. Browsing path:
    - `$(SKIADIR)\Source`
    - `$(SKIADIR)\Source\FMX`
    - `$(SKIADIR)\Source\VCL`
    
    11.3. Debug DCU path:
    - `$(SKIADIR)\Library\[rad_version_name]\[platform_name]\Debug`
      *eg $(SKIADIR)\Library\RAD Studio 11.0 Alexandria\Win32\Debug*
    
    11.4. Just in **iOSDevice64** (if you has compiled for it), add an extra path to the *Library path*:
    - `$(SKIADIR)\Binary\iOSDevice64\Release`
    
12. Click *Save*;
13. After made all steps above and in the order (the order matter), you are able to play with **Skia4Delphi**, and you can run our sample in folder `[skia4delphi_root]\Samples`.

  

### Remarks

1. The pre-built Skia binary for **Linux64** targets was compiled for Debian (eg Ubuntu) and Red Hat (eg CentOS) based systems. If you want another distro you will need to [rebuild;](BUILD.md)
2. For Red Hat based systems you must replace the `Binary/Linux64/Release/libsk4d.so` file with the `Binary/Linux64/Release/Other Distributions/Red Hat/libsk4d.so` file.
