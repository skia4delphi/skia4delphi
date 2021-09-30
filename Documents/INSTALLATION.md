<img src="/Assets/Artwork/LogoGradient.svg" width=360 height=200>



# Installation



## Prerequisites

#### Delphi XE6+

  - Console / VCL / FMX application platforms:
      - Windows 32-bit
      - Windows 64-bit


#### Delphi 10.4 Sydney+

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



## 1) Installation via GetIt

The easiest and recommended way to install the library is through the GetIt. Just open your RAD Studio and go to Tools > GetIt Package Manager... > search for "Skia".

![getit](/Assets/Documents/getit.png)

After finish the installation, you are able to play with skia, and you can run our sample.

**Important:** To use Skia in a new application project, you should [enable your project](https://github.com/viniciusfbb/skia4delphi#enabling-your-project)



## 2) Installation via setup

Another easy and recommended way to install the library is through our installer. Just download our setup from the [Releases page](https://github.com/viniciusfbb/skia4delphi/releases).

<img src="/Assets/Documents/installation1.png" width=340> <img src="/Assets/Documents/installation2.png" width=340>

You don't need to build/set nothing, just run the setup.
After finish the setup, you are able to play with skia, and you can run our sample.

**Important:** To use Skia in a new application project, you should [enable your project](https://github.com/viniciusfbb/skia4delphi#enabling-your-project)



## 3) Manually installation

### Adopted nomenclatures

For `[delphi_version_name]`
- DelphiXE6
- DelphiXE7
- DelphiXE8
- Delphi10Seattle
- Delphi10Berlin
- Delphi10Tokyo
- Delphi10Rio
- Delphi10Sydney
- Delphi11Alexandria

For `[platform_name]`
- Win32
- Win64
- Android
- Android64
- iOSDevice64
- OSX64
- OSXARM64
- Linux64

For `[skia4delphi_root]`
  The full path of our library after download and extract files


### Considerations

- Even if you are only interested in FMX applications, or even Console, it is essential to install the Vcl packages, as there is a plugin for the IDE in the package `Skia.Package.VCL.Designtime.dproj` that is necessary for [enabling Skia in your application project](https://github.com/viniciusfbb/skia4delphi#enabling-your-project)
- Don't enable Linux64 compilation in `Skia.Package.FMX.dproj` package unless you have FmxLinux installed.
- If you are cloning the repository for installation, be aware that it is not necessary to download submodules (the Externals folder), because we provide the pre-built binaries (located in the Binary folder) for all platforms (although you can also [compile your own binary files](/Documents/BUILD.md)).
- Do all the steps below in order.


### Steps

1. If you have another version of Skia4Delphi, uninstall and remove all files and settings that you made.
2. [Download our repository](https://github.com/viniciusfbb/skia4delphi/archive/refs/heads/main.zip).
3. Open RAD Studio, go to menu Tools > Options... > IDE > Environment Variables, and:

   3.1. Click in "New...", and add the variable name `SKIADIR`, and in "variable value" put your Skia4Delphi folder (full path), for example *C:\skia4delphi*, and click in "OK". From here, we'll call this path as `[skia4delphi_root]` in this documentation. But remember: in the step that is written `[skia4delphi_root]`, it must be the complete path (without environment variables).

   3.2. Still in the "Environment Variables" page, in section "User System Overrides", find the `PATH` variable double click in it to change it value. You need to add the these three folders (full path) in begin:
      - `[skia4delphi_root]\Binary\Shared\Win32;`

        Like: *C:\skia4delphi\Binary\Shared\Win32;*
      - `[skia4delphi_root]\Library\[delphi_version_name]\Win32\Release\Bpl;`

        Like: *C:\skia4delphi\Library\Delphi11Alexandria\Win32\Release\Bpl;*
      - `[skia4delphi_root]\Library\[delphi_version_name]\Win64\Release\Bpl;`

        Like: *C:\skia4delphi\Library\Delphi11Alexandria\Win64\Release\Bpl;*

   3.3. Click in "OK" and in "Save"

4. Open the project group of your RAD Studio version, file `[skia4delphi_root]\Packages\[delphi_version_name]\Skia4Delphi.groupproj`, like *C:\skia4delphi\Packages\Delphi11Alexandria\Skia4Delphi.groupproj*
5. Click in **Show Build Groups Panel**
6. All platforms are selected by default in "Skia.Package.RTL" and "Skia.Package.FMX", but you should select just the platforms that you want. Remenber that if you are in Delphi 10.3 Rio or older, you can't compile for non windows platforms. And remember also that the package `Skia.Package.FMX.[delphi_version_name].bpl` cannot be compiled by Linux64, unless that you have the FmxLinux installed.
7. Click in "Build the current build group"
8. Install the two installable packages (`Skia.Package.FMX.Designtime.[delphi_version_name].bpl` and `Skia.Package.VCL.Designtime.[delphi_version_name].bpl`) clicking with with right button and "Install".
9. In your RAD Studio, go to menu Tools > Options... > Language > Delphi > Library, and add for each platform that you have compiled, the paths:

   9.1. In "Library path":
      - `$(SKIADIR)\Library\[delphi_version_name]\[platform_name]\Release`

        Like: *$(SKIADIR)\Library\Delphi11Alexandria\Win32\Release*

   9.2. In "Browsing path":
      - `$(SKIADIR)\Source`
      - `$(SKIADIR)\Source\FMX`
      - `$(SKIADIR)\Source\VCL`

   9.3. In "Debug DCU path":
      - `$(SKIADIR)\Library\[delphi_version_name]\[platform_name]\Debug`

        Like: *$(SKIADIR)\Library\Delphi11Alexandria\Win32\Debug*

   9.4. Just in iOSDevice64 (if you has compiled for it), add an extra path to the "Library path":
      - `$(SKIADIR)\Binary\Static\iOSDevice64`

10. Click in "Save" the Options page.


#### Finished

After made all steps above and in the order (the order matter), you are able to play with skia, and you can run our sample in folder `[skia4delphi_root]\Samples`

**Important:** To use Skia in a new application project, you should [enable your project](https://github.com/viniciusfbb/skia4delphi#enabling-your-project)
