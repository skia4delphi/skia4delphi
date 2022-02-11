<p><a href="https://www.skia4delphi.org"><img src="../Assets/Artwork/logo-gradient.svg" alt="Logo" height="300" width="360" /></a></p>

# Build

**Skia4Delphi** has its own [forked version of Skia](../../../../skia) with the necessary edits to make the library work. To compile it is very simple, just run a native script, and it will help you build until the end.

  

## Prerequisites

| OS      | Targets                                          |
| ------- | ------------------------------------------------ |
| Linux   | Linux64, Android, Android64                      |
| macOS   | OSX64, OSXARM64, iOSDevice64, Android, Android64 |
| Windows | Win32, Win64, Android, Android64                 |

  

## Downloading

Run the command on Windows via command line or PowerShell, or from Linux/macOS via shell: `git clone https://github.com/skia4delphi/skia.git`

 

## Building

Just use the build script. To access help run the command:

- Through Windows via the command line:
  ```batch
  sk4d-build -h
  ```

- Through Linux or macOS via the terminal:
  ```bash
  ./sk4d-build.sh -h
  ```

### Examples

  - Building for Win64 through Windows: `sk4d-build --targets win64`
  - Building for Android64 through Windows: `sk4d-build --targets Android64 --ndk C:\android-ndk-r23b`
  - Building for iOSDevice64 through macOS: `./sk4d-build.sh --targets iosdevice64`
  - Building for Linux64 through Linux: `./sk4d-build.sh --targets linux64`

*Note: If you don't pass the ndk parameter to the Android build, the script will help you to download it.*
