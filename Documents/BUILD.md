<img src="/Assets/Artwork/LogoGradient.svg" width=360 height=200>

# Build Skia

The same [rules](https://skia.org/docs/user/build/) as the original Skia are used. Our Skia version was developed exclusively for the **Skia4Dephi** project and the minimum requirements follow the latest version of Delphi.

## 1. Preparing the Host

### 1.1 Windows

1. Install [Chocolatey](https://chocolatey.org/) package manager;

2. Install the necessary packages by running the command in Command Prompt or PowerShell, opened with admin rights:

   ```powershell
   choco install -y git python ninja
   ```




### 1.2 Linux

1. Open Shell Prompt;

2. Install the necessary packages by running the command:

   **Debian/Ubuntu**

   ```bash
   $ sudo apt-get -y install git ninja-build python
   ```

   **CentOS**

   ```bash
   $ dnf -y --enablerepo=powertools install git ninja-build python
   ```

   **Fedora**

   ```bash
   $ dnf -y install git ninja-build python
   ```



### 2.3 macOS

1. Install [Homebrew](https://brew.sh) package manager;

2. Open the Terminal;

3. Install the necessary packages by running the command:

   ```bash
   $ brew install ninja
   ```



## 2. Downloading

1. Open the Windows Command Prompt or PowerShell, or macOS Terminal, or Linux Shell Prompt, then go to the folder where Skia will be downloaded;

2. Download Skia by running the command:

   **Windows (Command Prompt)**

   ```batch
   git clone https://github.com/viniciusfbb/skia.git && cd skia && python tools\git-sync-deps
   ```

   **Windows (PowerShell)**

   ```powershell
   git clone https://github.com/viniciusfbb/skia.git; if ($?) { cd skia; python tools\git-sync-deps; }
   ```

   **Other platforms**

   ```bash
   $ git clone https://github.com/viniciusfbb/skia.git && cd skia && python tools/git-sync-deps
   ```



## 3. Building

### 3.1 Windows

The instructions below are for compiling using **Windows** as host.

#### Prebuild

1. Install the latest [LLVM](https://releases.llvm.org/download.html) stable release;

2. Download the free [Build Tools for Visual Studio 2019](https://visualstudio.microsoft.com/pt-br/downloads/#build-tools-for-visual-studio-2019);

3. Launch Build Tools for Visual Studio 2019 installer and then install the workload Desktop development with C++.

#### Build for Windows 32Bit

1. Access the Skia folder through Command Prompt or PowerShell;

2. Run the command to generate build files and if necessary change the LLVM installation directory:

   **Release (Command Prompt)**

   ```batch
   bin\gn gen shared\Win32\Release "--args=target_cpu="""x86""" target_os="""win""" is_official_build=true skia_use_system_expat=false skia_use_system_harfbuzz=false skia_use_system_icu=false skia_use_system_libjpeg_turbo=false skia_use_system_libpng=false skia_use_system_libwebp=false skia_use_system_zlib=false clang_win="""C:\Program Files\LLVM""""
   ```

   **Release (PowerShell)**

   ```powershell
   bin\gn gen shared\Win32\Release '--args=target_cpu="""x86""" target_os="""win""" is_official_build=true skia_use_system_expat=false skia_use_system_harfbuzz=false skia_use_system_icu=false skia_use_system_libjpeg_turbo=false skia_use_system_libpng=false skia_use_system_libwebp=false skia_use_system_zlib=false clang_win="""C:\Program Files\LLVM"""'
   ```

   **Debug (Command Prompt)**

   ```batch
   bin\gn gen shared\Win32\Debug "--args=target_cpu="""x86""" target_os="""win""" clang_win="""C:\Program Files\LLVM""""
   ```

   **Debug (PowerShell)**

   ```powershell
   bin\gn gen shared\Win32\Debug '--args=target_cpu="""x86""" target_os="""win""" clang_win="""C:\Program Files\LLVM"""'
   ```


3. Run the command to Build:

   **Release (Command Prompt)**

   ```batch
   ninja -C shared\Win32\Release sk4d
   ```

   **Release (PowerShell)**

   ```powershell
   ninja -C shared\Win32\Release sk4d
   ```

   **Debug (Command Prompt)**

   ```batch
   ninja -C shared\Win32\Debug sk4d
   ```

   **Debug (PowerShell)**

   ```powershell
   ninja -C shared\Win32\Debug sk4d
   ```



#### Build for Windows 64Bit

1. Access the Skia folder through Command Prompt or PowerShell;

2. Run the command to generate build files and if necessary change the LLVM installation directory:

   **Release (Command Prompt)**

   ```batch
   bin\gn gen shared\Win64\Release "--args=target_cpu="""x64""" target_os="""win""" is_official_build=true skia_use_system_expat=false skia_use_system_harfbuzz=false skia_use_system_icu=false skia_use_system_libjpeg_turbo=false skia_use_system_libpng=false skia_use_system_libwebp=false skia_use_system_zlib=false clang_win="""C:\Program Files\LLVM""""
   ```

   **Release (PowerShell)**

   ```powershell
   bin\gn gen shared\Win64\Release '--args=target_cpu="""x64""" target_os="""win""" is_official_build=true skia_use_system_expat=false skia_use_system_harfbuzz=false skia_use_system_icu=false skia_use_system_libjpeg_turbo=false skia_use_system_libpng=false skia_use_system_libwebp=false skia_use_system_zlib=false clang_win="""C:\Program Files\LLVM"""'
   ```

   **Debug (Command Prompt)**

   ```batch
   bin\gn gen shared\Win64\Debug "--args=target_cpu="""x64""" target_os="""win""" clang_win="""C:\Program Files\LLVM""""
   ```

   **Debug (PowerShell)**

   ```powershell
   bin\gn gen shared\Win64\Debug '--args=target_cpu="""x64""" target_os="""win""" clang_win="""C:\Program Files\LLVM"""'
   ```


3. Run the command to Build:

   **Release (Command Prompt)**

   ```batch
   ninja -C shared\Win64\Release sk4d
   ```

   **Release (PowerShell)**

   ```powershell
   ninja -C shared\Win64\Release sk4d
   ```

   **Debug (Command Prompt)**

   ```batch
   ninja -C shared\Win64\Debug sk4d
   ```

   **Debug (PowerShell)**

   ```powershell
   ninja -C shared\Win64\Debug sk4d
   ```



### 3.2 Linux

The instructions below are for compiling using **Linux** as host.

#### Prebuild

1. Open Shell Prompt;

2. Install the necessary packages by running the command:

   **Debian/Ubuntu**

   ```bash
   $ sudo apt-get -y install clang libfontconfig1-dev libgl1-mesa-dev llvm
   ```

   **CentOS/Fedora**

   ```bash
   $ dnf -y install clang fontconfig-devel llvm mesa-libGL-devel
   ```



#### Build for Linux 64Bit

1. Access the Skia folder through Shell Prompt;

2. Run the command to generate build files:

   **Release**

   ```bash
   $ bin/gn gen shared/Linux64/Release --args='target_cpu="x64" target_os="linux" is_official_build=true skia_use_system_expat=false skia_use_system_freetype2=false skia_use_system_harfbuzz=false skia_use_system_icu=false skia_use_system_libjpeg_turbo=false skia_use_system_libpng=false skia_use_system_libwebp=false skia_use_system_zlib=false cc="clang" cxx="clang++" ar="llvm-ar"'
   ```

   **Debug**

   ```bash
   $ bin/gn gen shared/Linux64/Debug --args='target_cpu="x64" target_os="linux" cc="clang" cxx="clang++" ar="llvm-ar"'
   ```


3. Run the command to Build:

   **Release**

   ```bash
   $ ninja -C shared/Linux64/Release sk4d
   ```

   **Debug**

   ```bash
   $ ninja -C shared/Linux64/Debug sk4d
   ```



### 3.3 macOS

The instructions below are for compiling using **macOS** as host.

#### Build for macOS 64Bit

1. Access the Skia folder through Terminal;

2. Run the command to generate build files:

   **Release**

   ```bash
   $ bin/gn gen shared/OSX64/Release --args='target_cpu="x64" target_os="mac" is_official_build=true skia_use_system_expat=false skia_use_system_harfbuzz=false skia_use_system_icu=false skia_use_system_libjpeg_turbo=false skia_use_system_libpng=false skia_use_system_libwebp=false skia_use_system_zlib=false'
   ```

   **Debug**

   ```bash
   $ bin/gn gen shared/OSX64/Debug --args='target_cpu="x64" target_os="mac"'
   ```


3. Run the command to Build:

   **Release**

   ```bash
   $ ninja -C shared/OSX64/Release sk4d
   ```

   **Debug**

   ```bash
   $ ninja -C shared/OSX64/Debug sk4d
   ```



#### Build for macOS ARM64

1. Access the Skia folder through Terminal;

2. Run the command to generate build files:

   **Release**

   ```bash
   $ bin/gn gen shared/OSXARM64/Release --args='target_cpu="arm64" target_os="mac" is_official_build=true skia_use_system_expat=false skia_use_system_harfbuzz=false skia_use_system_icu=false skia_use_system_libjpeg_turbo=false skia_use_system_libpng=false skia_use_system_libwebp=false skia_use_system_zlib=false'
   ```

   **Debug**

   ```bash
   $ bin/gn gen shared/OSXARM64/Debug --args='target_cpu="arm64" target_os="mac"'
   ```


3. Run the command to Build:

   **Release**

   ```bash
   $ ninja -C shared/OSXARM64/Release sk4d
   ```

   **Debug**

   ```bash
   $ ninja -C shared/OSXARM64/Debug sk4d
   ```



#### Building a Universal macOS Binary

Run the command to create a universal binary:

   **Release**

   ```bash
   $ lipo -create ./shared/OSXARM64/Release/sk4d.dylib ./shared/OSX64/Release/sk4d.dylib -output ./sk4d.dylib
   ```

   **Debug**

   ```bash
   $ lipo -create ./shared/OSXARM64/Debug/sk4d.dylib ./shared/OSX64/Debug/sk4d.dylib -output ./sk4d.dylib
   ```



### 3.4 iOS

The instructions below are for compiling using **macOS** as host.

#### Build for iOS ARM64

1. Access the Skia folder through Terminal;

2. Run the command to generate build files:

   **Release**

   ```bash
   $ bin/gn gen static/iOSDevice64/Release --args='target_cpu="arm64" target_os="ios" is_official_build=true skia_use_system_expat=false skia_use_system_harfbuzz=false skia_use_system_icu=false skia_use_system_libjpeg_turbo=false skia_use_system_libpng=false skia_use_system_libwebp=false skia_use_system_zlib=false'
   ```

   **Debug**

   ```bash
   $ bin/gn gen static/iOSDevice64/Debug --args='target_cpu="arm64" target_os="ios"'
   ```


3. Run the command to Build:

   **Release**

   ```bash
   $ ninja -C static/iOSDevice64/Release sk4d
   ```

   **Debug**

   ```bash
   $ ninja -C static/iOSDevice64/Debug sk4d
   ```



### 3.5 Android

The instructions below are for compiling using **Windows** or **Linux** or **macOS** as the host.

#### Prebuild

1. Download the latest [NDK](https://developer.android.com/ndk/downloads) LTS package for your development platform;

2. Unzip it to some folder.

#### Build for Android ARM

1. Access the Skia folder through Windows Command Prompt or PowerShell, or macOS Terminal, or Linux Shell Prompt;

2. **Make sure the NDK directory is correct** *(ndk="dir")* and run the command to generate the build files:

   **Release - Windows (Command Prompt)**

   ```batch
   bin\gn gen shared\Android\Release "--args=target_cpu="""arm""" target_os="""android""" is_official_build=true skia_use_system_expat=false skia_use_system_freetype2=false skia_use_system_harfbuzz=false skia_use_system_icu=false skia_use_system_libjpeg_turbo=false skia_use_system_libpng=false skia_use_system_libwebp=false skia_use_system_zlib=false ndk="""C:\ndk""""
   ```

   **Release - Windows (PowerShell)**

   ```powershell
   bin\gn gen shared\Android\Release '--args=target_cpu="""arm""" target_os="""android""" is_official_build=true skia_use_system_expat=false skia_use_system_freetype2=false skia_use_system_harfbuzz=false skia_use_system_icu=false skia_use_system_libjpeg_turbo=false skia_use_system_libpng=false skia_use_system_libwebp=false skia_use_system_zlib=false ndk="""C:\ndk"""'
   ```

   **Release - Other platforms**

   ```bash
   $ bin/gn gen shared/Android/Release --args='target_cpu="arm" target_os="android" is_official_build=true skia_use_system_expat=false skia_use_system_freetype2=false skia_use_system_harfbuzz=false skia_use_system_icu=false skia_use_system_libjpeg_turbo=false skia_use_system_libpng=false skia_use_system_libwebp=false skia_use_system_zlib=false ndk="/tmp/ndk"'
   ```

   **Debug - Windows (Command Prompt)**

   ```batch
   bin\gn gen shared\Android\Debug "--args=target_cpu="""arm""" target_os="""android""" ndk="""C:\ndk""""
   ```

   **Debug - Windows (PowerShell)**

   ```powershell
   bin\gn gen shared\Android\Debug '--args=target_cpu="""arm""" target_os="""android""" ndk="""C:\ndk"""'
   ```

   **Debug - Other platforms**

   ```bash
   $ bin/gn gen shared/Android/Debug --args='target_cpu="arm" target_os="android" ndk="/tmp/ndk"'
   ```


3. Run the command to Build:

   **Release - Windows (Command Prompt)**

   ```bash
   ninja -C shared\Android\Release sk4d
   ```

   **Release - Windows (PowerShell)**

   ```powershell
   ninja -C shared\Android\Release sk4d
   ```

   **Release - Other platforms**

   ```bash
   $ ninja -C shared/Android/Release sk4d
   ```

   **Debug - Windows (Command Prompt)**

   ```bash
   ninja -C shared\Android\Debug sk4d
   ```

   **Debug - Windows (PowerShell)**

   ```powershell
   ninja -C shared\Android\Debug sk4d
   ```

   **Debug - Other platforms**

   ```bash
   $ ninja -C shared/Android/Debug sk4d
   ```



#### Build for Android ARM64

1. Access the Skia folder through Windows Command Prompt or PowerShell, or macOS Terminal, or Linux Shell Prompt;

2. **Make sure the NDK directory is correct** *(ndk="dir")* and run the command to generate the build files:

   **Release - Windows (Command Prompt)**

   ```batch
   bin\gn gen shared\Android64\Release "--args=target_cpu="""arm64""" target_os="""android""" is_official_build=true skia_use_system_expat=false skia_use_system_freetype2=false skia_use_system_harfbuzz=false skia_use_system_icu=false skia_use_system_libjpeg_turbo=false skia_use_system_libpng=false skia_use_system_libwebp=false skia_use_system_zlib=false ndk="""C:\ndk""""
   ```

   **Release - Windows (PowerShell)**

   ```powershell
   bin\gn gen shared\Android64\Release '--args=target_cpu="""arm64""" target_os="""android""" is_official_build=true skia_use_system_expat=false skia_use_system_freetype2=false skia_use_system_harfbuzz=false skia_use_system_icu=false skia_use_system_libjpeg_turbo=false skia_use_system_libpng=false skia_use_system_libwebp=false skia_use_system_zlib=false ndk="""C:\ndk"""'
   ```

   **Release - Other platforms**

   ```bash
   $ bin/gn gen shared/Android64/Release --args='target_cpu="arm64" target_os="android" is_official_build=true skia_use_system_expat=false skia_use_system_freetype2=false skia_use_system_harfbuzz=false skia_use_system_icu=false skia_use_system_libjpeg_turbo=false skia_use_system_libpng=false skia_use_system_libwebp=false skia_use_system_zlib=false ndk="/tmp/ndk"'
   ```

   **Debug - Windows (Command Prompt)**

   ```batch
   bin\gn gen shared\Android64\Debug "--args=target_cpu="""arm64""" target_os="""android""" ndk="""C:\ndk""""
   ```

   **Debug - Windows (PowerShell)**

   ```powershell
   bin\gn gen shared\Android64\Debug '--args=target_cpu="""arm64""" target_os="""android""" ndk="""C:\ndk"""'
   ```

   **Debug - Other platforms**

   ```bash
   $ bin/gn gen shared/Android64/Debug --args='target_cpu="arm64" target_os="android" ndk="/tmp/ndk"'
   ```


3. Run the command to Build:

   **Release - Windows (Command Prompt)**

   ```batch
   ninja -C shared\Android64\Release sk4d
   ```

   **Release - Windows (PowerShell)**

   ```powershell
   ninja -C shared\Android64\Release sk4d
   ```

   **Release - Other platforms**

   ```bash
   $ ninja -C shared/Android64/Release sk4d
   ```

   **Debug - Windows (Command Prompt)**

   ```bash
   ninja -C shared\Android64\Debug sk4d
   ```

   **Debug - Windows (PowerShell)**

   ```powershell
   ninja -C shared\Android64\Debug sk4d
   ```

   **Debug - Other platforms**

   ```bash
   $ ninja -C shared/Android64/Debug sk4d
   ```
