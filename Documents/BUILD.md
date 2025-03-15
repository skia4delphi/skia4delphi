<p><a href="https://www.skia4delphi.org"><img src="../Assets/Artwork/logo-gradient.svg" alt="Logo" height="300" width="360" /></a></p>

# Build

**Skia4Delphi** has its own [forked version of Skia](../../../../skia) with the necessary edits to make the library work. The build files for Skia's fork are generated using the **GN** meta-build system, following the same compilation [instructions](https://skia.org/docs/user/build/) as the official Skia library. The only differences involve specific Skia4Delphi build variables, which must be considered during configuration.

The key variables for building are:

| Name                    | Type    | Description                                                   |
| ----------------------- | ------- | ------------------------------------------------------------- |
| is_sk4d_build           | Boolean | Enables the Skia4Delphi library target.                       |
| is_sk4d_component_build | Boolean | Determines whether the compiled library is static or dynamic. |

  

Once the build files have been generated, the library can be built directly using Ninja with the following command:

```shell
ninja -C <build dir> sk4d
```

  

## Known Issues

Unfortunately, static linking of more complex C++ libraries in Delphi presents certain limitations, requiring specific workarounds to ensure proper linkage.

Skia's library depends on a few builtin functions for static linking on certain platforms. However, Delphi does not provide the compiler runtime libraries for Clang, nor does the SDK Manager automatically retrieve the runtime libraries (compiler-rt) for macOS. Similarly, on Android, it does not add the NDK directory containing compiler-rt to the search path, creating challenges for vendors relying on static linking.

As there is currently no official solution for this, the internal build script implements workarounds by extracting the necessary object files from the compiler runtime, converting their symbols to weak to avoid conflicts, and appending them alongside the static library.

While this is not an ideal solution, it remains the only viable approach at this time. The required object files may vary depending on the toolchain, but as of now, the following modifications are applied:

- For OSX64, iOSDevice64, and iOSSimARM64: The object file `os_version_check.c.o` from compiler-rt builtins;
- For Android and Android64: The object file `emutls.c.o` from compiler-rt builtins.

