# Skia4Delphi validation guide

This guide describes how maintainers, automation agents, and CI jobs should
build and validate Skia4Delphi. It concentrates on the test projects under
`Tests` and the `Benchmark/FmxFPS` benchmark.

The central rule is that a successful compilation is not a successful test
run. Record the exact RAD Studio version, project, configuration, platform,
renderer/backend, process exit code, and machine-readable report for every
validation.

## Repository layout

- `Tests/Source` contains tests shared by all runners.
- `Tests/Source/VCL` contains VCL-specific tests and the VCL runner.
- `Tests/Source/FMX` contains FMX-specific tests and the FMX runner.
- `Tests/Source/Issues` contains issue-specific regression tests.
- `Tests/Assets` contains input assets and the `Expected-*.zip` archives.
- `Tests/Projects/<RAD Studio version>` contains the Console, VCL, and FMX
  projects for that compiler generation.
- `Tests/Binary/<Platform>/<Config>` is the configured output directory for
  all three test executables.
- `Tests/Objects/<Platform>/<Config>` contains their DCUs and intermediate
  files.
- `Benchmark/FmxFPS` contains the FMX rendering benchmark, with its own
  versioned projects, assets, binary output, and object output.

The three test runners serve different purposes:

| Runner | Scope | Automation report |
| --- | --- | --- |
| Console | Shared core tests without VCL/FMX-specific units | DUnitX NUnit XML |
| VCL | Shared tests plus VCL-specific integration and rendering tests | Custom JSON in CI mode |
| FMX | Shared tests plus FMX-specific integration, canvas, effect, and rendering tests | Custom JSON in CI mode |

A shared test is intentionally compiled into all three runners. This catches
framework initialization, compiler, and runtime differences that a Console-only
run could miss.

## Select the matching RAD Studio project set

Do not build a project saved by one RAD Studio generation with a different
generation merely because MSBuild accepts it. Select the directory that matches
the compiler under test and initialize that installation's environment with its
own `rsvars.bat`.

The repository currently has these project sets:

| Project directory | RAD Studio generation | BDS version used in a default installation path |
| --- | --- | --- |
| `RAD Studio XE7` | XE7 | `15.0` |
| `RAD Studio XE8` | XE8 | `16.0` |
| `RAD Studio 10.0 Seattle` | 10.0 Seattle | `17.0` |
| `RAD Studio 10.1 Berlin` | 10.1 Berlin | `18.0` |
| `RAD Studio 10.2 Tokyo` | 10.2 Tokyo | `19.0` |
| `RAD Studio 10.3 Rio` | 10.3 Rio | `20.0` |
| `RAD Studio 10.4 Sydney` | 10.4 Sydney | `21.0` |
| `RAD Studio 11 Alexandria` | 11 Alexandria | `22.0` |
| `RAD Studio 12 Athens` | 12 Athens | `23.0` |
| `RAD Studio 13 Florence` | 13 Florence | `37.0` |

For example, the projects in `Tests/Projects/RAD Studio 13 Florence` are for
RAD Studio 13.x, while the projects in `Tests/Projects/RAD Studio 12 Athens`
are for RAD Studio 12.x.

Every versioned test directory contains:

- `Skia_Tests_Console.dpr` and `Skia_Tests_Console.dproj`;
- `Skia_Tests_VCL.dpr` and `Skia_Tests_VCL.dproj`;
- `Skia_Tests_FMX.dpr` and `Skia_Tests_FMX.dproj`.

`Benchmark/FmxFPS/Projects` follows the same versioning scheme with one
`FmxFPS.dpr` and one `FmxFPS.dproj` per RAD Studio generation.

## Prerequisites

Before building, verify that:

1. The intended RAD Studio version and required target platform are installed.
2. Skia4Delphi and DUnitX paths required by the selected `.dproj` resolve in
   that RAD Studio environment.
3. The matching native Skia library, such as `sk4d.dll`, is available to the
   built executable through its directory or the initialized runtime path.
4. No download, installation, expected-image replacement, or other external
   change is being performed implicitly by the validation job.

Prefer `Release|Win64` for the normal Windows CI baseline, but validate another
configuration or platform when the change is specific to it. VCL is
Windows-only; FMX and Console projects may expose additional platforms in newer
RAD Studio versions. Report each platform independently.

## Build the test projects

Always build the `.dproj` with MSBuild after calling the matching
`rsvars.bat`. Do not substitute a direct `dcc32` or `dcc64` invocation for
project validation: the `.dproj` carries defines, unit paths, output paths,
platform settings, and deployment metadata.

This PowerShell example builds all three RAD Studio 13 test projects as
`Release|Win64`:

```powershell
$repo = 'C:\Workspace\Github\skia4delphi\skia4delphi'
$radFolder = 'RAD Studio 13 Florence'
$rsvars = 'C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat'

$projects = @(
    "$repo\Tests\Projects\$radFolder\Skia_Tests_Console.dproj"
    "$repo\Tests\Projects\$radFolder\Skia_Tests_VCL.dproj"
    "$repo\Tests\Projects\$radFolder\Skia_Tests_FMX.dproj"
)

foreach ($project in $projects) {
    $command = 'call "{0}" && msbuild "{1}" /t:Build /p:Config=Release /p:Platform=Win64 /v:minimal /nologo' -f $rsvars, $project
    & $env:ComSpec /d /c $command
    if ($LASTEXITCODE -ne 0) {
        throw "Build failed: $project"
    }
}
```

Replace both `$radFolder` and `$rsvars` when validating another RAD Studio
version. Keep build logs and reports separated by RAD Studio version; do not
allow one run to overwrite another run's evidence.

With the configuration above, the executables are written to:

```text
Tests\Binary\Win64\Release\Skia_Tests_Console.exe
Tests\Binary\Win64\Release\Skia_Tests_VCL.exe
Tests\Binary\Win64\Release\Skia_Tests_FMX.exe
```

### The repository source is the one under test

Every test project compiles the Skia4Delphi source directly: `Skia_Tests_*.dpr`
lists `System.Skia.API`, `System.Skia` and, for the VCL and FMX runners,
`Vcl.Skia` or the four `FMX.Skia*` units, each with its `..\..\..\Source\...`
path. A run therefore always exercises this repository, never an installed
Skia4Delphi and never the copy a recent RAD Studio bundles in its own RTL.

Nothing has to be added to `DCC_UnitSearchPath` for that, and nothing should be:
overriding it to point at another checkout only hides which source was built.
The compiled line count reported by the compiler is a quick confirmation - a
build of the Console runner reports around 35000 lines, most of which are
`Source\System.Skia.pas` and `Source\System.Skia.API.pas`.

The four units of the FMX runner that belong to Embarcadero
(`FMX.Skia.Filter`, `FMX.Skia.AnimatedCodec`, `FMX.Skia.Printer` and
`FMX.Skia.Canvas.Vulkan`) are deliberately *not* part of this repository and
are not listed in the projects. `FMX.Skia.Canvas` only uses them from RAD Studio
12 Athens on, where they are resolved from the RTL. If a change to the
repository alters the interface of `FMX.Skia` or `FMX.Skia.Canvas`, those
precompiled units stop matching and the build fails with:

```text
F2051 Unit FMX.Skia.Filter was compiled with a different version of
FMX.Skia.Canvas.TSkCanvasCustomClass
```

When that happens, copy the four `.pas` files from that installation's
`source\fmx` directory into `Source\FMX` for the duration of the run so they
are rebuilt too, point `DCC_UnitSearchPath` at `Source\FMX` so the copies win
over the RTL units, and delete them as soon as it finishes - they are Embarcadero
property and must never be committed:

```powershell
$units = 'FMX.Skia.Filter.pas', 'FMX.Skia.AnimatedCodec.pas',
         'FMX.Skia.Printer.pas', 'FMX.Skia.Canvas.Vulkan.pas'
foreach ($unit in $units) {
    Copy-Item "$env:BDS\source\fmx\$unit" "$repo\Source\FMX\$unit"
}
try {
    # build and run with /p:DCC_UnitSearchPath="$repo\Source\FMX"
}
finally {
    foreach ($unit in $units) {
        Remove-Item "$repo\Source\FMX\$unit" -ErrorAction SilentlyContinue
    }
}
```

Confirm with `git status --short -- Source` that none of them was left behind.

## Working directory and assets

On Windows, test assets are resolved from the process working directory using
`..\..\..\Assets`. Therefore, a `Release|Win64` test executable must normally
run with this working directory:

```text
Tests\Binary\Win64\Release
```

That relative path then resolves to `Tests/Assets`. Starting the executable
from the repository root, a generic temporary directory, or another output
directory can make tests read the wrong files, create unintended directories,
or fail even though the executable itself is valid.

`FmxFPS` has the same requirement for its own assets and should run from:

```text
Benchmark\FmxFPS\Binary\Win64\Release
```

CI launchers should always set the working directory explicitly.

The rule is different on the other platforms, because `TTestBase.RootAssetsPath`
resolves them differently:

| Platform | Assets root |
| --- | --- |
| Windows | `..\..\..\Assets` relative to the working directory |
| iOS and Android | the application documents directory |
| macOS | `../Resources/` relative to the executable |
| Linux and everything else | the directory of the executable |

So a Linux run does not take an `Assets` directory: the *contents* of
`Tests/Assets` have to be deployed next to the executable, giving
`<executable directory>/Fonts`, `<executable directory>/Images`, and so on.
The `Expected-*.zip` archives are only needed by the graphical runners to show
the expected image beside the actual one, so an unattended run does not have to
deploy them.

The Linux64 executables also need `libsk4d.so`, which is not deployed
automatically in runs outside the IDE. Copy it from `Binary/Shared/Linux64` next
to the executable and start the process with that directory in `LD_LIBRARY_PATH`.

The FMX runner additionally needs a display: it is a GTK application and aborts
with `Gtk-ERROR: Can't create a GtkStyleContext without a display connection`
when `DISPLAY` is unset. On a headless machine run it through `Xvfb`:

```sh
xvfb-run -a --server-args='-screen 0 1280x1024x24' \
    env LD_LIBRARY_PATH=. ./Skia_Tests_FMX --ci --ci-output=/tmp/fmx.json
```

The Console runner has no such requirement. VCL is Windows only.

## Run the Console tests

The Console executable uses the standard DUnitX command-line support. It does
not use the custom VCL/FMX `--ci` JSON mode. For unattended execution, disable
the final pause and request an NUnit XML report at an absolute path:

```powershell
$testBin = "$repo\Tests\Binary\Win64\Release"
$reportRoot = 'C:\Workspace\Skia4Delphi-CI\RAD13'
New-Item -ItemType Directory -Force -Path $reportRoot | Out-Null

$consoleReport = "$reportRoot\console.xml"
$console = Start-Process `
    -FilePath "$testBin\Skia_Tests_Console.exe" `
    -WorkingDirectory $testBin `
    -ArgumentList @(
        '--exitbehavior:Continue'
        "--xmlfile:$consoleReport"
    ) `
    -Wait -PassThru
```

For a directed Console run, DUnitX also accepts
`--run:<full-test-name>`. Keep the XML report enabled and confirm that its
executed test name and count match the intended target; a filter that discovers
zero tests is not successful validation.

Use an absolute XML path. A relative report path can fail with an
`EInOutError`, and the Console runner's outer exception handler may only print
the exception. Consequently, CI must require all of the following:

- the process completed within the timeout;
- the expected XML file exists and is non-empty;
- the XML parses successfully;
- its aggregate and individual results contain no unexpected failures or
  errors;
- the process exit code agrees with the report.

Do not declare the Console target successful from the process exit code alone.

## Run the VCL and FMX tests in CI mode

Without arguments, the VCL and FMX executables open their graphical runners.
The user can select tests, run them, inspect failures, and compare expected and
actual images.

For unattended execution, both runners implement these custom options:

| Option | Meaning |
| --- | --- |
| `--ci` | Automatically runs all registered tests, writes a report, sets the process exit code, and requests application termination. |
| `--ci-output:<absolute-path>` | Writes the JSON report to the specified path. `=` or a separate value is also accepted. |
| `--build-expected-images` | Enables expected-image generation. It is effective with `--ci` and is destructive to the `Tests/Assets/Expected-*.zip` archives; it is not a normal validation option. |

If `--ci-output` is omitted, the report defaults to
`<executable-name>.Results.json` in the current working directory. CI should
still pass an absolute path so that report discovery is deterministic.

Example runs:

```powershell
$vclReport = "$reportRoot\vcl.json"
$vcl = Start-Process `
    -FilePath "$testBin\Skia_Tests_VCL.exe" `
    -WorkingDirectory $testBin `
    -ArgumentList @('--ci', "--ci-output=$vclReport") `
    -Wait -PassThru

$fmxReport = "$reportRoot\fmx.json"
$fmx = Start-Process `
    -FilePath "$testBin\Skia_Tests_FMX.exe" `
    -WorkingDirectory $testBin `
    -ArgumentList @('--ci', "--ci-output=$fmxReport") `
    -Wait -PassThru
```

If the native Skia library is supplied by the RAD Studio installation, launch
the tests with that installation's `bin` and `bin64` directories in `PATH`, or
run them from an environment initialized by the same `rsvars.bat` used for the
build. Never satisfy a runtime error by silently downloading a different
binary.

The VCL and FMX report schema contains:

- `schema_version`, `runner`, `all_passed`, and `duration_ms`;
- `test_count`, `passed`, `failures`, `errors`, `ignored`, and
  `memory_leaks`;
- a `tests` array with each test's full name, status, duration, optional
  message, optional stack trace, and, for image-similarity failures, an
  `expected-image` entry name.

The runners use these exit codes:

| Exit code | Meaning |
| --- | --- |
| `0` | The test runner reported `AllPassed` and wrote the JSON report. |
| `1` | At least one test did not pass. |
| `2` | The runner could not write the JSON report. |

CI must parse the JSON and reconcile it with the exit code. A visible window,
a compiled executable, or the mere existence of a JSON file is not sufficient
evidence. Inspect aggregate counts and per-test records. If a report was
written but a GUI process does not terminate, preserve and parse the report,
then record the process-lifecycle problem separately; terminate only the
process started by that job.

For example, after checking that each report exists and is non-empty:

```powershell
$vclData = Get-Content -LiteralPath $vclReport -Raw | ConvertFrom-Json
$fmxData = Get-Content -LiteralPath $fmxReport -Raw | ConvertFrom-Json

if (($vcl.ExitCode -ne 0) -or (-not $vclData.all_passed)) {
    throw 'VCL tests failed; inspect vcl.json'
}
if (($fmx.ExitCode -ne 0) -or (-not $fmxData.all_passed)) {
    throw 'FMX tests failed; inspect fmx.json'
}
```

## `Tests/Assets/Expected-*.zip`

The eight `Expected-*.zip` archives contain full PNG images associated with
tests that call the Skia4Delphi image-similarity assertion helpers. They support
the graphical runners' expected-versus-actual preview and the expected-image
generation workflow.

An entry name is deterministic:

```text
lowercase MD5 of DUnitX ITestInfo.FullName + ".png"
```

For example, changing a fixture name, test method name, or test-case identity
changes the full test name and therefore changes the ZIP entry name.

The first hexadecimal character selects the archive: `0` or `1` uses
`Expected-01.zip`, `2` or `3` uses `Expected-23.zip`, continuing through
`Expected-ef.zip`. Every archive stores its PNG entries at the ZIP root.

The ZIP filename is not the perceptual image hash used by a test. Normal image
tests usually pass an expected perceptual hash and minimum similarity to
`Assert.AreSimilar`. That hash determines pass or failure. A message such as:

```text
Images are not similar. ... (hash: ...)
```

contains the actual perceptual hash observed during that run. Do not confuse
it with the MD5-based PNG filename, and never copy it into a test merely to make
CI green.

During a normal VCL/FMX run, the runner captures the last image checked by a
test. On an image failure it can extract only the matching expected-image
archive to a temporary directory, save the actual image to another temporary
directory, and present both images in the GUI. For CI image failures, the JSON
report adds the deterministic `expected-image` entry name so an agent can check
whether the corresponding baseline exists in the matching archive.

### Regenerating expected images

The following command is not a normal test command:

```text
Skia_Tests_FMX.exe --ci --build-expected-images --ci-output=<absolute-json-path>
```

`--build-expected-images` mirrors the graphical runner's "Generate expected
images" option. The runner extracts the existing archives, updates images from
successful image tests, builds every replacement through a temporary ZIP, and
then replaces the `Tests/Assets/Expected-*.zip` archive set when the run
finishes. Failed image tests can also cause their old extracted entry not to be
written back.

Therefore:

1. Never use this option merely to make a failing test pass.
2. Never run it against the repository archives during ordinary validation.
3. First copy the executable and `Assets` directory to a disposable staging
   tree and validate the generation behavior there.
4. Regenerate the repository archives only as an intentional, explicitly
   authorized baseline update.
5. Review every changed image for correctness across the relevant renderer,
   backend, DPI, scale, transforms, opacity, brushes, fonts, and platform.
6. Preserve the original archive hashes and inspect the final archive contents
   before accepting a replacement.

An `expected-image` entry missing from the current archive set is evidence to
investigate. It is not permission to regenerate the archives automatically.

## Add or change tests

### Choose the correct unit

- Add a test to an existing unit when that unit already owns the class or
  behavior under test.
- If the class has no test unit, create a focused unit such as
  `Tests/Source/Skia.Tests.<Subject>.pas`. Do not place an unrelated regression
  in whichever existing unit is easiest to edit.
- Put VCL-only tests in `Tests/Source/VCL` and use the
  `Skia.Tests.Vcl.<Subject>` naming pattern.
- Put FMX-only tests in `Tests/Source/FMX` and use the
  `Skia.Tests.FMX.<Subject>` naming pattern.
- Use `Tests/Source/Issues` for a genuinely issue-specific regression when a
  class- or behavior-focused unit would not be clearer.

A minimal shared test unit follows this structure:

```pascal
unit Skia.Tests.Example;

interface

{$SCOPEDENUMS ON}

uses
  DUnitX.TestFramework,
  System.Skia;

type
  [TestFixture]
  TSkExampleTests = class
  public
    [Test]
    procedure TestBehavior;
  end;

implementation

procedure TSkExampleTests.TestBehavior;
begin
  Assert.IsTrue(True);
end;

initialization
  TDUnitX.RegisterTestFixture(TSkExampleTests);
end.
```

In real tests, assert the public contract and the regression boundary rather
than implementation details. Derive the fixture from `TTestBase` when it needs
the shared asset paths, font registration, or related helpers. Use `[TestCase]`
for meaningful input variations, and guard APIs or platforms that are not
available in every supported compiler generation.

### Register a new unit in every applicable project

Creating the `.pas` file is not enough. Delphi only links the fixture when its
unit is included by the project.

For a new shared test unit, add it in alphabetical order to both files for all
three runners in every supported RAD Studio directory:

1. Add the unit to the `uses` list in every `Skia_Tests_Console.dpr`.
2. Add the unit to the `uses` list in every `Skia_Tests_VCL.dpr`.
3. Add the unit to the `uses` list in every `Skia_Tests_FMX.dpr`.
4. Add the matching `<DCCReference Include="..."/>` to every corresponding
   `.dproj`.

There are currently ten RAD Studio directories, so a new shared unit normally
changes 30 `.dpr` files and 30 `.dproj` files. A VCL-only unit normally changes
the ten VCL `.dpr`/`.dproj` pairs; an FMX-only unit normally changes the ten FMX
pairs.

Follow the ordering and relative-path style already used by neighboring units.
Do not copy one version's entire `.dproj` over another: platform lists,
deployment metadata, project versions, and compiler settings legitimately
differ. Preserve the existing line-ending style of every edited file.

Useful inclusion checks for a shared unit are:

```powershell
$unitName = 'Skia.Tests.Example'

@(rg -l --fixed-strings $unitName "$repo\Tests\Projects" -g '*.dpr').Count
@(rg -l --fixed-strings $unitName "$repo\Tests\Projects" -g '*.dproj').Count
```

Both counts should currently be 30 for a shared unit. Inspect the matching
lines as well; a count does not prove correct ordering or paths.

### Validate a new test

1. Build and run the narrowest affected project first for fast feedback.
2. Confirm that the new test is discovered and actually executed; compilation
   alone does not prove fixture registration.
3. For a shared test, run Console, VCL, and FMX.
4. Build with every RAD Studio generation whose compatibility could be affected
   by syntax, RTL, compiler, or project-file changes.
5. Run platform- or renderer-specific tests in their actual environment.
6. Report directed-test evidence separately from complete-suite evidence. If a
   complete suite stops at a pre-existing failure, do not describe the whole
   suite as green.

## Run the `FmxFPS` benchmark

`Benchmark/FmxFPS` creates 750 mixed FMX controls in a vertical scroll box and
simulates scrolling for approximately six seconds. It counts form paints and
reports frames per second. The benchmark is useful for detecting large
rendering-performance regressions, but it is not a correctness test and has no
built-in pass/fail FPS threshold.

The versioned benchmark projects currently define `SKIA`, and their `.dpr`
files enable `GlobalUseSkia`. The JSON `renderer` and `canvas` fields confirm
what actually ran. A comparison with the default FMX renderer requires an
explicitly controlled project/source variant; results from different renderers
must not be presented as a before-and-after regression comparison.

Build the project that matches the RAD Studio version:

```powershell
$benchmarkProject = "$repo\Benchmark\FmxFPS\Projects\$radFolder\FmxFPS.dproj"
$command = 'call "{0}" && msbuild "{1}" /t:Build /p:Config=Release /p:Platform=Win64 /v:minimal /nologo' -f $rsvars, $benchmarkProject
& $env:ComSpec /d /c $command
if ($LASTEXITCODE -ne 0) {
    throw 'FmxFPS build failed'
}
```

Run it in CI mode from its binary directory:

```powershell
$benchmarkBin = "$repo\Benchmark\FmxFPS\Binary\Win64\Release"
$benchmarkReport = "$reportRoot\fmxfps.json"

$benchmark = Start-Process `
    -FilePath "$benchmarkBin\FmxFPS.exe" `
    -WorkingDirectory $benchmarkBin `
    -ArgumentList @('--ci', "--ci-output=$benchmarkReport") `
    -Wait -PassThru
```

In CI mode, the benchmark skips its initial informational dialog, starts
automatically, writes JSON, and requests application termination. Its report
contains `renderer`, `canvas`, `quality`, `fps`, `paint_count`,
`duration_seconds`, `controls`, and `scroll_height`. Exit code `0` means the
measurement was written; exit code `2` means report writing failed. Exit code
`0` does not mean the FPS is acceptable.

For a meaningful comparison:

- build baseline and candidate with the same RAD Studio version,
  configuration, platform, and defines;
- use the same renderer and GPU/raster backend;
- keep machine load, power mode, window size, monitor, DPI, and foreground
  conditions stable;
- run each variant multiple times and retain every JSON report;
- compare a median and variability, not one favorable run;
- validate rendering correctness separately with the relevant tests.

Report FmxFPS independently from Console, VCL, and FMX test outcomes.

## CI evidence and final checklist

For every requested target, retain or report:

| Field | Required evidence |
| --- | --- |
| Toolchain | RAD Studio name/version and exact `rsvars.bat` |
| Build | Exact `.dproj`, configuration, platform, and MSBuild exit code |
| Runtime | Executable, working directory, renderer/backend, DPI where relevant, and timeout status |
| Result | Process exit code plus parsed XML or JSON |
| Tests | Aggregate counts and names/messages of failures, errors, or leaks |
| Benchmark | All raw JSON runs and the comparison method |
| Images | Expected entry name, actual failure message/hash, and whether the archive entry exists |

Before handing off source or project-file changes, also run:

```powershell
git status --short
git -c core.whitespace=cr-at-eol diff --check
git diff -- Tests/Assets/Expected.zip 'Tests/Assets/Expected-*.zip'
```

Confirm that unrelated checkout changes were preserved, no project set was
missed, and the expected-image archives did not change unless an intentional
baseline update was explicitly in scope.
