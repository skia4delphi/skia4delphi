<p align="center"><a href="https://www.skia4delphi.org"><img src="Assets/Artwork/logo-gradient.svg" alt="Logo" height="300" width="360" /></a></p>

<p>
  <a href="#compatibility"><img src="https://img.shields.io/static/v1?label=rad%20studio&message=xe7%2B&color=silver&style=for-the-badge&logo=delphi&logoColor=white" alt="Delphi XE7+ support" /></a>
  <a href="#compatibility"><img src="https://img.shields.io/static/v1?label=platforms&message=cross-platform&color=silver&style=for-the-badge&logo=delphi&logoColor=white" alt="Cross-platform support" /></a>
  <a href="#compatibility"><img src="https://img.shields.io/static/v1?label=applications&message=console%2C%20fmx%2C%20vcl&color=silver&style=for-the-badge&logo=delphi&logoColor=white" alt="Console, FMX, VCL support" /></a>
  <a href="https://t.me/skia4delphi"><img src="https://img.shields.io/static/v1?label=telegram&message=skia4delphi&color=silver&style=for-the-badge&logo=telegram&logoColor=white" alt="Telegram group" />
  <a href="https://www.youtube.com/@skia4delphi"><img src="https://img.shields.io/static/v1?label=youtube&message=skia4delphi&color=silver&style=for-the-badge&logo=youtube&logoColor=white" alt="YouTube channel" />
</p>

#

**[Skia4Delphi](https://skia4delphi.org)** is an open-source, cross-platform 2D graphics library for Delphi, utilizing the esteemed [Google's Skia](https://skia.org/) library.

https://user-images.githubusercontent.com/1863024/175955980-f6c57253-aaa3-4617-90dc-b0d9bf25e21b.mp4

## About

Skia is an exceptional open-source library dedicated to rendering 2D text, geometries and images, with a focus on precision, superior quality and high performance. It offers versatile APIs compatible with a wide range of hardware and software platforms.

Google's Skia Graphics Library functions as the graphics engine for numerous products, including Google Chrome, Chrome OS, Android, Flutter, Xamarin, Mozilla Firefox, Firefox OS, and more.

## Features

- Canvas 2D and Text Layout;
- CPU software rasterization;
- GPU-accelerated rendering;
- Right-to-Left rendering;
- SVG rendering and creation;
- PDF output;
- Runtime effects;
- Shading language;
- Shaders, mask and color filters;
- Image and path effects;
- Animated image player; (Lottie, GIF, WebP)
- Image codecs; (bmp, gif, ico, jpg, png, wbmp, webp and raw images)
  **and much more...**

## FMX render replacement

Using the **Skia4Delphi** library it is possible to override Firemonkey's graphic engine so that it can use Skia as its default Canvas. With that, your Firemonkey application will automatically:

- Draw with antialiasing on any platform (the drawing quality is based on the Form.Quality property);
- Increase the overall graphics performance of your application by up to 50% (even drawing with higher quality);
- Resize images with better quality (also based on Form.Quality);
- Support Right-To-Left text rendering;
- Fix dozens of inconsistencies in drawings (especially in corners and strokes, such as dashes, and in texts with special emojis);
- Increase the performance of the library in general ([controls](#controls-vclfmx), drawings, among others...).

[Learn more...](#fmx-render)

# Summary

- [Using the library](#using-the-library)
  - [Prerequisites](#prerequisites)
    - [Install](#install)
    - [Enable Skia](#enable-skia)
  - [Examples](#examples)
    - [Basic usage](#basic-usage)
    - [PDF](#pdf)
    - [Codecs](#codecs)
- [Integration with Delphi](#integration-with-delphi)
  - [Bitmap](#bitmap)
  - [Image formats](#image-formats)
  - **[FMX Render](#fmx-render)**
    - [Enable Skia Render](#enable-skia-render)
    - [Benchmark](#benchmark)
    - [Skia canvas](#skia-canvas)
    - [Right-to-Left](#right-to-left)
    - [Custom fonts](#custom-fonts)
- [Controls VCL/FMX](#controls-vclfmx)
  - [TSkAnimatedImage](#tskanimatedimage)
  - [TSkLabel](#tsklabel)
  - [TSkPaintBox](#tskpaintbox)
  - [TSkSvg](#tsksvg)
    - [Limitations](Documents/SVG.md#limitations)
- [Compatibility](#compatibility)
- [Known issues](#known-issues)
  - [Universal macOS Binary](#universal-macos-binary)
  - [Submit your app to the Mac App Store](#submit-your-app-to-the-mac-app-store)
- [Documentation](#documentation)
- [Version](#version)

# Using the library

## Prerequisites

### Install

You can install **Skia4Delphi** in 3 ways:

- Setup (recommended)

  Download the setup of [latest release](../../releases/latest) and install it.

  ![Skia4Delphi Installation](Assets/Documents/installation.png)

- Embarcadero's GetIt _(RAD Studio > Tools > GetIt Package Manager...)_

  <p><img src="https://user-images.githubusercontent.com/11139086/214978288-11c87e9e-7a8b-4686-82c0-5922676d26df.png#gh-light-mode-only" width="511" alt="GetIt" /></p>
  <p><img src="https://user-images.githubusercontent.com/11139086/214978346-c67bb0f6-ec96-4833-a1e4-7ee39d620e82.png#gh-dark-mode-only" width="511" alt="GetIt" /></p>

- Chocolatey package manager

  ```batch
  choco install skia4delphi
  ```

#### Remarks

1. Manual installation is possible, although it is not recommended; [Learn more...](Documents/INSTALLATION.md)
2. The pre-built Skia binaries were included in the source, but you can easily recompile them; [Learn more...](Documents/BUILD.md)

### Enable Skia

After install the **Skia4Delphi**, just right click in your application project and click **Enable Skia**.

![Menu](https://user-images.githubusercontent.com/16469061/153612703-81a9d1f8-8ae4-4977-b58f-6520a8318756.png#gh-light-mode-only)
![Menu](https://user-images.githubusercontent.com/16469061/153612789-38488c75-930a-48ac-8a6b-ea303f403e9e.png#gh-dark-mode-only)

#### Tip

To improve the quality and performance of FMX drawings, the replacement of the the FMX graphics engine with the **Skia4Delphi** render is automatically enabled. [Learn more...](#fmx-render)

## Examples

In this section you will find some examples of using **Skia4Delphi**, it works in **Console**, **FMX**, and **VCL** applications.
The code below is common code among all the examples in this section:

```pascal
uses
  Skia;

type
  TSkDrawExampleProc = reference to procedure(const ACanvas: ISkCanvas; const ADest: TRectF);

procedure DrawExample(const AWidth, AHeight: Integer; const ADrawProc: TSkDrawExampleProc);
begin
  var LSurface := TSkSurface.MakeRaster(AWidth, AHeight);
  LSurface.Canvas.Clear(TAlphaColors.Null);
  ADrawProc(LSurface.Canvas, RectF(0, 0, AWidth, AHeight));
  LSurface.MakeImageSnapshot.EncodeToFile('output.png');
end;
```

### Basic usage

The code below demonstrate how to draw shapes:

```pascal
DrawExample(256, 256,
  procedure (const ACanvas: ISkCanvas; const ADest: TRectF)
  begin
    var LPaint: ISkPaint := TSkPaint.Create;
    LPaint.AntiAlias := True;

    LPaint.Color := $FF4285F4;
    var LRect := TRectF.Create(PointF(10, 10), 100, 160);
    ACanvas.DrawRect(LRect, LPaint);

    var LOval: ISkRoundRect := TSkRoundRect.Create;
    LOval.SetOval(LRect);
    LOval.Offset(40, 80);
    LPaint.Color := $FFDB4437;
    ACanvas.DrawRoundRect(LOval, LPaint);

    LPaint.Color := $FF0F9D58;
    ACanvas.DrawCircle(180, 50, 25, LPaint);

    LRect.Offset(80, 50);
    LPaint.Color := $FFF4B400;
    LPaint.Style := TSkPaintStyle.Stroke;
    LPaint.StrokeWidth := 4;
    ACanvas.DrawRoundRect(LRect, 10, 10, LPaint);
  end);
```

This code results in the output below:

<p><img src="Assets/Documents/example1.svg" width="192" height="192" alt="Example 1" /></p>

[Learn more...](Documents/USAGE.md#basic-usage)

### PDF

With **Skia4Delphi** it is possible to create PDF documents and draw anything on them, from text to images. The example below demonstrates how to create an PDF document and draw an SVG inside it:

```pascal
  var LSVGDOM := TSkSVGDOM.MakeFromFile('Samples\Demo\Assets\lion.svg');
  var LSize := TSizeF.Create(600, 600);
  LSVGDOM.SetContainerSize(LSize);

  var LDocumentStream := TFileStream.Create('output.pdf', fmCreate);
  try
    var LDocument := TSkDocument.MakePDF(LDocumentStream);
    try
      var LCanvas := LDocument.BeginPage(LSize.Width, LSize.Height);
      try
        // Draw anything here with Skia canvas
        LSVGDOM.Render(LCanvas);
      finally
        LDocument.EndPage;
      end;
    finally
      LDocument.Close;
    end;
  finally
    LDocumentStream.Free;
  end;
```

This code results in the output below:

<p><img src="Assets/Documents/lion.svg" width="200" height="200" alt="Lion" /></p>

### Codecs

The **Skia4Delphi** library supports many image formats. See below the list:

- Supported formats for decoding

  | Image Format                   | Extensions  |
  | ------------------------------ | ----------- |
  | Bitmap                         | .bmp        |
  | GIF                            | .gif        |
  | Icon                           | .ico        |
  | JPEG                           | .jpg, .jpeg |
  | PNG                            | .png        |
  | Raw Adobe DNG Digital Negative | .dng        |
  | Raw Canon                      | .cr2        |
  | Raw Fujifilm RAF               | .raf        |
  | Raw Nikon                      | .nef, .nrw  |
  | Raw Olympus ORF                | .orf        |
  | Raw Panasonic                  | .rw2        |
  | Raw Pentax PEF                 | .pef        |
  | Raw Samsung SRW                | .srw        |
  | Raw Sony                       | .arw        |
  | WBMP                           | .wbmp       |
  | WebP                           | .webp       |

  _Note: Raw images are limited to non-windows platforms_

- Supported formats for encoding

  | Image Format | Extensions  |
  | ------------ | ----------- |
  | JPEG         | .jpg, .jpeg |
  | PNG          | .png        |
  | WebP         | .webp       |

#### About WebP

WebP is a modern image format that provides superior lossless and lossy compression for images. WebP lossless images are 26% smaller in size compared to PNGs. WebP lossy images are 25-34% smaller than comparable JPEG images at equivalent quality.

The example below demonstrates how to encoder to WebP format:

```pascal
  var LImage := TSkImage.MakeFromEncodedFile('Samples\Demo\Assets\kung-fu-panda.png');
  LImage.EncodeToFile('output.webp', TSkEncodedImageFormat.WEBP, 80);
  LImage.EncodeToFile('output.jpg', TSkEncodedImageFormat.JPEG, 80);
```

This code results in the output below:

<p><img src="Assets/Documents/kung-fu-panda.webp" width="400" alt="King Fu Panda" /></p>

| Format             | Size   |
| ------------------ | ------ |
| Png (100% quality) | 512 KB |
| Jpeg (80% quality) | 65 KB  |
| WebP (80% quality) | 51 KB  |

# Integration with Delphi

## Bitmap

It is possible to edit TBitmap (**VCL** or **FMX**) with Skia's canvas using the code below:

```pascal
uses
  Skia, Skia.FMX {or Skia.Vcl};

...

  var LBitmap := TBitmap.Create(100, 100);
  try
    LBitmap.SkiaDraw(
      procedure (const ACanvas: ISkCanvas)
      begin
        // Draw with Skia canvas...
      end);
```

## Image formats

The library registers the following codecs:

- **VCL**: .svg, .webp, .wbmp and raw images (.arw, .cr2, .dng, .nef, .nrw, .orf, .raf, .rw2, .pef and .srw).

- **FMX**: .bmp, .gif, .ico, .webp, .wbmp and raw images (.arw, .cr2, .dng, .nef, .nrw, .orf, .raf, .rw2, .pef and .srw).

As a result, any Delphi control, such as a TImage, can normally load these new formats automatically.

## **FMX Render**

It is possible to replace the default Canvas from FMX to Skia based Canvas. Once this feature is enabled, all FMX controls will be painted internally using **Skia4Delphi** automatically. With that it is possible to improve the quality and performance of the drawings throughout the FMX app, as well as generating better integration with other library features.

### Enable Skia Render

Open the source of your Delphi Application Project _(.dpr)_, include the `Skia.FMX` unit right **after** the `FMX.Forms` unit, and set the `GlobalUseSkia` to **True**, as in the example below:

```pascal
uses
  System.StartUpCopy,
  FMX.Forms,
  Skia.FMX,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  GlobalUseSkia := True;
  Application.Initialize;
  ...
```

#### Remarks

1. `Skia.FMX` unit must be included right after the `FMX.Forms`;
2. The **Skia Metal** render can be used by including the `FMX.Types` unit right **after** the `FMX.Forms` unit, and setting `GlobalUseMetal` to **True** together with `GlobalUseSkia` to improve the speed in iOS and macOS;
3. `GlobalUseSkia` has no effect on Linux. (although not supported, all [controls](#controls-vclfmx) work perfectly, just like the rest of the library)
4. This declaration of `GlobalUseSkia := True;`, as well as other variables of FMX itself, such as `GlobalUseMetal`, can also be made in the initialization of some unit instead of .dpr. Sometimes this is really necessary because if in the initialization or in the class constructor of some unit, bitmaps are used, the GlobalUseXXX declarations of the .dpr will have no effect. In this case, just create a unit in the project like "Project.Startup.pas", place the GlobalUseXXX declarations in the initialization of this new unit, and declare this new unit before any other unit of yours in the .dpr, that is, right after FMX.Forms.

### Benchmark

The performance test is a simulation of a real application, with hundreds of controls, to measure the FPS rate when sliding a vertical scroll.

| Device                                | Platform    |    FMX |      Skia |
| ------------------------------------- | ----------- | -----: | --------: |
| Motorola Moto 3rd Generation          | Android     | 25 fps |    38 fps |
| LG K40s                               | Android     | 30 fps |    47 fps |
| Samsung Galaxy A01 Core               | Android     | 20 fps |    26 fps |
| Samsung Galaxy S7 Edge                | Android64   | 53 fps |    56 fps |
| Samsung Galaxy S8 Plus                | Android64   | 50 fps |    55 fps |
| Apple iPhone 11                       | iOSDevice64 | 59 fps |    60 fps |
| Apple iPhone 12                       | iOSDevice64 | 59 fps |    59 fps |
| Apple MacBook Air Model A2337         | OSXARM64    | 58 fps | 30 fps \* |
| Intel Core i7-8565U / Radeon 520      | Win32       | 82 fps |    92 fps |
| Intel Core i7-8565U / Radeon 520      | Win64       | 83 fps |    91 fps |
| Intel Core i7-4500U / GeForce GT 720M | Win32       | 85 fps |    92 fps |
| Intel Core i7-4500U / GeForce GT 720M | Win64       | 86 fps |    93 fps |

#### Metal

| Device                        | Platform    |    FMX |   Skia |
| ----------------------------- | ----------- | -----: | -----: |
| Apple iPhone 11               | iOSDevice64 | 59 fps | 60 fps |
| Apple iPhone 12               | iOSDevice64 | 59 fps | 59 fps |
| Apple MacBook Air Model A2337 | OSXARM64    | 60 fps | 60 fps |

#### Remarks

1. Default FMX renderer does not use anti-aliasing on some platforms (like on mobile) while Skia Render uses it. That is, Skia has better performance and quality in the drawings than default FMX Render.

   | FMX default                                    | FMX with Skia render                             |
   | :--------------------------------------------: | :----------------------------------------------: |
   | ![FMX Circle](Assets/Documents/fmx-circle.png) | ![Skia Circle](Assets/Documents/skia-circle.png) |

2. On macOS `Skia4Delphi`'s default renderer does not have GPU acceleration. Therefore, it is highly recommended to use **Skia Metal** (combining the activation of `GlobalUseSkia` and `GlobalUseMetal`), to get the full performance of the machine.

3. Tests made from virtual machines are inconsistent with reality.

### Skia canvas

Using Skia's Render, during the Scene of a Bitmap, Control or Form, it is possible to access the Skia canvas property as follows:

#### In Bitmaps

```pascal
uses
  Skia, Skia.FMX.Graphics;

begin
  var LBitmap := TBitmap.Create(300, 300);
  try
    LBitmap.Canvas.BeginScene;
    try
      var LCanvas: ISkCanvas := TSkCanvasCustom(LBitmap.Canvas).Canvas;
      // Draw using Skia canvas (LCanvas) directly to unlock new features...
    finally
      LBitmap.Canvas.EndScene;
    end;
  finally
    LBitmap.Free;
  end;
end;
```

#### In Controls & Forms

```pascal
type
  TMyControl = class(TControl)
  protected
    procedure Paint; override;
  end;

implementation

uses
  Skia, Skia.FMX.Graphics;

procedure TMyControl.Paint;
begin
  var LCanvas: ISkCanvas := TSkCanvasCustom(Canvas).Canvas;
  // Draw using Skia canvas (LCanvas) directly to unlock new features...
end;
```

#### Remarks

1. `Canvas` property will only be available during Scene, that is, between the `BeginScene` and `EndScene` of the Bitmaps, and during paint events/methods for Controls and Forms (such as OnPaint, OnPainting, PaintChildren, among others);
2. Canvas for UI (created from a window _eg TRectangles, TCircles, objects inherited from TControl_) must draw exclusively from the **main thread**, while Canvas created from `TBitmap` are **thread safe**.

### Right-to-Left

Using Skia's render, your application will now support Right-To-Left text rendering. But for that you will need to make 3 changes to your project:

1. For RAD Studio prior to 11.3, open the source of your Delphi Application Project _(.dpr)_, include the line `Application.BiDiMode := TBiDiMode.bdRightToLeft;`, like below:

```pascal
program Project1;

uses
  System.StartUpCopy,
  FMX.Forms,
  System.Classes,
  Skia.FMX,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.BiDiMode := TBiDiMode.bdRightToLeft;
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
```

2. Set the property `BiDiMode` of your forms to `bdRightToLeft`;
3. Keyboard input controls like TEdit and TMemo, need to be fixed by Embarcadero, meanwhile, as a workaround, set the `ControlType` property of these controls to `Platform`.

### Custom fonts

Using Skia's renderer, it is possible to use custom font in any FMX control, on any platform in a very simple way. Just register them in the app initialization:

```pascal
program Project1;

uses
  System.StartUpCopy,
  FMX.Forms,
  Skia.FMX,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  GlobalUseSkia := True;
  TSkDefaultProviders.RegisterTypeface('Poppins.ttf');
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
```

# Controls VCL/FMX

## TSkAnimatedImage

**TSkAnimatedImage** is the control that can load and render animated images, including vector animations, in a very simple way. The supported formats are:

| Format           | Extensions     |
| ---------------- | -------------- |
| Lottie file      | .json, .lottie |
| Telegram Sticker | .tgs           |
| Animated GIF     | .gif           |
| Animated WebP    | .webp          |

The example below demonstrates how to play lottie files using **TSkAnimatedImage**:

```pascal
  var LAnimatedimage := TSkAnimatedImage.Create(Self);
  LAnimatedimage.LoadFromFile('Samples\Demo\Assets\rocket.json');
  LAnimatedimage.Parent := Self;
```

The example above results in the output below:

![Rocket](Assets/Documents/rocket.webp)

[Learn more...](Documents/ANIMATED-IMAGES.md#TSkAnimatedImage)

## TSkLabel

**TSkLabel** is the control that implements the SkParagraph internally, having several more features than the TLabel, such as:

- Font families; (font fallback list like in css)
- Font weight;
- Font slant;
- Support for multiple styles in text;
- Support for BiDi; (Right-to-Left)
- Support justify horizontal alignment;
- Support custom font; (without install the font)
- Supports background color on parts of the text;
- Limit the maximum number of lines;
- Auto size option; (width and height)
- Advanced decorations; (like underline wavy, overline, dashed line, among others...)
  **and much more...**

![Label](https://user-images.githubusercontent.com/16469061/153615162-e2f51dd6-b22e-4f34-9493-244122faa5ae.png#gh-light-mode-only)
![Label](https://user-images.githubusercontent.com/16469061/153615217-53e851a3-c20d-4cb9-92fb-b9b18319c342.png#gh-dark-mode-only)

[Learn more...](Documents/LABEL.md#tsklabel)

## TSkPaintBox

**TSkPaintBox** is the ideal control for painting with Skia API directly on the canvas with the event `OnDraw`:

```pascal
procedure TForm1.SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas;
  const ADest: TRectF; const AOpacity: Single);
begin
  var LPaint: ISkPaint := TSkPaint.Create;
  LPaint.Shader := TSkShader.MakeGradientSweep(ADest.CenterPoint,
    [$FFFCE68D, $FFF7CAA5, $FF2EBBC1, $FFFCE68D]);
  ACanvas.DrawPaint(LPaint);
end;
```

The example above results in the output below:

![Paint Box](Assets/Documents/paintbox.png)

_Note: The TSkPaintBox has a drawing caching system. To force a drawing refresh, call TSkPaintBox.Redraw. However, this cache system does not exist in FMX apps that have enabled [Skia4Delphi render](#fmx-render) for optimization reasons._

## TSkSvg

**TSkSvg** is the control to load and display SVG easily:

```pascal
  var LSvg := TSkSvg.Create(Self);
  LSvg.Svg.Source := TFile.ReadAllText('Samples\Demo\Assets\gorilla.svg');
  LSvg.Parent := Self;
```

The example above results in the output below:

<p><img src="Samples/Demo/Assets/gorilla.svg" width="200" height="200" alt="Gorilla" /></p>

[Learn more...](Documents/SVG.md)

# Compatibility

| RAD Studio                   | Platforms        |
| ---------------------------- | ---------------- |
| RAD Studio 11 Alexandria     | All Platforms    |
| RAD Studio 10.3 Rio or newer | Windows, Android |
| RAD Studio XE7 or newer      | Windows          |

For the platforms supported by **Skia4Delphi** (listed above), the OS versions supported by the library are the same [OS versions that RAD Studio supports.](https://docwiki.embarcadero.com/PlatformStatus/en/Main_Page)

# Known issues

Due to certain constraints within the IDE, there are specific limitations and workarounds.
*This topic will be updated as soon as these limitations no longer exist*

## Universal macOS Binary

When the project settings are configured to generate a universal binary for macOS (also known as fat library), the internal process executed by MSBuild only applies the "lipo" tool to the application, without an option to merge files present in the deployment configurations. As a result, the binary for macOS ARM64 targets (known in the IDE as "OSXARM64") is universal, containing both x64 and ARM64 versions, even when the option to generate a universal project binary is disabled.

## Submit your app to the Mac App Store

To submit a macOS application to the Apple Mac Store, it is necessary to sign all shared library files, and the deployment settings do not provide a method to sign files within them. In this case, follow the steps below for manual signing:

1. Transfer the **Skia4Delphi** shared library file to your macOS `Desktop` (if the target is OSXARM64, use `Binary\Shared\OSXARM64\libsk4d.dylib`. If it is OSX64, use `Binary\Shared\OSX64\libsk4d.dylib`);

2. Access your macOS `Desktop` via Terminal and run the following command:
   ```bash
   security find-identity -v -p codesigning
   ```

3. The output will present information formatted roughly like **XXXXX "Apple Development: YYYYY (ZZZZZ)"**. Use this information to execute the command below and sign the library:
   ```bash
   codesign --force --timestamp --sign "Apple Development: YYYYY (ZZZZZ)" libsk4d.dylib
   ```

4. Replace the original **Skia4Delphi** file with the signed file, then perform the "Clean" and "Build" operation on your project.

# Documentation

The APIs are very similar to Skia's, few methods and functions have been renamed for readability, so the [Skia documentation](https://skia.org/docs) can be used.

# Version

**[Skia4Delphi 6.0.0-beta3](/../../releases/latest)**

Skia Version used: [chrome/m107](https://github.com/google/skia/tree/chrome/m107)

# Sponsors & Partners

<p>
  <a href="https://www.a-dato.com">
    <img src="https://user-images.githubusercontent.com/11139086/186210969-0179cdbd-b65a-41cc-ad15-b7cc828a764f.png" alt="A-dato logo" width="200" /></a>
  <a href="https://www.delphistyles.com">
    <img src="https://user-images.githubusercontent.com/11139086/199366200-c5766e71-2684-4990-94bb-d44094fb90c4.png" alt="DelphiStyles logo" width="100" margin-left="100" /></a>
</p>

# Contributors

<a href="https://github.com/skia4delphi/skia4delphi/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=skia4delphi/skia4delphi" />
</a>

#

Help us responding a small questionnaire about our users in [this link](https://form.typeform.com/to/Qc6o3ELs)
