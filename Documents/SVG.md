<p><a href="https://www.skia4delphi.org"><img src="../Assets/Artwork/logo-gradient.svg" alt="Logo" height="300" width="360" /></a></p>

#  SVG

## Rendering SVG

The code below shows how to load and render a SVG file into an ISkCanvas:

```pascal
uses
  Skia, Skia.FMX; // or Skia.Vcl

var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create(100, 100);
  try
    LBitmap.SkiaDraw(
      procedure (const ACanvas: ISKCanvas)
      var
        LSvgBrush: TSkSvgBrush;
      begin
        LSvgBrush := TSkSvgBrush.Create;
        try
          LSvgBrush.Source := TFile.ReadAllText('Assets\Samples\gorilla.svg');
          LSvgBrush.Render(ACanvas, RectF(0, 0, LBitmap.Width, LBitmap.Height), 1);
        finally
          LSvgBrush.Free;
        end;
      end);
```

## Creating SVG

It is possible to create any type of SVG file using **Skia4Delphi** in an extremely simple way. Using the TSkSVGCanvas class, a SkCanvas is generated in which everything that you drawn using it, is transformed into an SVG file, even texts, gradients and effects. See an example:

```pascal
uses
  Skia;

type
  TSkDrawProc = reference to procedure(const ACanvas: ISkCanvas; const ADest: TRectF);

procedure CreateSVG(const AOutputFileName: string; const AWidth, AHeight: Integer; const ADrawProc: TSkDrawProc);
var
  LStream: TStream;
  LCanvas: ISkCanvas;
begin
  LStream := TFileStream.Create(AOutputFileName, fmCreate);
  try
    LCanvas := TSkSVGCanvas.Make(RectF(0, 0, AWidth, AHeight), LStream, [TSkSVGCanvasFlag.ConvertTextToPaths]);
    ADrawProc(LCanvas, RectF(0, 0, AWidth, AHeight));
    LCanvas := nil;
  finally
    LStream.Free;
  end;
end;
```

```pascal
CreateSVG('output.svg', 256, 256,
  procedure (const ACanvas: ISkCanvas; const ADest: TRectF)
  var
    LFont: ISkFont;
    LPaint: ISkPaint;
  begin
    LFont := TSkFont.Create(TSkTypeface.MakeFromFile('Assets\Samples\nunito-extrabold.ttf'), 23);
    LPaint := TSkPaint.Create;
    LPaint.Shader := TSkShader.MakeGradientLinear(PointF(0, 0), PointF(256, 145), $FFFF5F5F, $FF5B8DFE, TSkTileMode.Clamp);
 
    ACanvas.DrawSimpleText('"Each dream that you', 2, 25, LFont, LPaint);
    ACanvas.DrawSimpleText('leave behind is a part', 2, 55, LFont, LPaint);
    ACanvas.DrawSimpleText('of your future that will', 2, 85, LFont, LPaint);
    ACanvas.DrawSimpleText('no longer exist."', 2, 115, LFont, LPaint);
 
    LFont := TSkFont.Create(TSkTypeface.MakeFromFile('Assets\Samples\bonheur-royale-regular.ttf'), 28);
    LPaint.Shader := nil;
    LPaint.Color  := $FF5B8DFE;
    ACanvas.DrawSimpleText('(Steve Jobs)', 2, 150, LFont, LPaint);
  end);
```
This code results in the SVG file below:

<p><img src="../Assets/Documents/text-custom-font.svg" width="256" height="168" alt="Text Custom Font" /></p>

  

## TSkSvg

**TSkSvg** is the control (VCL or FMX) to load and display SVG easily:

```pascal
var
  LSvg: TSkSvg;
begin
  LSvg := TSkSvg.Create(Self);
  LSvg.Svg.Source := TFile.ReadAllText('Assets\Samples\gorilla.svg');
  LSvg.Parent := Self;
end;
```

The example above results in the output below:

<p><img src="../Assets/Samples/gorilla.svg" width="200" height="200" alt="Gorilla" /></p>

  

### SkSvg.Source property

Text property of SVG file content.

### SkSvg.OverrideColor property

When this property is Null (default value), the control will render the SVG in default color. But when this property have another value, the control will replace the SVG color to the `OverrideColor`. This is usefull in icons SVGs, for example.

### SkSvg.WrapMode property

Specifies whether and how to resize, replicate, and position the SVG draw inside the **TSkSvg** control. See the available options:

| TSkSvgWrapMode | Description                                                                                                                                                   |
|----------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Default        | Keep SVG stretch defined in SVG file                                                                                                                          |
| Fit (Default)  | Stretches SVG into the destination rectangle, preserving aspect ratio                                                                                         |
| FitCrop        | Stretches SVG into the destination rectangle, preserving aspect ratio, but cropping the excess                                                                |
| Original       | Display the SVG with its original dimensions                                                                                                                  |
| OriginalCenter | Display the SVG with its original dimensions but centered                                                                                                     |
| Place          | Places the SVG inside the destination rectangle. If the SVG is greater than the destination rectangle then the SVG is scaled down with aspect ratio preserved |
| Stretch        | Stretch the SVG to fill the entire destination rectangle                                                                                                      |
| Tile           | Tile (multiply) the SVG to cover the entire destination rectangle                                                                                             |

  

## Limitations

The **Skia4Delphi** rendering of SVG has two limitations:

- Does not support style element (see the workaround in next topic, [Remarks](#remarks))
- Does not support animated SVG

  

## Remarks

As said earlier, the **Skia4Delphi** doesn't support SVG style element yet, which are snippets with CSS code. However, the style element is not required. Some software only creates it because the generated svg file can be a fraction smaller with the style attribute than without the style attribute. To solve this problem, you should disable the generation of these styles whenever you generate your SVG. For example, in Adobe Illustrator when you export your vector as SVG, you need to set the follow configurations:

 - In Styling field: choose **Presentation Attributes** instead of Inline CSS (because CSS is not fully supported).

 <img src="../Assets/Documents/adobe-illustrator-exporting-svg.png">

But if you already have the incompatible .svg, there are several tools that you can use to solve this incompatibility, like for example the tool [sK1](https://github.com/sk1project/sk1-wx).

*Note: The incompatibility is only with the .svg file formatting, so, when using your editor's tool/options to solve this problem, it will generate another .svg file but with the same image. In other words, the draw that your new .svg will generate will be absolutely the same, however, compatible with many more applications.*
