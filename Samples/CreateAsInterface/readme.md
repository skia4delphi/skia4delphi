# Skia4Delphi Proposal & Demo to CreateAsInterface with `.Make` 

Most of the Skia API is exposed as interfaces. This takes advantage of reference counting, which is nice to have. Usually, the class has limited public methods and most functionality is exposed through the interface. Here is the declaration for `TSkPaint` and `ISkPaint` for example.

**Update:** Changed the proposed method name to **`.Make`** instead of *`.CreateAsInterface`* per [viniciusfbb suggestion](https://github.com/skia4delphi/skia4delphi/pull/408#issuecomment-2787132333).   

```Delphi
type
   { ISkPaint }

  ISkPaint = interface(ISkObject) 
    ['{C95825F8-0D51-4BCE-8945-84AFE6264213}']
    function GetAlpha: Byte;
    function GetAlphaF: Single;

    { ... 50 other PUBLIC members ...}

    property StrokeWidth: Single read GetStrokeWidth write SetStrokeWidth;
    property Style: TSkPaintStyle read GetStyle write SetStyle;
  end;

  { TSkPaint }

  TSkPaint = class(TSkObject, ISkPaint)
  strict private
    function GetAlpha: Byte;
    function GetAlphaF: Single;
    
    { ... 34 other STRICT PRIVATE members ... }

    procedure SetStrokeWidth(const AValue: Single);
    procedure SetStyle(const AValue: TSkPaintStyle);
  public
    constructor Create; overload;
    constructor Create(const APaint: ISkPaint); overload;
    constructor Create(const AStyle: TSkPaintStyle); overload;
  end;
  ```

This is fine for *traditional* variable declarations:

```Delphi
var
  paint: ISkPaint;
begin
  paint := TSkPaint.Create;
  paint.Color := TAlphaColors.Darkorange;
end;
```

Unfortunately, it is *incompatible* with **type inferance**:

```Delphi
begin
  var inferred := TSkPaint.Create;
  inferred.Color := TAlphaColors.Darkorange; // E2003 Undeclared identifier: 'Color'
end;
```

Forces the more verbose form of inline variable declaration:

```Delphi
begin
  var explicit: ISkPaint := TSkPaint.Create();
  explicit.Color := TAlphaColors.Darkorange;
end;
```

The proposed `.Make` methods fix this:

```Delphi
begin
  var better := TSkPaint.Make();
  better.Color := TAlphaColors.Darkorange;
end;
```

Simply duplicate the existing constructors:

```Delphi
public  
  constructor Create; overload;
  constructor Create(const APaint: ISkPaint); overload;
  constructor Create(const AStyle: TSkPaintStyle); overload;
end;
```

Making as a `class function` with the name `Make` using the same arguments, and returning the `interface`. Finally `deprictate` the constructor.

```Delphi
public  
    class function Make: ISkPaint; overload; static;
    class function Make(const AStyle: TSkPaintStyle): ISkPaint; overload; static;
    class function MakeCopy(const APaint: ISkPaint): ISkPaint; static;
    constructor Create; overload; deprecated 'Use TSkPaint.Make instead.';
    constructor Create(const APaint: ISkPaint); overload; deprecated 'Use TSkPaint.MakeCopy instead.';
    constructor Create(const AStyle: TSkPaintStyle); overload; deprecated 'Use TSkPaint.Make instead.';
end;
```

The implementation calls the existing constructors, but since `Result` is explicitly the Interface, the code works as expected:

```Delphi
implementation

{ TSkPaint }

class function TSkPaint.Make: ISkPaint;
begin
  Result := TSkPaint.Create();
end;

class function TSkPaint.MakeCopy(const APaint: ISkPaint): ISkPaint;
begin
  Result := TSkPaint.Create(APaint);
end;

class function TSkPaint.Make(const AStyle: TSkPaintStyle): ISkPaint;
begin
  Result := TSkPaint.Create(AStyle);
end;
```

Type inference is as preferable. Beyond less typing it makes code more maintainable (type only needs to be changed in one place) and reduces errors resulting from mismatched types and constructors. The easier it is to take advantage of this feature in Delphi the better.

## Modified classes

* TSkColorFilter - For color transformations
* TSkFont - Font rendering and text metrics handling
* TSkPaint - Graphics styling configuration including colors, strokes, and effects
* TSkParagraphBuilder - A builder class for creating formatted text paragraphs with advanced text layout features like styles, fonts, and text decorations
* TSkParagraphStyle - Styling configuration for paragraph layouts
* TSkPath - Vector path definition and manipulation
* TSkPathBuilder - Used for building paths incrementally
* TSkPathMeasure - For measuring and extracting information about paths
* TSkPictureRecorder - Used for recording drawing commands
* TSkPixmap - For pixel buffer access
* TSkRegion - Represents a 2D set of pixels for clipping or hit-testing
* TSkRoundRect - Used for rounded rectangles
* TSkRuntimeBlenderBuilder - A builder class for creating custom blend modes at runtime, enabling custom pixel blending operations
* TSkRuntimeShaderBuilder - A builder class for creating custom shaders at runtime, allowing dynamic generation of graphical effects and patterns
* TSkShaper - Text shaping engine for complex script rendering
* TSkString - String handling utilities for Skia
* TSkStrutStyle - Text layout configuration for line spacing and alignment
* TSkTextStyle - Text styling configuration for paragraphs
* TSkUnicode - Unicode text processing and manipulation utilities for international text handling
