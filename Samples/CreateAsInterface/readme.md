# Skia4Delphi `AsInterface` Proposal & Demo

Most of the Skia API is exposed as interfaces. This takes advantage of reference counting, which is nice to have. Usually, the class has limited public methods and most functionality is exposed through the interface.

**Example:**

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
    
    { ... 34 other STRICT PRIVATE memebers ... }

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

The proposed `CreateAsInterace` methods fix this:

```Delphi
begin
  var better := TSkPaint.Create();
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

Making them a `class function` named `CreateAsInterface` with the same arguements, and returning the `interface`:

```Delphi
public  
  class function CreateAsInterface: ISkPaint; overload;
  class function CreateAsInterface(const APaint: ISkPaint): ISkPaint; overload;
  class function CreateAsInterface(const AStyle: TSkPaintStyle): ISkPaint; overload;
end;
```

Any name could be used (`Init`, `AsInterface`, `George`, etc.), but this one makes them very visible in class completion to aid in discovery.

The implementation just calls the existing constructors, but since the Result is explicitly the Interface, this works as expected:

```Delphi
implementation

{ TSkPaint }

class function TSkPaint.CreateAsInterface: ISkPaint;
begin
  Result := TSkPaint.Create();
end;

class function TSkPaint.CreateAsInterface(const APaint: ISkPaint): ISkPaint;
begin
  Result := TSkPaint.Create(APaint);
end;

class function TSkPaint.CreateAsInterface(const AStyle: TSkPaintStyle): ISkPaint;
begin
  Result := TSkPaint.Create(AStyle);
end;
```

Type inferance is as preferable. Beyond less typing it makes code more maintainable (type only needs to be changed in one place) and reduces errors resulting from mismatched types and constructors. The easier it is to take advantage of this feature in Delphi the better.
