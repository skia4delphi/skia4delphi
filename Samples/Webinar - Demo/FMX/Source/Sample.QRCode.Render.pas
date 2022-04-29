// This example only focuses on QR code rendering and not encoding. There are several examples on the internet
// of how to encode a QR code, for example: https://github.com/foxitsoftware/DelphiZXingQRCode
unit Sample.QRCode.Render;

interface

uses
  { Skia }
  Skia;

type
  T2DBooleanArray = array of array of Boolean;

  { IQRCodeRender }

  IQRCodeRender = interface
    function AsPath: ISkPath;
    function AsSVG: string; overload;
    function AsSVG(const APaint: ISkPaint): string; overload;
  end;

  { TQRCodeRender }

  TQRCodeRender = record
    class function MakeRounded(const AModules: T2DBooleanArray): IQRCodeRender; overload; static;
    class function MakeRounded(const AModules: T2DBooleanArray; const ALogoModules: Integer; const AModuleSize: Single = 1): IQRCodeRender; overload; static;
  end;

implementation

uses
  { Delphi }
  System.Types,
  System.Classes,
  System.Math,
  System.Math.Vectors;

type
  { TQRCodeRoundedRender }

  TQRCodeRoundedRender = class(TInterfacedObject, IQRCodeRender)
  private const
    PositionModulesCount = 7;
  private
    FLogoModules: Integer;
    FModuleCount: Integer;
    FModules: T2DBooleanArray;
    FModuleSize: Single;
    function AsPath: ISkPath;
    function AsSVG: string; overload;
    function AsSVG(const APaint: ISkPaint): string; overload;
  public
    constructor Create(const AModules: T2DBooleanArray; const ALogoModules: Integer; const AModuleSize: Single);
  end;

{ TQRCodeRender }

class function TQRCodeRender.MakeRounded(
  const AModules: T2DBooleanArray; const ALogoModules: Integer;
  const AModuleSize: Single): IQRCodeRender;
begin
  Result := TQRCodeRoundedRender.Create(AModules, ALogoModules, AModuleSize);
end;

class function TQRCodeRender.MakeRounded(
  const AModules: T2DBooleanArray): IQRCodeRender;
begin
  Result := MakeRounded(AModules, 0);
end;

{ TQRCodeRoundedRender }

function TQRCodeRoundedRender.AsPath: ISkPath;

  procedure AddPositionPatternPath(const APathBuilder: ISkPathBuilder; const ARow, AColumn: Integer);
  const
    RadiusProportion = 1/3;
  var
    LRect: TRectF;
  begin
    APathBuilder.FillType := TSkPathFillType.EvenOdd;
    LRect := TRectF.Create(PointF(AColumn * FModuleSize, ARow * FModuleSize), PositionModulesCount * FModuleSize, PositionModulesCount * FModuleSize);
    APathBuilder.AddRoundRect(TSkRoundRect.Create(LRect.Round, LRect.Width * RadiusProportion, LRect.Height * RadiusProportion));
    LRect.Inflate(-FModuleSize, -FModuleSize);
    APathBuilder.AddRoundRect(TSkRoundRect.Create(LRect.Round, LRect.Width * RadiusProportion, LRect.Height * RadiusProportion));
    LRect.Inflate(-FModuleSize, -FModuleSize);
    APathBuilder.AddRoundRect(TSkRoundRect.Create(LRect.Round, LRect.Width * RadiusProportion, LRect.Height * RadiusProportion));
  end;

  procedure AddModuleRoundedDark(const APathBuilder: ISkPathBuilder; const ALeft, ATop, ARight, ABottom: Integer; const ARadius: Single; const NW, NE, SE, SW: Boolean);

    procedure lal(const B: Boolean; X0, Y0, X1, Y1, R0, R1: Single);
    begin
      if B then
      begin
        APathBuilder.LineTo(X0 + R0, Y0 + R1);
        APathBuilder.ArcTo(PointF(X0, Y0), PointF(X1, Y1), ARadius);
      end
      else
        APathBuilder.LineTo(X0, Y0);
    end;

  begin
    if NW then
      APathBuilder.moveTo(ALeft + ARadius, ATop)
    else
      APathBuilder.moveTo(ALeft, ATop);
    lal(NE, ARight, ATop, ARight, ABottom, -ARadius, 0);
    lal(SE, ARight, ABottom, ALeft, ABottom, 0, -ARadius);
    lal(SW, ALeft, ABottom, ALeft, ATop, ARadius, 0);
    lal(NW, ALeft, ATop, ARight, ATop, 0, ARadius);
  end;

  procedure AddModuleRoundedLight(const APathBuilder: ISkPathBuilder; const ALeft, ATop, ARight, ABottom: Integer; const ARadius: Single; const NW, NE, SE, SW: Boolean);

    procedure mlla(const AX, AY: Integer; const AR0, AR1: Single);
    begin
      APathBuilder.MoveTo(AX + AR0, AY);
      APathBuilder.LineTo(AX, AY);
      APathBuilder.LineTo(AX, AY + AR1);
      APathBuilder.ArcTo(PointF(AX, AY), PointF(AX + AR0, AY), ARadius);
    end;

  begin
    if NW then
      mlla(ALeft, ATop, ARadius, ARadius);
    if NE then
      mlla(ARight, ATop, -ARadius, ARadius);
    if SE then
      mlla(ARight, ABottom, -ARadius, -ARadius);
    if SW then
      mlla(ALeft, ABottom, ARadius, -ARadius);
  end;

  function IsDark(const ARow, AColumn: Integer): Boolean;
  begin
    Result := (ARow >= 0) and (ARow < FModuleCount) and (AColumn >= 0) and (AColumn < FModuleCount);
    if Result then
      Result := FModules[ARow][AColumn];
  end;

  procedure AddModuleRounded(const APathBuilder: ISkPathBuilder; const ALeft, ATop, AWidth: Single; const ARow, ACol: Integer);
  const
    RadiusProportion = 0.5;
  var
    LRight: Integer;
    LBottom: Integer;
    LRowT: Integer;
    LRowB: Integer;
    LColL: Integer;
    LColR: Integer;
    LRadius: Single;
    LCenter: Boolean;
    LNorthwest: Boolean;
    LNorth: Boolean;
    LNortheast: Boolean;
    LEast: Boolean;
    LSoutheast: Boolean;
    LSouth: Boolean;
    LSouthwest: Boolean;
    LWest: Boolean;
  begin
    LRight := Round(ALeft + AWidth);
    LBottom := Round(ATop + AWidth);
    LRowT := ARow - 1;
    LRowB := ARow + 1;
    LColL := ACol - 1;
    LColR := ACol + 1;
    LRadius := AWidth * RadiusProportion;
    LCenter := IsDark(ARow, ACol);
    LNorthwest := IsDark(LRowT, LColL);
    LNorth := IsDark(LRowT, ACol);
    LNortheast := IsDark(LRowT, LColR);
    LEast := IsDark(ARow, LColR);
    LSoutheast := IsDark(LRowB, LColR);
    LSouth := IsDark(LRowB, ACol);
    LSouthwest := IsDark(LRowB, LColL);
    LWest := IsDark(ARow, LColL);

    if LCenter then
      AddModuleRoundedDark(APathBuilder, Round(ALeft), Round(ATop), LRight, LBottom, LRadius, (not LNorth) and (not LWest), (not LNorth) and (not LEast), (not LSouth) and (not LEast), (not LSouth) and (not LWest))
    else
      AddModuleRoundedLight(APathBuilder, Round(ALeft), Round(ATop), LRight, LBottom, LRadius / 2, LNorth and LWest and LNorthwest, LNorth and LEast and LNortheast, LSouth and LEast and LSoutheast, LSouth and LWest and LSouthwest);
  end;

var
  LPathBuilder: ISkPathBuilder;
  LRow: Integer;
  LColumn: Integer;
begin
  LPathBuilder := TSkPathBuilder.Create;
  AddPositionPatternPath(LPathBuilder, 0, 0);
  AddPositionPatternPath(LPathBuilder, FModuleCount - PositionModulesCount, 0);
  AddPositionPatternPath(LPathBuilder, 0, FModuleCount - PositionModulesCount);
  if FLogoModules > 0 then
    LPathBuilder.AddCircle(PointF(FModuleCount * FModuleSize, FModuleCount * FModuleSize) / 2, (FLogoModules + 0.9) * FModuleSize * 0.5);

  for LRow := 0 to FModuleCount - 1 do
    for LColumn := 0 to FModuleCount - 1 do
      AddModuleRounded(LPathBuilder, LColumn * FModuleSize, LRow * FModuleSize, FModuleSize, LRow, LColumn);
  Result := LPathBuilder.Detach;
end;

function TQRCodeRoundedRender.AsSVG: string;
begin
  Result := AsSVG(nil);
end;

function TQRCodeRoundedRender.AsSVG(const APaint: ISkPaint): string;
var
  LStream: TStringStream;
  LCanvas: ISkCanvas;
  LPaint: ISkPaint;
begin
  LStream := TStringStream.Create;
  try
    LCanvas := TSkSVGCanvas.Make(RectF(0, 0, FModuleCount, FModuleCount), LStream);
    if APaint <> nil then
      LPaint := APaint
    else
      LPaint := TSkPaint.Create;
    LCanvas.DrawPath(AsPath, LPaint);
    LCanvas := nil;
    Result := LStream.DataString;
  finally
    LStream.Free;
  end;
end;

constructor TQRCodeRoundedRender.Create(const AModules: T2DBooleanArray; const ALogoModules: Integer; const AModuleSize: Single);

  procedure RemoveSquareArea(var AModules: T2DBooleanArray; const ARow, AColumn, ACount: Integer);
  var
    LColumn: Integer;
    LRow: Integer;
  begin
    for LRow := ARow to ARow + ACount - 1 do
      for LColumn := AColumn to AColumn + ACount - 1 do
        AModules[LRow][LColumn] := False;
  end;

begin
  inherited Create;
  FModules := Copy(AModules);
  FModuleSize := AModuleSize;
  FModuleCount := Length(AModules);
  FLogoModules := ALogoModules;
  Assert(FModuleCount > ((PositionModulesCount + 1) * 2) + FLogoModules);
  Assert(((FModuleCount - FLogoModules) mod 2) = 0);
  // Remove position pattern
  RemoveSquareArea(FModules, 0, 0, PositionModulesCount + 1);
  RemoveSquareArea(FModules, FModuleCount - 1 - PositionModulesCount, 0, PositionModulesCount + 1);
  RemoveSquareArea(FModules, 0, FModuleCount - 1 - PositionModulesCount, PositionModulesCount + 1);
  if FLogoModules > 0 then
    RemoveSquareArea(FModules, ((FModuleCount - FLogoModules) div 2) - 1, ((FModuleCount - FLogoModules) div 2) - 1, FLogoModules + 2);
end;

end.
