{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2021-2026 Skia4Delphi Project.                           }
{                                                                        }
{ Use of this source code is governed by the MIT license that can be     }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit Skia.Tests.Vertices;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  System.Types,
  System.UITypes,
  DUnitX.TestFramework,

  { Skia }
  System.Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkVerticesTests }

  [TestFixture]
  TSkVerticesTests = class(TTestBase)
  strict private
    FBuffer: TBytes;
    FPixmap: ISkPixmap;
    function CreateSurface(const ASize: Integer): ISkSurface;
    function DrawVertices(const AVertices: ISkVertices; const ABlendMode: TSkBlendMode = TSkBlendMode.Dest;
      const AShader: ISkShader = nil): ISkPixmap;
  public
    [Test]
    procedure TestDrawWithIndices;
    [Test]
    procedure TestDrawWithShader;
    [Test]
    procedure TestMakeCopy;
    [Test]
    procedure TestTriangleColors;
    [Test]
    procedure TestTriangleStrip;
  end;

implementation

uses
  { Delphi }
  System.Math,
  System.Math.Vectors;

{ TSkVerticesTests }

function TSkVerticesTests.CreateSurface(const ASize: Integer): ISkSurface;
begin
  SetLength(FBuffer, ASize * ASize * 4);
  FPixmap := TSkPixmap.Create(TSkImageInfo.Create(ASize, ASize, TSkColorType.BGRA8888,
    TSkAlphaType.Premul, TSkColorSpace.MakeSRGB), FBuffer, ASize * 4);
  Result := TSkSurface.MakeRasterDirect(FPixmap);
  Assert.IsNotNull(Result, 'Invalid ISkSurface (nil)');
  Result.Canvas.Clear(TAlphaColors.Null);
end;

function TSkVerticesTests.DrawVertices(const AVertices: ISkVertices;
  const ABlendMode: TSkBlendMode; const AShader: ISkShader): ISkPixmap;
var
  LPaint: ISkPaint;
  LSurface: ISkSurface;
begin
  Assert.IsNotNull(AVertices, 'Invalid ISkVertices (nil)');
  LSurface := CreateSurface(100);
  LPaint := TSkPaint.Create;
  LPaint.Shader := AShader;
  LSurface.Canvas.DrawVertices(AVertices, ABlendMode, LPaint);
  Result := FPixmap;
end;

procedure TSkVerticesTests.TestDrawWithIndices;
var
  LPixmap: ISkPixmap;
  LVertices: ISkVertices;
begin
  LVertices := TSkVertices.MakeCopy(TSkVertexMode.Triangles,
    [PointF(0, 0), PointF(100, 0), PointF(100, 100), PointF(0, 100)], nil,
    [TAlphaColors.Red, TAlphaColors.Red, TAlphaColors.Red, TAlphaColors.Red],
    [0, 1, 2, 0, 2, 3]);
  LPixmap := DrawVertices(LVertices);
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[10, 10], 2, 'The indexed quad should cover the upper left');
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[90, 90], 2, 'The indexed quad should cover the lower right');
end;

procedure TSkVerticesTests.TestDrawWithShader;
var
  LPixmap: ISkPixmap;
  LVertices: ISkVertices;
begin
  LVertices := TSkVertices.MakeCopy(TSkVertexMode.Triangles,
    [PointF(0, 0), PointF(100, 0), PointF(0, 100)], nil, nil);
  LPixmap := DrawVertices(LVertices, TSkBlendMode.Src, TSkShader.MakeColor(TAlphaColors.Blue));
  Assert.AreSameColor(TAlphaColors.Blue, LPixmap.Colors[10, 10], 2, 'The paint shader should fill the triangle');
  Assert.AreSameColor(TAlphaColors.Null, LPixmap.Colors[90, 90], 2, 'The area outside the triangle should stay empty');
end;

procedure TSkVerticesTests.TestMakeCopy;
begin
  Assert.IsNotNull(TSkVertices.MakeCopy(TSkVertexMode.Triangles,
    [PointF(0, 0), PointF(10, 0), PointF(0, 10)], nil, nil), '(positions only)');
  Assert.IsNotNull(TSkVertices.MakeCopy(TSkVertexMode.TriangleFan,
    [PointF(0, 0), PointF(10, 0), PointF(10, 10), PointF(0, 10)],
    [PointF(0, 0), PointF(1, 0), PointF(1, 1), PointF(0, 1)],
    [TAlphaColors.Red, TAlphaColors.Lime, TAlphaColors.Blue, TAlphaColors.White]), '(positions, textures and colors)');
end;

procedure TSkVerticesTests.TestTriangleColors;
var
  LPixmap: ISkPixmap;
  LVertices: ISkVertices;
begin
  LVertices := TSkVertices.MakeCopy(TSkVertexMode.Triangles,
    [PointF(0, 0), PointF(100, 0), PointF(0, 100)], nil,
    [TAlphaColors.Red, TAlphaColors.Red, TAlphaColors.Red]);
  LPixmap := DrawVertices(LVertices);
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[5, 5], 2, 'The triangle should be filled with the vertex color');
  Assert.AreSameColor(TAlphaColors.Black, DrawVertices(LVertices, TSkBlendMode.SrcOver).Colors[5, 5], 2,
    'With SrcOver the paint color replaces the vertex colors');
  Assert.AreSameColor(TAlphaColors.Null, LPixmap.Colors[95, 95], 2, 'The area outside the triangle should stay empty');
end;

procedure TSkVerticesTests.TestTriangleStrip;
var
  LPixmap: ISkPixmap;
  LVertices: ISkVertices;
begin
  LVertices := TSkVertices.MakeCopy(TSkVertexMode.TriangleStrip,
    [PointF(0, 0), PointF(0, 100), PointF(100, 0), PointF(100, 100)], nil,
    [TAlphaColors.Lime, TAlphaColors.Lime, TAlphaColors.Lime, TAlphaColors.Lime]);
  LPixmap := DrawVertices(LVertices);
  Assert.AreSameColor(TAlphaColors.Lime, LPixmap.Colors[50, 50], 2, 'The strip should cover the whole quad');
  Assert.AreSameColor(TAlphaColors.Lime, LPixmap.Colors[95, 5], 2, 'The strip should cover the upper right corner');
end;

initialization
  TDUnitX.RegisterTestFixture(TSkVerticesTests);
end.
