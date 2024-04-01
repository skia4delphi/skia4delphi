{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2021-2024 Skia4Delphi Project.                           }
{                                                                        }
{ Use of this source code is governed by the MIT license that can be     }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit Skia.Tests.Path;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  DUnitX.TestFramework,

  { Skia }
  System.Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkPathTests }

  [TestFixture]
  TSkPathTests = class(TTestBase)
  private
    function CreateSimplePath: ISkPath;
  public
    [TestCase('Simple Path', 'simple-path.elements.txt')]
    procedure TestBasicIterator(const AExpectedIteratorOutputFileName: string);
    [TestCase('Simple Path2', 'simple-path2.elements.txt')]
    procedure TestBasicToSVG(const AExpectedIteratorOutputFileName: string);
    [TestCase('Discord Icon Path',   'discord.svg-path.txt,' +   'discord.elements.txt')]
    [TestCase('Firefox Icon Path',   'firefox.svg-path.txt,' +   'firefox.elements.txt')]
    [TestCase('Microsoft Icon Path', 'microsoft.svg-path.txt,' + 'microsoft.elements.txt')]
    [TestCase('Telegram Icon Path',  'telegram.svg-path.txt,' +  'telegram.elements.txt')]
    [TestCase('Tesla Icon Path',     'tesla.svg-path.txt,' +     'tesla.elements.txt')]
    procedure TestIteratorFromSVGPath(const ASVGPathInputFileName, AExpectedIteratorOutputFileName: string);
    [Test]
    procedure TestIsEmpty;
    [Test]
    procedure TestIsRect;
    [TestCase('Simple Path Serialize', 'simple-path.serialized.bin')]
    procedure TestPathSerialize(const AExpectedSerializedOutputFileName: string);
  end;

implementation

uses
  { Delphi }
  System.Classes,
  System.Types,
  System.IOUtils;

{ TSkPathTests }

function TSkPathTests.CreateSimplePath: ISkPath;
var
  LOval: ISkRoundRect;
  LPathBuilder: ISkPathBuilder;
  LRect: TRectF;
begin
  LRect := TRectF.Create(TPointF.Create(10, 10), 100, 160);
  LOval := TSkRoundRect.Create;
  LOval.SetOval(LRect);
  LOval.Offset(40, 80);
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.AddRect(LRect);
  LPathBuilder.AddRoundRect(LOval);
  LRect.Offset(80, 50);
  LPathBuilder.AddOval(LRect);

  LPathBuilder.MoveTo(0, -10);
  LPathBuilder.QuadTo(100, 100, -10, 0);

  Result := LPathBuilder.Detach;
end;

procedure TSkPathTests.TestBasicIterator(const AExpectedIteratorOutputFileName: string);
var
  LPath: ISkPath;
begin
  LPath := CreateSimplePath;
  Assert.AreEqual(TFile.ReadAllText(AssetsPath + AExpectedIteratorOutputFileName).Trim, PathToText(LPath));
end;

procedure TSkPathTests.TestBasicToSVG(
  const AExpectedIteratorOutputFileName: string);
var
  LPath: ISkPath;
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.MoveTo(10, 20);
  LPathBuilder.LineTo(35, 45);
  LPathBuilder.LineTo(60, 55);
  LPathBuilder.CubicTo(80, 80, 5, 5, 100, 100);
  LPathBuilder.LineTo(40, 40);
  LPathBuilder.Close;
  LPath := TSkPath.Create(LPathBuilder.Detach.ToSVG);
  Assert.AreEqual(TFile.ReadAllText(AssetsPath + AExpectedIteratorOutputFileName).Trim, PathToText(LPath));
end;

procedure TSkPathTests.TestIsEmpty;

  procedure Check(APath: ISkPath; const AExpected: Boolean; const AMessage: string);
  begin
    Assert.AreEqual(AExpected, APath.IsEmpty, AMessage);
  end;

var
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  Check(LPathBuilder.Snapshot, True, '(Empty path)');

  LPathBuilder.AddRect(RectF(10, 10, 50, 70));
  Check(LPathBuilder.Snapshot, False, '(Path with single rect)');

  LPathBuilder.Reset;
  LPathBuilder.MoveTo(10, 20);
  Check(LPathBuilder.Snapshot, False, '(Path with moveTo)');

  LPathBuilder.Reset;
  Check(LPathBuilder.Snapshot, True, '(Path after reset)');

  LPathBuilder.Reset;
  LPathBuilder.AddPolygon([PointF(0, 0), PointF(0, 80), PointF(80, 80), PointF(80, 0), PointF(40, 0), PointF(20, 0)], False);
  Check(LPathBuilder.Snapshot, False, '(Path with rect polygon)');
end;

procedure TSkPathTests.TestIsRect;

  procedure Check(APath: ISkPath; const AExpected: Boolean; const AMessage: string);
  begin
    Assert.AreEqual(AExpected, APath.IsRect, AMessage);
  end;

var
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  Check(LPathBuilder.Snapshot, False, '(Empty path)');

  LPathBuilder.AddRect(RectF(10, 10, 50, 70));
  Check(LPathBuilder.Snapshot, True, '(Path with single rect)');

  LPathBuilder.MoveTo(10, 20);
  Check(LPathBuilder.Snapshot, True, '(Path with rect and moveTo)');

  LPathBuilder.LineTo(10, 30);
  Check(LPathBuilder.Snapshot, False, '(Path with rect and lineTo)');

  LPathBuilder.Reset;
  LPathBuilder.AddPolygon([PointF(0, 0), PointF(0, 80), PointF(80, 80), PointF(80, 0), PointF(40, 0), PointF(20, 0)], False);
  Check(LPathBuilder.Snapshot, True, '(Path with rect polygon)');
end;

procedure TSkPathTests.TestIteratorFromSVGPath(const ASVGPathInputFileName,
  AExpectedIteratorOutputFileName: string);
var
  LPath: ISkPath;
begin
  LPath := TSkPath.Create(TFile.ReadAllText(AssetsPath + ASVGPathInputFileName));
  Assert.AreEqual(TFile.ReadAllText(AssetsPath + AExpectedIteratorOutputFileName).Trim, PathToText(LPath));
end;

procedure TSkPathTests.TestPathSerialize(
  const AExpectedSerializedOutputFileName: string);
var
  LPath: ISkPath;
begin
  LPath := CreateSimplePath;
  Assert.AreEqualBytes(TFile.ReadAllBytes(AssetsPath + AExpectedSerializedOutputFileName), LPath.Serialize);
end;

initialization
  TDUnitX.RegisterTestFixture(TSkPathTests);
end.
