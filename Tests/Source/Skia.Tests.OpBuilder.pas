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
unit Skia.Tests.OpBuilder;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  System.Types,
  DUnitX.TestFramework,

  { Skia }
  System.Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkOpBuilderTests }

  [TestFixture]
  TSkOpBuilderTests = class(TTestBase)
  strict private
    function MakeRectPath(const ARect: TRectF): ISkPath;
  public
    [Test]
    procedure TestDetachResetsTheBuilder;
    [Test]
    procedure TestDifference;
    [Test]
    procedure TestEmptyBuilder;
    [Test]
    procedure TestIntersect;
    [Test]
    procedure TestMatchesPathOp;
    [Test]
    procedure TestUnionOfThreePaths;
  end;

implementation

uses
  { Delphi }
  System.Math,
  System.Math.Vectors;

{ TSkOpBuilderTests }

function TSkOpBuilderTests.MakeRectPath(const ARect: TRectF): ISkPath;
var
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.AddRect(ARect);
  Result := LPathBuilder.Detach;
end;

procedure TSkOpBuilderTests.TestDetachResetsTheBuilder;
var
  LOpBuilder: ISkOpBuilder;
  LPath: ISkPath;
begin
  LOpBuilder := TSkOpBuilder.Create;
  LOpBuilder.Add(MakeRectPath(RectF(0, 0, 100, 100)), TSkPathOp.Union);
  LPath := LOpBuilder.Detach;
  Assert.IsNotNull(LPath);
  Assert.IsFalse(LPath.IsEmpty, 'The first result should contain the added path');

  LPath := LOpBuilder.Detach;
  Assert.IsTrue((LPath = nil) or LPath.IsEmpty, 'Detach should reset the builder');
end;

procedure TSkOpBuilderTests.TestDifference;
var
  LOpBuilder: ISkOpBuilder;
  LPath: ISkPath;
begin
  LOpBuilder := TSkOpBuilder.Create;
  LOpBuilder.Add(MakeRectPath(RectF(0, 0, 100, 100)), TSkPathOp.Union);
  LOpBuilder.Add(MakeRectPath(RectF(50, 0, 200, 100)), TSkPathOp.Difference);
  LPath := LOpBuilder.Detach;
  Assert.IsNotNull(LPath);
  Assert.AreSameRect(RectF(0, 0, 50, 100), LPath.Bounds, TEpsilon.Position);
end;

procedure TSkOpBuilderTests.TestEmptyBuilder;
var
  LOpBuilder: ISkOpBuilder;
  LPath: ISkPath;
begin
  LOpBuilder := TSkOpBuilder.Create;
  Assert.IsNotNull(LOpBuilder);
  LPath := LOpBuilder.Detach;
  Assert.IsTrue((LPath = nil) or LPath.IsEmpty, 'A builder without paths should produce an empty path');
end;

procedure TSkOpBuilderTests.TestIntersect;
var
  LOpBuilder: ISkOpBuilder;
  LPath: ISkPath;
begin
  LOpBuilder := TSkOpBuilder.Create;
  LOpBuilder.Add(MakeRectPath(RectF(0, 0, 100, 100)), TSkPathOp.Union);
  LOpBuilder.Add(MakeRectPath(RectF(60, 20, 200, 80)), TSkPathOp.Intersect);
  LPath := LOpBuilder.Detach;
  Assert.IsNotNull(LPath);
  Assert.AreSameRect(RectF(60, 20, 100, 80), LPath.Bounds, TEpsilon.Position);
end;

procedure TSkOpBuilderTests.TestMatchesPathOp;
var
  LBuilt: ISkPath;
  LOpBuilder: ISkOpBuilder;
  LPath1: ISkPath;
  LPath2: ISkPath;
  LSymmetricDifference: ISkPath;
begin
  LPath1 := MakeRectPath(RectF(0, 0, 100, 100));
  LPath2 := MakeRectPath(RectF(50, 50, 150, 150));

  LOpBuilder := TSkOpBuilder.Create;
  LOpBuilder.Add(LPath1, TSkPathOp.Union);
  LOpBuilder.Add(LPath2, TSkPathOp.Union);
  LBuilt := LOpBuilder.Detach;
  Assert.IsNotNull(LBuilt);

  LSymmetricDifference := LBuilt.Op(LPath1.Op(LPath2, TSkPathOp.Union), TSkPathOp.&Xor);
  Assert.IsNotNull(LSymmetricDifference);
  Assert.IsTrue(LSymmetricDifference.IsEmpty, 'ISkOpBuilder and ISkPath.Op should cover the same area');
end;

procedure TSkOpBuilderTests.TestUnionOfThreePaths;
var
  LOpBuilder: ISkOpBuilder;
  LPath: ISkPath;
begin
  LOpBuilder := TSkOpBuilder.Create;
  LOpBuilder.Add(MakeRectPath(RectF(0, 0, 50, 50)), TSkPathOp.Union);
  LOpBuilder.Add(MakeRectPath(RectF(40, 40, 90, 90)), TSkPathOp.Union);
  LOpBuilder.Add(MakeRectPath(RectF(80, 80, 130, 130)), TSkPathOp.Union);
  LPath := LOpBuilder.Detach;
  Assert.IsNotNull(LPath);
  Assert.AreSameRect(RectF(0, 0, 130, 130), LPath.Bounds, TEpsilon.Position);
end;

initialization
  TDUnitX.RegisterTestFixture(TSkOpBuilderTests);
end.
