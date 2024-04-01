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
unit Skia.Tests.RotationScaleMatrix;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  System.Types,
  System.Math.Vectors,
  DUnitX.TestFramework,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkRotationScaleMatrixTests }

  [TestFixture]
  TSkRotationScaleMatrixTests = class(TTestBase)
  public
    [TestCase('1', '45,100,100,200,200')]
    [TestCase('2', '55,80,100,210,200')]
    [TestCase('3', '90,0,0,100,100')]
    procedure CreateDegreesCenter(const ADegreeAngle, ADestLeft, ADestTop, ADestRight, ADestBottom: Single);
  end;

implementation

uses
  { Delphi }
  System.Math,
  System.Classes,
  System.UITypes,
  System.IOUtils,

  { Skia }
  System.Skia;

{ TSkRotationScaleMatrixTests }

procedure TSkRotationScaleMatrixTests.CreateDegreesCenter(const ADegreeAngle,
  ADestLeft, ADestTop, ADestRight, ADestBottom: Single);
var
  LDest: TRectF;
  LMatrix: TMatrix;
  LRotationScaleMatrix: TSkRotationScaleMatrix;
begin
  LDest := RectF(ADestLeft, ADestTop, ADestRight, ADestBottom);
  // Generating rotation at the center of the LDest rectangle
  LMatrix := TMatrix.CreateTranslation(-LDest.CenterPoint.X, -LDest.CenterPoint.Y) *
    (TMatrix.CreateRotation(DegToRad(ADegreeAngle)) *
    TMatrix.CreateTranslation(LDest.CenterPoint.X, LDest.CenterPoint.Y));
  LRotationScaleMatrix := TSkRotationScaleMatrix.CreateDegrees(1, ADegreeAngle, LDest.CenterPoint.X, LDest.CenterPoint.Y, LDest.CenterPoint.X, LDest.CenterPoint.Y);
  Assert.IsTrue(LRotationScaleMatrix.ToMatrix.EqualsTo(LMatrix));
end;

initialization
  TDUnitX.RegisterTestFixture(TSkRotationScaleMatrixTests);
end.
