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
unit Skia.Tests.ColorSpace;

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
  { TSkColorSpaceTests }

  [TestFixture]
  TSkColorSpaceTests = class(TTestBase)
  public
    [Test]
    procedure TestICCProfileRoundTrip;
    // TODO: Investigate possible issue.
    // [Test]
    // procedure TestICCProfileToBytes;
    [Test]
    procedure TestIsEqual;
    [Test]
    procedure TestLinearGamma;
    [Test]
    procedure TestMakeRGB;
    [Test]
    procedure TestNumericalTransferFunction;
    [Test]
    procedure TestSRGB;
    [Test]
    procedure TestSRGBGamma;
    [Test]
    procedure TestToXyz;
    [Test]
    procedure TestTransferFunctionTransform;
    [Test]
    procedure TestPrimaries;
    [Test]
    procedure TestNamedTransferFunctions;
    [Test]
    procedure TestXyzOperations;
  end;

implementation

uses
  { Delphi }
  System.Math,
  System.Math.Vectors;

{ TSkColorSpaceTests }

procedure TSkColorSpaceTests.TestICCProfileRoundTrip;
var
  LColorSpace: ISkColorSpace;
  LProfile: ISkColorSpaceICCProfile;
  LXyz: TSkColorSpaceXyz;
begin
  LColorSpace := TSkColorSpace.MakeSRGB;
  LProfile := LColorSpace.ToProfile;
  Assert.IsNotNull(LProfile, 'Invalid ISkColorSpaceICCProfile (nil)');
  Assert.IsTrue(LProfile.ToXyz(LXyz), 'The sRGB profile should expose its matrix');
  Assert.IsTrue(LXyz = TSkColorSpaceXyz.SRGB, 'The profile matrix should be the sRGB one');

  Assert.IsNull(TSkColorSpaceICCProfile.MakeFromBytes(TBytes.Create(1, 2, 3)), 'Invalid profile bytes should not create a profile');
  Assert.IsNull(TSkColorSpaceICCProfile.MakeFromBytes(nil), 'Empty profile bytes should not create a profile');
end;

// TODO: Investigate possible issue.
//
// ISkColorSpaceICCProfile.ToBytes always answers an empty array for a profile
// produced by ISkColorSpace.ToProfile, because skcms only fills the buffer of a
// profile that was parsed from ICC bytes. A color space therefore cannot be
// serialized through this API, and TSkColorSpace.Make cannot be round tripped.
//
// procedure TSkColorSpaceTests.TestICCProfileToBytes;
// var
//   LColorSpace: ISkColorSpace;
//   LProfile: ISkColorSpaceICCProfile;
// begin
//   LColorSpace := TSkColorSpace.MakeSRGB;
//   LProfile := LColorSpace.ToProfile;
//   Assert.IsTrue(Length(LProfile.ToBytes) > 0, 'The profile should be serializable');
//   Assert.IsTrue(TSkColorSpace.Make(TSkColorSpaceICCProfile.MakeFromBytes(LProfile.ToBytes)).IsEqual(LColorSpace),
//     'The rebuilt color space should be sRGB');
// end;

procedure TSkColorSpaceTests.TestIsEqual;
var
  LColorSpace: ISkColorSpace;
begin
  LColorSpace := TSkColorSpace.MakeSRGB;
  Assert.IsTrue(LColorSpace.IsEqual(TSkColorSpace.MakeSRGB), 'Two sRGB color spaces should be equal');
  Assert.IsFalse(LColorSpace.IsEqual(TSkColorSpace.MakeSRGBLinear), 'sRGB and linear sRGB should not be equal');
  Assert.WillRaise(
    procedure
    begin
      LColorSpace.IsEqual(nil);
    end, ESkArgumentException, 'Comparing with nil should be rejected');
end;

procedure TSkColorSpaceTests.TestLinearGamma;
var
  LColorSpace: ISkColorSpace;
begin
  LColorSpace := TSkColorSpace.MakeSRGBLinear;
  Assert.IsNotNull(LColorSpace);
  Assert.IsTrue(LColorSpace.GammaIsLinear, '(GammaIsLinear)');
  Assert.IsFalse(LColorSpace.GammaCloseToSRGB, '(GammaCloseToSRGB)');
  Assert.IsFalse(LColorSpace.IsSRGB, '(IsSRGB)');

  LColorSpace := TSkColorSpace.MakeSRGB.MakeLinearGamma;
  Assert.IsNotNull(LColorSpace);
  Assert.IsTrue(LColorSpace.GammaIsLinear, 'MakeLinearGamma should produce a linear color space');
  Assert.IsTrue(LColorSpace.IsEqual(TSkColorSpace.MakeSRGBLinear), 'MakeLinearGamma of sRGB should be linear sRGB');
end;

procedure TSkColorSpaceTests.TestMakeRGB;
var
  LColorSpace: ISkColorSpace;
begin
  LColorSpace := TSkColorSpace.MakeRGB(TSkColorSpaceTransferFunction.SRGB, TSkColorSpaceXyz.SRGB);
  Assert.IsNotNull(LColorSpace);
  Assert.IsTrue(LColorSpace.IsSRGB, 'The sRGB transfer function and matrix should produce sRGB');
  Assert.IsTrue(LColorSpace.IsEqual(TSkColorSpace.MakeSRGB));

  LColorSpace := TSkColorSpace.MakeRGB(TSkColorSpaceTransferFunction.Linear, TSkColorSpaceXyz.AdobeRGB);
  Assert.IsNotNull(LColorSpace);
  Assert.IsTrue(LColorSpace.GammaIsLinear, 'The linear transfer function should produce a linear color space');
  Assert.IsFalse(LColorSpace.IsSRGB);
end;

procedure TSkColorSpaceTests.TestNumericalTransferFunction;
var
  LTransferFunction: TSkColorSpaceTransferFunction;
begin
  Assert.IsTrue(TSkColorSpace.MakeSRGB.IsNumericalTransferFunction(LTransferFunction), 'sRGB has a numerical transfer function');
  Assert.IsTrue(LTransferFunction = TSkColorSpaceTransferFunction.SRGB, 'The transfer function should be the sRGB one');

  Assert.IsTrue(TSkColorSpace.MakeSRGBLinear.IsNumericalTransferFunction(LTransferFunction), 'Linear sRGB has a numerical transfer function');
  Assert.IsTrue(LTransferFunction = TSkColorSpaceTransferFunction.Linear, 'The transfer function should be the linear one');
end;

procedure TSkColorSpaceTests.TestSRGB;
var
  LColorSpace: ISkColorSpace;
begin
  LColorSpace := TSkColorSpace.MakeSRGB;
  Assert.IsNotNull(LColorSpace);
  Assert.IsTrue(LColorSpace.IsSRGB, '(IsSRGB)');
  Assert.IsTrue(LColorSpace.GammaCloseToSRGB, '(GammaCloseToSRGB)');
  Assert.IsFalse(LColorSpace.GammaIsLinear, '(GammaIsLinear)');
end;

procedure TSkColorSpaceTests.TestSRGBGamma;
var
  LColorSpace: ISkColorSpace;
begin
  LColorSpace := TSkColorSpace.MakeSRGBLinear.MakeSRGBGamma;
  Assert.IsNotNull(LColorSpace);
  Assert.IsTrue(LColorSpace.IsSRGB, 'MakeSRGBGamma of linear sRGB should be sRGB');
  Assert.IsTrue(LColorSpace.IsEqual(TSkColorSpace.MakeSRGB));
end;

procedure TSkColorSpaceTests.TestToXyz;
var
  LXyz: TSkColorSpaceXyz;
begin
  Assert.IsTrue(TSkColorSpace.MakeSRGB.ToXyz(LXyz), 'sRGB should expose its matrix');
  Assert.IsTrue(LXyz = TSkColorSpaceXyz.SRGB, 'The matrix should be the sRGB one');
  Assert.IsFalse(LXyz = TSkColorSpaceXyz.AdobeRGB, 'The sRGB and Adobe RGB matrices differ');
end;

procedure TSkColorSpaceTests.TestTransferFunctionTransform;
var
  LInverted: TSkColorSpaceTransferFunction;
  LTransferFunction: TSkColorSpaceTransferFunction;
begin
  LTransferFunction := TSkColorSpaceTransferFunction.Linear;
  Assert.AreEqual(0.5, LTransferFunction.Transform(0.5), TEpsilon.Vector, 'The linear transfer function is the identity');

  LTransferFunction := TSkColorSpaceTransferFunction.SRGB;
  Assert.AreEqual(0.0, LTransferFunction.Transform(0), TEpsilon.Vector, '(0)');
  Assert.AreEqual(1.0, LTransferFunction.Transform(1), TEpsilon.Vector, '(1)');
  Assert.IsTrue(LTransferFunction.Transform(0.5) < 0.5, 'The sRGB transfer function is convex');

  Assert.IsTrue(LTransferFunction.Invert(LInverted), 'The sRGB transfer function should be invertible');
  Assert.AreEqual(0.5, LInverted.Transform(LTransferFunction.Transform(0.5)), 0.001, 'Transform and its inverse should cancel out');
end;

procedure TSkColorSpaceTests.TestNamedTransferFunctions;
var
  LTransferFunction: TSkColorSpaceTransferFunction;
begin
  LTransferFunction := TSkColorSpaceTransferFunction.TwoDotTwo;
  Assert.AreEqual(Power(0.5, 2.2), LTransferFunction.Transform(0.5), 0.0001, 'The gamma 2.2 curve');

  LTransferFunction := TSkColorSpaceTransferFunction.HLG;
  Assert.IsTrue(LTransferFunction <> TSkColorSpaceTransferFunction.SRGB, 'HLG is not sRGB');
  Assert.IsNotNull(TSkColorSpace.MakeRGB(LTransferFunction, TSkColorSpaceXyz.Rec2020),
    'HLG should build a color space');

  LTransferFunction := TSkColorSpaceTransferFunction.PQ;
  Assert.IsTrue(LTransferFunction <> TSkColorSpaceTransferFunction.HLG, 'PQ is not HLG');
  Assert.IsNotNull(TSkColorSpace.MakeRGB(LTransferFunction, TSkColorSpaceXyz.Rec2020),
    'PQ should build a color space');
end;

procedure TSkColorSpaceTests.TestPrimaries;
var
  LPrimaries: TSkColorSpacePrimaries;
  LXyz: TSkColorSpaceXyz;
begin
  LPrimaries := TSkColorSpacePrimaries.Create(0.64, 0.33, 0.30, 0.60, 0.15, 0.06, 0.3127, 0.3290);
  LXyz := LPrimaries.ToColorSpaceXyz;
  Assert.AreEqual(TSkColorSpaceXyz.SRGB.M11, LXyz.M11, 0.001, 'The sRGB primaries should build the sRGB matrix');
  Assert.AreEqual(TSkColorSpaceXyz.SRGB.M33, LXyz.M33, 0.001, '(M33)');
end;

procedure TSkColorSpaceTests.TestXyzOperations;
var
  LXyz: TSkColorSpaceXyz;
begin
  LXyz := TSkColorSpaceXyz.Identity;
  Assert.AreEqual(1.0, LXyz.Determinant, TEpsilon.Vector, 'The identity determinant is 1');
  Assert.IsTrue(LXyz.Inverse = LXyz, 'The identity is its own inverse');
  Assert.IsTrue(TSkColorSpaceXyz.SRGB * LXyz = TSkColorSpaceXyz.SRGB, 'Multiplying by the identity should not change the matrix');

  LXyz := TSkColorSpaceXyz.SRGB;
  Assert.IsTrue(LXyz.Determinant <> 0, 'The sRGB matrix should be invertible');
  Assert.IsTrue(LXyz * LXyz.Inverse = TSkColorSpaceXyz.Identity, 'A matrix times its inverse is the identity');
  Assert.AreEqual(LXyz.Adjoint.M11, LXyz.Inverse.M11 * LXyz.Determinant, 0.001,
    'The inverse is the adjoint divided by the determinant');
  Assert.IsTrue(LXyz <> TSkColorSpaceXyz.Rec2020, 'sRGB and Rec2020 differ');
  Assert.IsTrue(LXyz <> TSkColorSpaceXyz.DisplayP3, 'sRGB and Display P3 differ');
end;

initialization
  TDUnitX.RegisterTestFixture(TSkColorSpaceTests);
end.
