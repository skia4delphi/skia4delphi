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
unit Skia.Tests.FontMetrics;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  DUnitX.TestFramework,

  { Skia }
  System.Skia;

type
  { TSkFontMetricsTests }

  [TestFixture]
  TSkFontMetricsTests = class
  public
    [Test]
    procedure TestEquality;
  end;

implementation

{ TSkFontMetricsTests }

procedure TSkFontMetricsTests.TestEquality;
var
  LMetrics1: TSkFontMetrics;
  LMetrics2: TSkFontMetrics;
begin
  FillChar(LMetrics1, SizeOf(LMetrics1), 0);
  LMetrics2 := LMetrics1;
  Assert.IsTrue(LMetrics1 = LMetrics2);

  LMetrics2.CapHeight := 1;
  Assert.IsFalse(LMetrics1 = LMetrics2);
end;

initialization
  TDUnitX.RegisterTestFixture(TSkFontMetricsTests);
end.
