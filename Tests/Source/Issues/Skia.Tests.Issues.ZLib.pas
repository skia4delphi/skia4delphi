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
unit Skia.Tests.Issues.ZLib;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  DUnitX.TestFramework;

type
  { TZLibTests }

  /// <summary>
  ///    Test Delphi native zlib support. We need to ensure that the zlib used by Skia will not conflict with the
  ///    zlib used by Delphi.
  /// </summary>
  [TestFixture]
  TZLibTests = class
  public
    [Test]
    [TestCase('1', '1234456789011234567890')]
    [TestCase('2', 'aJghbGFf678POujqQPoq^7&(^$#!@+/`')]
    procedure TestCompressDecompressString(const AText: string);
  end;

implementation

uses
  { Delphi }
  System.SysUtils,
  System.ZLib;

{ TZLibTests }

procedure TZLibTests.TestCompressDecompressString(const AText: string);
var
  LCompressedData: TBytes;
  LDecompressedText: string;
begin
  Assert.WillNotRaise(
    procedure
    begin
      LCompressedData := ZCompressStr(AText);
      LDecompressedText := ZDeCompressStr(LCompressedData);
    end);
  Assert.AreEqual(AText, LDecompressedText);
end;

initialization
  TDUnitX.RegisterTestFixture(TZLibTests);
end.

