{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2011-2022 Google LLC.                                    }
{ Copyright (c) 2021-2022 Skia4Delphi Project.                           }
{                                                                        }
{ Use of this source code is governed by a BSD-style license that can be }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit Skia.Tests.Vcl.TSkLabel;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  System.Types,
  System.UITypes,
  DUnitX.TestFramework,

  { Skia }
  Skia,
  Skia.Vcl,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkLabelTests }

  [TestFixture]
  TSkLabelTests = class(TTestBase)
  private
    const
      ShortText = 'Label1';
      {$IFDEF MSWINDOWS}
      LongText = 'Lorem ipsum dolor sit amet consectetur adipiscing elit sed do eiusmod tempor incididunt ut labore ' +
        'et dolore magna aliqua. Cursus mattis molestie a iaculis at erat pellentesque. Venenatis lectus magna fringilla ' +
        'urna porttitor rhoncus dolor purus. Malesuada bibendum arcu vitae elementum curabitur vitae nunc. Platea dictumst ' +
        'vestibulum rhoncus est pellentesque. Tellus integer feugiat scelerisque varius morbi enim nunc faucibus. Sem nulla ' +
        'pharetra diam sit amet nisl. Habitasse platea dictumst quisque sagittis purus sit amet volutpat. Est lorem ipsum ' +
        'dolor sit. Tellus in hac habitasse platea dictumst vestibulum. Sapien faucibus et molestie ac feugiat sed lectus ' +
        'vestibulum mattis. Orci nulla pellentesque dignissim enim sit. Massa enim nec dui nunc mattis enim. Tempor orci ' +
        'dapibus ultrices in iaculis. Egestas integer eget aliquet nibh praesent. Venenatis tellus in metus vulputate. Enim ' +
        'ut tellus elementum sagittis vitae et leo. In iaculis nunc sed augue lacus viverra vitae congue eu. Feugiat pretium ' +
        'nibh ipsum consequat nisl. Lobortis feugiat vivamus at augue eget arcu dictum.';
      {$ENDIF}
    procedure Test(const AText: string; const ABitmapSize: TSize; const AScale, AFontSize: Single; const ATextTopLeft, AMaxSize: TPointF; const AMaxLines: Integer; const ARightToLeft: Boolean; const AHorizontalAlign: TSkTextHorzAlign; const AVerticalAlign: TSkTextVertAlign; const ATrimming: TSkTextTrimming; const AColor: TAlphaColor; const AExpectedTextRect: TRectF; const ACheckTextRect: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
  protected
    function AssetsPath: string; override;
  public
    {$IFDEF MSWINDOWS}
    [TestCase('1.Short', ShortText + ',100,40,1,0,0,65535,65535,14,1,false,Leading,Leading,Character,claBlack,0,0,41,19,0.99,Dw8PHz////////////////////////////////////8D/wP///////////////////////////8')]
    [TestCase('2.Short blue', ShortText + ',100,40,1,0,0,65535,65535,14,1,false,Leading,Leading,Character,#FF9F9FFF,0,0,41,19,0.99,Dw8PHx////9/f39/X////39/f39f////f39/f//////8APwAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('3.Short horizontal leading', ShortText + ',100,40,1,0,0,100,40,14,1,false,Leading,Leading,Character,claBlack,0,0,41,19,0.99,Dw8PHz////////////////////////////////////8D/wP///////////////////////////8')]
    [TestCase('4.Short horizontal center', ShortText + ',100,40,1,0,0,100,40,14,1,false,Center,Leading,Character,claBlack,0,0,41,19,0.99,w8PDw+f///////Pj5///////8+Pn///////////////4H/gf//////////////////////////8')]
    [TestCase('5.Short horizontal trailing', ShortText + ',100,40,1,0,0,100,40,14,1,false,Trailing,Leading,Character,claBlack,0,0,41,19,0.99,8PDw+Pj//////PD5+//////9/Pn7////////////////oP+A//////////////////////////8')]
    [TestCase('6.Short vertical center', ShortText + ',100,40,1,0,0,100,40,14,1,false,Leading,Center,Character,claBlack,0,0,41,19,0.99,Hx8fHx8fHx9/f39/X19fX39/f3/fX19f////////////////U/8D/wP/A/8D//////////////8')]
    [TestCase('7.Short horizontal center, vertical center', ShortText + ',100,40,1,0,0,100,40,14,1,false,Center,Center,Character,claBlack,0,0,41,19,0.99,5+fnw8PDw8f///fjw8fPz///9/vT78/P////////////////+p/4H/gf+B/4H/////////////8')]
    [TestCase('8.Short horizontal trailing, vertical center', ShortText + ',100,40,1,0,0,100,40,14,1,false,Trailing,Center,Character,claBlack,0,0,41,19,0.99,/Pz4+PDw+Pj//Pj58/f+/P/8+P3///78/////////////////7T/ov+g/4D/gv////////////8')]
    [TestCase('9.Short vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,1,false,Leading,Trailing,Character,claBlack,0,0,41,19,0.99,////Px8PDw////9/X09PT////39fb89P//////////////////////////9T/wP/A/8D/wP///8')]
    [TestCase('10.Short horizontal center, vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,1,false,Center,Trailing,Character,claBlack,0,0,41,19,////58PDw8P////nw8fPz////+fD39/v///////////////////////////6n/gf+B/4H/gf//8')]
    [TestCase('11.Short horizontal trailing, vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,1,false,Trailing,Trailing,Character,claBlack,0,0,41,19,0.99,////+Pjw8PD////5+/f+/P////n7//7+////////////////////////////tP+i/6D/gP+C//8')]
    [TestCase('12.Long', LongText + ',100,40,1,0,0,65535,65535,14,0,false,Leading,Leading,Character,claBlack,0,0,6643,19,0.99,AAAAAP///////PDg///////9+vD///////////////8CAAIA/v////////////////////////8')]
    [TestCase('13.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,0,false,Leading,Leading,Character,claBlack,0,0,6643,1558,0.99,/wMDE3sBITv///Pz+8ft////+/P73+3/////////7/8CBwIH/v///////v+M+wSDhIMEkP////8')]
    [TestCase('14.Long horizontal center', LongText + ',100,40,1,0,0,100,40,14,0,false,Center,Leading,Character,claBlack,0,0,6643,1558,0.99,/4GB0/+Bgfv//fHz/8fN/////fv/19////////////+QFYAF/7//////33/GXYJBgkuCSf////8')]
    [TestCase('15.Long horizontal trailing', LongText + ',100,40,1,0,0,100,40,14,0,false,Trailing,Leading,Character,claBlack,0,0,6643,1558,0.99,/8DAyPyAkNb//PDp/8fc3v/99O3/z93e///////f/d7gAOAA/9///////7/jLsAg4SDBCP////8')]
    [TestCase('16.Long vertical center', LongText + ',100,40,1,0,0,100,40,14,0,false,Leading,Center,Character,claBlack,0,0,6643,1558,0.99,zwMDD48DAwP/f3Nvz0dPT///+2/PV19f/////9/fX98InwgvIA/7//////99/wAPAAcAB/7///8')]
    [TestCase('17.Long horizontal center, vertical center', LongText + ',100,40,1,0,0,100,40,14,0,false,Center,Center,Character,claBlack,0,0,6643,1558,0.99,w4GBw8eBgcP//fHjx8fPz///+/fH79/P//////fv3+/Cp8Ir6CP+///////ff8ADwAPAA/////8')]
    [TestCase('18.Long horizontal trailing, vertical center', LongText + ',100,40,1,0,0,100,40,14,0,false,Trailing,Center,Character,claBlack,0,0,6643,1558,0.99,9sDA4OLAwMD//PDh48fOzP/9+Pnj187M/////////8zwCPAI8gj////////v3+BA4ADgAP/f//8')]
    [TestCase('19.Long vertical trailing', LongText + ',100,40,1,0,0,100,40,14,0,false,Leading,Trailing,Character,claBlack,0,0,6643,1558,0.99,9wcDBwcDAwP/f3NnR0dPT/9/+/dHd2////////////8CHwAfAh/Pf//////7/wpPCQ8JD0oP//8')]
    [TestCase('20.Long horizontal center, vertical trailing', LongText + ',100,40,1,0,0,100,40,14,0,false,Center,Trailing,Character,claBlack,0,0,6643,1558,0.99,34GBgcOBgYH//fHhw8fPzf//9eXD3//9///////////CA8IDwAP7z///////f8KDwkPSA9KL//8')]
    [TestCase('21.Long horizontal trailing, vertical trailing', LongText + ',100,40,1,0,0,100,40,14,0,false,Trailing,Trailing,Character,claBlack,0,0,6643,1558,0.99,7uDA4ODAwMD//PDh48fOzP/98fPj797c//////////74EPgA+BD+e///////n/Cg8ADyAPSi//8')]
    [TestCase('22.Short low height horizontal leading', ShortText + ',100,13,1,0,2,100,9,14,1,false,Leading,Leading,None,claBlack,0,2,41,21,0.99,Hx8fHx8fHx9/f39/X19fX39/f3/f/19f//////////////////9T/1P/U/8D//////////////8')]
    [TestCase('23.Short low height horizontal center', ShortText + ',100,13,1,0,2,100,9,14,1,false,Center,Leading,None,claBlack,0,2,41,21,0.99,w8PDw8PDw8P///Pjw8fPz///8+Pb3//P///////////////////6n/qf+p/4H/////////////8')]
    [TestCase('24.Short low height horizontal trailing', ShortText + ',100,13,1,0,2,100,9,14,1,false,Trailing,Leading,None,claBlack,0,2,41,21,0.99,+Pj4+Pj4+Pj//Pj5+//+/P/8+Pn7//78////////////////////pP+k/6T/oP////////////8')]
    [TestCase('25.Short low height horizontal leading vertical center', ShortText + ',100,13,1,0,2,100,9,12,1,false,Leading,Center,None,claBlack,0,2,35,18,0.99,Hx8fHx8fHx//////39/f3//////f39/f////////3/9T/1P/A/8D/wv/A/8D//////////////8')]
    [TestCase('26.Short low height horizontal center vertical center', ShortText + ',100,13,1,0,2,100,9,12,1,false,Center,Center,None,claBlack,0,2,35,18,0.99,w8PDw8PDw8P///Pjw8fPz///+/vz5+/f/////////9/6n/qf+F/4H/kf+B/4H/////////////8')]
    [TestCase('27.Short low height horizontal trailing vertical center', ShortText + ',100,13,1,0,2,100,9,12,1,false,Trailing,Center,None,claBlack,0,2,35,18,0.99,+Pj4+Pj4+Pj//Pj5+//+/P/8//////7+//////////7/1P/U/8D/0P/A/8D/wP////////////8')]
    [TestCase('28.Short low size horizontal leading vertical center', ShortText + ',40,13,1,5,2,30,9,12,1,false,Leading,Center,None,claBlack,5,2,40,18,0.99,///DwYHDw8P///Phw8PPz///8/fz6//P///////////e89730kfcB9iH1LfUF/////////////8')]
    [TestCase('29.Short low size horizontal center vertical center', ShortText + ',40,13,1,5,2,30,9,12,1,false,Center,Center,None,claBlack,5,2,40,18,0.99,///hwcHBwcH///Hhw8fNzf//8/f77+/P////////7//O887zyAPMg8iDyJPIk/////////////8')]
    [TestCase('30.Short low size horizontal trailing vertical center', ShortText + ',40,13,1,5,2,30,9,12,1,false,Trailing,Center,None,claBlack,5,2,40,18,0.99,///BwcHBwcH///Hhw8fFzf//8/fz7+fP////////7//ve+976SPsE+gD6BvoG/////////////8')]
    [TestCase('31.Short', ShortText + ',100,40,1,0,0,65535,65535,14,1,true,Trailing,Leading,Character,claBlack,0,0,41,19,0.99,Dw8PHz////////////////////////////////////8D/wP///////////////////////////8')]
    [TestCase('32.Short horizontal leading', ShortText + ',100,40,1,0,0,100,40,14,1,true,Trailing,Leading,Character,claBlack,0,0,41,19,0.99,Dw8PHz////////////////////////////////////8D/wP///////////////////////////8')]
    [TestCase('33.Short horizontal center', ShortText + ',100,40,1,0,0,100,40,14,1,true,Center,Leading,Character,claBlack,0,0,41,19,0.99,w8PDw+f///////Pj5///////8+Pn///////////////4H/gf//////////////////////////8')]
    [TestCase('34.Short horizontal trailing', ShortText + ',100,40,1,0,0,100,40,14,1,true,Leading,Leading,Character,claBlack,0,0,41,19,0.99,8PDw+Pj//////PD5+//////9/Pn7////////////////oP+A//////////////////////////8')]
    [TestCase('35.Short vertical center', ShortText + ',100,40,1,0,0,100,40,14,1,true,Trailing,Center,Character,claBlack,0,0,41,19,0.99,Hx8fHx8fHx9/f39/X19fX39/f3/fX19f////////////////U/8D/wP/A/8D//////////////8')]
    [TestCase('36.Short horizontal center, vertical center', ShortText + ',100,40,1,0,0,100,40,14,1,true,Center,Center,Character,claBlack,0,0,41,19,0.99,5+fnw8PDw8f///fjw8fPz///9/vT78/P////////////////+p/4H/gf+B/4H/////////////8')]
    [TestCase('37.Short horizontal trailing, vertical center', ShortText + ',100,40,1,0,0,100,40,14,1,true,Leading,Center,Character,claBlack,0,0,41,19,0.99,/Pz4+PDw+Pj//Pj58/f+/P/8+P3///78/////////////////7T/ov+g/4D/gv////////////8')]
    [TestCase('38.Short vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,1,true,Trailing,Trailing,Character,claBlack,0,0,41,19,0.99,////Px8PDw////9/X09PT////39fb89P//////////////////////////9T/wP/A/8D/wP///8')]
    [TestCase('39.Short horizontal center, vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,1,true,Center,Trailing,Character,claBlack,0,0,41,19,////58PDw8P////nw8fPz////+fD39/v///////////////////////////6n/gf+B/4H/gf//8')]
    [TestCase('40.Short horizontal trailing, vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,1,true,Leading,Trailing,Character,claBlack,0,0,41,19,0.99,////+Pjw8PD////5+/f+/P////n7//7+////////////////////////////tP+i/6D/gP+C//8')]
    [TestCase('41.Long', LongText + ',100,40,1,0,0,65535,65535,14,0,true,Trailing,Leading,Character,claBlack,0,0,6643,19,0.99,AAAAAP//////+PDg///////9+fL///////////////8AAAAA/3////////////////////////8')]
    [TestCase('42.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,0,true,Trailing,Leading,Character,claBlack,0,0,6643,1558,0.99,/wMDE3sBITv///Pz+8ft////+/P73+3/////////7/8CBwIH/v///////v+M+wSDhIMEkP////8')]
    [TestCase('43.Long horizontal center', LongText + ',100,40,1,0,0,100,40,14,0,true,Center,Leading,Character,claBlack,0,0,6643,1558,0.99,/4GB0/+Bgfv//fHz/8fN/////fv/19////////////+QFYAF/7//////33/GXYJBgkuCSf////8')]
    [TestCase('44.Long horizontal trailing', LongText + ',100,40,1,0,0,100,40,14,0,true,Leading,Leading,Character,claBlack,0,0,6643,1558,0.99,/8DAyPyAkNb//PDp/8fc3v/99O3/z93e///////f/d7gAOAA/9///////7/jLsAg4SDBCP////8')]
    [TestCase('45.Long vertical center', LongText + ',100,40,1,0,0,100,40,14,0,true,Trailing,Center,Character,claBlack,0,0,6643,1558,0.99,zwMDD48DAwP/f3Nvz0dPT///+2/PV19f/////9/fX98InwgvIA/7//////99/wAPAAcAB/7///8')]
    [TestCase('46.Long horizontal center, vertical center', LongText + ',100,40,1,0,0,100,40,14,0,true,Center,Center,Character,claBlack,0,0,6643,1558,0.99,w4GBw8eBgcP//fHjx8fPz///+/fH79/P//////fv3+/Cp8Ir6CP+///////ff8ADwAPAA/////8')]
    [TestCase('47.Long horizontal trailing, vertical center', LongText + ',100,40,1,0,0,100,40,14,0,true,Leading,Center,Character,claBlack,0,0,6643,1558,0.99,9sDA4OLAwMD//PDh48fOzP/9+Pnj187M/////////8zwCPAI8gj////////v3+BA4ADgAP/f//8')]
    [TestCase('48.Long vertical trailing', LongText + ',100,40,1,0,0,100,40,14,0,true,Trailing,Trailing,Character,claBlack,0,0,6643,1558,0.99,9wcDBwcDAwP/f3NnR0dPT/9/+/dX929///////////8CHwAfAh/Pf//////9/whPgA8ADyAv//8')]
    [TestCase('49.Long horizontal center, vertical trailing', LongText + ',100,40,1,0,0,100,40,14,0,true,Center,Trailing,Character,claBlack,0,0,6643,1558,0.99,34GBgcGBgYH//fHhw8fPzf//9eXD3//d///////////CA8IDwAP7z///////f8ID4CPAA8AL//8')]
    [TestCase('50.Long horizontal trailing, vertical trailing', LongText + ',100,40,1,0,0,100,40,14,0,true,Leading,Trailing,Character,claBlack,0,0,6643,1558,0.99,7uDA4ODgwMD//PDh4+fOzP/98fPj79/O//////////74EPgA+BD+e///////3/CA+gDwAPCC//8')]
    [TestCase('51.Short low height horizontal leading', ShortText + ',100,13,1,0,2,100,9,14,1,true,Trailing,Leading,None,claBlack,0,2,41,21,0.99,Hx8fHx8fHx9/f39/X19fX39/f3/f/19f//////////////////9T/1P/U/8D//////////////8')]
    [TestCase('52.Short low height horizontal center', ShortText + ',100,13,1,0,2,100,9,14,1,true,Center,Leading,None,claBlack,0,2,41,21,0.99,w8PDw8PDw8P///Pjw8fPz///8+Pb3//P///////////////////6n/qf+p/4H/////////////8')]
    [TestCase('53.Short low height horizontal trailing', ShortText + ',100,13,1,0,2,100,9,14,1,true,Leading,Leading,None,claBlack,0,2,41,21,0.99,+Pj4+Pj4+Pj//Pj5+//+/P/8+Pn7//78////////////////////pP+k/6T/oP////////////8')]
    [TestCase('54.Short low height horizontal leading vertical center', ShortText + ',100,13,1,0,2,100,9,12,1,true,Trailing,Center,None,claBlack,0,2,35,18,0.99,Hx8fHx8fHx//////39/f3//////f39/f////////3/9T/1P/A/8D/wv/A/8D//////////////8')]
    [TestCase('55.Short low height horizontal center vertical center', ShortText + ',100,13,1,0,2,100,9,12,1,true,Center,Center,None,claBlack,0,2,35,18,0.99,w8PDw8PDw8P///Pjw8fPz///+/vz5+/f/////////9/6n/qf+F/4H/kf+B/4H/////////////8')]
    [TestCase('56.Short low height horizontal trailing vertical center', ShortText + ',100,13,1,0,2,100,9,12,1,true,Leading,Center,None,claBlack,0,2,35,18,0.99,+Pj4+Pj4+Pj//Pj5+//+/P/8//////7+//////////7/1P/U/8D/0P/A/8D/wP////////////8')]
    [TestCase('57.Short low size horizontal leading vertical center', ShortText + ',40,13,1,5,2,30,9,12,1,true,Trailing,Center,None,claBlack,5,2,40,18,0.99,///DwYHDw8P///Phw8PPz///8/fz6//P///////////e89730kfcB9iH1LfUF/////////////8')]
    [TestCase('58.Short low size horizontal center vertical center', ShortText + ',40,13,1,5,2,30,9,12,1,true,Center,Center,None,claBlack,5,2,40,18,0.99,///hwcHBwcH///Hhw8fNzf//8/f77+/P////////7//O887zyAPMg8iDyJPIk/////////////8')]
    [TestCase('59.Short low size horizontal trailing vertical center', ShortText + ',40,13,1,5,2,30,9,12,1,true,Leading,Center,None,claBlack,5,2,40,18,0.99,///BwcHBwcH///Hhw8fFzf//8/fz7+fP////////7//ve+976SPsE+gD6BvoG/////////////8')]
    {$ENDIF}
    procedure GenericTest(const AText: string; const AControlWidth, AControlHeight: Integer; const AScale, ATextLeft, ATextTop, AMaxWidth, AMaxHeight, AFontSize: Single; const AMaxLines: Integer; const ARightToLeft: Boolean; const AHorizontalAlign: TSkTextHorzAlign; const AVerticalAlign: TSkTextVertAlign; const ATrimming: TSkTextTrimming; const AColor: string; const AExpectedTextLeft, AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    procedure Setup; override;
  end;

implementation

uses
  { Delphi }
  System.Classes,
  System.UIConsts,
  System.Math,
  System.Math.Vectors;

type
  TSkLabelEx = class(TSkLabel)
  strict private
    FRightToLeft: Boolean;
  public
    constructor Create(AOwner: TComponent; ARightToLeft: Boolean); reintroduce;
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single); override;
    function UseRightToLeftAlignment: Boolean; override;
    property ParagraphBounds;
  end;

{ TSkLabelTests }

function TSkLabelTests.AssetsPath: string;
begin
  Result := CombinePaths(inherited AssetsPath, 'TextLayout');
end;

procedure TSkLabelTests.GenericTest(const AText: string; const AControlWidth,
  AControlHeight: Integer; const AScale, ATextLeft, ATextTop, AMaxWidth, AMaxHeight,
  AFontSize: Single; const AMaxLines: Integer; const ARightToLeft: Boolean;
  const AHorizontalAlign: TSkTextHorzAlign; const AVerticalAlign: TSkTextVertAlign;
  const ATrimming: TSkTextTrimming; const AColor: string; const AExpectedTextLeft,
  AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  Test(AText, TSize.Create(AControlWidth, AControlHeight), AScale, AFontSize,
    PointF(ATextLeft, ATextTop), PointF(AMaxWidth, AMaxHeight), AMaxLines,
    ARightToLeft, AHorizontalAlign, AVerticalAlign, ATrimming, StringToAlphaColor(AColor),
    RectF(AExpectedTextLeft, AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom),
    True, AMinSimilarity, AExpectedImageHash);
end;

procedure TSkLabelTests.Setup;
begin
  inherited;
  RegisterFontFiles(TSkTypefaceManager.Provider);
end;

procedure TSkLabelTests.Test(const AText: string; const ABitmapSize: TSize;
  const AScale, AFontSize: Single; const ATextTopLeft, AMaxSize: TPointF;
  const AMaxLines: Integer; const ARightToLeft: Boolean;
  const AHorizontalAlign: TSkTextHorzAlign;
  const AVerticalAlign: TSkTextVertAlign; const ATrimming: TSkTextTrimming;
  const AColor: TAlphaColor; const AExpectedTextRect: TRectF;
  const ACheckTextRect: Boolean; const AMinSimilarity: Double;
  const AExpectedImageHash: string);

  function RectToString(const R: TRectF): string;
  begin
    Result := '(' + FloatToStr(R.Left, TFormatSettings.Invariant) + ',' + FloatToStr(R.Top, TFormatSettings.Invariant) + ',' +
      FloatToStr(R.Right, TFormatSettings.Invariant) + ',' + FloatToStr(R.Bottom, TFormatSettings.Invariant) + ')';
  end;

const
  TextRectEpsilon = TEpsilon.Position;
var
  LSurface: ISkSurface;
  LLabel: TSkLabel;
  LTextRect: TRectF;
begin
  Assert.AreSameValue(AScale, 1, 0, 'Scale not implemented yet');
  LSurface := TSkSurface.MakeRaster(ABitmapSize.Width, ABitmapSize.Height);
  LSurface.Canvas.Clear(TAlphaColors.Null);

  LLabel := TSkLabelEx.Create(nil, ARightToLeft);
  try
    LLabel.AutoSize := False;
    LLabel.SetBounds(Round(ATextTopLeft.X), Round(ATextTopLeft.Y), Round(AMaxSize.X), Round(AMaxSize.Y));
    LLabel.TextSettings.BeginUpdate;
    try
      LLabel.TextSettings.Font.Families := DefaultFontFamily;
      LLabel.TextSettings.MaxLines := AMaxLines;
      LLabel.TextSettings.HorzAlign := AHorizontalAlign;
      LLabel.TextSettings.VertAlign := AVerticalAlign;
      LLabel.TextSettings.Font.Size := AFontSize;
      LLabel.TextSettings.FontColor := AColor;
      LLabel.TextSettings.Trimming := ATrimming;
    finally
      LLabel.TextSettings.EndUpdate;
    end;
    LLabel.Caption := AText;
    if ACheckTextRect then
    begin
      LTextRect := TSkLabelEx(LLabel).ParagraphBounds;
      LTextRect.Offset(ATextTopLeft);
      Assert.AreSameValue(AExpectedTextRect.Left, LTextRect.Left, TextRectEpsilon, Format('in TextRect. Rect obtained %s', [RectToString(LTextRect)]));
      Assert.AreSameValue(AExpectedTextRect.Top, LTextRect.Top, TextRectEpsilon, Format('in TextRect. Rect obtained %s', [RectToString(LTextRect)]));
      Assert.AreSameValue(AExpectedTextRect.Right, LTextRect.Right, TextRectEpsilon, Format('in TextRect. Rect obtained %s', [RectToString(LTextRect)]));
      Assert.AreSameValue(AExpectedTextRect.Bottom, LTextRect.Bottom, TextRectEpsilon, Format('in TextRect. Rect obtained %s', [RectToString(LTextRect)]));
    end;
    TSkLabelEx(LLabel).Draw(LSurface.Canvas, TRectF.Create(LLabel.BoundsRect), LLabel.Opacity / 255);
  finally
    LLabel.Free;
  end;
  Assert.AreSimilar(AExpectedImageHash, LSurface.MakeImageSnapshot, AMinSimilarity);
end;

{ TSkLabelEx }

constructor TSkLabelEx.Create(AOwner: TComponent; ARightToLeft: Boolean);
begin
  FRightToLeft := ARightToLeft;
  inherited Create(AOwner);
end;

procedure TSkLabelEx.Draw(const ACanvas: ISkCanvas; const ADest: TRectF;
  const AOpacity: Single);
begin
  inherited;
end;

function TSkLabelEx.UseRightToLeftAlignment: Boolean;
begin
  Result := FRightToLeft;
end;

initialization
  TDUnitX.RegisterTestFixture(TSkLabelTests);
end.
