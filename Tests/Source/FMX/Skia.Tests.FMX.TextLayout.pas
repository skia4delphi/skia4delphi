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
unit Skia.Tests.FMX.TextLayout;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  System.Types,
  System.UITypes,
  DUnitX.TestFramework,
  FMX.Types,

  { Skia }
  Skia,
  Skia.FMX,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkTextLayoutTests }

  [TestFixture]
  TSkTextLayoutTests = class(TTestBase)
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
    procedure Test(const AText: string; const ABitmapSize: TSize; const AScale, AFontSize: Single; const ATextTopLeft, AMaxSize: TPointF; const AWordWrap, ARightToLeft: Boolean; const AHorizontalAlign, AVerticalAlign: TTextAlign; const ATrimming: TTextTrimming; const AColor: TAlphaColor; const AExpectedTextRect: TRectF; const ACheckTextRect: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
  protected
    function AssetsPath: string; override;
  public
    [Test]
    procedure CheckRegionForRangeWithoutLength;
    {$IFDEF MSWINDOWS}
    [TestCase('1.Short', ShortText + ',100,40,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,0.99,Dw8PHz////////////////////////////////////8D/wP///////////////////////////8')]
    [TestCase('2.Short blue', ShortText + ',100,40,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,#FF9F9FFF,0,0.38,40.2,19,0.99,Dw8PHx////9/f39/X////39/f39f////f39/f//////8APwAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('3.Short horizontal leading', ShortText + ',100,40,1,0,0,100,40,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,0.99,Dw8PHz////////////////////////////////////8D/wP///////////////////////////8')]
    [TestCase('4.Short horizontal center', ShortText + ',100,40,1,0,0,100,40,14,false,false,Center,Leading,Character,claBlack,29.899,0.38,70.1,19,0.99,w8PDw+f///////Pj5///////8+Pn///////////////4H/gf//////////////////////////8')]
    [TestCase('5.Short horizontal trailing', ShortText + ',100,40,1,0,0,100,40,14,false,false,Trailing,Leading,Character,claBlack,59.8,0.38,100,19,0.99,8PDw+Pj//////PD5+//////9/Pn7////////////////oP+A//////////////////////////8')]
    [TestCase('6.Short vertical center', ShortText + ',100,40,1,0,0,100,40,14,false,false,Leading,Center,Character,claBlack,0,11.07,40.2,29.69,0.99,Hx8fHx8fHx9/f39/X19fX39/f3/fX19f////////////////U/8D/wP/A/8D//////////////8')]
    [TestCase('7.Short horizontal center, vertical center', ShortText + ',100,40,1,0,0,100,40,14,false,false,Center,Center,Character,claBlack,29.9,11.07,70.1,29.69,0.99,5+fnw8PDw8f///fjw8fPz///9/vT78/P////////////////+p/4H/gf+B/4H/////////////8')]
    [TestCase('8.Short horizontal trailing, vertical center', ShortText + ',100,40,1,0,0,100,40,14,false,false,Trailing,Center,Character,claBlack,59.8,11.07,100,29.69,0.99,/Pz4+PDw+Pj//Pj58/f+/P/8+P3///78/////////////////7T/ov+g/4D/gv////////////8')]
    [TestCase('9.Short vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,false,false,Leading,Trailing,Character,claBlack,0,21.76,40.2,40.38,0.99,////Px8PDw////9/X09PT////39fb89P//////////////////////////9T/wP/A/8D/wP///8')]
    [TestCase('10.Short horizontal center, vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,false,false,Center,Trailing,Character,claBlack,29.9,21.76,70.1,40.38,0.99,////58PDw8P////nw8fPz////+fD39/v///////////////////////////6n/gf+B/4H/gf//8')]
    [TestCase('11.Short horizontal trailing, vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,false,false,Trailing,Trailing,Character,claBlack,59.8,21.76,100,40.38,0.99,////+Pjw8PD////5+/f+/P////n7//7+////////////////////////////tP+i/6D/gP+C//8')]
    [TestCase('12.Long', LongText + ',100,40,1,0,0,65535,65535,14,true,false,Leading,Leading,Character,claBlack,0,0.38,6642.3398,19,0.99,AAAAAP///////PDg///////9+vD///////////////8CAAIA/v////////////////////////8')]
    [TestCase('13.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,false,Leading,Leading,Character,claBlack,0,0.38,89.74,38,0.99,/wMDE3sBITv///Pz+8ft////+/P73+3/////////7/8CBwIH/v///////v+M+wSDhIMEkP////8')]
    [TestCase('14.Long horizontal center', LongText + ',100,40,1,0,0,100,40,14,true,false,Center,Leading,Character,claBlack,0.58,0.38,94.04,38,0.99,/4GB0/sBITv//fHz+0dtf////fv7X21///////9/7X+QFYAF/7///////v+cuwSDBIMEgP////8')]
    [TestCase('15.Long horizontal trailing', LongText + ',100,40,1,0,0,100,40,14,true,false,Trailing,Leading,Character,claBlack,3.83,0.38,103.83,38,0.99,/8CAyfsBCTv//PDp+0dNf//99O37V19////////f/3/gAOAA/9///////v+smwCDBIMEoP////8')]
    [TestCase('16.Long vertical center', LongText + ',100,40,1,0,0,100,40,14,true,false,Leading,Center,Character,claBlack,0,1.57,89.74,39.19,0.99,/wMBA38BISP///Hj/8fv7///9fv/3+//////////7/8CBwIHAg////////+e+wSDhIMEkP////8')]
    [TestCase('17.Long horizontal center, vertical center', LongText + ',100,40,1,0,0,100,40,14,true,false,Center,Center,Character,claBlack,0.58,1.57,94.04,39.19,0.99,/4GB0/8BITv//fHz/0dtf/////v/X21/////////7f+AhYAFyYX///////+e/wSDhIMEgP////8')]
    [TestCase('18.Long horizontal trailing, vertical center', LongText + ',100,40,1,0,0,100,40,14,true,false,Trailing,Center,Character,claBlack,3.83,1.57,103.83,39.19,0.99,/8CAyfsBKTv//PDp+0dtf//99e37139////////f/3/gAOAA4EL/3/////+u/wCDBIMEgP////8')]
    [TestCase('19.Long vertical trailing', LongText + ',100,40,1,0,0,100,40,14,true,false,Leading,Trailing,Character,claBlack,0,2.76,89.74,40.38,0.99,/5MDAyMBAQH//3NjY0dPTf///3tjX2/P////////f/8CBwKHAgf+//////++/wiDBJMEggyS//8')]
    [TestCase('20.Long horizontal center, vertical trailing', LongText + ',100,40,1,0,0,100,40,14,true,false,Center,Trailing,Character,claBlack,0.58,2.76,94.04,40.38,0.99,/8GBgbsBAQH//fHh+0dPTf/9/+n711/P//////////+gAZAFiIX/v//////+/wCDBIcEggSC//8')]
    [TestCase('21.Long horizontal trailing, vertical trailing', LongText + ',100,40,1,0,0,100,40,14,true,false,Trailing,Trailing,Character,claBlack,3.83,2.76,103.83,40.38,0.99,/8DAyMkJCQH//PDpy09NTf/89e3L313P///////////gQOAA4AD/3//////+/wCDBIcEgkUi//8')]
    [TestCase('22.Short low height horizontal leading', ShortText + ',100,13,1,0,2,100,9,14,false,false,Leading,Leading,None,claBlack,0,2.38,40.2,11,0.99,Hx8fHx8fHx9/f39/X19fX39/f3/f/19f//////////////////9T/1P/U/8D//////////////8')]
    [TestCase('23.Short low height horizontal center', ShortText + ',100,13,1,0,2,100,9,14,false,false,Center,Leading,None,claBlack,29.9,2.38,70.1,11,0.99,w8PDw8PDw8P///Pjw8fPz///8+Pb3//P///////////////////6n/qf+p/4H/////////////8')]
    [TestCase('24.Short low height horizontal trailing', ShortText + ',100,13,1,0,2,100,9,14,false,false,Trailing,Leading,None,claBlack,59.8,2.38,100,11,0.99,+Pj4+Pj4+Pj//Pj5+//+/P/8+Pn7//78////////////////////pP+k/6T/oP////////////8')]
    [TestCase('25.Short low height horizontal leading vertical center', ShortText + ',100,13,1,0,2,100,9,12,false,false,Leading,Center,None,claBlack,0,2.04,34.46,11.04,0.99,Hx8fHx8fHx//////39/f3//////f39/f////////3/9T/1P/A/8D/wv/A/8D//////////////8')]
    [TestCase('26.Short low height horizontal center vertical center', ShortText + ',100,13,1,0,2,100,9,12,false,false,Center,Center,None,claBlack,32.77,2.04,67.23,11.04,0.99,w8PDw8PDw8P///Pjw8fPz///+/vz5+/f/////////9/6n/qf+F/4H/kf+B/4H/////////////8')]
    [TestCase('27.Short low height horizontal trailing vertical center', ShortText + ',100,13,1,0,2,100,9,12,false,false,Trailing,Center,None,claBlack,65.54,2.04,100,11.04,0.99,+Pj4+Pj4+Pj//Pj5+//+/P/8//////7+//////////7/1P/U/8D/0P/A/8D/wP////////////8')]
    [TestCase('28.Short low size horizontal leading vertical center', ShortText + ',40,13,1,5,2,30,9,12,false,false,Leading,Center,None,claBlack,5,2.04,35,11.04,0.99,///DwYHDw8P///Phw8PPz///8/fz6//P///////////e89730kfcB9iH1LfUF/////////////8')]
    [TestCase('29.Short low size horizontal center vertical center', ShortText + ',40,13,1,5,2,30,9,12,false,false,Center,Center,None,claBlack,5,2.04,35,11.04,0.99,/f3BwcHBwcH//fHhw8fPzf/99+/r38/N/////////+/96/3r8Iv5C/EL6SvoK/////////////8')]
    [TestCase('30.Short low size horizontal trailing vertical center', ShortText + ',40,13,1,5,2,30,9,12,false,false,Trailing,Center,None,claBlack,5,2.04,35,11.04,0.99,///DgYGDg4P///Phw8fPz////+vj1+/v///////////71/vX4RfyF+IXwlfSV/////////////8')]
    [TestCase('31.Short', ShortText + ',100,40,1,0,0,65535,65535,14,false,true,Trailing,Leading,Character,claBlack,0,0.38,40.2,19,0.99,Dw8PHz////////////////////////////////////8D/wP///////////////////////////8')]
    [TestCase('32.Short horizontal leading', ShortText + ',100,40,1,0,0,100,40,14,false,true,Trailing,Leading,Character,claBlack,0,0.38,40.2,19,0.99,Dw8PHz////////////////////////////////////8D/wP///////////////////////////8')]
    [TestCase('33.Short horizontal center', ShortText + ',100,40,1,0,0,100,40,14,false,true,Center,Leading,Character,claBlack,29.9,0.38,70.1,19,0.99,w8PDw+f///////Pj5///////8+Pn///////////////4H/gf//////////////////////////8')]
    [TestCase('34.Short horizontal trailing', ShortText + ',100,40,1,0,0,100,40,14,false,true,Leading,Leading,Character,claBlack,59.8,0.38,100,19,0.99,8PDw+Pj//////PD5+//////9/Pn7////////////////oP+A//////////////////////////8')]
    [TestCase('35.Short vertical center', ShortText + ',100,40,1,0,0,100,40,14,false,true,Trailing,Center,Character,claBlack,0,11.07,40.2,29.69,0.99,Hx8fHx8fHx9/f39/X19fX39/f3/fX19f////////////////U/8D/wP/A/8D//////////////8')]
    [TestCase('36.Short horizontal center, vertical center', ShortText + ',100,40,1,0,0,100,40,14,false,true,Center,Center,Character,claBlack,29.9,11.07,70.1,29.69,0.99,5+fnw8PDw8f///fjw8fPz///9/vT78/P////////////////+p/4H/gf+B/4H/////////////8')]
    [TestCase('37.Short horizontal trailing, vertical center', ShortText + ',100,40,1,0,0,100,40,14,false,true,Leading,Center,Character,claBlack,59.8,11.07,100,29.69,0.99,/Pz4+PDw+Pj//Pj58/f+/P/8+P3///78/////////////////7T/ov+g/4D/gv////////////8')]
    [TestCase('38.Short vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,false,true,Trailing,Trailing,Character,claBlack,0,21.76,40.2,40.38,0.99,////Px8PDw////9/X09PT////39fb89P//////////////////////////9T/wP/A/8D/wP///8')]
    [TestCase('39.Short horizontal center, vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,false,true,Center,Trailing,Character,claBlack,29.9,21.76,70.1,40.38,0.99,////58PDw8P////nw8fPz////+fD39/v///////////////////////////6n/gf+B/4H/gf//8')]
    [TestCase('40.Short horizontal trailing, vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,false,true,Leading,Trailing,Character,claBlack,59.8,21.76,100,40.38,0.99,////+Pjw8PD////5+/f+/P////n7//7+////////////////////////////tP+i/6D/gP+C//8')]
    [TestCase('41.Long', LongText + ',100,40,1,0,0,65535,65535,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,6642.34,19,0.99,AAAAAP//////+PDg///////9+fL///////////////8AAAAA/3////////////////////////8')]
    [TestCase('42.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,89.74,38,0.99,/wMDE3sBITv///Pz+8ft////+/P73+3/////////7/8CBwIH/v///////v+M+wSDhIMEkP////8')]
    [TestCase('43.Long horizontal center', LongText + ',100,40,1,0,0,100,40,14,true,true,Center,Leading,Character,claBlack,0.58,0.38,94.04,38,0.99,/4GB0/sBITv//fHz+0dtf////fv7X21///////9/7X+QFYAF/7///////v+cuwSDBIMEgP////8')]
    [TestCase('44.Long horizontal trailing', LongText + ',100,40,1,0,0,100,40,14,true,true,Leading,Leading,Character,claBlack,3.83,0.38,103.83,38,0.99,/8CAyfsBCTv//PDp+0dNf//99O37V19////////f/3/gAOAA/9///////v+smwCDBIMEoP////8')]
    [TestCase('45.Long vertical center', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Center,Character,claBlack,0,1.57,89.74,39.19,0.99,/wMBA38BISP///Hj/8fv7///9fv/3+//////////7/8CBwIHAg////////+e+wSDhIMEkP////8')]
    [TestCase('46.Long horizontal center, vertical center', LongText + ',100,40,1,0,0,100,40,14,true,true,Center,Center,Character,claBlack,0.58,1.57,94.04,39.19,0.99,/4GB0/8BITv//fHz/0dtf/////v/X21/////////7f+AhYAFyYX///////+e/wSDhIMEgP////8')]
    [TestCase('47.Long horizontal trailing, vertical center', LongText + ',100,40,1,0,0,100,40,14,true,true,Leading,Center,Character,claBlack,3.83,1.57,103.83,39.19,0.99,/8CAyfsBKTv//PDp+0dtf//99e37139////////f/3/gAOAA4EL/3/////+u/wCDBIMEgP////8')]
    [TestCase('48.Long vertical trailing', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Trailing,Character,claBlack,0,2.76,89.74,40.38,0.99,/5MDAyMBAQH//3NjY0dPTf///3tjX2/P////////f/8CBwKHAgf+//////++/wiDBJMEggyS//8')]
    [TestCase('49.Long horizontal center, vertical trailing', LongText + ',100,40,1,0,0,100,40,14,true,true,Center,Trailing,Character,claBlack,0.58,2.76,94.04,40.38,0.99,/8GBgbsBAQH//fHh+0dPTf/9/+n711/P//////////+gAZAFiIX/v//////+/wCDBIcEggSC//8')]
    [TestCase('50.Long horizontal trailing, vertical trailing', LongText + ',100,40,1,0,0,100,40,14,true,true,Leading,Trailing,Character,claBlack,3.83,2.76,103.83,40.38,0.99,/8DAyMkJCQH//PDpy09NTf/89e3L313P///////////gQOAA4AD/3//////+/wCDBIcEgkUi//8')]
    [TestCase('51.Short low height horizontal leading', ShortText + ',100,13,1,0,2,100,9,14,false,true,Trailing,Leading,None,claBlack,0,2.38,40.2,11,0.99,Hx8fHx8fHx9/f39/X19fX39/f3/f/19f//////////////////9T/1P/U/8D//////////////8')]
    [TestCase('52.Short low height horizontal center', ShortText + ',100,13,1,0,2,100,9,14,false,true,Center,Leading,None,claBlack,29.9,2.38,70.1,11,0.99,w8PDw8PDw8P///Pjw8fPz///8+Pb3//P///////////////////6n/qf+p/4H/////////////8')]
    [TestCase('53.Short low height horizontal trailing', ShortText + ',100,13,1,0,2,100,9,14,false,true,Leading,Leading,None,claBlack,59.8,2.38,100,11,0.99,+Pj4+Pj4+Pj//Pj5+//+/P/8+Pn7//78////////////////////pP+k/6T/oP////////////8')]
    [TestCase('54.Short low height horizontal leading vertical center', ShortText + ',100,13,1,0,2,100,9,12,false,true,Trailing,Center,None,claBlack,0,2.04,34.46,11.04,0.99,Hx8fHx8fHx//////39/f3//////f39/f////////3/9T/1P/A/8D/wv/A/8D//////////////8')]
    [TestCase('55.Short low height horizontal center vertical center', ShortText + ',100,13,1,0,2,100,9,12,false,true,Center,Center,None,claBlack,32.77,2.04,67.23,11.04,0.99,w8PDw8PDw8P///Pjw8fPz///+/vz5+/f/////////9/6n/qf+F/4H/kf+B/4H/////////////8')]
    [TestCase('56.Short low height horizontal trailing vertical center', ShortText + ',100,13,1,0,2,100,9,12,false,true,Leading,Center,None,claBlack,65.54,2.04,100,11.04,0.99,+Pj4+Pj4+Pj//Pj5+//+/P/8//////7+//////////7/1P/U/8D/0P/A/8D/wP////////////8')]
    [TestCase('57.Short low size horizontal leading vertical center', ShortText + ',40,13,1,5,2,30,9,12,false,true,Trailing,Center,None,claBlack,5,2.04,35,11.04,0.99,///DwYHDw8P///Phw8PPz///8/fz6//P///////////e89730kfcB9iH1LfUF/////////////8')]
    [TestCase('58.Short low size horizontal center vertical center', ShortText + ',40,13,1,5,2,30,9,12,false,true,Center,Center,None,claBlack,5,2.04,35,11.04,0.99,/f3BwcHBwcH//fHhw8fPzf/99+/r38/N/////////+/96/3r8Iv5C/EL6SvoK/////////////8')]
    [TestCase('59.Short low size horizontal trailing vertical center', ShortText + ',40,13,1,5,2,30,9,12,false,true,Leading,Center,None,claBlack,5,2.04,35,11.04,0.99,///DgYGDg4P///Phw8fPz////+vj1+/v///////////71/vX4RfyF+IXwlfSV/////////////8')]
    {$ENDIF}
    procedure GenericTest(const AText: string; const ABitmapWidth, ABitmapHeight: Integer; const AScale, ATextLeft, ATextTop, AMaxWidth, AMaxHeight, AFontSize: Single; const AWordWrap, ARightToLeft: Boolean; const AHorizontalAlign, AVerticalAlign: TTextAlign; const ATrimming: TTextTrimming; const AColor: string; const AExpectedTextLeft, AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    procedure Setup; override;
  end;

implementation

uses
  { Delphi }
  System.UIConsts,
  System.Math,
  System.Math.Vectors,
  {$IF CompilerVersion >= 29}
  FMX.Utils,
  {$ENDIF}
  FMX.Graphics,
  FMX.TextLayout;

{ TSkTextLayoutTests }

function TSkTextLayoutTests.AssetsPath: string;
begin
  Result := CombinePaths(inherited AssetsPath, 'TextLayout');
end;

procedure TSkTextLayoutTests.CheckRegionForRangeWithoutLength;
var
  LLayout: TTextLayout;
  LRegion: TRegion;
begin
  LLayout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    LLayout.BeginUpdate;
    try
      LLayout.Text := ShortText;
      LLayout.Font.Family := DefaultFontFamily;
      LLayout.Font.Size := 12;
      LLayout.WordWrap := False;
    finally
      LLayout.EndUpdate;
    end;
    LRegion := LLayout.RegionForRange(TTextRange.Create(0, 0));
    Assert.IsTrue((Length(LRegion) = 1) and (LRegion[0].Height > 1) and SameValue(LRegion[0].Left, 0, TEpsilon.Position));
    LRegion := LLayout.RegionForRange(TTextRange.Create(Length(LLayout.Text), 0));
    Assert.IsTrue((Length(LRegion) = 1) and (LRegion[0].Height > 1));
  finally
    LLayout.Free;
  end;
end;

procedure TSkTextLayoutTests.GenericTest(const AText: string; const ABitmapWidth,
  ABitmapHeight: Integer; const AScale, ATextLeft, ATextTop, AMaxWidth, AMaxHeight,
  AFontSize: Single; const AWordWrap, ARightToLeft: Boolean; const AHorizontalAlign,
  AVerticalAlign: TTextAlign; const ATrimming: TTextTrimming; const AColor: string;
  const AExpectedTextLeft, AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  Test(AText, TSize.Create(ABitmapWidth, ABitmapHeight), AScale, AFontSize,
    PointF(ATextLeft, ATextTop), PointF(AMaxWidth, AMaxHeight), AWordWrap,
    ARightToLeft, AHorizontalAlign, AVerticalAlign, ATrimming, StringToAlphaColor(AColor),
    RectF(AExpectedTextLeft, AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom),
    True, AMinSimilarity, AExpectedImageHash);
end;

procedure TSkTextLayoutTests.Setup;
begin
  inherited;
  RegisterFontFiles(TSkTypefaceManager.Provider);
end;

procedure TSkTextLayoutTests.Test(const AText: string; const ABitmapSize: TSize;
  const AScale, AFontSize: Single; const ATextTopLeft, AMaxSize: TPointF;
  const AWordWrap, ARightToLeft: Boolean; const AHorizontalAlign,
  AVerticalAlign: TTextAlign; const ATrimming: TTextTrimming;
  const AColor: TAlphaColor; const AExpectedTextRect: TRectF;
  const ACheckTextRect: Boolean; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
const
  TextRectEpsilon = TEpsilon.Position;
var
  LBitmap: TBitmap;
  LLayout: TTextLayout;
  LTextRect: TRectF;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(ABitmapSize.Width, ABitmapSize.Height);
    LBitmap.BitmapScale := AScale;
    LBitmap.Canvas.BeginScene;
    try
      LBitmap.Canvas.Clear(TAlphaColors.Null);
      LLayout := TTextLayoutManager.DefaultTextLayout.Create(LBitmap.Canvas);
      try
        LLayout.BeginUpdate;
        try
          LLayout.TopLeft := ATextTopLeft;
          LLayout.MaxSize := AMaxSize;
          LLayout.Text := AText;
          LLayout.WordWrap := AWordWrap;
          LLayout.HorizontalAlign := AHorizontalAlign;
          LLayout.VerticalAlign := AVerticalAlign;
          LLayout.Font.Size := AFontSize;
          LLayout.Color := AColor;
          LLayout.RightToLeft := ARightToLeft;
          LLayout.Trimming := ATrimming;
        finally
          LLayout.EndUpdate;
        end;
        if ACheckTextRect then
        begin
          LTextRect := LLayout.TextRect;
          Assert.AreSameValue(AExpectedTextRect.Left, LTextRect.Left, TextRectEpsilon, Format('in TextRect. Rect obtained %s', [RectToString(LTextRect)]));
          Assert.AreSameValue(AExpectedTextRect.Top, LTextRect.Top, TextRectEpsilon, Format('in TextRect. Rect obtained %s', [RectToString(LTextRect)]));
          Assert.AreSameValue(AExpectedTextRect.Right, LTextRect.Right, TextRectEpsilon, Format('in TextRect. Rect obtained %s', [RectToString(LTextRect)]));
          Assert.AreSameValue(AExpectedTextRect.Bottom, LTextRect.Bottom, TextRectEpsilon, Format('in TextRect. Rect obtained %s', [RectToString(LTextRect)]));
        end;
        LLayout.RenderLayout(LBitmap.Canvas);
      finally
        LLayout.Free;
      end;
    finally
      LBitmap.Canvas.EndScene;
    end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TSkTextLayoutTests);
end.
