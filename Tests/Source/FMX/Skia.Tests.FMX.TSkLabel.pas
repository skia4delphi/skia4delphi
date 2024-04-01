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
unit Skia.Tests.FMX.TSkLabel;

interface

{$SCOPEDENUMS ON}
{$IFDEF MSWINDOWS}
  {$DEFINE TEXT_RENDER}
{$ENDIF}

uses
  { Delphi }
  System.SysUtils,
  System.Types,
  System.UITypes,
  DUnitX.TestFramework,
  FMX.Types,

  { Skia }
  System.Skia,
  FMX.Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkLabelTests }

  [TestFixture]
  TSkLabelTests = class(TTestBase)
  private
    const
      ShortText = 'Label1';
      {$IFDEF TEXT_RENDER}
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
    procedure Test(const AText: string; const ABitmapSize: TSize; const AScale, AFontSize: Single; const ATextTopLeft, AMaxSize: TPointF; const AMaxLines: Integer; const ARightToLeft: Boolean; const AHorizontalAlign: TSkTextHorzAlign; const AVerticalAlign: TTextAlign; const ATrimming: TTextTrimming; const AColor: TAlphaColor; const AExpectedTextRect: TRectF; const ACheckTextRect: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
  protected
    function AssetsPath: string; override;
  public
    {$IFDEF TEXT_RENDER}
    [TestCase('1.Short', ShortText + ',100,40,1,0,0,65535,65535,14,1,false,Leading,Leading,Character,claBlack,0,0,41,19,0.97,Hx8fH/////////////////////////////////////8D/wP///////////////////////////8')]
    [TestCase('2.Short blue', ShortText + ',100,40,1,0,0,65535,65535,14,1,false,Leading,Leading,Character,#FF9F9FFF,0,0,41,19,0.97,Dw8PH/////9/f39//////39/f3//////f39/f//////8APwAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('3.Short horizontal leading', ShortText + ',100,40,1,0,0,100,40,14,1,false,Leading,Leading,Character,claBlack,0,0,41,19,0.97,Hx8fH/////////////////////////////////////8D/wP///////////////////////////8')]
    [TestCase('4.Short horizontal center', ShortText + ',100,40,1,0,0,100,40,14,1,false,Center,Leading,Character,claBlack,0,0,41,19,0.97,w8PDw/////////Pj////////8+P////////////////4H/gf//////////////////////////8')]
    [TestCase('5.Short horizontal trailing', ShortText + ',100,40,1,0,0,100,40,14,1,false,Trailing,Leading,Character,claBlack,0,0,41,19,0.97,+PDw+P///////PD5///////9/Pn/////////////////oP+A//////////////////////////8')]
    [TestCase('6.Short vertical center', ShortText + ',100,40,1,0,0,100,40,14,1,false,Leading,Center,Character,claBlack,0,0,41,19,0.97,////Dw8fH/////9vT19f/////2/PX1//////////////////U/8D/wP/A/8D//////////////8')]
    [TestCase('7.Short horizontal center, vertical center', ShortText + ',100,40,1,0,0,100,40,14,1,false,Center,Center,Character,claBlack,0,0,41,19,0.97,////w8PDx//////jw8fP//////vT78//////////////////+p/4H/gf+B/4H/////////////8')]
    [TestCase('8.Short horizontal trailing, vertical center', ShortText + ',100,40,1,0,0,100,40,14,1,false,Trailing,Center,Character,claBlack,0,0,41,19,0.97,////8PD4+P/////x8//+//////3///7//////////////////7T/ov+g/4D/gv////////////8')]
    [TestCase('9.Short vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,1,false,Leading,Trailing,Character,claBlack,0,0,41,19,0.97,/////x8PDx//////X09PX/////9fb89f//////////////////////////9T/wP/A/8D/wP///8')]
    [TestCase('10.Short horizontal center, vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,1,false,Center,Trailing,Character,claBlack,0,0,41,19,0.97,/////8PDw8P/////w8fPz//////D39/v///////////////////////////6n/gf+B/4H/gf//8')]
    [TestCase('11.Short horizontal trailing, vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,1,false,Trailing,Trailing,Character,claBlack,0,0,41,19,0.97,//////jw8PD/////+/f+/P/////7//7+////////////////////////////tP+i/6D/gP+C//8')]
    [TestCase('12.Long', LongText + ',100,40,1,0,0,65535,65535,14,0,false,Leading,Leading,Character,claBlack,0,0,6643,19,0.97,/wAAAP///////PDg///////9+vD///////////////8CAAIA/v////////////////////////8')]
    [TestCase('13.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,0,false,Leading,Leading,Character,claBlack,0,0,6643,1558,0.97,/wMBt/8BIX////H3/8fv////+/f/3+//////////7/8CBwIH/v///////v+MqwSDhIMEk/////8')]
    [TestCase('14.Long horizontal center', LongText + ',100,40,1,0,0,100,40,14,0,false,Center,Leading,Character,claBlack,0,0,6643,1558,0.97,/4GB//8Agf///fH//0fN/////f//19////////////+QFYAF/7//////33/GXYJBgkuCSf////8')]
    [TestCase('15.Long horizontal trailing', LongText + ',100,40,1,0,0,100,40,14,0,false,Trailing,Leading,Character,claBlack,0,0,6643,1558,0.97,/8CA+f+AgP///PD5/8fO///99v3//+//////////7//gEuAC/9//////77/pysAAwADEQP////8')]
    [TestCase('16.Long vertical center', LongText + ',100,40,1,0,0,100,40,14,0,false,Leading,Center,Character,claBlack,0,0,6643,1558,0.97,/wMDz/8DAyP/f3Pv/0dPb///++//V19////////fX/8InwgvIA/7//////99/wAPAAcAB/7///8')]
    [TestCase('17.Long horizontal center, vertical center', LongText + ',100,40,1,0,0,100,40,14,0,false,Center,Center,Character,claBlack,0,0,6643,1558,0.97,/4GB5/+BgZH//fHn/8fP3f//+/f/79/d///////v3//Cp8Ir6CP+///////ff8ADwAPAA/////8')]
    [TestCase('18.Long horizontal trailing, vertical center', LongText + ',100,40,1,0,0,100,40,14,0,false,Trailing,Center,Character,claBlack,0,0,6643,1558,0.97,/8LA8v/AwNj//vDz/8fO3P//8vf/197c/////////tzwCfAK8gD/v//////33/AA8BLwAv/f//8')]
    [TestCase('19.Long vertical trailing', LongText + ',100,40,1,0,0,100,40,14,0,false,Leading,Trailing,Character,claBlack,0,0,6643,1558,0.97,/wUABDcfHx//fXBld19fX/99+vV3f1/f/////////98CEAASAhDPf/////+//wn/Cf8B/wL///8')]
    [TestCase('20.Long horizontal center, vertical trailing', LongText + ',100,40,1,0,0,100,40,14,0,false,Center,Trailing,Character,claBlack,0,0,6643,1558,0.97,/wEAAOfDw8P/fXBh58fPz/99+vHn5+/f/////////99CEAKSAhTPf//////7//Cf8B/wH/Bf//8')]
    [TestCase('21.Long horizontal trailing, vertical trailing', LongText + ',100,40,1,0,0,100,40,14,0,false,Trailing,Trailing,Character,claBlack,0,0,6643,1558,0.97,/wAAAPH4+Pj/fHBh8//+/P98cvHz//78//////////8CAAKSAgDPf///////v/+k/5D/kP+g//8')]
    [TestCase('22.Short low height horizontal leading', ShortText + ',100,13,1,0,2,100,9,14,1,false,Leading,Leading,None,claBlack,0,2,41,21,0.97,Hx8fHx8fHx9/f39/X19fX39/f3/f/19f//////////////////9T/1P/U/8D//////////////8')]
    [TestCase('23.Short low height horizontal center', ShortText + ',100,13,1,0,2,100,9,14,1,false,Center,Leading,None,claBlack,0,2,41,21,0.97,w8PDw8PDw8P///Pjw8fPz///8+Pb3//P///////////////////6n/qf+p/4H/////////////8')]
    [TestCase('24.Short low height horizontal trailing', ShortText + ',100,13,1,0,2,100,9,14,1,false,Trailing,Leading,None,claBlack,0,2,41,21,0.97,+Pj4+Pj4+Pj//Pj5+//+/P/8+Pn7//78////////////////////pP+k/6T/oP////////////8')]
    [TestCase('25.Short low height horizontal leading vertical center', ShortText + ',100,13,1,0,2,100,9,12,1,false,Leading,Center,None,claBlack,0,2,35,18,0.97,Hx8fHx8fHx//////39/f3//////f39/f////////3/9T/1P/A/8D/wv/A/8D//////////////8')]
    [TestCase('26.Short low height horizontal center vertical center', ShortText + ',100,13,1,0,2,100,9,12,1,false,Center,Center,None,claBlack,0,2,35,18,0.97,4+Pj4+Pj4+P///Pj4+fv7///+/vz5+/////////////6n/qf+F/4H/kf+B/4H/////////////8')]
    [TestCase('27.Short low height horizontal trailing vertical center', ShortText + ',100,13,1,0,2,100,9,12,1,false,Trailing,Center,None,claBlack,0,2,35,18,0.97,+Pj4+Pj4+Pj//Pj5+//+/P/8//////7+//////////7/1P/U/8D/0P/A/8D/wP////////////8')]
    [TestCase('28.Short low size horizontal leading vertical center', ShortText + ',40,13,1,5,2,30,9,12,1,false,Leading,Center,None,claBlack,5,2,40,18,0.97,//+rgYGBg8P///vhw8PPz///+/fz6//P///////////e99730kfcB9iH1LfUF/////////////8')]
    [TestCase('29.Short low size horizontal center vertical center', ShortText + ',40,13,1,5,2,30,9,12,1,false,Center,Center,None,claBlack,5,2,40,18,0.97,///lwcHBwcH///Xhw8fFzf//9/f77+/t////////7//u++776AvsE+iD6JvoG/////////////8')]
    [TestCase('30.Short low size horizontal trailing vertical center', ShortText + ',40,13,1,5,2,30,9,12,1,false,Trailing,Center,None,claBlack,5,2,40,18,0.97,///h4cHBwcH///Hhw8fPwf//8/fT99/B//////////vve+977CPuQ+xD6EvqS/////////////8')]
    [TestCase('31.Short', ShortText + ',100,40,1,0,0,65535,65535,14,1,true,Trailing,Leading,Character,claBlack,0,0,41,19,0.97,Hx8fH/////////////////////////////////////8D/wP///////////////////////////8')]
    [TestCase('32.Short horizontal leading', ShortText + ',100,40,1,0,0,100,40,14,1,true,Trailing,Leading,Character,claBlack,0,0,41,19,0.97,Hx8fH/////////////////////////////////////8D/wP///////////////////////////8')]
    [TestCase('33.Short horizontal center', ShortText + ',100,40,1,0,0,100,40,14,1,true,Center,Leading,Character,claBlack,0,0,41,19,0.97,w8PDw/////////Pj////////8+P////////////////4H/gf//////////////////////////8')]
    [TestCase('34.Short horizontal trailing', ShortText + ',100,40,1,0,0,100,40,14,1,true,Leading,Leading,Character,claBlack,0,0,41,19,0.97,+PDw+P///////PD5///////9/Pn/////////////////oP+A//////////////////////////8')]
    [TestCase('35.Short vertical center', ShortText + ',100,40,1,0,0,100,40,14,1,true,Trailing,Center,Character,claBlack,0,0,41,19,0.97,////Dw8fH/////9vT19f/////2/PX1//////////////////U/8D/wP/A/8D//////////////8')]
    [TestCase('36.Short horizontal center, vertical center', ShortText + ',100,40,1,0,0,100,40,14,1,true,Center,Center,Character,claBlack,0,0,41,19,0.97,////w8PDx//////jw8fP//////vT78//////////////////+p/4H/gf+B/4H/////////////8')]
    [TestCase('37.Short horizontal trailing, vertical center', ShortText + ',100,40,1,0,0,100,40,14,1,true,Leading,Center,Character,claBlack,0,0,41,19,0.97,////8PD4+P/////x8//+//////3///7//////////////////7T/ov+g/4D/gv////////////8')]
    [TestCase('38.Short vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,1,true,Trailing,Trailing,Character,claBlack,0,0,41,19,0.97,/////x8PDx//////X09PX/////9fb89f//////////////////////////9T/wP/A/8D/wP///8')]
    [TestCase('39.Short horizontal center, vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,1,true,Center,Trailing,Character,claBlack,0,0,41,19,0.97,/////8PDw8P/////w8fPz//////D39/v///////////////////////////6n/gf+B/4H/gf//8')]
    [TestCase('40.Short horizontal trailing, vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,1,true,Leading,Trailing,Character,claBlack,0,0,41,19,0.97,//////jw8PD/////+/f+/P/////7//7+////////////////////////////tP+i/6D/gP+C//8')]
    [TestCase('41.Long', LongText + ',100,40,1,0,0,65535,65535,14,0,true,Trailing,Leading,Character,claBlack,0,0,6643,19,0.97,/wAAAP//////+PDg///////9+fL///////////////8AAAAA/3////////////////////////8')]
    [TestCase('42.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,0,true,Trailing,Leading,Character,claBlack,0,0,6643,1558,0.97,/wMBt/8BIX////H3/8fv////+/f/3+//////////7/8CBwIH/v///////v+MqwSDhIMEk/////8')]
    [TestCase('43.Long horizontal center', LongText + ',100,40,1,0,0,100,40,14,0,true,Center,Leading,Character,claBlack,0,0,6643,1558,0.97,/4GB//8Agf///fH//0fN/////f//19////////////+QFYAF/7//////33/GXYJBgkuCSf////8')]
    [TestCase('44.Long horizontal trailing', LongText + ',100,40,1,0,0,100,40,14,0,true,Leading,Leading,Character,claBlack,0,0,6643,1558,0.97,/8CA+f+AgP///PD5/8fO///99v3//+//////////7//gEuAC/9//////77/pysAAwADEQP////8')]
    [TestCase('45.Long vertical center', LongText + ',100,40,1,0,0,100,40,14,0,true,Trailing,Center,Character,claBlack,0,0,6643,1558,0.97,/wMDz/8DAyP/f3Pv/0dPb///++//V19////////fX/8InwgvIA/7//////99/wAPAAcAB/7///8')]
    [TestCase('46.Long horizontal center, vertical center', LongText + ',100,40,1,0,0,100,40,14,0,true,Center,Center,Character,claBlack,0,0,6643,1558,0.97,/4GB5/+BgZH//fHn/8fP3f//+/f/79/d///////v3//Cp8Ir6CP+///////ff8ADwAPAA/////8')]
    [TestCase('47.Long horizontal trailing, vertical center', LongText + ',100,40,1,0,0,100,40,14,0,true,Leading,Center,Character,claBlack,0,0,6643,1558,0.97,/8LA8v/AwNj//vDz/8fO3P//8vf/197c/////////tzwCfAK8gD/v//////33/AA8BLwAv/f//8')]
    [TestCase('48.Long vertical trailing', LongText + ',100,40,1,0,0,100,40,14,0,true,Trailing,Trailing,Character,claBlack,0,0,6643,1558,0.97,/wUABDcfHx//fXBld19fX/99+vV3X39f/////////98CEAASAhDPf////////4H/EP8Q/wP///8')]
    [TestCase('49.Long horizontal center, vertical trailing', LongText + ',100,40,1,0,0,100,40,14,0,true,Center,Trailing,Character,claBlack,0,0,6643,1558,0.97,/wEAAOfjw8P/fXBh5+fPz/99+vHn9+////////////9CEAKSAhTPf/////////pP+A/5D/of//8')]
    [TestCase('50.Long horizontal trailing, vertical trailing', LongText + ',100,40,1,0,0,100,40,14,0,true,Leading,Trailing,Character,claBlack,0,0,6643,1558,0.97,/wAAAPD4+Pj/fHBh8//+/P98cvHz//7+//////////8CAAKSAgDPf///////3/+E/4D/gP+C//8')]
    [TestCase('51.Short low height horizontal leading', ShortText + ',100,13,1,0,2,100,9,14,1,true,Trailing,Leading,None,claBlack,0,2,41,21,0.97,Hx8fHx8fHx9/f39/X19fX39/f3/f/19f//////////////////9T/1P/U/8D//////////////8')]
    [TestCase('52.Short low height horizontal center', ShortText + ',100,13,1,0,2,100,9,14,1,true,Center,Leading,None,claBlack,0,2,41,21,0.97,w8PDw8PDw8P///Pjw8fPz///8+Pb3//P///////////////////6n/qf+p/4H/////////////8')]
    [TestCase('53.Short low height horizontal trailing', ShortText + ',100,13,1,0,2,100,9,14,1,true,Leading,Leading,None,claBlack,0,2,41,21,0.97,+Pj4+Pj4+Pj//Pj5+//+/P/8+Pn7//78////////////////////pP+k/6T/oP////////////8')]
    [TestCase('54.Short low height horizontal leading vertical center', ShortText + ',100,13,1,0,2,100,9,12,1,true,Trailing,Center,None,claBlack,0,2,35,18,0.97,Hx8fHx8fHx//////39/f3//////f39/f////////3/9T/1P/A/8D/wv/A/8D//////////////8')]
    [TestCase('55.Short low height horizontal center vertical center', ShortText + ',100,13,1,0,2,100,9,12,1,true,Center,Center,None,claBlack,0,2,35,18,0.97,4+Pj4+Pj4+P///Pj4+fv7///+/vz5+/////////////6n/qf+F/4H/kf+B/4H/////////////8')]
    [TestCase('56.Short low height horizontal trailing vertical center', ShortText + ',100,13,1,0,2,100,9,12,1,true,Leading,Center,None,claBlack,0,2,35,18,0.97,+Pj4+Pj4+Pj//Pj5+//+/P/8//////7+//////////7/1P/U/8D/0P/A/8D/wP////////////8')]
    [TestCase('57.Short low size horizontal leading vertical center', ShortText + ',40,13,1,5,2,30,9,12,1,true,Trailing,Center,None,claBlack,5,2,40,18,0.97,//+rgYGBg8P///vhw8PPz///+/fz6//P///////////e99730kfcB9iH1LfUF/////////////8')]
    [TestCase('58.Short low size horizontal center vertical center', ShortText + ',40,13,1,5,2,30,9,12,1,true,Center,Center,None,claBlack,5,2,40,18,0.97,///lwcHBwcH///Xhw8fFzf//9/f77+/t////////7//u++776AvsE+iD6JvoG/////////////8')]
    [TestCase('59.Short low size horizontal trailing vertical center', ShortText + ',40,13,1,5,2,30,9,12,1,true,Leading,Center,None,claBlack,5,2,40,18,0.97,///h4cHBwcH///Hhw8fPwf//8/fT99/B//////////vve+977CPuQ+xD6EvqS/////////////8')]
    {$ENDIF}
    procedure GenericTest(const AText: string; const AControlWidth, AControlHeight: Integer; const AScale, ATextLeft, ATextTop, AMaxWidth, AMaxHeight, AFontSize: Single; const AMaxLines: Integer; const ARightToLeft: Boolean; const AHorizontalAlign: TSkTextHorzAlign; const AVerticalAlign: TTextAlign; const ATrimming: TTextTrimming; const AColor: string; const AExpectedTextLeft, AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [Setup]
    procedure Setup; override;
  end;

implementation

uses
  { Delphi }
  System.Classes,
  System.UIConsts,
  System.Math,
  System.Math.Vectors,
  {$IF CompilerVersion >= 29}
  FMX.Utils,
  {$ENDIF}
  FMX.Graphics,
  FMX.Forms;

type
  TSkLabelAccess = class(TSkLabel)
  public
    property ParagraphBounds;
  end;

const
  TextRectEpsilon = 2;

{ TSkLabelTests }

function TSkLabelTests.AssetsPath: string;
begin
  Result := CombinePaths(RootAssetsPath, 'TextLayout');
end;

procedure TSkLabelTests.GenericTest(const AText: string; const AControlWidth,
  AControlHeight: Integer; const AScale, ATextLeft, ATextTop, AMaxWidth, AMaxHeight,
  AFontSize: Single; const AMaxLines: Integer; const ARightToLeft: Boolean;
  const AHorizontalAlign: TSkTextHorzAlign; const AVerticalAlign: TTextAlign;
  const ATrimming: TTextTrimming; const AColor: string; const AExpectedTextLeft,
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
  RegisterFontFiles(TSkDefaultProviders.TypefaceFont);
end;

procedure TSkLabelTests.Test(const AText: string; const ABitmapSize: TSize;
  const AScale, AFontSize: Single; const ATextTopLeft, AMaxSize: TPointF;
  const AMaxLines: Integer; const ARightToLeft: Boolean;
  const AHorizontalAlign: TSkTextHorzAlign;
  const AVerticalAlign: TTextAlign; const ATrimming: TTextTrimming;
  const AColor: TAlphaColor; const AExpectedTextRect: TRectF;
  const ACheckTextRect: Boolean; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LLabel: TSkLabel;
  LSavedBiDiMode: TBiDiMode;
  LTextRect: TRectF;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(ABitmapSize.Width, ABitmapSize.Height);
    LBitmap.BitmapScale := AScale;
    LBitmap.Canvas.BeginScene;
    try
      LBitmap.Canvas.Clear(TAlphaColors.Null);
      LSavedBiDiMode := Application.BiDiMode;
      if ARightToLeft then
        Application.BiDiMode := TBiDiMode.bdRightToLeft
      else
        Application.BiDiMode := TBiDiMode.bdLeftToRight;
      try
        LLabel := TSkLabel.Create(nil);
        try
          LLabel.BeginUpdate;
          try
            LLabel.AutoSize := False;
            LLabel.SetBounds(ATextTopLeft.X, ATextTopLeft.Y, AMaxSize.X, AMaxSize.Y);
            LLabel.TextSettings.BeginUpdate;
            try
              LLabel.TextSettings.Font.Families := DefaultFontFamily;
              LLabel.TextSettings.MaxLines := AMaxLines;
              LLabel.TextSettings.HorzAlign := AHorizontalAlign;
              LLabel.TextSettings.VertAlign := AVerticalAlign;
              LLabel.TextSettings.Font.Families := DefaultFontFamily;
              LLabel.TextSettings.Font.Size := AFontSize;
              LLabel.TextSettings.FontColor := AColor;
              LLabel.TextSettings.Trimming := ATrimming;
            finally
              LLabel.TextSettings.EndUpdate;
            end;
            LLabel.Text := AText;
          finally
            LLabel.EndUpdate;
          end;
          if ACheckTextRect then
          begin
            LTextRect := TSkLabelAccess(LLabel).ParagraphBounds;
            LTextRect.Offset(ATextTopLeft);
            Assert.AreSameRect(AExpectedTextRect, LTextRect, TextRectEpsilon);
          end;
          LLabel.PaintTo(LBitmap.Canvas, TRectF.Create(PointF(LLabel.Position.X, LLabel.Position.Y), LLabel.Width, LLabel.Height));
        finally
          LLabel.Free;
        end;
      finally
        Application.BiDiMode := LSavedBiDiMode;
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
  TDUnitX.RegisterTestFixture(TSkLabelTests);
end.
