{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2021-2023 Skia4Delphi Project.                           }
{                                                                        }
{ Use of this source code is governed by the MIT license that can be     }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit Skia.Tests.FMX.TextLayout;

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
  FMX.TextLayout,

  { Skia }
  System.Skia,
  FMX.Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkTextLayoutTests }

  [TestFixture]
  TSkTextLayoutTests = class(TTestBase)
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
    procedure Test(const AText: string; const ABitmapSize: TSize; const AScale, AFontSize: Single; const ATextTopLeft, AMaxSize: TPointF; const AWordWrap, ARightToLeft: Boolean; const AHorizontalAlign, AVerticalAlign: TTextAlign; const ATrimming: TTextTrimming; const AColor: TAlphaColor; const AExpectedTextRect: TRectF; const ACheckTextRect: Boolean; const AAttributes: TArray<TTextAttributedRange>; const AMinSimilarity: Double; const AExpectedImageHash: string);
  public
    [Test]
    procedure CheckRegionForRangeWithoutLength;
    {$IFDEF TEXT_RENDER}
    [TestCase('1.Short', ShortText + ',100,40,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,0.97,Hx8fH/////////////////////////////////////8D/wP///////////////////////////8')]
    [TestCase('2.Short blue', ShortText + ',100,40,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,#FF9F9FFF,0,0.38,40.2,19,0.97,Dw8PH/////9/f39//////39/f3//////f39/f//////8APwAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('3.Short horizontal leading', ShortText + ',100,40,1,0,0,100,40,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,0.97,Hx8fH/////////////////////////////////////8D/wP///////////////////////////8')]
    [TestCase('4.Short horizontal center', ShortText + ',100,40,1,0,0,100,40,14,false,false,Center,Leading,Character,claBlack,29.899,0.38,70.1,19,0.97,w8PDw/////////Pj////////8+P////////////////4H/gf//////////////////////////8')]
    [TestCase('5.Short horizontal trailing', ShortText + ',100,40,1,0,0,100,40,14,false,false,Trailing,Leading,Character,claBlack,59.8,0.38,100,19,0.97,+PDw+P///////PD5///////9/Pn/////////////////oP+A//////////////////////////8')]
    [TestCase('6.Short vertical center', ShortText + ',100,40,1,0,0,100,40,14,false,false,Leading,Center,Character,claBlack,0,11.07,40.2,29.69,0.97,////Dw8fH/////9vT19f/////2/PX1//////////////////U/8D/wP/A/8D//////////////8')]
    [TestCase('7.Short horizontal center, vertical center', ShortText + ',100,40,1,0,0,100,40,14,false,false,Center,Center,Character,claBlack,29.9,11.07,70.1,29.69,0.97,////w8PDx//////jw8fP//////vT78//////////////////+p/4H/gf+B/4H/////////////8')]
    [TestCase('8.Short horizontal trailing, vertical center', ShortText + ',100,40,1,0,0,100,40,14,false,false,Trailing,Center,Character,claBlack,59.8,11.07,100,29.69,0.97,////8PD4+P/////x8//+//////3///7//////////////////7T/ov+g/4D/gv////////////8')]
    [TestCase('9.Short vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,false,false,Leading,Trailing,Character,claBlack,0,21.76,40.2,40.38,0.97,/////x8PDx//////X09PX/////9fb89f//////////////////////////9T/wP/A/8D/wP///8')]
    [TestCase('10.Short horizontal center, vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,false,false,Center,Trailing,Character,claBlack,29.9,21.76,70.1,40.38,0.97,/////8PDw8P/////w8fPz//////D39/v///////////////////////////6n/gf+B/4H/gf//8')]
    [TestCase('11.Short horizontal trailing, vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,false,false,Trailing,Trailing,Character,claBlack,59.8,21.76,100,40.38,0.97,//////jw8PD/////+/f+/P/////7//7+////////////////////////////tP+i/6D/gP+C//8')]
    [TestCase('12.Long', LongText + ',100,40,1,0,0,65535,65535,14,true,false,Leading,Leading,Character,claBlack,0,0.38,6642.3398,19,0.97,/wAAAP///////PDg///////9+vD///////////////8CAAIA/v////////////////////////8')]
    [TestCase('13.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,false,Leading,Leading,Character,claBlack,0,0.38,98.84,38,0.97,/wMBv/8BIX////H//8fv////+///3+//////////7/8CBwIH/v///////v+M+wSDhIMEkP////8')]
    [TestCase('14.Long horizontal center', LongText + ',100,40,1,0,0,100,40,14,true,false,Center,Leading,Character,claBlack,0.58,0.38,99.42,38,0.97,/4GB//8BIX///fH//0dtf////f//X21///////9/7X+QFYAF/7///////v+cuwSDBIMEgP////8')]
    [TestCase('15.Long horizontal trailing', LongText + ',100,40,1,0,0,100,40,14,true,false,Trailing,Leading,Character,claBlack,3.83,0.38,103.83,38,0.97,/8CA+f8BCf///PD5/0dP///99v3/18/////////f7//gEuAC/9///////v+lMwADACMFIv////8')]
    [TestCase('16.Long vertical center', LongText + ',100,40,1,0,0,100,40,14,true,false,Leading,Center,Character,claBlack,0,1.57,98.84,39.19,0.97,/wMBt/8BISH/f3H3/0dvbf9/9f//X299////////b38CBwIHAg////////+e+wSDhIMEkP////8')]
    [TestCase('17.Long horizontal center, vertical center', LongText + ',100,40,1,0,0,100,40,14,true,false,Center,Center,Character,claBlack,0.58,1.57,99.42,39.19,0.97,/4mB//8BASn//fH//0dNbf///f//V19t//////9//++AgYgFyIf/v/////+u/wCDBIMEgP////8')]
    [TestCase('18.Long horizontal trailing, vertical center', LongText + ',100,40,1,0,0,100,40,14,true,false,Trailing,Center,Character,claBlack,3.83,1.57,103.83,39.19,0.97,/8CA+f8BCSv//PD5/0dPa///9v3/199r/////////2vgAOAC4sL/3/////+u/wADCCMAAP////8')]
    [TestCase('19.Long vertical trailing', LongText + ',100,40,1,0,0,100,40,14,true,false,Leading,Trailing,Character,claBlack,0,2.76,98.84,40.38,0.97,/5MBI38JAQH//3Fjf09PTf///Xt/X2/P////////f/8CBwKHAgf+//////++/wiDBJMEggyS//8')]
    [TestCase('20.Long horizontal center, vertical trailing', LongText + ',100,40,1,0,0,100,40,14,true,false,Center,Trailing,Character,claBlack,0.58,2.76,99.42,40.38,0.97,/92Bkf8JAQH//fHx/09PTf/9//n/31/P//////////+gAZAFiIX/v//////+/wCDBIcEggSC//8')]
    [TestCase('21.Long horizontal trailing, vertical trailing', LongText + ',100,40,1,0,0,100,40,14,true,false,Trailing,Trailing,Character,claBlack,3.83,2.76,103.83,40.38,0.97,/+iAyPsJAQH//PDp+09PTf/89/37/9/P///////////gAOAS4AL/3/////++/wADAAMAAlUi//8')]
    [TestCase('22.Short low height horizontal leading', ShortText + ',100,13,1,0,2,100,9,14,false,false,Leading,Leading,None,claBlack,0,2.38,40.2,11,0.97,Hx8fHx8fHx9/f39/X19fX39/f3/f/19f//////////////////9T/1P/U/8D//////////////8')]
    [TestCase('23.Short low height horizontal center', ShortText + ',100,13,1,0,2,100,9,14,false,false,Center,Leading,None,claBlack,29.9,2.38,70.1,11,0.97,w8PDw8PDw8P///Pjw8fPz///8+Pb3//P///////////////////6n/qf+p/4H/////////////8')]
    [TestCase('24.Short low height horizontal trailing', ShortText + ',100,13,1,0,2,100,9,14,false,false,Trailing,Leading,None,claBlack,59.8,2.38,100,11,0.97,+Pj4+Pj4+Pj//Pj5+//+/P/8+Pn7//78////////////////////pP+k/6T/oP////////////8')]
    [TestCase('25.Short low height horizontal leading vertical center', ShortText + ',100,13,1,0,2,100,9,12,false,false,Leading,Center,None,claBlack,0,2.04,34.46,11.04,0.97,Hx8fHx8fHx//////39/f3//////f39/f////////3/9T/1P/A/8D/wv/A/8D//////////////8')]
    [TestCase('26.Short low height horizontal center vertical center', ShortText + ',100,13,1,0,2,100,9,12,false,false,Center,Center,None,claBlack,32.77,2.04,67.23,11.04,0.97,4+Pj4+Pj4+P///Pj4+fv7///+/vz5+/////////////6n/qf+F/4H/kf+B/4H/////////////8')]
    [TestCase('27.Short low height horizontal trailing vertical center', ShortText + ',100,13,1,0,2,100,9,12,false,false,Trailing,Center,None,claBlack,65.54,2.04,100,11.04,0.97,+Pj4+Pj4+Pj//Pj5+//+/P/8//////7+//////////7/1P/U/8D/0P/A/8D/wP////////////8')]
    [TestCase('28.Short low size horizontal leading vertical center', ShortText + ',40,13,1,5,2,30,9,12,false,false,Leading,Center,None,claBlack,5,2.04,35,11.04,0.97,//+rgYGBg8P///vhw8PPz///+/fz6//P///////////e89730kfcB9iH1LfUF/////////////8')]
    [TestCase('29.Short low size horizontal center vertical center', ShortText + ',40,13,1,5,2,30,9,12,false,false,Center,Center,None,claBlack,5,2.04,35,11.04,0.97,//3JwcHBwcX//fnhw8fPzf/9/+/r38/N/////////+/96/3r8Iv5C/EL6SvoK/////////////8')]
    [TestCase('30.Short low size horizontal trailing vertical center', ShortText + ',40,13,1,5,2,30,9,12,false,false,Trailing,Center,None,claBlack,5,2.04,35,11.04,0.97,///RgYGBg4f///Hhw8fPz////+vj1+/v///////////71/vX4RfyF+IXwlfSV/////////////8')]
    [TestCase('31.Short', ShortText + ',100,40,1,0,0,65535,65535,14,false,true,Trailing,Leading,Character,claBlack,0,0.38,40.2,19,0.97,Hx8fH/////////////////////////////////////8D/wP///////////////////////////8')]
    [TestCase('32.Short horizontal leading', ShortText + ',100,40,1,0,0,100,40,14,false,true,Trailing,Leading,Character,claBlack,0,0.38,40.2,19,0.97,Hx8fH/////////////////////////////////////8D/wP///////////////////////////8')]
    [TestCase('33.Short horizontal center', ShortText + ',100,40,1,0,0,100,40,14,false,true,Center,Leading,Character,claBlack,29.9,0.38,70.1,19,0.97,w8PDw/////////Pj////////8+P////////////////4H/gf//////////////////////////8')]
    [TestCase('34.Short horizontal trailing', ShortText + ',100,40,1,0,0,100,40,14,false,true,Leading,Leading,Character,claBlack,59.8,0.38,100,19,0.97,+PDw+P///////PD5///////9/Pn/////////////////oP+A//////////////////////////8')]
    [TestCase('35.Short vertical center', ShortText + ',100,40,1,0,0,100,40,14,false,true,Trailing,Center,Character,claBlack,0,11.07,40.2,29.69,0.97,////Dw8fH/////9vT19f/////2/PX1//////////////////U/8D/wP/A/8D//////////////8')]
    [TestCase('36.Short horizontal center, vertical center', ShortText + ',100,40,1,0,0,100,40,14,false,true,Center,Center,Character,claBlack,29.9,11.07,70.1,29.69,0.97,////w8PDx//////jw8fP//////vT78//////////////////+p/4H/gf+B/4H/////////////8')]
    [TestCase('37.Short horizontal trailing, vertical center', ShortText + ',100,40,1,0,0,100,40,14,false,true,Leading,Center,Character,claBlack,59.8,11.07,100,29.69,0.97,////8PD4+P/////x8//+//////3///7//////////////////7T/ov+g/4D/gv////////////8')]
    [TestCase('38.Short vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,false,true,Trailing,Trailing,Character,claBlack,0,21.76,40.2,40.38,0.97,/////x8PDx//////X09PX/////9fb89f//////////////////////////9T/wP/A/8D/wP///8')]
    [TestCase('39.Short horizontal center, vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,false,true,Center,Trailing,Character,claBlack,29.9,21.76,70.1,40.38,0.97,/////8PDw8P/////w8fPz//////D39/v///////////////////////////6n/gf+B/4H/gf//8')]
    [TestCase('40.Short horizontal trailing, vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,false,true,Leading,Trailing,Character,claBlack,59.8,21.76,100,40.38,0.97,//////jw8PD/////+/f+/P/////7//7+////////////////////////////tP+i/6D/gP+C//8')]
    [TestCase('41.Long', LongText + ',100,40,1,0,0,65535,65535,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,6642.34,19,0.97,/wAAAP//////+PDg///////9+fL///////////////8AAAAA/3////////////////////////8')]
    [TestCase('42.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,98.84,38,0.97,/wMBv/8BIX////H//8fv////+///3+//////////7/8CBwIH/v///////v+M+wSDhIMEkP////8')]
    [TestCase('43.Long horizontal center', LongText + ',100,40,1,0,0,100,40,14,true,true,Center,Leading,Character,claBlack,0.58,0.38,99.42,38,0.97,/4GB//8BIX///fH//0dtf////f//X21///////9/7X+QFYAF/7///////v+cuwSDBIMEgP////8')]
    [TestCase('44.Long horizontal trailing', LongText + ',100,40,1,0,0,100,40,14,true,true,Leading,Leading,Character,claBlack,3.83,0.38,103.83,38,0.97,/8CA+f8BCf///PD5/0dP///99v3/18/////////f7//gEuAC/9///////v+lMwADACMFIv////8')]
    [TestCase('45.Long vertical center', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Center,Character,claBlack,0,1.57,98.84,39.19,0.97,/wMBt/8BISH/f3H3/0dvbf9/9f//X299////////b38CBwIHAg////////+e+wSDhIMEkP////8')]
    [TestCase('46.Long horizontal center, vertical center', LongText + ',100,40,1,0,0,100,40,14,true,true,Center,Center,Character,claBlack,0.58,1.57,99.42,39.19,0.97,/4mB//8BASn//fH//0dNbf///f//V19t//////9//++AgYgFyIf/v/////+u/wCDBIMEgP////8')]
    [TestCase('47.Long horizontal trailing, vertical center', LongText + ',100,40,1,0,0,100,40,14,true,true,Leading,Center,Character,claBlack,3.83,1.57,103.83,39.19,0.97,/8CA+f8BCSv//PD5/0dPa///9v3/199r/////////2vgAOAC4sL/3/////+u/wADCCMAAP////8')]
    [TestCase('48.Long vertical trailing', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Trailing,Character,claBlack,0,2.76,98.84,40.38,0.97,/5MBI38JAQH//3Fjf09PTf///Xt/X2/P////////f/8CBwKHAgf+//////++/wiDBJMEggyS//8')]
    [TestCase('49.Long horizontal center, vertical trailing', LongText + ',100,40,1,0,0,100,40,14,true,true,Center,Trailing,Character,claBlack,0.58,2.76,99.42,40.38,0.97,/92Bkf8JAQH//fHx/09PTf/9//n/31/P//////////+gAZAFiIX/v//////+/wCDBIcEggSC//8')]
    [TestCase('50.Long horizontal trailing, vertical trailing', LongText + ',100,40,1,0,0,100,40,14,true,true,Leading,Trailing,Character,claBlack,3.83,2.76,103.83,40.38,0.97,/+iAyPsJAQH//PDp+09PTf/89/37/9/P///////////gAOAS4AL/3/////++/wADAAMAAlUi//8')]
    [TestCase('51.Short low height horizontal leading', ShortText + ',100,13,1,0,2,100,9,14,false,true,Trailing,Leading,None,claBlack,0,2.38,40.2,11,0.97,Hx8fHx8fHx9/f39/X19fX39/f3/f/19f//////////////////9T/1P/U/8D//////////////8')]
    [TestCase('52.Short low height horizontal center', ShortText + ',100,13,1,0,2,100,9,14,false,true,Center,Leading,None,claBlack,29.9,2.38,70.1,11,0.97,w8PDw8PDw8P///Pjw8fPz///8+Pb3//P///////////////////6n/qf+p/4H/////////////8')]
    [TestCase('53.Short low height horizontal trailing', ShortText + ',100,13,1,0,2,100,9,14,false,true,Leading,Leading,None,claBlack,59.8,2.38,100,11,0.97,+Pj4+Pj4+Pj//Pj5+//+/P/8+Pn7//78////////////////////pP+k/6T/oP////////////8')]
    [TestCase('54.Short low height horizontal leading vertical center', ShortText + ',100,13,1,0,2,100,9,12,false,true,Trailing,Center,None,claBlack,0,2.04,34.46,11.04,0.97,Hx8fHx8fHx//////39/f3//////f39/f////////3/9T/1P/A/8D/wv/A/8D//////////////8')]
    [TestCase('55.Short low height horizontal center vertical center', ShortText + ',100,13,1,0,2,100,9,12,false,true,Center,Center,None,claBlack,32.77,2.04,67.23,11.04,0.97,4+Pj4+Pj4+P///Pj4+fv7///+/vz5+/////////////6n/qf+F/4H/kf+B/4H/////////////8')]
    [TestCase('56.Short low height horizontal trailing vertical center', ShortText + ',100,13,1,0,2,100,9,12,false,true,Leading,Center,None,claBlack,65.54,2.04,100,11.04,0.97,+Pj4+Pj4+Pj//Pj5+//+/P/8//////7+//////////7/1P/U/8D/0P/A/8D/wP////////////8')]
    [TestCase('57.Short low size horizontal leading vertical center', ShortText + ',40,13,1,5,2,30,9,12,false,true,Trailing,Center,None,claBlack,5,2.04,35,11.04,0.97,//+rgYGBg8P///vhw8PPz///+/fz6//P///////////e89730kfcB9iH1LfUF/////////////8')]
    [TestCase('58.Short low size horizontal center vertical center', ShortText + ',40,13,1,5,2,30,9,12,false,true,Center,Center,None,claBlack,5,2.04,35,11.04,0.97,//3JwcHBwcX//fnhw8fPzf/9/+/r38/N/////////+/96/3r8Iv5C/EL6SvoK/////////////8')]
    [TestCase('59.Short low size horizontal trailing vertical center', ShortText + ',40,13,1,5,2,30,9,12,false,true,Leading,Center,None,claBlack,5,2.04,35,11.04,0.97,///RgYGBg4f///Hhw8fPz////+vj1+/v///////////71/vX4RfyF+IXwlfSV/////////////8')]
    {$ENDIF}
    procedure GenericTest(const AText: string; const ABitmapWidth, ABitmapHeight: Integer; const AScale, ATextLeft, ATextTop, AMaxWidth, AMaxHeight, AFontSize: Single; const AWordWrap, ARightToLeft: Boolean; const AHorizontalAlign, AVerticalAlign: TTextAlign; const ATrimming: TTextTrimming; const AColor: string; const AExpectedTextLeft, AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    {$IFDEF TEXT_RENDER}
    [TestCase('1.Short attr middle', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,32.08,19,2,3,8,False,claRed,0.97,Hx8fHx8ff/9/f39/X19//39/f39fX3///////3//f/83/zf/N/83//////////////////////8')]
    [TestCase('2.Short attr empty', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,2,0,8,False,claRed,0.97,Hx8fHx8f//9/f39/X1///3///39fX/////////////8D/wP/A/8D//////////////////////8')]
    [TestCase('3.Short attr begin', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,30.8,19,0,3,8,False,claRed,0.97,Hx8fHx8///9/f39/X3///3////9ff//////////////H/8f/x//H//////////////////////8')]
    [TestCase('4.Short attr end', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,32.38,19,3,20,8,False,claRed,0.97,Pz8fHx8/f/9/f39/X39//3///39ff3///////1//f/8P/w//D/8P//////////////////////8')]
    [TestCase('5.Short attr middle with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,41.2,27,2,3,8,False,claRed,0.97,X18PDw8PHx9/f39vT09fX39/f+/vb39f//////////97/3v/W/8b/1v/G/8b/xv///////////8')]
    [TestCase('6.Short attr middle big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,45.61,24,2,3,18,False,claRed,0.97,Dw8PDw8PHz9/f39vT09ff3///29vb19/f///b29vf38kADwAPAA8ADwAPAAAAAAAAAAAAAAAAAA')]
    [TestCase('7.Short attr empty big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,2,0,18,False,claRed,0.97,Hx8fHx8f//9/f39/X1///3///39fX/////////////8D/wP/A/8D//////////////////////8')]
    [TestCase('8.Short attr begin big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,46.47,24,0,3,18,False,claRed,0.97,Dw8PDw8PHx9/f39vT09fX3///+9PX19ff///719f31+QAPgA+ADwAPAA8AAAAAAAAAAAAAAAAAA')]
    [TestCase('9.Short attr end big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,45.42,24,3,20,18,False,claRed,0.97,Dw8PDw8PHz9/f39vT09ff39//+/vT19/f3//7+9PX38GAA4ADgAOAAYADgAAAAAAAAAAAAAAAAA')]
    [TestCase('10.Short attr middle big with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,54.73,27,2,3,18,False,claRed,0.97,r68PDw8PDw///39vT09PT///f///f09P//////////9/f35/Xn8ff19/H38ffx9///////////8')]
    [TestCase('11.Short attr middle bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,32.82,19,2,3,8,True,claRed,0.97,Hx8fHx8f//9/f39/X1///39/f39fX////////3////83/zf/N/83//////////////////////8')]
    [TestCase('12.Short attr empty bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,2,0,8,True,claRed,0.97,Hx8fHx8f//9/f39/X1///3///39fX/////////////8D/wP/A/8D//////////////////////8')]
    [TestCase('13.Short attr begin bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,31.61,19,0,3,8,True,claRed,0.97,Hx8fHx8///9/f39/X3///39///9ff//////////////H/8f/x//H//////////////////////8')]
    [TestCase('14.Short attr end bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,33.15,19,3,20,8,True,claRed,0.97,Hx8fHx8f//9/f39/X1///3///39fX////////1////8P/w//D/8P//////////////////////8')]
    [TestCase('15.Short attr middle bold with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,41.93,27,2,3,8,True,claRed,0.97,f18fDw8fHx9/f39vT19fX39/f+/vf39f//////////99/3n/Wf8Z/1n/Gf8Z/xn///////////8')]
    [TestCase('16.Short attr middle big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,47.28,24,2,3,18,True,claRed,0.97,Dw8PDw8PHz9/f39vT09ff3///29PT19/f///b29Pf38mADwAPAA+AD4APgAAAAAAAAAAAAAAAAA')]
    [TestCase('17.Short attr empty big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,2,0,18,True,claRed,0.97,Hx8fHx8f//9/f39/X1///3///39fX/////////////8D/wP/A/8D//////////////////////8')]
    [TestCase('18.Short attr begin big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,48.31,24,0,3,18,True,claRed,0.97,Hx8PDw8fHz/////vz9/f/////+/P39//////79/f3/+QAPgA+AD4APgA+AAAAAAAAAAAAAAAAAA')]
    [TestCase('19.Short attr end big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,47.15,24,3,20,18,True,claRed,0.97,j48PDw8PHx///39vT09fX////+/vT19f/////////////1//H/8X/x//D/////////////////8')]
    [TestCase('20.Short attr middle big bold with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,56.39,27,2,3,18,True,claRed,0.97,r68PBwcPDw///39nR09PT///f+fnb19P//////////9/f35/X38ff19/H38ffx9///////////8')]
    {$ENDIF}
    procedure TestAttributes1(const AText: string; const ABitmapWidth, ABitmapHeight: Integer; const AScale, ATextLeft, ATextTop, AMaxWidth, AMaxHeight, AFontSize: Single; const AWordWrap, ARightToLeft: Boolean; const AHorizontalAlign, AVerticalAlign: TTextAlign; const ATrimming: TTextTrimming; const AColor: string; const AExpectedTextLeft, AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom: Single; const AAttrRangePos, AAttrRangeLength: Integer; const AAttrFontSize: Single; const AAttrFontBold: Boolean; const AAttrColor: string; const AMinSimilarity: Double; const AExpectedImageHash: string);
    {$IFDEF TEXT_RENDER}
    [TestCase('1.Short attr middle', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,32.17,19,1,2,8,False,claRed,4,1,8,False,claBlue,0.97,Hx8fHx8f//9/f39/X1///39///9fX////////3////9H/0f/R/9H//////////////////////8')]
    [TestCase('2.Short attr empty', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,37.06,19,2,0,8,False,claRed,3,1,8,False,claBlue,0.97,Hx8fHx8f//9/f39/X1///3///39fX/////////////8D/wP/A/8D//////////////////////8')]
    [TestCase('3.Short attr begin', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,27.66,19,0,3,8,False,claRed,2,2,8,False,claBlue,0.97,Hx8fHx8///9/f39/X3///3/////ff////////9/////P/4//j/+P//////////////////////8')]
    [TestCase('4.Short attr end', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,32.38,19,3,20,8,False,claRed,5,15,8,False,claBlue,0.97,Pz8fHx8/f/9/f39/X39//3///39ff3///////3//f/8P/wf/B/8H//////////////////////8')]
    [TestCase('5.Short attr middle with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,34.73,27,2,3,8,False,claRed,5,1,8,False,claBlue,0.97,fz8/Hx8fHx9/f39/X19fX39/f//fX19f//////////9//3//X/8f/1//G/8b/xv///////////8')]
    [TestCase('6.Short attr middle big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,45.61,24,2,3,18,False,claRed,4,1,18,False,claBlue,0.97,Dw8PDw8PHz9/f39vT09ff3///29vb19////////////7/3n/Of85/zn/Of////////////////8')]
    [TestCase('7.Short attr empty big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,42.29,24,2,0,18,False,claRed,3,1,18,False,claBlue,0.97,Hx8fHx8fHx9/f39/X19fX39////fX19f/////////////1H/Af8B/wn/Af////////////////8')]
    [TestCase('8.Short attr begin big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,37.46,24,0,3,18,False,claRed,2,2,8,False,claBlue,0.97,Hx8fHx8fHx9/f39/X19fX39////fX19ff3///99f31+AAOAA4ADgAOAA4AAAAAAAAAAAAAAAAAA')]
    [TestCase('9.Short attr end big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,45.42,24,3,20,18,False,claRed,5,15,18,False,claBlue,0.97,Dw8PDw8PHz9/f39vT09ff39//+/vT19////////////9/13/Hf8N/w3/Df////////////////8')]
    [TestCase('10.Short attr middle big with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,53.65,27,2,3,18,False,claRed,5,1,18,False,claBlue,0.97,r68PDw8PDw///39vT09PT///f///f09P//////////9//37/Xv8e/17/Hv8e/x7///////////8')]
    [TestCase('11.Short attr middle bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,32.49,19,2,3,8,True,claRed,4,1,8,False,claBlue,0.97,Hx8fHx8ff/9/f39/X19//39/f39fX3///////3//f/83/yf/N/83//////////////////////8')]
    [TestCase('12.Short attr empty bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,37.06,19,2,0,8,True,claRed,3,1,8,False,claBlue,0.97,Hx8fHx8f//9/f39/X1///3///39fX/////////////8D/wP/A/8D//////////////////////8')]
    [TestCase('13.Short attr begin bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,28.22,19,0,3,8,True,claRed,2,2,8,False,claBlue,0.97,Hx8fHx8///9/f39/X3///3////9ff////////9/////H/4f/h/+H//////////////////////8')]
    [TestCase('14.Short attr end bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,32.86,19,3,20,8,True,claRed,5,15,8,False,claBlue,0.97,Hx8fHx8///9/f39/X3///3///39ff////////1////8P/wf/D/8P//////////////////////8')]
    [TestCase('15.Short attr middle bold with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,35.75,27,2,3,8,True,claRed,5,1,8,True,claBlue,0.97,f38/Hx8fHx9/f39/X19fX39/f//fX19f//////////9//3//X/8f/1v/G/8b/xv///////////8')]
    [TestCase('16.Short attr middle big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,46.52,24,2,3,18,True,claRed,4,1,18,False,claBlue,0.97,Hw8PDw8fHz9/f39vT19ff3///29vf19/f///b29/f38gADgAOAA4ADgAOAAAAAAAAAAAAAAAAAA')]
    [TestCase('17.Short attr empty big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,42.29,24,2,0,18,True,claRed,3,1,18,False,claBlue,0.97,Hx8fHx8fHx9/f39/X19fX39////fX19f/////////////1H/Af8B/wn/Af////////////////8')]
    [TestCase('18.Short attr begin big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,38.71,24,0,3,18,True,claRed,2,2,8,False,claBlue,0.97,Hx8fHx8fHz9/f39/X19ff39////fX19/f3///99f33+AAOAA4ADgAOAA4AAAAAAAAAAAAAAAAAA')]
    [TestCase('19.Short attr end big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,46.5,24,3,20,18,True,claRed,5,15,18,False,claBlue,0.97,j48PDw8fHx///39vT19fX////+/vX19f///////////9/13/Hf8V/x3/Df////////////////8')]
    [TestCase('20.Short attr middle big bold with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,55.31,27,2,3,18,True,claRed,5,1,18,False,claBlue,0.97,r68PDw8PDw///39vT09PT///f+/vb19P//9/7+9vX08TABMAEgAfAB8AHwAfAB8AAAAAAAAAAAA')]
    {$ENDIF}
    procedure TestAttributes2(const AText: string; const ABitmapWidth, ABitmapHeight: Integer; const AScale, ATextLeft, ATextTop, AMaxWidth, AMaxHeight, AFontSize: Single; const AWordWrap, ARightToLeft: Boolean; const AHorizontalAlign, AVerticalAlign: TTextAlign; const ATrimming: TTextTrimming; const AColor: string; const AExpectedTextLeft, AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom: Single; const AAttr1RangePos, AAttr1RangeLength: Integer; const AAttr1FontSize: Single; const AAttr1FontBold: Boolean; const AAttr1Color: string; const AAttr2RangePos, AAttr2RangeLength: Integer; const AAttr2FontSize: Single; const AAttr2FontBold: Boolean; const AAttr2Color: string; const AMinSimilarity: Double; const AExpectedImageHash: string);
    {$IFDEF TEXT_RENDER}
    [TestCase('1.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,-0.3,99.62,39,0,100000,10,False,claRed,0,1,10,True,claLime,1,0,10,True,claDeeppink,0.97,//+rAwP/EwD///vjw//fzP//++vz/9/t////6/////9nuwAAAAD/6O/w//AAAAAAVMz//v//IgA')]
    [TestCase('2.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.04,100,29,0,100000,10,False,claRed,0,1,10,True,claLime,1,1,12,True,claDeeppink,0.97,/wEAAwMD/////fDjw8f////98ePrx/////3x4+/n//9//zc7AAAAAN/o//D//AAAAAAAAAAAAAA')]
    [TestCase('3.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,-0.29,100,30,0,100000,10,False,claRed,0,1,10,True,claLime,0,1,13,True,claDeeppink,0.97,/ykAAwMD/////fDjw8f/////9ePr///////14+v///////87AAAAAAAA//jv8KdQAAAAAAAAAAA')]
    [TestCase('4.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,-0.3,99.88,39,0,100000,10,False,claRed,0,2,10,True,claLime,0,1,10,True,claDeeppink,0.97,//srAwP/EwD///vjw//fyP//++vz/9/t////6/////+3uwAAAAD/6O/w//AAAAAA9sz//v//IgA')]
    [TestCase('5.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,100,32,0,100000,10,False,claRed,0,0,10,True,claLime,0,1,14,True,claDeeppink,0.97,/yEAf/8DA///fXB//0dP///99X//70////31f//vz///////BAAAAAAAAAD/+P/8AAAAAAAAAAA')]
    [TestCase('6.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,-0.3,98.01,39,0,100000,10,False,claRed,0,2,10,True,claLime,1,99999,10,True,claDeeppink,0.97,//uhAQH/BwH///Hhw//Pyf//8/Hn/8/N///3+f////1/9AAAAAD9///+//4AAAAAdGT//P/+IQA')]
    [TestCase('7.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,-0.3,98.01,39,0,100000,10,False,claRed,0,2,10,True,claLime,1,10001,10,True,claDeeppink,0.97,//uhAQH/BwH///Hhw//Pyf//8/Hn/8/N///3+f////1/9AAAAAD9///+//4AAAAAdGT//P/+IQA')]
    [TestCase('8.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,-0.3,98.01,39,0,100000,10,False,claRed,0,2,10,True,claLime,1,99998,10,True,claDeeppink,0.97,//uhAQH/BwH///Hhw//Pyf//8/Hn/8/N///3+f////1/9AAAAAD9///+//4AAAAAdGT//P/+IQA')]
    [TestCase('9.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,84.72,35,10,100000,12,False,claRed,0,1,14,True,claLime,1,0,10,True,claDeeppink,0.97,/wMDh3cDM/////Pn98f/////+/f37/////////////8AHwAf/v////////////////////////8')]
    [TestCase('10.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,84.72,35,10,100000,12,False,claRed,0,0,14,True,claLime,1,0,10,True,claDeeppink,0.97,/wMDB38DM/////Pn/8f/////+/f/7/////////////8CHwIf/v/+//////////////////////8')]
    [TestCase('11.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,84.72,35,10,100000,12,False,claRed,0,1,14,True,claLime,0,0,10,True,claDeeppink,0.97,/wMDh3cDM/////Pn98f/////+/f37/////////////8AHwAf/v////////////////////////8')]
    [TestCase('12.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,84.72,35,10,100000,12,False,claRed,0,1,14,True,claLime,0,1,10,True,claDeeppink,0.97,/wMDB28DM/////Pn78f/////+/fv7/////////////8AH4Af/v/+//////////////////////8')]
    [TestCase('13.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,84.72,35,10,100000,12,False,claRed,0,1,14,True,claLime,1,1,10,True,claDeeppink,0.97,/wMDg28DI/////Pj78fv////+/fv7+//////////7/9AP0Af/v////////////////////////8')]
    [TestCase('14.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,84.72,35,10,100000,12,False,claRed,0,1,14,True,claLime,3,3,10,True,claDeeppink,0.97,/wMDh2cDI/////Pn58fv////++fn7+//////////7/8cPxw//f////////////////////////8')]
    [TestCase('15.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,84.72,35,10,100000,12,False,claRed,1,1,14,True,claLime,2,1,10,True,claDeeppink,0.97,/wMDh28DI///f3Pnb0dv//9/+/fv72//////////b/8CHwIf/v////////////////////////8')]
    [TestCase('16.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,84.72,35,10,100000,12,False,claRed,1,1,14,True,claLime,3,0,10,True,claDeeppink,0.97,/wMBh3cDI/////Hn98fv////+ff37+//////////7/8AHwAf/v////////////////////////8')]
    [TestCase('17.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,99.15,35,10,100000,12,False,claRed,0,10,14,True,claLime,1,9,10,True,claDeeppink,0.97,/wAABR8PH///fHBlX09f///+/GV/T3////78ZX9ff/9//j/+AgAAAAAAQgD+AP4A/4AAAAAAAAA')]
    [TestCase('18.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,100,35,10,100000,12,False,claRed,0,11,14,True,claLime,1,9,10,True,claDeeppink,0.97,/wAADR8PH///fHBtX09f///99m1/T3///////3//f/9/H38f//////////////////////////8')]
    [TestCase('19.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,84.72,35,10,100000,12,False,claRed,0,10,14,True,claLime,8,4,10,True,claDeeppink,0.97,/xMB8yMDA///f3HzY0dP//9///vn72//////////b/8APwB//3//f/////////////////////8')]
    [TestCase('20.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,100,35,10,100000,12,False,claRed,0,11,14,True,claLime,1,9,10,True,claDeeppink,0.97,/wAADR8PH///fHBtX09f///99m1/T3///////3//f/9/H38f//////////////////////////8')]
    [TestCase('21.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,97.97,35,10,100000,12,False,claRed,0,11,14,True,claLime,1,10,10,True,claDeeppink,0.97,/wEABx8PH///fXBnX09f///9/Gd/T3////38Z39ff/9//j/+AgAAAAAAQgD+AP4A/4AAAAAAAAA')]
    [TestCase('22.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,97.45,35,10,100000,12,False,claRed,0,11,14,True,claLime,1,11,10,True,claDeeppink,0.97,/wEABR8PH///fXBlX09f///99GV/T3////30ZX9ff/9//j/+AgAAAAAAQgD+AP4A/4AAAAAAAAA')]
    [TestCase('23.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,100,35,10,100000,12,False,claRed,3,8,14,True,claLime,1,9,10,True,claDeeppink,0.97,/wAADx8PH///fHBvX09f//9+dn9/T3///////3//f/9/H38f//////////////////////////8')]
    [TestCase('24.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,93.29,38,10,8,12,False,claRed,11,3,14,True,claLime,13,3,10,True,claDeeppink,0.97,/wMDt/8BAff///P3/8fP////+/f/7+//////////7/8CHwIf/v//////Pf899zkHOQc5Bf////8')]
    [TestCase('25.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,94.46,38,10,10,12,False,claRed,11,6,14,True,claLime,12,2,10,True,claDeeppink,0.97,/wMDt/8hYXf///P3/+fv////+/f/7//////////////CnwIf/v//////3//H98dHx0fGRf////8')]
    {$ENDIF}
    procedure TestAttributes3(const AText: string; const ABitmapWidth, ABitmapHeight: Integer; const AScale, ATextLeft, ATextTop, AMaxWidth, AMaxHeight, AFontSize: Single; const AWordWrap, ARightToLeft: Boolean; const AHorizontalAlign, AVerticalAlign: TTextAlign; const ATrimming: TTextTrimming; const AColor: string; const AExpectedTextLeft, AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom: Single; const AAttr1RangePos, AAttr1RangeLength: Integer; const AAttr1FontSize: Single; const AAttr1FontBold: Boolean; const AAttr1Color: string; const AAttr2RangePos, AAttr2RangeLength: Integer; const AAttr2FontSize: Single; const AAttr2FontBold: Boolean; const AAttr2Color: string;  const AAttr3RangePos, AAttr3RangeLength: Integer; const AAttr3FontSize: Single; const AAttr3FontBold: Boolean; const AAttr3Color: string; const AMinSimilarity: Double; const AExpectedImageHash: string);
    {$IFDEF TEXT_RENDER}
    [TestCase('1.Short attr middle', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,2,3,claRed,0.97,Hx8fHx8f//9/f39/X1///39/f39fX////////3////87/zv/O/87//////////////////////8')]
    [TestCase('2.Short attr empty', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,2,0,claRed,0.97,Hx8fHx8f//9/f39/X1///3///39fX/////////////8D/wP/A/8D//////////////////////8')]
    [TestCase('3.Short attr begin', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,0,3,claRed,0.97,Hx8fHx8f//9/f39/X1///3///39fX//////////////x/+H/4f/h//////////////////////8')]
    [TestCase('4.Short attr end', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,3,20,claRed,0.97,Hx8fHx8f//9/f39/X1///3///39fX////////1////8P/w//D/8P//////////////////////8')]
    [TestCase('5.Short attr middle with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,57.43,27,2,3,claRed,0.97,r6cHBwcHDx///3dnR0dPX///d/f3d09f//////////9/f39/X38ff19/H38ffx9///////////8')]
    {$ENDIF}
    procedure TestAttributesFontNil(const AText: string; const ABitmapWidth, ABitmapHeight: Integer; const AScale, ATextLeft, ATextTop, AMaxWidth, AMaxHeight, AFontSize: Single; const AWordWrap, ARightToLeft: Boolean; const AHorizontalAlign, AVerticalAlign: TTextAlign; const ATrimming: TTextTrimming; const AColor: string; const AExpectedTextLeft, AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom: Single; const AAttrRangePos, AAttrRangeLength: Integer; const AAttrColor: string; const AMinSimilarity: Double; const AExpectedImageHash: string);
    {$IFDEF TEXT_RENDER}
    [TestCase('1.Short attr middle', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,32.08,19,2,3,8,False,claRed,0.97,Hx8fHx8///9/f39/X3///39/f39ff////////3////83/zf/N/83//////////////////////8')]
    [TestCase('2.Short attr empty', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,2,0,8,False,claRed,0.97,Hx8fHx8f//9/f39/X1///3///39fX/////////////8D/wP/A/8D//////////////////////8')]
    [TestCase('3.Short attr begin', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,30.8,19,0,3,8,False,claRed,0.97,Hx8fHx8///9/f39/X3///3////9ff//////////////H/8f/x//H//////////////////////8')]
    [TestCase('4.Short attr end', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,32.38,19,3,20,8,False,claRed,0.97,Hx8fHx8ff/9/f39/X19//3///39fX3///////1//f/8P/w//D/8P//////////////////////8')]
    [TestCase('5.Short attr middle with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,41.2,27,2,3,8,False,claRed,0.97,X18fDw8fHx9/f39vT19fX39/f+/vf39f//////////97/3v/W/8b/1v/G/8b/xv///////////8')]
    [TestCase('6.Short attr middle big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,45.61,24,2,3,18,False,claRed,0.97,Hw8PDw8PHz9/f39vT09ff3///29vb19/f///b29vf38kADwAPAA8ADwAPAAAAAAAAAAAAAAAAAA')]
    [TestCase('7.Short attr empty big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,2,0,18,False,claRed,0.97,Hx8fHx8f//9/f39/X1///3///39fX/////////////8D/wP/A/8D//////////////////////8')]
    [TestCase('8.Short attr begin big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,46.47,24,0,3,18,False,claRed,0.97,Hx8PDw8PHx9/f39vT09fX3///+9PX19ff///719f31+QAPAA+ADwAPAA8AAAAAAAAAAAAAAAAAA')]
    [TestCase('9.Short attr end big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,45.42,24,3,20,18,False,claRed,0.97,jw8PDw8PHz//f39vT09ff/9//+/vT19//3//7+9PX38GAA4ADgAfAA4ADgAAAAAAAAAAAAAAAAA')]
    [TestCase('10.Short attr middle big with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,54.73,27,2,3,18,False,claRed,0.97,768PDw8PDw///39vT09PT///f//ff09P//////////9/f35/Xn8ff19/H38ffx9///////////8')]
    [TestCase('11.Short attr middle bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,32.82,19,2,3,8,True,claRed,0.97,Hx8fHx8f//9/f39/X1///39/f39fX////////3////83/zf/N/83//////////////////////8')]
    [TestCase('12.Short attr empty bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,2,0,8,True,claRed,0.97,Hx8fHx8f//9/f39/X1///3///39fX/////////////8D/wP/A/8D//////////////////////8')]
    [TestCase('13.Short attr begin bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,31.61,19,0,3,8,True,claRed,0.97,Hx8fHx8///9/f39/X3///39///9ff//////////////H/8f/x//H//////////////////////8')]
    [TestCase('14.Short attr end bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,33.15,19,3,20,8,True,claRed,0.97,Hx8fHx8f//9/f39/X1///3///39fX////////1////8P/w//D/8f//////////////////////8')]
    [TestCase('15.Short attr middle bold with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,41.93,27,2,3,8,True,claRed,0.97,f18fDw8fHx9/f39vT19fX39/f+/v/39f//////////99/3n/Wf8Z/1n/Gf8Z/xn///////////8')]
    [TestCase('16.Short attr middle big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,47.28,24,2,3,18,True,claRed,0.97,jw8PDw8PHz//f39vT09ff////29PT19/////b29Pf38mADwAPAA+AD4APgAAAAAAAAAAAAAAAAA')]
    [TestCase('17.Short attr empty big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,2,0,18,True,claRed,0.97,Hx8fHx8f//9/f39/X1///3///39fX/////////////8D/wP/A/8D//////////////////////8')]
    [TestCase('18.Short attr begin big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,48.31,24,0,3,18,True,claRed,0.97,Px8PDw8fHz/////vz9/f/////+/P39//////79/f3/+QAPgA+AD4APgA+AAAAAAAAAAAAAAAAAA')]
    [TestCase('19.Short attr end big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,47.15,24,3,20,18,True,claRed,0.97,j48PDw8PH9///39vT09f3////+/vT1/f/////////////1//H/8X/x//D//v//////////////8')]
    [TestCase('20.Short attr middle big bold with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,56.39,27,2,3,18,True,claRed,0.97,r68PBwcPDw///39nR09PT///f+fnb19P//////////9/f35/X38ff19/H38ffx9///////////8')]
    {$ENDIF}
    procedure TestAttributesStrikeOut(const AText: string; const ABitmapWidth, ABitmapHeight: Integer; const AScale, ATextLeft, ATextTop, AMaxWidth, AMaxHeight, AFontSize: Single; const AWordWrap, ARightToLeft: Boolean; const AHorizontalAlign, AVerticalAlign: TTextAlign; const ATrimming: TTextTrimming; const AColor: string; const AExpectedTextLeft, AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom: Single; const AAttrRangePos, AAttrRangeLength: Integer; const AAttrFontSize: Single; const AAttrFontBold: Boolean; const AAttrColor: string; const AMinSimilarity: Double; const AExpectedImageHash: string);
    {$IFDEF TEXT_RENDER}
    [TestCase('1.Short attr middle', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,32.08,19,2,3,8,False,claRed,0.97,Hx8fHx8ff/9/f39/X19//39/f3/fX3//////////f/83/zf/N/83//////////////////////8')]
    [TestCase('2.Short attr empty', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,2,0,8,False,claRed,0.97,Hx8fHx8f//9/f39/X1///3///39fX/////////////8D/wP/A/8D//////////////////////8')]
    [TestCase('3.Short attr begin', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,30.8,19,0,3,8,False,claRed,0.97,Hx8fHx8/f/9/f39/X39//3////9ff3/////////////H/8f/x//H//////////////////////8')]
    [TestCase('4.Short attr end', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,32.38,19,3,20,8,False,claRed,0.97,Px8fHx8ff/9/f39/X19//3///39fX3///////1//f/8P/w//D/8P//////////////////////8')]
    [TestCase('5.Short attr middle with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,41.2,27,2,3,8,False,claRed,0.97,X18fDw8fHx9/f39vT19fX39/f+/vf39f//////////97/3v/W/8b/1v/G/8b/xv///////////8')]
    [TestCase('6.Short attr middle big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,45.61,24,2,3,18,False,claRed,0.97,Hx8PDw8PHx9/f39vT09fX3///29vz19ff///b2/Pf18kADwAPAA8ADwAPAA0ADwAAAAAAAAAAAA')]
    [TestCase('7.Short attr empty big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,2,0,18,False,claRed,0.97,Hx8fHx8f//9/f39/X1///3///39fX/////////////8D/wP/A/8D//////////////////////8')]
    [TestCase('8.Short attr begin big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,46.47,24,0,3,18,False,claRed,0.97,Px8PDw8PHx9/f39vT09fX3///+9PX19ff///719f31+QAPAA+ADwAPAA8ABoAPgAAAAAAAAAAAA')]
    [TestCase('9.Short attr end big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,45.42,24,3,20,18,False,claRed,0.97,zw8PDw8PDw//f39vT09PT/9//+/vT09P/3//7+9PT08GAA4ADgAOAAYADgAfAB8AAAAAAAAAAAA')]
    [TestCase('10.Short attr middle big with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,54.73,27,2,3,18,False,claRed,0.97,768PDw8PDw///39vT09PT///f///f89P//////////9/f35/Xn8ff19/H38ffx9//f////////8')]
    [TestCase('11.Short attr middle bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,32.82,19,2,3,8,True,claRed,0.97,Hx8fHx8fv/9/f39/X1///39/f3/fX/////////////83/zf/N/83//////////////////////8')]
    [TestCase('12.Short attr empty bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,2,0,8,True,claRed,0.97,Hx8fHx8f//9/f39/X1///3///39fX/////////////8D/wP/A/8D//////////////////////8')]
    [TestCase('13.Short attr begin bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,31.61,19,0,3,8,True,claRed,0.97,Hx8fHx8/f/9/f39/X39//39///9ff3/////////////H/8f/x//H//////////////////////8')]
    [TestCase('14.Short attr end bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,33.15,19,3,20,8,True,claRed,0.97,Hx8fHx8f//9/f39/X1///3///39fX////////1////8P/w//D/8P//////////////////////8')]
    [TestCase('15.Short attr middle bold with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,41.93,27,2,3,8,True,claRed,0.97,f18fDw8fHx9/f39vT19fX39/f+/vf39f//////////99/3n/Wf8Z/1n/Gf8Z/xn///////////8')]
    [TestCase('16.Short attr middle big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,47.28,24,2,3,18,True,claRed,0.97,jw8PDw8PHx//f39vT09fX////29Pz19f////b2/Pf18mADwAPAA+AD4APgAAAD4AAAAAAAAAAAA')]
    [TestCase('17.Short attr empty big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,2,0,18,True,claRed,0.97,Hx8fHx8f//9/f39/X1///3///39fX/////////////8D/wP/A/8D//////////////////////8')]
    [TestCase('18.Short attr begin big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,48.31,24,0,3,18,True,claRed,0.97,Px8PDw8fHx/////vz9/f3////+/P39/f////79/f39+QAPgA+AD4APgA+AAAAPgAAAAAAAAAAAA')]
    [TestCase('19.Short attr end big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,47.15,24,3,20,18,True,claRed,0.97,z48PDw8PD4///39vT09Pz////+/vT0/P////7+9PT88GAA4AHgAWAB4ADgAfAB8AAAAAAAAAAAA')]
    [TestCase('20.Short attr middle big bold with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,56.39,27,2,3,18,True,claRed,0.97,r6+PDw8PDw////9vT09PT////+/vb89P////7+9vz08TABIAEwAfAB8AHwAfAB4AHwAAAAAAAAA')]
    {$ENDIF}
    procedure TestAttributesUnderline(const AText: string; const ABitmapWidth, ABitmapHeight: Integer; const AScale, ATextLeft, ATextTop, AMaxWidth, AMaxHeight, AFontSize: Single; const AWordWrap, ARightToLeft: Boolean; const AHorizontalAlign, AVerticalAlign: TTextAlign; const ATrimming: TTextTrimming; const AColor: string; const AExpectedTextLeft, AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom: Single; const AAttrRangePos, AAttrRangeLength: Integer; const AAttrFontSize: Single; const AAttrFontBold: Boolean; const AAttrColor: string; const AMinSimilarity: Double; const AExpectedImageHash: string);
    {$IFDEF TEXT_RENDER}
    [TestCase('1', ShortText + ',9,false,false,Leading,Leading,Character,claBlack,0,0.03,25.84,12,0.97,///vAgMDA//////jw8PL//////vv88v/////////////+eSd9X3nHfV9BJ3///////////////8')]
    [TestCase('2', ShortText + ',10,false,false,Leading,Leading,Character,claBlack,0,-0.3,28.72,13,0.97,///+AAACA/////7hw8fP/////+nL78////////////9/9X/xZJV1pWeFdbUElf////////////8')]
    [TestCase('3', ShortText + ',11,false,false,Leading,Leading,Character,claBlack,0,0.37,31.59,15,0.97,///twAEDA/////3hQ0dP/////elb30////////////9/9X/xZJV0tWcVZzVktQSV//////////8')]
    [TestCase('4', ShortText + ',12,false,false,Leading,Leading,Character,claBlack,0,0.04,34.46,16,0.97,///8CAAAC/////xpQ0dP/////X1Pz1////////////999X3xffVklXQ1ZRVldXS1BJX///////8')]
    [TestCase('5', ShortText + ',13,false,false,Leading,Leading,Character,claBlack,0,-0.29,37.33,17,0.97,///OAAICD/////5hQ0dP/////mlL70////////////999X3xZJVkhXUlZQVENQSVBJ3///////8')]
    [TestCase('6', ShortText + ',14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,0.97,//9eQAICAw///35hQ0dPT///f2lL709P//////////995X3hbMFklXQFZQVEJQSlBJX///////8')]
    [TestCase('7', ShortText + ',15,false,false,Leading,Leading,Character,claBlack,0,0.05,43.07,20,0.97,//9tAQEBB////31hQ0dP////fWlL72////////////955XnhYJFklXUFZQVkJWSlBJX///////8')]
    [TestCase('8', ShortText + ',16,false,false,Leading,Leading,Character,claBlack,0,-0.28,45.95,21,0.97,///sAAAAB/////xhQ0dP/////WVL71////////////999X31ZNFkFXUFZRVlNQSVrN3///////8')]
    [TestCase('9', ShortText + ',17,false,false,Leading,Leading,Character,claBlack,0,0.39,48.82,23,0.97,//9sQAAAB////3xhQ0dP////fW1Pz1////////////999X3xZJVkFWUlZQVlNWS1BJX///////8')]
    [TestCase('10', ShortText + ',18,false,false,Leading,Leading,Character,claBlack,0,0.06,51.69,24,0.97,//9MAAABB////3xhQ0dP////fWVLx0////////////999X3xbNFklXUlZQVlNWS1BJX///////8')]
    [TestCase('11', ShortText + ',19,false,false,Leading,Leading,Character,claBlack,0,-0.27,54.56,25,0.97,///MAAAAB/////xhQ0dP/////W1L70/////////////99331ffFklXWlZQVlNWS1BJX///////8')]
    [TestCase('12', ShortText + ',20,false,false,Leading,Leading,Character,claBlack,0,0.4,57.43,27,0.97,///+QAAAA3////5hQ0dPf////2lP509////////////593n1efFklWQVdQVlNWS1BJX///////8')]
    [TestCase('13', ShortText + ',21,false,false,Leading,Leading,Character,claBlack,0,0.07,60.3,28,0.97,///8QAAAA/////xhQ0dP/////W1Pz0////////////999331ffFklXWlZQVlNWS1BJX///////8')]
    [TestCase('14', ShortText + ',22,false,false,Leading,Leading,Character,claBlack,0,-0.26,63.17,29,0.97,///sQAAAA/////xhQ0dP/////GlLb0////////////999X3xffVklXQVZRVlNWS1BJX///////8')]
    [TestCase('15', 'This is test text,14,false,false,Leading,Leading,Character,claBlack,0,0.38,91.41,19,0.97,f39/86CAgID////z48fOzP////fz197M//////////8b/z7WPtYIEAoQSBBQVEBQSBD///////8')]
    {$ENDIF}
    procedure TestTrimmingWithMaxWidthVeryFit(const AText: string; const AFontSize: Single; const AWordWrap, ARightToLeft: Boolean; const AHorizontalAlign, AVerticalAlign: TTextAlign; const ATrimming: TTextTrimming; const AColor: string; const AExpectedTextLeft, AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [Setup]
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
  FMX.Graphics;

const
  TextRectEpsilon = 2;

{ TSkTextLayoutTests }

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
    True, nil, AMinSimilarity, AExpectedImageHash);
end;

procedure TSkTextLayoutTests.Setup;
begin
  inherited;
  RegisterFontFiles(TSkDefaultProviders.TypefaceFont);
end;

procedure TSkTextLayoutTests.Test(const AText: string; const ABitmapSize: TSize;
  const AScale, AFontSize: Single; const ATextTopLeft, AMaxSize: TPointF;
  const AWordWrap, ARightToLeft: Boolean; const AHorizontalAlign,
  AVerticalAlign: TTextAlign; const ATrimming: TTextTrimming;
  const AColor: TAlphaColor; const AExpectedTextRect: TRectF;
  const ACheckTextRect: Boolean;
  const AAttributes: TArray<TTextAttributedRange>; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LLayout: TTextLayout;
  I: Integer;
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
          LLayout.Font.Family := DefaultFontFamily;
          LLayout.Font.Size := AFontSize;
          LLayout.Color := AColor;
          LLayout.RightToLeft := ARightToLeft;
          LLayout.Trimming := ATrimming;
          for I := 0 to Length(AAttributes) - 1 do
            LLayout.AddAttribute(AAttributes[I]);
        finally
          LLayout.EndUpdate;
        end;
        if ACheckTextRect then
          Assert.AreSameRect(AExpectedTextRect, LLayout.TextRect, TextRectEpsilon);
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

procedure TSkTextLayoutTests.TestAttributes1(const AText: string;
  const ABitmapWidth, ABitmapHeight: Integer; const AScale, ATextLeft, ATextTop,
  AMaxWidth, AMaxHeight, AFontSize: Single; const AWordWrap,
  ARightToLeft: Boolean; const AHorizontalAlign, AVerticalAlign: TTextAlign;
  const ATrimming: TTextTrimming; const AColor: string; const AExpectedTextLeft,
  AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom: Single;
  const AAttrRangePos, AAttrRangeLength: Integer; const AAttrFontSize: Single;
  const AAttrFontBold: Boolean; const AAttrColor: string;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LFont: TFont;
begin
  LFont := TFont.Create;
  try
    LFont.Size := AAttrFontSize;
    if AAttrFontBold then
      LFont.Style := LFont.Style + [TFontStyle.fsBold];
    Test(AText, TSize.Create(ABitmapWidth, ABitmapHeight), AScale, AFontSize,
      PointF(ATextLeft, ATextTop), PointF(AMaxWidth, AMaxHeight), AWordWrap,
      ARightToLeft, AHorizontalAlign, AVerticalAlign, ATrimming, StringToAlphaColor(AColor),
      RectF(AExpectedTextLeft, AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom),
      True, [TTextAttributedRange.Create(TTextRange.Create(AAttrRangePos, AAttrRangeLength),
        TTextAttribute.Create(LFont, StringToAlphaColor(AAttrColor)))],
      AMinSimilarity, AExpectedImageHash);
  finally
    {$IF CompilerVersion >= 32}
    LFont.Free;
    {$ENDIF}
  end;
end;

procedure TSkTextLayoutTests.TestAttributes2(const AText: string;
  const ABitmapWidth, ABitmapHeight: Integer; const AScale, ATextLeft, ATextTop,
  AMaxWidth, AMaxHeight, AFontSize: Single; const AWordWrap,
  ARightToLeft: Boolean; const AHorizontalAlign, AVerticalAlign: TTextAlign;
  const ATrimming: TTextTrimming; const AColor: string; const AExpectedTextLeft,
  AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom: Single;
  const AAttr1RangePos, AAttr1RangeLength: Integer;
  const AAttr1FontSize: Single; const AAttr1FontBold: Boolean;
  const AAttr1Color: string; const AAttr2RangePos,
  AAttr2RangeLength: Integer; const AAttr2FontSize: Single;
  const AAttr2FontBold: Boolean; const AAttr2Color: string;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LFont1: TFont;
  LFont2: TFont;
begin
  LFont1 := TFont.Create;
  LFont2 := TFont.Create;
  try
    LFont1.Size := AAttr1FontSize;
    if AAttr1FontBold then
      LFont1.Style := LFont1.Style + [TFontStyle.fsBold];
    LFont2.Size := AAttr2FontSize;
    if AAttr2FontBold then
      LFont2.Style := LFont2.Style + [TFontStyle.fsBold];

    Test(AText, TSize.Create(ABitmapWidth, ABitmapHeight), AScale, AFontSize,
      PointF(ATextLeft, ATextTop), PointF(AMaxWidth, AMaxHeight), AWordWrap,
      ARightToLeft, AHorizontalAlign, AVerticalAlign, ATrimming, StringToAlphaColor(AColor),
      RectF(AExpectedTextLeft, AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom),
      True, [TTextAttributedRange.Create(TTextRange.Create(AAttr1RangePos, AAttr1RangeLength),
        TTextAttribute.Create(LFont1, StringToAlphaColor(AAttr1Color))),
        TTextAttributedRange.Create(TTextRange.Create(AAttr2RangePos, AAttr2RangeLength),
        TTextAttribute.Create(LFont2, StringToAlphaColor(AAttr2Color)))],
      AMinSimilarity, AExpectedImageHash);
  finally
    {$IF CompilerVersion >= 32}
    LFont1.Free;
    LFont2.Free;
    {$ENDIF}
  end;
end;

procedure TSkTextLayoutTests.TestAttributes3(const AText: string;
  const ABitmapWidth, ABitmapHeight: Integer; const AScale, ATextLeft, ATextTop,
  AMaxWidth, AMaxHeight, AFontSize: Single; const AWordWrap,
  ARightToLeft: Boolean; const AHorizontalAlign, AVerticalAlign: TTextAlign;
  const ATrimming: TTextTrimming; const AColor: string; const AExpectedTextLeft,
  AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom: Single;
  const AAttr1RangePos, AAttr1RangeLength: Integer;
  const AAttr1FontSize: Single; const AAttr1FontBold: Boolean;
  const AAttr1Color: string; const AAttr2RangePos, AAttr2RangeLength: Integer;
  const AAttr2FontSize: Single; const AAttr2FontBold: Boolean;
  const AAttr2Color: string; const AAttr3RangePos, AAttr3RangeLength: Integer;
  const AAttr3FontSize: Single; const AAttr3FontBold: Boolean;
  const AAttr3Color: string; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LFont1: TFont;
  LFont2: TFont;
  LFont3: TFont;
begin
  LFont1 := TFont.Create;
  LFont2 := TFont.Create;
  LFont3 := TFont.Create;
  try
    LFont1.Size := AAttr1FontSize;
    if AAttr1FontBold then
      LFont1.Style := LFont1.Style + [TFontStyle.fsBold];
    LFont2.Size := AAttr2FontSize;
    if AAttr2FontBold then
      LFont2.Style := LFont2.Style + [TFontStyle.fsBold];
    LFont3.Size := AAttr3FontSize;
    if AAttr3FontBold then
      LFont3.Style := LFont3.Style + [TFontStyle.fsBold];

    Test(AText, TSize.Create(ABitmapWidth, ABitmapHeight), AScale, AFontSize,
      PointF(ATextLeft, ATextTop), PointF(AMaxWidth, AMaxHeight), AWordWrap,
      ARightToLeft, AHorizontalAlign, AVerticalAlign, ATrimming, StringToAlphaColor(AColor),
      RectF(AExpectedTextLeft, AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom),
      True, [TTextAttributedRange.Create(TTextRange.Create(AAttr1RangePos, AAttr1RangeLength),
        TTextAttribute.Create(LFont1, StringToAlphaColor(AAttr1Color))),
        TTextAttributedRange.Create(TTextRange.Create(AAttr2RangePos, AAttr2RangeLength),
        TTextAttribute.Create(LFont2, StringToAlphaColor(AAttr2Color))),
        TTextAttributedRange.Create(TTextRange.Create(AAttr3RangePos, AAttr3RangeLength),
        TTextAttribute.Create(LFont3, StringToAlphaColor(AAttr3Color)))],
      AMinSimilarity, AExpectedImageHash);
  finally
    {$IF CompilerVersion >= 32}
    LFont1.Free;
    LFont2.Free;
    LFont3.Free;
    {$ENDIF}
  end;
end;

procedure TSkTextLayoutTests.TestAttributesFontNil(const AText: string;
  const ABitmapWidth, ABitmapHeight: Integer; const AScale, ATextLeft, ATextTop,
  AMaxWidth, AMaxHeight, AFontSize: Single; const AWordWrap,
  ARightToLeft: Boolean; const AHorizontalAlign, AVerticalAlign: TTextAlign;
  const ATrimming: TTextTrimming; const AColor: string; const AExpectedTextLeft,
  AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom: Single;
  const AAttrRangePos, AAttrRangeLength: Integer; const AAttrColor: string;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  Test(AText, TSize.Create(ABitmapWidth, ABitmapHeight), AScale, AFontSize,
    PointF(ATextLeft, ATextTop), PointF(AMaxWidth, AMaxHeight), AWordWrap,
    ARightToLeft, AHorizontalAlign, AVerticalAlign, ATrimming, StringToAlphaColor(AColor),
    RectF(AExpectedTextLeft, AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom),
    True, [TTextAttributedRange.Create(TTextRange.Create(AAttrRangePos, AAttrRangeLength),
      TTextAttribute.Create(nil, StringToAlphaColor(AAttrColor)))],
    AMinSimilarity, AExpectedImageHash);
end;

procedure TSkTextLayoutTests.TestAttributesStrikeOut(const AText: string;
  const ABitmapWidth, ABitmapHeight: Integer; const AScale, ATextLeft, ATextTop,
  AMaxWidth, AMaxHeight, AFontSize: Single; const AWordWrap,
  ARightToLeft: Boolean; const AHorizontalAlign, AVerticalAlign: TTextAlign;
  const ATrimming: TTextTrimming; const AColor: string; const AExpectedTextLeft,
  AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom: Single;
  const AAttrRangePos, AAttrRangeLength: Integer; const AAttrFontSize: Single;
  const AAttrFontBold: Boolean; const AAttrColor: string;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LFont: TFont;
begin
  LFont := TFont.Create;
  try
    LFont.Size := AAttrFontSize;
    LFont.Style := [TFontStyle.fsStrikeOut];
    if AAttrFontBold then
      LFont.Style := LFont.Style + [TFontStyle.fsBold];
    Test(AText, TSize.Create(ABitmapWidth, ABitmapHeight), AScale, AFontSize,
      PointF(ATextLeft, ATextTop), PointF(AMaxWidth, AMaxHeight), AWordWrap,
      ARightToLeft, AHorizontalAlign, AVerticalAlign, ATrimming, StringToAlphaColor(AColor),
      RectF(AExpectedTextLeft, AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom),
      True, [TTextAttributedRange.Create(TTextRange.Create(AAttrRangePos, AAttrRangeLength),
        TTextAttribute.Create(LFont, StringToAlphaColor(AAttrColor)))],
      AMinSimilarity, AExpectedImageHash);
  finally
    {$IF CompilerVersion >= 32}
    LFont.Free;
    {$ENDIF}
  end;
end;

procedure TSkTextLayoutTests.TestAttributesUnderline(const AText: string;
  const ABitmapWidth, ABitmapHeight: Integer; const AScale, ATextLeft, ATextTop,
  AMaxWidth, AMaxHeight, AFontSize: Single; const AWordWrap,
  ARightToLeft: Boolean; const AHorizontalAlign, AVerticalAlign: TTextAlign;
  const ATrimming: TTextTrimming; const AColor: string; const AExpectedTextLeft,
  AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom: Single;
  const AAttrRangePos, AAttrRangeLength: Integer; const AAttrFontSize: Single;
  const AAttrFontBold: Boolean; const AAttrColor: string;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LFont: TFont;
begin
  LFont := TFont.Create;
  try
    LFont.Size := AAttrFontSize;
    LFont.Style := [TFontStyle.fsUnderline];
    if AAttrFontBold then
      LFont.Style := LFont.Style + [TFontStyle.fsBold];
    Test(AText, TSize.Create(ABitmapWidth, ABitmapHeight), AScale, AFontSize,
      PointF(ATextLeft, ATextTop), PointF(AMaxWidth, AMaxHeight), AWordWrap,
      ARightToLeft, AHorizontalAlign, AVerticalAlign, ATrimming, StringToAlphaColor(AColor),
      RectF(AExpectedTextLeft, AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom),
      True, [TTextAttributedRange.Create(TTextRange.Create(AAttrRangePos, AAttrRangeLength),
        TTextAttribute.Create(LFont, StringToAlphaColor(AAttrColor)))],
      AMinSimilarity, AExpectedImageHash);
  finally
    {$IF CompilerVersion >= 32}
    LFont.Free;
    {$ENDIF}
  end;
end;

procedure TSkTextLayoutTests.TestTrimmingWithMaxWidthVeryFit(
  const AText: string; const AFontSize: Single; const AWordWrap,
  ARightToLeft: Boolean; const AHorizontalAlign, AVerticalAlign: TTextAlign;
  const ATrimming: TTextTrimming; const AColor: string; const AExpectedTextLeft,
  AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LLayout: TTextLayout;
begin
  LBitmap := TBitmap.Create;
  try
    LLayout := TTextLayoutManager.DefaultTextLayout.Create(LBitmap.Canvas);
    try
      LLayout.BeginUpdate;
      try
        LLayout.Text := AText;
        LLayout.WordWrap := AWordWrap;
        LLayout.HorizontalAlign := AHorizontalAlign;
        LLayout.VerticalAlign := AVerticalAlign;
        LLayout.Font.Family := DefaultFontFamily;
        LLayout.Font.Size := AFontSize;
        LLayout.Color := StringToAlphaColor(AColor);
        LLayout.RightToLeft := ARightToLeft;
        LLayout.Trimming := ATrimming;
      finally
        LLayout.EndUpdate;
      end;

      Assert.AreSameRect(RectF(AExpectedTextLeft, AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom), LLayout.TextRect, TextRectEpsilon);
      LLayout.MaxSize := LLayout.TextRect.Size;

      LBitmap.SetSize(Ceil(LLayout.MaxSize.X), Ceil(LLayout.MaxSize.Y));
      LBitmap.Canvas.BeginScene;
      try
        LBitmap.Canvas.Clear(TAlphaColors.Null);
        LLayout.RenderLayout(LBitmap.Canvas);
      finally
        LBitmap.Canvas.EndScene;
      end;
    finally
      LLayout.Free;
    end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TSkTextLayoutTests);
end.
