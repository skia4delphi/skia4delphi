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
    [TestCase('1.Short', ShortText + ',100,40,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,0.97,0kPy+vOpM56C/J1TT0X1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('2.Short blue', ShortText + ',100,40,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,#FF9F9FFF,0,0.38,40.2,19,0.97,0/+BPPOrW/6C/LK7X0T1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('3.Short horizontal leading', ShortText + ',100,40,1,0,0,100,40,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,0.97,0kPy+vOpM56C/J1TT0X1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('4.Short horizontal center', ShortText + ',100,40,1,0,0,100,40,14,false,false,Center,Leading,Character,claBlack,29.899,0.38,70.1,19,0.97,yY9Ce/i3W77A8LB7vwX0hKCufPCLY3D+4ttbUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('5.Short horizontal trailing', ShortText + ',100,40,1,0,0,100,40,14,false,false,Trailing,Leading,Character,claBlack,59.8,0.38,100,19,0.97,1VeyOvjAV15B9aCDDwb0o/h+vumIc+B87NlPUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('6.Short vertical center', ShortText + ',100,40,1,0,0,100,40,14,false,false,Leading,Center,Character,claBlack,0,11.07,40.2,29.69,0.97,1/cBe/rXa/7A9Z+Pr0b1lawOPvCVV1A96OCDAf0etKtfe+UUkI8490pUX8nq4ANfeQXVgw4+GA')]
    [TestCase('7.Short horizontal center, vertical center', ShortText + ',100,40,1,0,0,100,40,14,false,false,Center,Center,Character,claBlack,29.9,11.07,70.1,29.69,0.97,2kdC+vnHez4B9597L4b1hcjOvfOSZ6D95uSDIT0krCsfPOgPdI/6/VhYz0fs5/seuQXjN879EQ')]
    [TestCase('8.Short horizontal trailing, vertical center', ShortText + ',100,40,1,0,0,100,40,14,false,false,Trailing,Center,Character,claBlack,59.8,11.07,100,29.69,0.97,1VeyOvjAV15B9aCDDwb0g5TevvKlT7D95uWbUX0lq4MPfOUVVA86+GBAn0jr3d/eOQTgXw4/Fw')]
    [TestCase('9.Short vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,false,false,Leading,Trailing,Character,claBlack,0,21.76,40.2,40.38,0.97,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDEb0kslvvuuQprP859FxUX8nq4ANfeQXVgw4+GA')]
    [TestCase('10.Short horizontal center, vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,false,false,Center,Trailing,Character,claBlack,29.9,21.76,70.1,40.38,0.97,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDwTworYe/e+YGBP96911Mf0jqyQ8/uQbjP149Fg')]
    [TestCase('11.Short horizontal trailing, vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,false,false,Trailing,Trailing,Character,claBlack,59.8,21.76,100,40.38,0.97,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GAg/wnv6q8O+QXKx379Fg')]
    [TestCase('12.Long', LongText + ',100,40,1,0,0,65535,65535,14,true,false,Leading,Leading,Character,claBlack,0,0.38,6642.3398,19,0.97,xVcyu/WpQ34C+KFPn0Tvl8RePeyNc1D95dBfUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('13.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,false,Leading,Leading,Character,claBlack,0,0.38,98.84,38,0.97,xVcyu/WpQ34C+KFPn0TyjrT+vOyMk1C+49BnIbwit1sfu+EcjA/69UhQX0ns16eueAnpj479Eg')]
    [TestCase('14.Long horizontal center', LongText + ',100,40,1,0,0,100,40,14,true,false,Center,Leading,Character,claBlack,0.58,0.38,99.42,38,0.97,zpeCu/mqMy4C9KpvXwX2mcxuPeuIoyA+6dNXMXwjtlcf++EejD/69UZMPwns1quOeAnoh569Eg')]
    [TestCase('15.Long horizontal trailing', LongText + ',100,40,1,0,0,100,40,14,true,false,Trailing,Leading,Character,claBlack,3.83,0.38,103.83,38,0.97,yIuyO/utc46B8raHT4Xwn+DeveqQb2C96N5Xsfwktl8f++EhiD869kVEDwns1KN+eAnoc759Eg')]
    [TestCase('16.Long vertical center', LongText + ',100,40,1,0,0,100,40,14,true,false,Leading,Center,Character,claBlack,0,1.57,98.84,39.19,0.97,w1Mie/aqY35C9qJTH0XyjcQe/e2UlxC+5NB78Tsks0sf++IedO+59FFQnwns17OeOArmi049FA')]
    [TestCase('17.Long horizontal center, vertical center', LongText + ',100,40,1,0,0,100,40,14,true,false,Center,Center,Character,claBlack,0.58,1.57,99.42,39.19,0.97,yYNy+/irS74B861r30X1l9SufeyPm6A96dF3AXwks0cv++IfeB969E9Ij4nr1rd++Anjg079Ew')]
    [TestCase('18.Long horizontal trailing, vertical center', LongText + ',100,40,1,0,0,100,40,14,true,false,Trailing,Center,Character,claBlack,3.83,1.57,103.83,39.19,0.97,xYcSO/uqf/5A8a9738Xvn8zefeuZU/B86NprcXwltEsPu+IjdC/69Ew8X4nr1KtOuAnic169Ew')]
    [TestCase('19.Long vertical trailing', LongText + ',100,40,1,0,0,100,40,14,true,false,Leading,Trailing,Character,claBlack,0,2.76,98.84,40.38,0.97,wz8i+/Wuc16C9KBrj4Xyicwe/e+cm+D95dR/8TslsFNvO+YdXP/581lQf8nr2sMOOQrhly79FQ')]
    [TestCase('20.Long horizontal center, vertical trailing', LongText + ',100,40,1,0,0,100,40,14,true,false,Center,Trailing,Character,claBlack,0.58,2.76,99.42,40.38,0.97,xW8y+/erX07B8a5rL0b0k9y+/e2TlxD96NKP8XsksUtvu+UdYC+681hMb0nr2cfOuAndiy79FQ')]
    [TestCase('21.Long horizontal trailing, vertical trailing', LongText + ',100,40,1,0,0,100,40,14,true,false,Trailing,Trailing,Character,claBlack,3.83,2.76,103.83,40.38,0.97,xHOyOvqmj64A8Kt7P8bvmLxuPeyjP5C86NxzIXwks0sv++QgXF969FU8P0nr1bue+Ajcez69FQ')]
    [TestCase('22.Short low height horizontal leading', ShortText + ',100,13,1,0,2,100,9,14,false,false,Leading,Leading,None,claBlack,0,2.38,40.2,11,0.97,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDkb0jq0sv++Mn0K/591NUX8nq4ANfeQXVgw4+GA')]
    [TestCase('23.Short low height horizontal center', ShortText + ',100,13,1,0,2,100,9,14,false,false,Center,Leading,None,claBlack,29.9,2.38,70.1,11,0.97,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDkTwntWM/u+UQKK+692Fsn4jp2P/++AXmSx7+Ew')]
    [TestCase('24.Short low height horizontal trailing', ShortText + ',100,13,1,0,2,100,9,14,false,false,Trailing,Leading,None,claBlack,59.8,2.38,100,11,0.97,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GAwrwnw58seuQbO445+EQ')]
    [TestCase('25.Short low height horizontal leading vertical center', ShortText + ',100,13,1,0,2,100,9,12,false,false,Leading,Center,None,claBlack,0,2.04,34.46,11.04,0.97,0O9h+/nMX75B9527r0X1lawOPvCVV1A96OCDgTwbxHuv+t4cxK+39TVUX8nq4ANfeQXVgw4+GA')]
    [TestCase('26.Short low height horizontal center vertical center', ShortText + ',100,13,1,0,2,100,9,12,false,false,Center,Center,None,claBlack,32.77,2.04,67.23,11.04,0.97,22dCevrFf06B9p9z34X1hsQOPfSDi8B95dZzYXwmrFvfu+gSVO86/lFQP8fq1/9eeAb3/739Dw')]
    [TestCase('27.Short low height horizontal trailing vertical center', ShortText + ',100,13,1,0,2,100,9,12,false,false,Trailing,Center,None,claBlack,65.54,2.04,100,11.04,0.97,1VeyOvjAV15B9aCDD8bvgIyuvvCpQwC95+WnUX0lq4MPfOUVVA86+GAwf4nv1beO+gTfw94+Ew')]
    [TestCase('28.Short low size horizontal leading vertical center', ShortText + ',40,13,1,5,2,30,9,12,false,false,Leading,Center,None,claBlack,5,2.04,35,11.04,0.97,y+/hOfjTf+5A9p97vwb0grRuf/OWTwD+5/JvEf0npyuf+usUpH/6AFNIL4by5a8uNwYJ5H29CQ')]
    [TestCase('29.Short low size horizontal center vertical center', ShortText + ',40,13,1,5,2,30,9,12,false,false,Center,Center,None,claBlack,5,2.04,35,11.04,0.97,2TdCuv/Qu17B9Zt/Dwf5kOief/GeJ3A86+OjoTwsuRc/OusSTM97+lFo/0Xr56veuAUCrA3/Dg')]
    [TestCase('30.Short low size horizontal trailing vertical center', ShortText + ',40,13,1,5,2,30,9,12,false,false,Trailing,Center,None,claBlack,5,2.04,35,11.04,0.97,zzeiegDJo04B9Z5/7wb2f9iuvfKUhyC+6+trMbwnxGdfOuYWQH87+EZ0bwbwzbv+dwcE2P39DA')]
    [TestCase('31.Short', ShortText + ',100,40,1,0,0,65535,65535,14,false,true,Trailing,Leading,Character,claBlack,0,0.38,40.2,19,0.97,0kPy+vOpM56C/J1TT0X1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('32.Short horizontal leading', ShortText + ',100,40,1,0,0,100,40,14,false,true,Trailing,Leading,Character,claBlack,0,0.38,40.2,19,0.97,0kPy+vOpM56C/J1TT0X1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('33.Short horizontal center', ShortText + ',100,40,1,0,0,100,40,14,false,true,Center,Leading,Character,claBlack,29.9,0.38,70.1,19,0.97,yY9Ce/i3W77A8LB7vwX0hKCufPCLY3D+4ttbUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('34.Short horizontal trailing', ShortText + ',100,40,1,0,0,100,40,14,false,true,Leading,Leading,Character,claBlack,59.8,0.38,100,19,0.97,1VeyOvjAV15B9aCDDwb0o/h+vumIc+B87NlPUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('35.Short vertical center', ShortText + ',100,40,1,0,0,100,40,14,false,true,Trailing,Center,Character,claBlack,0,11.07,40.2,29.69,0.97,1/cBe/rXa/7A9Z+Pr0b1lawOPvCVV1A96OCDAf0etKtfe+UUkI8490pUX8nq4ANfeQXVgw4+GA')]
    [TestCase('36.Short horizontal center, vertical center', ShortText + ',100,40,1,0,0,100,40,14,false,true,Center,Center,Character,claBlack,29.9,11.07,70.1,29.69,0.97,2kdC+vnHez4B9597L4b1hcjOvfOSZ6D95uSDIT0krCsfPOgPdI/6/VhYz0fs5/seuQXjN879EQ')]
    [TestCase('37.Short horizontal trailing, vertical center', ShortText + ',100,40,1,0,0,100,40,14,false,true,Leading,Center,Character,claBlack,59.8,11.07,100,29.69,0.97,1VeyOvjAV15B9aCDDwb0g5TevvKlT7D95uWbUX0lq4MPfOUVVA86+GBAn0jr3d/eOQTgXw4/Fw')]
    [TestCase('38.Short vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,false,true,Trailing,Trailing,Character,claBlack,0,21.76,40.2,40.38,0.97,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDEb0kslvvuuQprP859FxUX8nq4ANfeQXVgw4+GA')]
    [TestCase('39.Short horizontal center, vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,false,true,Center,Trailing,Character,claBlack,29.9,21.76,70.1,40.38,0.97,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDwTworYe/e+YGBP96911Mf0jqyQ8/uQbjP149Fg')]
    [TestCase('40.Short horizontal trailing, vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,false,true,Leading,Trailing,Character,claBlack,59.8,21.76,100,40.38,0.97,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GAg/wnv6q8O+QXKx379Fg')]
    [TestCase('41.Long', LongText + ',100,40,1,0,0,65535,65535,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,6642.34,19,0.97,z3ciu/asL66C96hLv8TuksgOveyMf5B959BHUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('42.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,98.84,38,0.97,xVcyu/WpQ34C+KFPn0TyjrT+vOyMk1C+49BnYb0mtXPPeuMigH8690c8Pwnu2Kd+OAfjY469FA')]
    [TestCase('43.Long horizontal center', LongText + ',100,40,1,0,0,100,40,14,true,true,Center,Leading,Character,claBlack,0.58,0.38,99.42,38,0.97,zpeCu/mqMy4C9KpvXwX2mcxuPeuIoyA+6dNXMf0mt4ufeuMjfJ8690k8P4nu2Kt+eAfjb259FA')]
    [TestCase('44.Long horizontal trailing', LongText + ',100,40,1,0,0,100,40,14,true,true,Leading,Leading,Character,claBlack,3.83,0.38,103.83,38,0.97,yIuyO/utc46B8raHT4Xwn+DeveqQb2C96N5XYX0nuY9/uuMjeL969084P0nv3qeuuAbfh249FA')]
    [TestCase('45.Long vertical center', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Center,Character,claBlack,0,1.57,98.84,39.19,0.97,w1Mie/aqY35C9qJTH0XyjcQe/e2UlxC+5NB7Mb0ntGuPOuMlZJ/69U8or0nt2Kee+Abaa159Fg')]
    [TestCase('46.Long horizontal center, vertical center', LongText + ',100,40,1,0,0,100,40,14,true,true,Center,Center,Character,claBlack,0.58,1.57,99.42,39.19,0.97,yYNy+/irS74B861r30X1l9SufeyPm6A96dF3IX0otYdf+uMmYM/69VAsn4nt16+eeAfacz49Fg')]
    [TestCase('47.Long horizontal trailing, vertical center', LongText + ',100,40,1,0,0,100,40,14,true,true,Leading,Center,Character,claBlack,3.83,1.57,103.83,39.19,0.97,xYcSO/uqf/5A8a9738Xvn8zefeuZU/B86NprYX0pt4tfOuQlXN969lQkn4nu3aveuAbXi049Fg')]
    [TestCase('48.Long vertical trailing', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Trailing,Character,claBlack,0,2.76,98.84,40.38,0.97,wz8i+/Wuc16C9KBrj4Xyicwe/e+cm+D95dR/8fwmtHeveuQhTJ+69Fgcnwnt2r8+uQXUf149GA')]
    [TestCase('49.Long horizontal center, vertical trailing', LongText + ',100,40,1,0,0,100,40,14,true,true,Center,Trailing,Character,claBlack,0.58,2.76,99.42,40.38,0.97,xW8y+/erX07B8a5rL0b0k9y+/e2TlxD96NKPEX0otZOPeuUjUM/69Fkcj0nt2cdOOQbUhz49GA')]
    [TestCase('50.Long horizontal trailing, vertical trailing', LongText + ',100,40,1,0,0,100,40,14,true,true,Leading,Trailing,Character,claBlack,3.83,2.76,103.83,40.38,0.97,xHOyOvqmj64A8Kt7P8bvmLxuPeyjP5C86NxzUT0pt4+PeuUiTN969VsUr4nt4LuO+QXRnz49GA')]
    [TestCase('51.Short low height horizontal leading', ShortText + ',100,13,1,0,2,100,9,14,false,true,Trailing,Leading,None,claBlack,0,2.38,40.2,11,0.97,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDkb0jq0sv++Mn0K/591NUX8nq4ANfeQXVgw4+GA')]
    [TestCase('52.Short low height horizontal center', ShortText + ',100,13,1,0,2,100,9,14,false,true,Center,Leading,None,claBlack,29.9,2.38,70.1,11,0.97,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDkTwntWM/u+UQKK+692Fsn4jp2P/++AXmSx7+Ew')]
    [TestCase('53.Short low height horizontal trailing', ShortText + ',100,13,1,0,2,100,9,14,false,true,Leading,Leading,None,claBlack,59.8,2.38,100,11,0.97,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GAwrwnw58seuQbO445+EQ')]
    [TestCase('54.Short low height horizontal leading vertical center', ShortText + ',100,13,1,0,2,100,9,12,false,true,Trailing,Center,None,claBlack,0,2.04,34.46,11.04,0.97,0O9h+/nMX75B9527r0X1lawOPvCVV1A96OCDgTwbxHuv+t4cxK+39TVUX8nq4ANfeQXVgw4+GA')]
    [TestCase('55.Short low height horizontal center vertical center', ShortText + ',100,13,1,0,2,100,9,12,false,true,Center,Center,None,claBlack,32.77,2.04,67.23,11.04,0.97,22dCevrFf06B9p9z34X1hsQOPfSDi8B95dZzYXwmrFvfu+gSVO86/lFQP8fq1/9eeAb3/739Dw')]
    [TestCase('56.Short low height horizontal trailing vertical center', ShortText + ',100,13,1,0,2,100,9,12,false,true,Leading,Center,None,claBlack,65.54,2.04,100,11.04,0.97,1VeyOvjAV15B9aCDD8bvgIyuvvCpQwC95+WnUX0lq4MPfOUVVA86+GAwf4nv1beO+gTfw94+Ew')]
    [TestCase('57.Short low size horizontal leading vertical center', ShortText + ',40,13,1,5,2,30,9,12,false,true,Trailing,Center,None,claBlack,5,2.04,35,11.04,0.97,y+/hOfjTf+5A9p97vwb0grRuf/OWTwD+5/JvEf0npyuf+usUpH/6AFNIL4by5a8uNwYJ5H29CQ')]
    [TestCase('58.Short low size horizontal center vertical center', ShortText + ',40,13,1,5,2,30,9,12,false,true,Center,Center,None,claBlack,5,2.04,35,11.04,0.97,2TdCuv/Qu17B9Zt/Dwf5kOief/GeJ3A86+OjoTwsuRc/OusSTM97+lFo/0Xr56veuAUCrA3/Dg')]
    [TestCase('59.Short low size horizontal trailing vertical center', ShortText + ',40,13,1,5,2,30,9,12,false,true,Leading,Center,None,claBlack,5,2.04,35,11.04,0.97,zzeiegDJo04B9Z5/7wb2f9iuvfKUhyC+6+trMbwnxGdfOuYWQH87+EZ0bwbwzbv+dwcE2P39DA')]
    {$ENDIF}
    procedure GenericTest(const AText: string; const ABitmapWidth, ABitmapHeight: Integer; const AScale, ATextLeft, ATextTop, AMaxWidth, AMaxHeight, AFontSize: Single; const AWordWrap, ARightToLeft: Boolean; const AHorizontalAlign, AVerticalAlign: TTextAlign; const ATrimming: TTextTrimming; const AColor: string; const AExpectedTextLeft, AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    {$IFDEF TEXT_RENDER}
    [TestCase('1.Short attr middle', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,32.08,19,2,3,8,False,claRed,0.97,zM9Ru/bVW76C+I5n70X1lawOPvCVV1A96OCDcT0lr29vvOQSXK95+F9UX8nq4ANfeQXVgw4+GA')]
    [TestCase('2.Short attr empty', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,2,0,8,False,claRed,0.97,0sex+/fLR67C+ZpvP0b1lawOPvCVV1A96OCDYX0lrWN//OQSWK/5919UX8nq4ANfeQXVgw4+GA')]
    [TestCase('3.Short attr begin', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,30.8,19,0,3,8,False,claRed,0.97,zu+xu/jAZ46C9pNXz0X1lawOPvCVV1A96OCDYT0lrm9PPOUSYO/592FUX8nq4ANfeQXVgw4+GA')]
    [TestCase('4.Short attr end', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,32.38,19,3,20,8,False,claRed,0.97,zrMh+/TUL84C+YNz70X1lawOPvCVV1A96OCDcT0lrmt/vOQSYJ95+F9UX8nq4ANfeQXVgw4+GA')]
    [TestCase('5.Short attr middle with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,41.2,27,2,3,8,False,claRed,0.97,2iMSu/jSX14A85x/T0b1lawOPvCVV1A96OCDcb0bqqPf+uMZANC391BUX8nq4ANfeQXVgw4+GA')]
    [TestCase('6.Short attr middle big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,45.61,24,2,3,18,False,claRed,0.97,059hOvjUuz6B85Wjn0b1lawOPvCVV1A96OCDMT4guJ8/u+cTbC+590RUX8nq4ANfeQXVgw4+GA')]
    [TestCase('7.Short attr empty big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,2,0,18,False,claRed,0.97,0sex+/fLR67C+ZpvP0b1lawOPvCVV1A96OCDYX0lrWN//OQSWK/5919UX8nq4ANfeQXVgw4+GA')]
    [TestCase('8.Short attr begin big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,46.47,24,0,3,18,False,claRed,0.97,zqdxevbf377A9YVXD0f1lawOPvCVV1A96OCDof0gpo/vOuoUjF94+EZUX8nq4ANfeQXVgw4+GA')]
    [TestCase('9.Short attr end big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,45.42,24,3,20,18,False,claRed,0.97,ybvBOv3Kg/5A9JyXD0f1lawOPvCVV1A96OCDIT4gsa+f++cReC/590ZUX8nq4ANfeQXVgw4+GA')]
    [TestCase('10.Short attr middle big with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,54.73,27,2,3,18,False,claRed,0.97,uauR+vnHn54A842jXwb1kqSuPfGYU3D95uBzcX4cuWPv++IR8A85+EZU34jq3wc/OQbfY879Fg')]
    [TestCase('11.Short attr middle bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,32.82,19,2,3,8,True,claRed,0.97,xatBO/beY94C+Yd3L0b1lawOPvCVV1A96OCDcf0ksG9/POQRXK95+F5UX8nq4ANfeQXVgw4+GA')]
    [TestCase('12.Short attr empty bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,2,0,8,True,claRed,0.97,0sex+/fLR67C+ZpvP0b1lawOPvCVV1A96OCDYX0lrWN//OQSWK/5919UX8nq4ANfeQXVgw4+GA')]
    [TestCase('13.Short attr begin bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,31.61,19,0,3,8,True,claRed,0.97,1Mfhe/fNd97C9Y4zX0X1lawOPvCVV1A96OCDcT0lsGd//OQRaM+592JUX8nq4ANfeQXVgw4+GA')]
    [TestCase('14.Short attr end bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,33.15,19,3,20,8,True,claRed,0.97,yJexevfQV94C+YCL/0X1lawOPvCVV1A96OCDcX0lr2OPfOQSXK85+F5UX8nq4ANfeQXVgw4+GA')]
    [TestCase('15.Short attr middle bold with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,41.93,27,2,3,8,True,claRed,0.97,2CsSu/jRa34A85t7X0b1lawOPvCVV1A96OCDkb0ZtbtvuuMd9O/39k1UX8nq4ANfeQXVgw4+GA')]
    [TestCase('16.Short attr middle big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,47.28,24,2,3,18,True,claRed,0.97,xkehufre145A85KvX0f1lawOPvCVV1A96OCDsT4ewqvPuucLbD+59kpUX8nq4ANfeQXVgw4+GA')]
    [TestCase('17.Short attr empty big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,2,0,18,True,claRed,0.97,0sex+/fLR67C+ZpvP0b1lawOPvCVV1A96OCDYX0lrWN//OQSWK/5919UX8nq4ANfeQXVgw4+GA')]
    [TestCase('18.Short attr begin big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,48.31,24,0,3,18,True,claRed,0.97,tT9huvvcV3/B9l9f30b1lawOPvCVV1A96OCD4b0cq6tveuoHtN939DFUX8nq4ANfeQXWgw4+GA')]
    [TestCase('19.Short attr end big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,47.15,24,3,20,18,True,claRed,0.97,u6fhOQLGtx7A8p2bP0f1lawOPvCVV1A96OCD4f4htX+/O+kKcM95+UNUX8nq4ANfeQXVgw4+GA')]
    [TestCase('20.Short attr middle big bold with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,56.39,27,2,3,18,True,claRed,0.97,p39x+vzC625A84ij30b1kKSOvfGZU3D95t9vcX8bxR9PPOUIBAA6/EJUv4jq3wcfOQbhX759Fg')]
    {$ENDIF}
    procedure TestAttributes1(const AText: string; const ABitmapWidth, ABitmapHeight: Integer; const AScale, ATextLeft, ATextTop, AMaxWidth, AMaxHeight, AFontSize: Single; const AWordWrap, ARightToLeft: Boolean; const AHorizontalAlign, AVerticalAlign: TTextAlign; const ATrimming: TTextTrimming; const AColor: string; const AExpectedTextLeft, AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom: Single; const AAttrRangePos, AAttrRangeLength: Integer; const AAttrFontSize: Single; const AAttrFontBold: Boolean; const AAttrColor: string; const AMinSimilarity: Double; const AExpectedImageHash: string);
    {$IFDEF TEXT_RENDER}
    [TestCase('1.Short attr middle', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,32.17,19,1,2,8,False,claRed,4,1,8,False,claBlue,0.97,yc+Re/XLTy7C941zP0b1lawOPvCVV1A96OCDcT0lr29vvOQSYM/592FUX8nq4ANfeQXVgw4+GA')]
    [TestCase('2.Short attr empty', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,37.06,19,2,0,8,False,claRed,3,1,8,False,claBlue,0.97,0cfR+/bQI65C+ZFf30X1lawOPvCVV1A96OCDcT0lrmtvPOUTXJ85+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('3.Short attr begin', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,27.66,19,0,3,8,False,claRed,2,2,8,False,claBlue,0.97,1vfRe/jCfx7C9ZFrD0b1lawOPvCVV1A96OCDYX0lrXNPPOUTZO/592BUX8nq4ANfeQXVgw4+GA')]
    [TestCase('4.Short attr end', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,32.38,19,3,20,8,False,claRed,5,15,8,False,claBlue,0.97,zrdRu/TUK85C+YNzD0b1lawOPvCVV1A96OCDcT0lrmtvvOQSYJ95+F9UX8nq4ANfeQXVgw4+GA')]
    [TestCase('5.Short attr middle with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,34.73,27,2,3,8,False,claRed,5,1,8,False,claBlue,0.97,0QMSO/bRUz6B9pOX70X1lawOPvCVV1A96OCDIX0bsq+/uuEb7P+39UxUX8nq4ANfeQXVgw4+GA')]
    [TestCase('6.Short attr middle big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,45.61,24,2,3,18,False,claRed,4,1,18,False,claBlue,0.97,zr+BuvjRw07B85en/0b1lawOPvCVV1A96OCD8T0gtKc/O+cUdO/49kNUX8nq4ANfeQXVgw4+GA')]
    [TestCase('7.Short attr empty big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,42.29,24,2,0,18,False,claRed,3,1,18,False,claBlue,0.97,y8fRu/zVfy7B9paXL0f1lawOPvCVV1A96OCDkXwes8sfu+YYaC9490VUX8nq4ANfeQXVgw4+GA')]
    [TestCase('8.Short attr begin big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,37.46,24,0,3,18,False,claRed,2,2,8,False,claBlue,0.97,1OcBO/fXg84A9ZNf30X1lawOPvCVV1A96OCDEX0fpq8fO+kWjG+490dUX8nq4ANfeQXVgw4+GA')]
    [TestCase('9.Short attr end big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,45.42,24,3,20,18,False,claRed,5,15,18,False,claBlue,0.97,wruhev7Ll+4A8p+bj0f1lawOPvCVV1A96OCDEf4fsLuvu+gSeB8590VUX8nq4ANfeQXVgw4+GA')]
    [TestCase('10.Short attr middle big with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,53.65,27,2,3,18,False,claRed,5,1,18,False,claBlue,0.97,uaNRuvjHpy6A85CjDwb1kqQOPvCVV0D95+BzcX4cumsPPOIR8C/5+EJYD8nq4ANfuQXdY+69Fw')]
    [TestCase('11.Short attr middle bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,32.49,19,2,3,8,True,claRed,4,1,8,False,claBlue,0.97,x6tRu/XfT77C+IxzH0b1lawOPvCVV1A96OCDcf0ksG9vfOQSXK95+F9UX8nq4ANfeQXVgw4+GA')]
    [TestCase('12.Short attr empty bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,37.06,19,2,0,8,True,claRed,3,1,8,False,claBlue,0.97,0cfR+/bQI65C+ZFf30X1lawOPvCVV1A96OCDcT0lrmtvPOUTXJ85+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('13.Short attr begin bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,28.22,19,0,3,8,True,claRed,2,2,8,False,claBlue,0.97,3Pux+/fNf/7B9Zdfb0X1lawOPvCVV1A96OCDgb0lrmtf/OQSbN/592BUX8nq4ANfeQXVgw4+GA')]
    [TestCase('14.Short attr end bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,32.86,19,3,20,8,True,claRed,5,15,8,False,claBlue,0.97,x6chu/bQTx6D+X6HH0b1lawOPvCVV1A96OCDcT0lr2ePfOQRXJ85+F5UX8nq4ANfeQXVgw4+GA')]
    [TestCase('15.Short attr middle bold with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,35.75,27,2,3,8,True,claRed,5,1,8,True,claBlue,0.97,0QMSO/bRVz6B9pOX70X1lawOPvCVV1A96OCDYT0ZvLcvOuIc9B/49E5UX8nq4ANfeQXVgw4+GA')]
    [TestCase('16.Short attr middle big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,46.52,24,2,3,18,True,claRed,4,1,18,False,claBlue,0.97,vVvBefrZ5w7B84q7n0f1lawOPvCVV1A96OCDEb4dwLu/euUMdN+49EVUX8nq4ANfeQXVgw4+GA')]
    [TestCase('17.Short attr empty big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,42.29,24,2,0,18,True,claRed,3,1,18,False,claBlue,0.97,y8fRu/zVfy7B9paXL0f1lawOPvCVV1A96OCDkXwes8sfu+YYaC9490VUX8nq4ANfeQXVgw4+GA')]
    [TestCase('18.Short attr begin big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,38.71,24,0,3,18,True,claRed,2,2,8,False,claBlue,0.97,0KcRe/Xtgz7A9YNPv0X1lawOPvCVV1A96OCDAT0cp8OPeusQuF/39jhUX8nq4ANfeQXVgw4+GA')]
    [TestCase('19.Short attr end big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,46.5,24,3,20,18,True,claRed,5,15,18,False,claBlue,0.97,vH/B+QHNv46A7567n0f1lawOPvCVV1A96OCD0T4guZt/u+kNfK/59UtUX8nq4ANfeQXVgw4+GA')]
    [TestCase('20.Short attr middle big bold with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,55.31,27,2,3,18,True,claRed,5,1,18,False,claBlue,0.97,qH9ROvzC7y6A84ufr0b1kKDevfCXW0B96N5zgX8bxCNPPOUIBAB6/EJUv0jr4Qc/eQXiV949Fw')]
    {$ENDIF}
    procedure TestAttributes2(const AText: string; const ABitmapWidth, ABitmapHeight: Integer; const AScale, ATextLeft, ATextTop, AMaxWidth, AMaxHeight, AFontSize: Single; const AWordWrap, ARightToLeft: Boolean; const AHorizontalAlign, AVerticalAlign: TTextAlign; const ATrimming: TTextTrimming; const AColor: string; const AExpectedTextLeft, AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom: Single; const AAttr1RangePos, AAttr1RangeLength: Integer; const AAttr1FontSize: Single; const AAttr1FontBold: Boolean; const AAttr1Color: string; const AAttr2RangePos, AAttr2RangeLength: Integer; const AAttr2FontSize: Single; const AAttr2FontBold: Boolean; const AAttr2Color: string; const AMinSimilarity: Double; const AExpectedImageHash: string);
    {$IFDEF TEXT_RENDER}
    [TestCase('1.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,-0.3,99.62,39,0,100000,10,False,claRed,0,1,10,True,claLime,1,0,10,True,claDeeppink,0.97,4Kfhu/jPd87C/peHbwT5cCS//fCKp5D/490LMf0jnjfvO+wKbG/490AYH0jm4t/uegPiE878Dg')]
    [TestCase('2.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.04,100,29,0,100000,10,False,claRed,0,1,10,True,claLime,1,1,12,True,claDeeppink,0.97,2DfSe/K3O/4C+5Fvf4P0lPiOPe+Rp5B+5dL/wD0fuZ9Pu+YNgH96+0Vs70bs5LN+egXxc/4+DQ')]
    [TestCase('3.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,-0.29,100,30,0,100000,10,False,claRed,0,1,10,True,claLime,0,1,13,True,claDeeppink,0.97,zTNiO/Gsh77C/oIbXwP0mOBOve6Yn1D+5NQH8T0dwI+f++URqH86+0d4v8bt4rvOeQf+d86+Dg')]
    [TestCase('4.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,-0.3,99.88,39,0,100000,10,False,claRed,0,2,10,True,claLime,0,1,10,True,claDeeppink,0.97,4Z9BfPbKb45BAZV7XwT5cSS/PfGLp5B/5N4HMf0jnjfvO+wKbG/490AYH0jm4t/uegPiE878Dg')]
    [TestCase('5.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,100,32,0,100000,10,False,claRed,0,0,10,True,claLime,0,1,14,True,claDeeppink,0.97,xx8i+++qhz4C+4QPz0Pzm9DO/Oymj+D949f7EH4ex3cf/OQb4G86+Up8T4fw3et+OAkLcB4+EQ')]
    [TestCase('6.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,-0.3,98.01,39,0,100000,10,False,claRed,0,2,10,True,claLime,1,99999,10,True,claDeeppink,0.97,7F+BvPrZh/4C/punz0P5UEy/fvCWv4D/3uUf8XscrTOvu+n/P9949TZ8z8fs0yuPuwDYQ+58DA')]
    [TestCase('7.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,-0.3,98.01,39,0,100000,10,False,claRed,0,2,10,True,claLime,1,10001,10,True,claDeeppink,0.97,7F+BvPrZh/4C/punz0P5UEy/fvCWv4D/3uUf8XscrTOvu+n/P9949TZ8z8fs0yuPuwDYQ+58DA')]
    [TestCase('8.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,-0.3,98.01,39,0,100000,10,False,claRed,0,2,10,True,claLime,1,99998,10,True,claDeeppink,0.97,7F+BvPrZh/4C/punz0P5UEy/fvCWv4D/3uUf8XscrTOvu+n/P9949TZ8z8fs0yuPuwDYQ+58DA')]
    [TestCase('9.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,97.29,35,10,100000,12,False,claRed,0,1,14,True,claLime,1,0,10,True,claDeeppink,0.97,w/vhuvWka64C+aRzPwTzirTOPO6Pi0A+4c5XwT0jy2Mf/OQnyA86+Dlsnwjx1d/ueAr6a369Dw')]
    [TestCase('10.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,97.29,35,10,100000,12,False,claRed,0,0,14,True,claLime,1,0,10,True,claDeeppink,0.97,xVcyu/WpQ34C+KFPnwTzirDefO6QizC+4c5bwT0jy2Mf/OQnyA86+Dlsnwjx1d/ueAr6a369Dw')]
    [TestCase('11.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,97.29,35,10,100000,12,False,claRed,0,1,14,True,claLime,0,0,10,True,claDeeppink,0.97,w/vhuvWka64C+aRzPwTzirTOPO6Pi0A+4c5XwT0jy2Mf/OQnyA86+Dlsnwjx1d/ueAr6a369Dw')]
    [TestCase('12.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,97.29,35,10,100000,12,False,claRed,0,1,14,True,claLime,0,1,10,True,claDeeppink,0.97,w0uSu/OpYx6C+J5bL4TziaiufO+Ri2B+4c5TwT0jy2Mf/OQnyA86+Dlsnwjx1d/ueAr6a369Dw')]
    [TestCase('13.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,97.29,35,10,100000,12,False,claRed,0,1,14,True,claLime,1,1,10,True,claDeeppink,0.97,wPsh+/OwYx6D+KF7v4Pziqi+fO+Si2C+4c9TwT0jy2Mf/OQnyA86+Dlsnwjx1d/ueAr6a369Dw')]
    [TestCase('14.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,97.29,35,10,100000,12,False,claRed,0,1,14,True,claLime,3,3,10,True,claDeeppink,0.97,xseBOvaqp24C+pyDvwP0irA+PPGTh1A+4tJfwT0jy2Mf/OQnyA86+Dlsnwjx1d/ueAr6a369Dw')]
    [TestCase('15.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,97.29,35,10,100000,12,False,claRed,1,1,14,True,claLime,2,1,10,True,claDeeppink,0.97,tz/Se/StTw4D96R7z8TyiazOPO+Pi1C+4c5PwT0jy2Mf/OQnyA86+Dlsnwjx1d/ueAr6a369Dw')]
    [TestCase('16.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,97.29,35,10,100000,12,False,claRed,1,1,14,True,claLime,3,0,10,True,claDeeppink,0.97,ulOSu/esU86C96ZvTwXzirDufO6QizC+4c5XwT0jy2Mf/OQnyA86+Dlsnwjx1d/ueAr6a369Dw')]
    [TestCase('17.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,99.15,35,10,100000,12,False,claRed,0,10,14,True,claLime,1,9,10,True,claDeeppink,0.97,wvfBeu+fU87C9aRbb8P0mNQ+fe2gj4D+5Nr3sD0jy18//OQi7E/6+EBof4jy1wtf+Qr8j579DQ')]
    [TestCase('18.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,100,35,10,100000,12,False,claRed,0,11,14,True,claLime,1,9,10,True,claDeeppink,0.97,wvfBeu+fU87C9aRbb8PvkNi+/eugi+B+6eM3sT0jy18//OQi7E/6+EBof4jy1wtf+Qr8j579DQ')]
    [TestCase('19.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,97.29,35,10,100000,12,False,claRed,0,10,14,True,claLime,8,4,10,True,claDeeppink,0.97,w4Py/P2+W24E8tyDv8LxYLyO+/Ciq+A+4+T/wD0jy2Mf/OQnyA86+Dlsnwjx1d/ueAr6a369Dw')]
    [TestCase('20.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,100,35,10,100000,12,False,claRed,0,11,14,True,claLime,1,9,10,True,claDeeppink,0.97,wvfBeu+fU87C9aRbb8PvkNi+/eugi+B+6eM3sT0jy18//OQi7E/6+EBof4jy1wtf+Qr8j579DQ')]
    [TestCase('21.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,97.97,35,10,100000,12,False,claRed,0,11,14,True,claLime,1,10,10,True,claDeeppink,0.97,wvfBeu+fU87C9aRbb0P1muj+/O+mo5C+5NXvsD0jy18//OQi7E/6+EBof4jy1wtf+Qr8j579DQ')]
    [TestCase('22.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,99.84,35,10,100000,12,False,claRed,0,11,14,True,claLime,1,11,10,True,claDeeppink,0.97,wvfBeu+fU87C9aRbbwP2mvAOve+mp5C+5NXrsD0jy18//OQi7E/6+EBof4jy1wtf+Qr8j579DQ')]
    [TestCase('23.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,100,35,10,100000,12,False,claRed,3,8,14,True,claLime,1,9,10,True,claDeeppink,0.97,x18i+++lM25C9KM734Pwj9h+veuei8C+6OM7sT0jy18//OQi7E/6+EBof4jy1wtf+Qr8j579DQ')]
    [TestCase('24.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,99.76,38,10,8,12,False,claRed,11,3,14,True,claLime,13,3,10,True,claDeeppink,0.97,xVcyu/WpQ34C+KFPnwTzirDefO6QizC+4c5bsfwmwI+/++MnfB+6+Tw0b8ns3bNueAnml659EQ')]
    [TestCase('25.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,true,true,Trailing,Leading,Character,claBlack,0,0.38,100,38,10,10,12,False,claRed,11,6,14,True,claLime,12,2,10,True,claDeeppink,0.97,xVcyu/WpQ34C+KFPnwTzirDefO6QizC+4c5bsT4q5ZevO+NQYC89+B80b4nt3auO+Ajjh359Eg')]
    {$ENDIF}
    procedure TestAttributes3(const AText: string; const ABitmapWidth, ABitmapHeight: Integer; const AScale, ATextLeft, ATextTop, AMaxWidth, AMaxHeight, AFontSize: Single; const AWordWrap, ARightToLeft: Boolean; const AHorizontalAlign, AVerticalAlign: TTextAlign; const ATrimming: TTextTrimming; const AColor: string; const AExpectedTextLeft, AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom: Single; const AAttr1RangePos, AAttr1RangeLength: Integer; const AAttr1FontSize: Single; const AAttr1FontBold: Boolean; const AAttr1Color: string; const AAttr2RangePos, AAttr2RangeLength: Integer; const AAttr2FontSize: Single; const AAttr2FontBold: Boolean; const AAttr2Color: string;  const AAttr3RangePos, AAttr3RangeLength: Integer; const AAttr3FontSize: Single; const AAttr3FontBold: Boolean; const AAttr3Color: string; const AMinSimilarity: Double; const AExpectedImageHash: string);
    {$IFDEF TEXT_RENDER}
    [TestCase('1.Short attr middle', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,2,3,claRed,0.97,1K9R+/baiz7D+Yl7b0X1lawOPvCVV1A96OCDkX0lr2efPOQRWK85+F1UX8nq4ANfeQXVgw4+GA')]
    [TestCase('2.Short attr empty', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,2,0,claRed,0.97,0sex+/fLR67C+ZpvP0b1lawOPvCVV1A96OCDYX0lrWN//OQSWK/5919UX8nq4ANfeQXVgw4+GA')]
    [TestCase('3.Short attr begin', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,0,3,claRed,0.97,18vB+/TYf67C+JZXz0T1lawOPvCVV1A96OCDgT0lsGd/fOQQYN+592BUX8nq4ANfeQXVgw4+GA')]
    [TestCase('4.Short attr end', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,3,20,claRed,0.97,1Kcx+/jVV87C+Yp7r0X1lawOPvCVV1A96OCDkb0lrl+PvOQSWK85+F5UX8nq4ANfeQXVgw4+GA')]
    [TestCase('5.Short attr middle with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,57.43,27,2,3,claRed,0.97,tMNhev3Ez25A846LT0b1j6BuvfGbT4D95t9vYX4ctV8PvOMR9C95+0VUv4jq3wcfOQbhX759Fg')]
    {$ENDIF}
    procedure TestAttributesFontNil(const AText: string; const ABitmapWidth, ABitmapHeight: Integer; const AScale, ATextLeft, ATextTop, AMaxWidth, AMaxHeight, AFontSize: Single; const AWordWrap, ARightToLeft: Boolean; const AHorizontalAlign, AVerticalAlign: TTextAlign; const ATrimming: TTextTrimming; const AColor: string; const AExpectedTextLeft, AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom: Single; const AAttrRangePos, AAttrRangeLength: Integer; const AAttrColor: string; const AMinSimilarity: Double; const AExpectedImageHash: string);
    {$IFDEF TEXT_RENDER}
    [TestCase('1.Short attr middle', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,32.08,19,2,3,8,False,claRed,0.97,yMNBO/fYW87C+Itz/0X1lawOPvCVV1A96OCDcf0kr29vfOQSXK95+F5UX8nq4ANfeQXVgw4+GA')]
    [TestCase('2.Short attr empty', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,2,0,8,False,claRed,0.97,0sex+/fLR67C+ZpvP0b1lawOPvCVV1A96OCDYX0lrWN//OQSWK/5919UX8nq4ANfeQXVgw4+GA')]
    [TestCase('3.Short attr begin', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,30.8,19,0,3,8,False,claRed,0.97,0+PRu/jFY44C9pNLf0X1lawOPvCVV1A96OCDYT0lrm9fPOUSZN/592FUX8nq4ANfeQXVgw4+GA')]
    [TestCase('4.Short attr end', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,32.38,19,3,20,8,False,claRed,0.97,yp8Bu/XUN86C+IF3D0b1lawOPvCVV1A96OCDcT0lrmd/vOQSYJ95+F9UX8nq4ANfeQXVgw4+GA')]
    [TestCase('5.Short attr middle with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,41.2,27,2,3,8,False,claRed,0.97,2iMSu/jSX14A85x/T0b1lawOPvCVV1A96OCDgT0brqPP+uMa/O93909UX8nq4ANfeQXVgw4+GA')]
    [TestCase('6.Short attr middle big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,45.61,24,2,3,18,False,claRed,0.97,zGtROvrZyx5B85PD/0b1lawOPvCVV1A96OCDcX4gup9vu+YQYC9590RUX8nq4ANfeQXVgw4+GA')]
    [TestCase('7.Short attr empty big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,2,0,18,False,claRed,0.97,0sex+/fLR67C+ZpvP0b1lawOPvCVV1A96OCDYX0lrWN//OQSWK/5919UX8nq4ANfeQXVgw4+GA')]
    [TestCase('8.Short attr begin big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,46.47,24,0,3,18,False,claRed,0.97,0Gexevfq2y6B9Xlvz0b1lawOPvCVV1A96OCDwf0fr3N/O+gJpB9490NUX8nq4ANfeQXVgw4+GA')]
    [TestCase('9.Short attr end big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,45.42,24,3,20,18,False,claRed,0.97,xqex+v7Pp54A9KKTX0f1lawOPvCVV1A96OCDkT4irJ+v++gSXD+5+UZUX8nq4ANfeQXVgw4+GA')]
    [TestCase('10.Short attr middle big with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,54.73,27,2,3,18,False,claRed,0.97,uauROvrHn54A842jXwb1kqSuPfGYU3D95uBzsb4bvWO/e+MP8E/590VU34jq3wc/OQbfY879Fg')]
    [TestCase('11.Short attr middle bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,32.82,19,2,3,8,True,claRed,0.97,wp8xu/bgY+4C+YR/L0b1lawOPvCVV1A96OCDcf0ksG9/POQRXK95+F5UX8nq4ANfeQXVgw4+GA')]
    [TestCase('12.Short attr empty bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,2,0,8,True,claRed,0.97,0sex+/fLR67C+ZpvP0b1lawOPvCVV1A96OCDYX0lrWN//OQSWK/5919UX8nq4ANfeQXVgw4+GA')]
    [TestCase('13.Short attr begin bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,31.61,19,0,3,8,True,claRed,0.97,2L/xe/fRd96C9Y4vL0X1lawOPvCVV1A96OCDgT0lsGN//OQQbM+592JUX8nq4ANfeQXVgw4+GA')]
    [TestCase('14.Short attr end bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,33.15,19,3,20,8,True,claRed,0.97,xoeROvjRW95C+H6PD0b1lawOPvCVV1A96OCDgX0lr2OffOQRWK85+F5UX8nq4ANfeQXVgw4+GA')]
    [TestCase('15.Short attr middle bold with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,41.93,27,2,3,8,True,claRed,0.97,2CsSu/jRa34A85t7X0b1lawOPvCVV1A96OCDkT0Zt7tfuuMd9P/39k1UX8nq4ANfeQXVgw4+GA')]
    [TestCase('16.Short attr middle big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,47.28,24,2,3,18,True,claRed,0.97,wiehefzh424A85DDj0f1lawOPvCVV1A96OCD4X4ewqvvOucJZD/59kpUX8nq4ANfeQXVgw4+GA')]
    [TestCase('17.Short attr empty big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,2,0,18,True,claRed,0.97,0sex+/fLR67C+ZpvP0b1lawOPvCVV1A96OCDYX0lrWN//OQSWK/5919UX8nq4ANfeQXVgw4+GA')]
    [TestCase('18.Short attr begin big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,48.31,24,0,3,18,True,claRed,0.97,uA+RuvvjU89B9lhrr0b1lawOPvCVV1A96OCDAf4bsZPP+uj/x6938zBUX8nq4ANfeQXWgw4+GA')]
    [TestCase('19.Short attr end big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,47.15,24,3,20,18,True,claRed,0.97,uJfhuQPL187/8qGXb4f1lqgOPvCVV1A96OB/Mb8jsW/P++kLXN85+0NYX8nq4ANfeQXWgw4+GA')]
    [TestCase('20.Short attr middle big bold with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,56.39,27,2,3,18,True,claRed,0.97,p39x+vzC625A84ij30b1kKSOvfGZU3D95t9vkf8ayB8vfOUGADA6/EFUv4jq3wcfOQbhX759Fg')]
    {$ENDIF}
    procedure TestAttributesStrikeOut(const AText: string; const ABitmapWidth, ABitmapHeight: Integer; const AScale, ATextLeft, ATextTop, AMaxWidth, AMaxHeight, AFontSize: Single; const AWordWrap, ARightToLeft: Boolean; const AHorizontalAlign, AVerticalAlign: TTextAlign; const ATrimming: TTextTrimming; const AColor: string; const AExpectedTextLeft, AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom: Single; const AAttrRangePos, AAttrRangeLength: Integer; const AAttrFontSize: Single; const AAttrFontBold: Boolean; const AAttrColor: string; const AMinSimilarity: Double; const AExpectedImageHash: string);
    {$IFDEF TEXT_RENDER}
    [TestCase('1.Short attr middle', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,32.08,19,2,3,8,False,claRed,0.97,zMtR+/bVW76C+I1r70X1lawOPvCVV1A96OCDMb0jsIcfvOMRXH+5915UX8nq4ANfeQXVgw4+GA')]
    [TestCase('2.Short attr empty', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,2,0,8,False,claRed,0.97,0sex+/fLR67C+ZpvP0b1lawOPvCVV1A96OCDYX0lrWN//OQSWK/5919UX8nq4ANfeQXVgw4+GA')]
    [TestCase('3.Short attr begin', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,30.8,19,0,3,8,False,claRed,0.97,z++xu/jBY46C9pNTv0X1lawOPvCVV1A96OCDcX0krl9fPOYOeL/59lxUX8nq4ANfeQXVgw4+GA')]
    [TestCase('4.Short attr end', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,32.38,19,3,20,8,False,claRed,0.97,zqsx+/TUM84C+YJ3/0X1lawOPvCVV1A96OCDoX0lsV9/fOQPXL9592BUX8nq4ANfeQXVgw4+GA')]
    [TestCase('5.Short attr middle with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,41.2,27,2,3,8,False,claRed,0.97,2iMSu/jSX14A85x/T0b1lawOPvCVV1A96OCDgb0brLO/+uMd/J/39ktUX8nq4ANfeQXVgw4+GA')]
    [TestCase('6.Short attr middle big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,45.61,24,2,3,18,False,claRed,0.97,059hOvjUuz6B85Wjn0b1lawOPvCVV1A96OCD4X4fwKcfO+YfkN+49kFUX8nq4ANfeQXVgw4+GA')]
    [TestCase('7.Short attr empty big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,2,0,18,False,claRed,0.97,0sex+/fLR67C+ZpvP0b1lawOPvCVV1A96OCDYX0lrWN//OQSWK/5919UX8nq4ANfeQXVgw4+GA')]
    [TestCase('8.Short attr begin big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,46.47,24,0,3,18,False,claRed,0.97,zqdxevbf377A9YVXD0f1lawOPvCVV1A96OCDkb0dr4MPuuUh3G/39DxUX8nq4ANfeQXVgw4+GA')]
    [TestCase('9.Short attr end big', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,45.42,24,3,20,18,False,claRed,0.97,ybvBOv3Kg/5A9JyXD0f1lawOPvCVV1A96OCDcb4euq//O+cVtE+5+EZUX8nq4ANfeQXVgw4+GA')]
    [TestCase('10.Short attr middle big with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,54.73,27,2,3,18,False,claRed,0.97,uauR+vnHn54A842jXwb1kqSuPfGYU3D95uBzsX4exI9fu+MX5C/59ThU34jq3wc/OQbfY879Fg')]
    [TestCase('11.Short attr middle bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,32.82,19,2,3,8,True,claRed,0.97,xadBe/beY94C+YZ7L0b1lawOPvCVV1A96OCDMX0jsYc/POMQXH95911UX8nq4ANfeQXVgw4+GA')]
    [TestCase('12.Short attr empty bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,2,0,8,True,claRed,0.97,0sex+/fLR67C+ZpvP0b1lawOPvCVV1A96OCDYX0lrWN//OQSWK/5919UX8nq4ANfeQXVgw4+GA')]
    [TestCase('13.Short attr begin bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,31.61,19,0,3,8,True,claRed,0.97,1cfhe/fOd94C9o4zX0X1lawOPvCVV1A96OCDgX0ksFd/POYMgK/59lxUX8nq4ANfeQXVgw4+GA')]
    [TestCase('14.Short attr end bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,33.15,19,3,20,8,True,claRed,0.97,yI+xuvfRV94C+X+PD0b1lawOPvCVV1A96OCDsb0lsVefPOQOWL85919UX8nq4ANfeQXVgw4+GA')]
    [TestCase('15.Short attr middle bold with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,41.93,27,2,3,8,True,claRed,0.97,2CsSu/jRa34A85t7X0b1lawOPvCVV1A96OCDob0Zt89PuuMh9K939klUX8nq4ANfeQXVgw4+GA')]
    [TestCase('16.Short attr middle big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,47.28,24,2,3,18,True,claRed,0.97,xkehufre145A85KvX0f1lawOPvCVV1A96OCDcb8dyq+/+uUWnO849kZUX8nq4ANfeQXVgw4+GA')]
    [TestCase('17.Short attr empty big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,2,0,18,True,claRed,0.97,0sex+/fLR67C+ZpvP0b1lawOPvCVV1A96OCDYX0lrWN//OQSWK/5919UX8nq4ANfeQXVgw4+GA')]
    [TestCase('18.Short attr begin big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,48.31,24,0,3,18,True,claRed,0.97,tT9huvvcV3/B9l9f30b1lawOPvCVV1A96OCD0T0ZtZ9/eeUVAPB28CdUX8nq4ANfeQXWgw4+GA')]
    [TestCase('19.Short attr end big bold', ShortText + ',100,30,1,0,0,65535,65535,14,false,false,Leading,Leading,Character,claBlack,0,0.06,47.15,24,3,20,18,True,claRed,0.97,u6fhOQLGtx7A8p2bP0f1lawOPvCVV1A96OCDEf8fv4M/vOgPtB86+kJUT8nq4ANfeQXWg/49GA')]
    [TestCase('20.Short attr middle big bold with big textlayout size', ShortText + ',100,30,1,0,0,65535,65535,20,false,false,Leading,Leading,Character,claBlack,0,0.4,56.39,27,2,3,18,True,claRed,0.97,p39x+vzC625A84ij30b1kKSOvfGZU3D95t9vof8d0Eev++UO9E+6+TRUv4jq3wcfOQbhX759Fg')]
    {$ENDIF}
    procedure TestAttributesUnderline(const AText: string; const ABitmapWidth, ABitmapHeight: Integer; const AScale, ATextLeft, ATextTop, AMaxWidth, AMaxHeight, AFontSize: Single; const AWordWrap, ARightToLeft: Boolean; const AHorizontalAlign, AVerticalAlign: TTextAlign; const ATrimming: TTextTrimming; const AColor: string; const AExpectedTextLeft, AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom: Single; const AAttrRangePos, AAttrRangeLength: Integer; const AAttrFontSize: Single; const AAttrFontBold: Boolean; const AAttrColor: string; const AMinSimilarity: Double; const AExpectedImageHash: string);
    {$IFDEF TEXT_RENDER}
    [Test]
    {$ENDIF}
    procedure TestJapaneseLocale;
    {$IFDEF TEXT_RENDER}
    [Test]
    {$ENDIF}
    procedure TestSubpixelText;
    [Test]
    procedure TestWindowSurfacePropertiesWithoutWindow;
    {$IFDEF TEXT_RENDER}
    [TestCase('1', ShortText + ',9,false,false,Leading,Leading,Character,claBlack,0,0.03,25.84,12,0.97,y/NheQPRF99A9397rwb3guiOf/WnJ4C86Oa34bom1Cv/eeQ7wP/69R9AL8bt7K+u9gr8y71+CA')]
    [TestCase('2', ShortText + ',10,false,false,Leading,Leading,Character,claBlack,0,-0.3,28.72,13,0.97,1APCuQHTCz/B9ohrr8b2ftxOv/WrKwC96OW/EXsk15O/OuMmCAD79yc4n8bu9KMeeAcIDI5+CA')]
    [TestCase('3', ShortText + ',11,false,false,Leading,Leading,Character,claBlack,0,0.37,31.59,15,0.97,zgsief/LG08A9pRjHwf1e/QeP/SkU/D85+OzgXsly2/PuuEm3N869jD03gbv66ceOAb+Ex69Bw')]
    [TestCase('4', ShortText + ',12,false,false,Leading,Leading,Character,claBlack,0,0.04,34.46,16,0.97,0hMCugDU9z4B9olfv8bziOTOf/CtP9D86uO/cTsmzkOvOuQw4L869yz4/obu8aMOuAf3A649CA')]
    [TestCase('5', ShortText + ',13,false,false,Leading,Leading,Character,claBlack,0,-0.29,37.33,17,0.97,zx8iuf7LE9//9J1TX8fzg/R+P/KpW8A86NvHIbwiypPvuuIhHHB6+DQk/8bs9aPeuAX9I869Cg')]
    [TestCase('6', ShortText + ',14,false,false,Leading,Leading,Character,claBlack,0,0.38,40.2,19,0.97,1SeiOQDQB69A9Y9Xvwb2heg+v/OnO8C86OG7AXwkzoffOuQnCJC6+DIsf0bt9rsueAf+F16+CQ')]
    [TestCase('7', ShortText + ',15,false,false,Leading,Leading,Character,claBlack,0,0.05,43.07,20,0.97,0SdiOf/MDw9A9JhLL0f0hPReP/KnT6D86NzHIbwkyH//uuMn7I/69zf47gbt8b9euAX4H459Cg')]
    [TestCase('8', ShortText + ',16,false,false,Leading,Leading,Character,claBlack,0,-0.28,45.95,21,0.97,0jNSuf/LDz+A9JtDP4fygcyO//KrSxC95+SvYfwiw4cPu+QdAHD6+Tnwzobw6q9uOAQAQG49Bw')]
    [TestCase('9', ShortText + ',17,false,false,Leading,Leading,Character,claBlack,0,0.39,48.82,23,0.97,0BtSef3LA/+/9J9TXwfxf9xev/GoXwB959+zMTwjxX9fO+Mh6I86+DYI/wbw7K8+uAX7O879CA')]
    [TestCase('10', ShortText + ',18,false,false,Leading,Leading,Character,claBlack,0,0.06,51.69,24,0.97,1h9iOf/SB5+A9ZVLzwb0idSu//KoN7D86OCzQTwky3sve+Mo8H+6+DcYv4bu8rceeAf+M06+CQ')]
    [TestCase('11', ShortText + ',19,false,false,Leading,Leading,Character,claBlack,0,-0.27,54.56,25,0.97,2iuyuf/QA8+A9JhH7wb3gMxu//WyOyD96eK/kXwlylffOuUo6E/69zz07sbt7bv+9wb3L+79CQ')]
    [TestCase('12', ShortText + ',20,false,false,Leading,Leading,Character,claBlack,0,0.4,57.43,27,0.97,3SdS+v7W3z6B9JZLz8b1h7yOf/OyOyC96eSzIfwkxS8Pe+Qo7B969jLonsbu7a/etwf5Hz6+CA')]
    [TestCase('13', ShortText + ',21,false,false,Leading,Leading,Character,claBlack,0,0.07,60.3,28,0.97,2SPyuf/U9x4B9ZRH34b1h9Cuv/OuPwD96eWvEfwkyT/fuuQs3B+69jL0/gbu7afuNwf5F979CQ')]
    [TestCase('14', ShortText + ',22,false,false,Leading,Leading,Character,claBlack,0,-0.26,63.17,29,0.97,2CcCev/U6w6B9ZFXr8b0jNS+v/GrO9A86uKvIfwjyV8fu+MnAED69zMEz4bt8KdeOAb+I979CQ')]
    [TestCase('15', 'This is test text,14,false,false,Leading,Leading,Character,claBlack,0,0.38,91.41,19,0.97,z7dhuvbZr+7A94iPr4X0ZJjOvvetNzD+5Oab4Xwivbc/u+QcvH85+UNAL8jv3M+u+QcESH6+DA')]
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
  FMX.Graphics,

  { Skia }
  FMX.Skia.Canvas;

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

procedure TSkTextLayoutTests.TestJapaneseLocale;
var
  OldTextLocale: string;
begin
  OldTextLocale := GlobalSkiaTextLocale;
  GlobalSkiaTextLocale := 'ja';
  try
    Test('直', {BitmapSize} TSize.Create(100, 100), {Scale} 1, {FontSize} 72, {TopLeft} PointF(10, 3),
      {MaxSize} PointF(1000, 1000), {WordWrap} False, {RightToLeft} False, {HorizontalAlign} TTextAlign.Leading,
      {VerticalAlign} TTextAlign.Leading, {Trimming} TTextTrimming.None, TAlphaColors.Black,
      {ExpectedTextRect} TRectF.Create(0, 0, 0, 0), {CheckTextRect} False, {Attributes} [], {MinSimilarity} 0.98,
      {ExpectedImageHash} 'sD/iefuqA4+A7K5/H0b0YvDuO/mLp0D+4NpjUXsku7t/euwiVM/7/0RQb4bq5b8+OAgNlC0+Cw');
  finally
    GlobalSkiaTextLocale := OldTextLocale;
  end;
end;

procedure TSkTextLayoutTests.TestSubpixelText;

  function CountColorFringes(const ASubpixelText: Boolean): Integer;
  var
    LLayout: TSkTextLayout;
    LSurface: ISkSurface;
  begin
    GlobalSkiaSubpixelText := ASubpixelText;
    LSurface := TSkSurface.MakeRaster(TSkImageInfo.Create(100, 40), TSkSurfaceProperties.Create([], TSkPixelGeometry.RGBHorizontal));
    LSurface.Canvas.Clear(TAlphaColors.White);
    LLayout := TSkTextLayout.Create;
    try
      LLayout.BeginUpdate;
      try
        LLayout.Text := ShortText;
        LLayout.Font.Family := DefaultFontFamily;
        LLayout.Font.Size := 14;
        LLayout.Color := TAlphaColors.Black;
      finally
        LLayout.EndUpdate;
      end;
      LLayout.RenderLayout(LSurface.Canvas);
    finally
      LLayout.Free;
    end;
    Result := CountColoredPixels(LSurface);
  end;

var
  LOldSubpixelText: Boolean;
begin
  LOldSubpixelText := GlobalSkiaSubpixelText;
  try
    Assert.AreEqual(0, CountColorFringes(False), 'Without GlobalSkiaSubpixelText the text should be drawn in grayscale');
    Assert.IsTrue(CountColorFringes(True) > 0, 'With GlobalSkiaSubpixelText the text should be drawn with LCD antialiasing');
  finally
    GlobalSkiaSubpixelText := LOldSubpixelText;
  end;
end;

procedure TSkTextLayoutTests.TestWindowSurfacePropertiesWithoutWindow;
var
  LOldSubpixelText: Boolean;
begin
  LOldSubpixelText := GlobalSkiaSubpixelText;
  try
    GlobalSkiaSubpixelText := True;
    Assert.IsTrue(SkWindowSurfaceProperties(nil) = TSkSurfaceProperties.Create([], TSkPixelGeometry.Unknown),
      'Without a window the pixel geometry should be unknown');
  finally
    GlobalSkiaSubpixelText := LOldSubpixelText;
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
