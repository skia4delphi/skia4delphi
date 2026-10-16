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
    type
      TSkLabelClass = class of TSkLabel;
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
    procedure Test(const AText: string; const ABitmapSize: TSize; const AScale, AFontSize: Single; const ATextTopLeft, AMaxSize: TPointF; const AMaxLines: Integer; const ARightToLeft: Boolean; const AHorizontalAlign: TSkTextHorzAlign; const AVerticalAlign: TTextAlign; const ATrimming: TTextTrimming; const AColor: TAlphaColor; const AExpectedTextRect: TRectF; const ACheckTextRect: Boolean; ALabelClass: TSkLabelClass; const AMinSimilarity: Double; const AExpectedImageHash: string);
  protected
    function AssetsPath: string; override;
  public
    {$IFDEF TEXT_RENDER}
    [TestCase('1.Short', ShortText + ',100,40,1,0,0,65535,65535,14,1,false,Leading,Leading,Character,claBlack,0,0,41,19,0.97,0kPy+vOpM56C/J1TT0X1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('2.Short blue', ShortText + ',100,40,1,0,0,65535,65535,14,1,false,Leading,Leading,Character,#FF9F9FFF,0,0,41,19,0.97,0/+BPPOrW/6C/LK7X0T1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('3.Short horizontal leading', ShortText + ',100,40,1,0,0,100,40,14,1,false,Leading,Leading,Character,claBlack,0,0,41,19,0.97,0kPy+vOpM56C/J1TT0X1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('4.Short horizontal center', ShortText + ',100,40,1,0,0,100,40,14,1,false,Center,Leading,Character,claBlack,0,0,41,19,0.97,yY9Ce/i3W77A8LB7vwX0hKCufPCLY3D+4ttbUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('5.Short horizontal trailing', ShortText + ',100,40,1,0,0,100,40,14,1,false,Trailing,Leading,Character,claBlack,0,0,41,19,0.97,1VeyOvjAV15B9aCDDwb0o/h+vumIc+B87NlPUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('6.Short vertical center', ShortText + ',100,40,1,0,0,100,40,14,1,false,Leading,Center,Character,claBlack,0,0,41,19,0.97,1/cBe/rXa/7A9Z+Pr0b1lawOPvCVV1A96OCDAf0etKtfe+UUkI8490pUX8nq4ANfeQXVgw4+GA')]
    [TestCase('7.Short horizontal center, vertical center', ShortText + ',100,40,1,0,0,100,40,14,1,false,Center,Center,Character,claBlack,0,0,41,19,0.97,2kdC+vnHez4B9597L4b1hcjOvfOSZ6D95uSDIT0krCsfPOgPdI/6/VhYz0fs5/seuQXjN879EQ')]
    [TestCase('8.Short horizontal trailing, vertical center', ShortText + ',100,40,1,0,0,100,40,14,1,false,Trailing,Center,Character,claBlack,0,0,41,19,0.97,1VeyOvjAV15B9aCDDwb0g5TevvKlT7D95uWbUX0lq4MPfOUVVA86+GBAn0jr3d/eOQTgXw4/Fw')]
    [TestCase('9.Short vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,1,false,Leading,Trailing,Character,claBlack,0,0,41,19,0.97,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDEb0kslvvuuQprP859FxUX8nq4ANfeQXVgw4+GA')]
    [TestCase('10.Short horizontal center, vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,1,false,Center,Trailing,Character,claBlack,0,0,41,19,0.97,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDwTworYe/e+YGBP96911Mf0jqyQ8/uQbjP149Fg')]
    [TestCase('11.Short horizontal trailing, vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,1,false,Trailing,Trailing,Character,claBlack,0,0,41,19,0.97,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GAg/wnv6q8O+QXKx379Fg')]
    [TestCase('12.Long', LongText + ',100,40,1,0,0,65535,65535,14,0,false,Leading,Leading,Character,claBlack,0,0,6643,19,0.97,xVcyu/WpQ34C+KFPn0Tvl8RePeyNc1D95dBfUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('13.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,0,false,Leading,Leading,Character,claBlack,0,0,6643,1558,0.97,xVcyu/WpQ34C+KFPn0TyjrT+vOyMk1C+49BnIbwit1sfu+EcjA/69UhgX4ns2bOuOAruk569Eg')]
    [TestCase('14.Long horizontal center', LongText + ',100,40,1,0,0,100,40,14,0,false,Center,Leading,Character,claBlack,0,0,6643,1558,0.97,zpeCu/mqMy4C9KpvXwX2mcxuPeuIoyA+6dNXYb0nuIe/euIjfL8690lMTwnt2qNOOAjlZ679Ew')]
    [TestCase('15.Long horizontal trailing', LongText + ',100,40,1,0,0,100,40,14,0,false,Trailing,Leading,Character,claBlack,0,0,6643,1558,0.97,yIuyO/utc46B8raHT4Xwn+DeveqQb2C96N5XMT0ovq9vuuUeWO8691cMbwnu4ce++Abfx459EQ')]
    [TestCase('16.Long vertical center', LongText + ',100,40,1,0,0,100,40,14,0,false,Leading,Center,Character,claBlack,0,0,6643,1558,0.97,w2MS+/WwTz5B86czr8X0itBefe2OiwB+5NeDIXwls0/fuuMhdA+69E48H4nq0dfueAnoQy49Fg')]
    [TestCase('17.Long horizontal center, vertical center', LongText + ',100,40,1,0,0,100,40,14,0,false,Center,Center,Character,claBlack,0,0,6643,1558,0.97,zZ/yOvmpX06B8q97n4X0ksgu/uybb9D96NNPYX0qt6OP+uIhUO/691RQb0ns1bPOuArnkz69FA')]
    [TestCase('18.Long horizontal trailing, vertical center', LongText + ',100,40,1,0,0,100,40,14,0,false,Trailing,Center,Character,claBlack,0,0,6643,1558,0.97,y4vC+vm1X94A8bKDfwXwjtj+fO2ZWxC95syLcfwpt68PO+YXLK87+FII/wnu3asO+QbWh+59FQ')]
    [TestCase('19.Long vertical trailing', LongText + ',100,40,1,0,0,100,40,14,0,false,Leading,Trailing,Character,claBlack,0,0,6643,1558,0.97,wivievW0h36B8phbDwbzjqj+vO6aa9B86NBj8fwlsGMfe+QefL859F9cXwnr3gtPOQXXew4+GA')]
    [TestCase('20.Long horizontal center, vertical trailing', LongText + ',100,40,1,0,0,100,40,14,0,false,Center,Trailing,Character,claBlack,0,0,6643,1558,0.97,wi/iOva0f25B8plfDwbzj6jePO+Za8D8581nwXwosKvfe+UKGC/791xIz0jryQf/OAfhRy49GA')]
    [TestCase('21.Long horizontal trailing, vertical trailing', LongText + ',100,40,1,0,0,100,40,14,0,false,Trailing,Trailing,Character,claBlack,0,0,6643,1558,0.97,wS/yeva0f44B8plXD8byj7DOvO6XX4D8585rQX0lrIMfPOUVWP/592A0r0ns3K++uAXRmy59Fg')]
    [TestCase('22.Short low height horizontal leading', ShortText + ',100,13,1,0,2,100,9,14,1,false,Leading,Leading,None,claBlack,0,2,41,21,0.97,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDkb0jq0sv++Mn0K/591NUX8nq4ANfeQXVgw4+GA')]
    [TestCase('23.Short low height horizontal center', ShortText + ',100,13,1,0,2,100,9,14,1,false,Center,Leading,None,claBlack,0,2,41,21,0.97,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDkbwnt18vu+URLM9692Fobwjq2fvuuAbmPw5+Ew')]
    [TestCase('24.Short low height horizontal trailing', ShortText + ',100,13,1,0,2,100,9,14,1,false,Trailing,Leading,None,claBlack,0,2,41,21,0.97,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GAsD0rw58v+eAbM816+EQ')]
    [TestCase('25.Short low height horizontal leading vertical center', ShortText + ',100,13,1,0,2,100,9,12,1,false,Leading,Center,None,claBlack,0,2,35,18,0.97,0O9h+/nMX75B9527r0X1lawOPvCVV1A96OCDgTwbxHuv+t4cxK+39TVUX8nq4ANfeQXVgw4+GA')]
    [TestCase('26.Short low height horizontal center vertical center', ShortText + ',100,13,1,0,2,100,9,12,1,false,Center,Center,None,claBlack,0,2,35,18,0.97,2l9C+vnEd16B9px/n4X1hsT+PPSDi8A95dZvUTwmsF//O+gTTO+6/U9UDwfr0wd/+AX3+709Dw')]
    [TestCase('27.Short low height horizontal trailing vertical center', ShortText + ',100,13,1,0,2,100,9,12,1,false,Trailing,Center,None,claBlack,0,2,35,18,0.97,1VeyOvjAV15B9aCDD4bwgIhuvvCoP/C85+afUX0lq4MPfOUVVA86+GA8r0nv0ruuugTdww7/Ew')]
    [TestCase('28.Short low size horizontal leading vertical center', ShortText + ',40,13,1,5,2,30,9,12,1,false,Leading,Center,None,claBlack,5,2,40,18,0.97,y+/hOfjTf+5A9p97v4bzf7ROv/OWV0C+5/JvEf0npyuf+usUpH/6AFNIL4by5a8uNwYJ5H29CQ')]
    [TestCase('29.Short low size horizontal center vertical center', ShortText + ',40,13,1,5,2,30,9,12,1,false,Center,Center,None,claBlack,5,2,40,18,0.97,xi8SOvnBr04B955zbwb0fMDevvWTU3B95e2Dsfwkr28f++oWfL+6AE9UTwbz3Ls+9wQA4O38DA')]
    [TestCase('30.Short low size horizontal trailing vertical center', ShortText + ',40,13,1,5,2,30,9,12,1,false,Trailing,Center,None,claBlack,5,2,40,18,0.97,zF+S+vy2wy7C+Jt3H4b3e+TuPvegS3A8596zkXsjvJd/u+gUQF/7/j1Uv4bx0oPeuAP+z+38EA')]
    [TestCase('31.Short', ShortText + ',100,40,1,0,0,65535,65535,14,1,true,Trailing,Leading,Character,claBlack,0,0,41,19,0.97,0kPy+vOpM56C/J1TT0X1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('32.Short horizontal leading', ShortText + ',100,40,1,0,0,100,40,14,1,true,Trailing,Leading,Character,claBlack,0,0,41,19,0.97,0kPy+vOpM56C/J1TT0X1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('33.Short horizontal center', ShortText + ',100,40,1,0,0,100,40,14,1,true,Center,Leading,Character,claBlack,0,0,41,19,0.97,yY9Ce/i3W77A8LB7vwX0hKCufPCLY3D+4ttbUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('34.Short horizontal trailing', ShortText + ',100,40,1,0,0,100,40,14,1,true,Leading,Leading,Character,claBlack,0,0,41,19,0.97,1VeyOvjAV15B9aCDDwb0o/h+vumIc+B87NlPUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('35.Short vertical center', ShortText + ',100,40,1,0,0,100,40,14,1,true,Trailing,Center,Character,claBlack,0,0,41,19,0.97,1/cBe/rXa/7A9Z+Pr0b1lawOPvCVV1A96OCDAf0etKtfe+UUkI8490pUX8nq4ANfeQXVgw4+GA')]
    [TestCase('36.Short horizontal center, vertical center', ShortText + ',100,40,1,0,0,100,40,14,1,true,Center,Center,Character,claBlack,0,0,41,19,0.97,2kdC+vnHez4B9597L4b1hcjOvfOSZ6D95uSDIT0krCsfPOgPdI/6/VhYz0fs5/seuQXjN879EQ')]
    [TestCase('37.Short horizontal trailing, vertical center', ShortText + ',100,40,1,0,0,100,40,14,1,true,Leading,Center,Character,claBlack,0,0,41,19,0.97,1VeyOvjAV15B9aCDDwb0g5TevvKlT7D95uWbUX0lq4MPfOUVVA86+GBAn0jr3d/eOQTgXw4/Fw')]
    [TestCase('38.Short vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,1,true,Trailing,Trailing,Character,claBlack,0,0,41,19,0.97,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDEb0kslvvuuQprP859FxUX8nq4ANfeQXVgw4+GA')]
    [TestCase('39.Short horizontal center, vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,1,true,Center,Trailing,Character,claBlack,0,0,41,19,0.97,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDwTworYe/e+YGBP96911Mf0jqyQ8/uQbjP149Fg')]
    [TestCase('40.Short horizontal trailing, vertical trailing', ShortText + ',100,40,1,0,0,100,40,14,1,true,Leading,Trailing,Character,claBlack,0,0,41,19,0.97,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GAg/wnv6q8O+QXKx379Fg')]
    [TestCase('41.Long', LongText + ',100,40,1,0,0,65535,65535,14,0,true,Trailing,Leading,Character,claBlack,0,0,6643,19,0.97,z3ciu/asL66C96hLv8TuksgOveyMf5B959BHUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('42.Long horizontal leading', LongText + ',100,40,1,0,0,100,40,14,0,true,Trailing,Leading,Character,claBlack,0,0,6643,1558,0.97,xVcyu/WpQ34C+KFPn0TyjrT+vOyMk1C+49BnIbwit1sfu+EcjA/69UhgX4ns2bOuOAruk569Eg')]
    [TestCase('43.Long horizontal center', LongText + ',100,40,1,0,0,100,40,14,0,true,Center,Leading,Character,claBlack,0,0,6643,1558,0.97,zpeCu/mqMy4C9KpvXwX2mcxuPeuIoyA+6dNXYb0nuIe/euIjfL8690lMTwnt2qNOOAjlZ679Ew')]
    [TestCase('44.Long horizontal trailing', LongText + ',100,40,1,0,0,100,40,14,0,true,Leading,Leading,Character,claBlack,0,0,6643,1558,0.97,yIuyO/utc46B8raHT4Xwn+DeveqQb2C96N5XMT0ovq9vuuUeWO8691cMbwnu4ce++Abfx459EQ')]
    [TestCase('45.Long vertical center', LongText + ',100,40,1,0,0,100,40,14,0,true,Trailing,Center,Character,claBlack,0,0,6643,1558,0.97,w2MS+/WwTz5B86czr8X0itBefe2OiwB+5NeDIXwls0/fuuMhdA+69E48H4nq0dfueAnoQy49Fg')]
    [TestCase('46.Long horizontal center, vertical center', LongText + ',100,40,1,0,0,100,40,14,0,true,Center,Center,Character,claBlack,0,0,6643,1558,0.97,zZ/yOvmpX06B8q97n4X0ksgu/uybb9D96NNPYX0qt6OP+uIhUO/691RQb0ns1bPOuArnkz69FA')]
    [TestCase('47.Long horizontal trailing, vertical center', LongText + ',100,40,1,0,0,100,40,14,0,true,Leading,Center,Character,claBlack,0,0,6643,1558,0.97,y4vC+vm1X94A8bKDfwXwjtj+fO2ZWxC95syLcfwpt68PO+YXLK87+FII/wnu3asO+QbWh+59FQ')]
    [TestCase('48.Long vertical trailing', LongText + ',100,40,1,0,0,100,40,14,0,true,Trailing,Trailing,Character,claBlack,0,0,6643,1558,0.97,wivievW0h36B8phbDwbzjqj+vO6aa9B86NBj4TwnsofvOuQeYC+6811cXwnr3gtPOQXXew4+GA')]
    [TestCase('49.Long horizontal center, vertical trailing', LongText + ',100,40,1,0,0,100,40,14,0,true,Center,Trailing,Character,claBlack,0,0,6643,1558,0.97,wi/iOva0f25B8plfDwbzj6jePO+Za8D8581n4fworZ/fu+UJGP8691xQ74jrzv8eeQjlQ/78GA')]
    [TestCase('50.Long horizontal trailing, vertical trailing', LongText + ',100,40,1,0,0,100,40,14,0,true,Leading,Trailing,Character,claBlack,0,0,6643,1558,0.97,wS/yeva0f44B8plXD8byj7DOvO6XX4D8585rQX0lrIMfPOUVWP/592A0H8rt47f+eAfPqx59Fg')]
    [TestCase('51.Short low height horizontal leading', ShortText + ',100,13,1,0,2,100,9,14,1,true,Trailing,Leading,None,claBlack,0,2,41,21,0.97,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDkb0jq0sv++Mn0K/591NUX8nq4ANfeQXVgw4+GA')]
    [TestCase('52.Short low height horizontal center', ShortText + ',100,13,1,0,2,100,9,14,1,true,Center,Leading,None,claBlack,0,2,41,21,0.97,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDkbwnt18vu+URLM9692Fobwjq2fvuuAbmPw5+Ew')]
    [TestCase('53.Short low height horizontal trailing', ShortText + ',100,13,1,0,2,100,9,14,1,true,Leading,Leading,None,claBlack,0,2,41,21,0.97,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GAsD0rw58v+eAbM816+EQ')]
    [TestCase('54.Short low height horizontal leading vertical center', ShortText + ',100,13,1,0,2,100,9,12,1,true,Trailing,Center,None,claBlack,0,2,35,18,0.97,0O9h+/nMX75B9527r0X1lawOPvCVV1A96OCDgTwbxHuv+t4cxK+39TVUX8nq4ANfeQXVgw4+GA')]
    [TestCase('55.Short low height horizontal center vertical center', ShortText + ',100,13,1,0,2,100,9,12,1,true,Center,Center,None,claBlack,0,2,35,18,0.97,2l9C+vnEd16B9px/n4X1hsT+PPSDi8A95dZvUTwmsF//O+gTTO+6/U9UDwfr0wd/+AX3+709Dw')]
    [TestCase('56.Short low height horizontal trailing vertical center', ShortText + ',100,13,1,0,2,100,9,12,1,true,Leading,Center,None,claBlack,0,2,35,18,0.97,1VeyOvjAV15B9aCDD4bwgIhuvvCoP/C85+afUX0lq4MPfOUVVA86+GA8r0nv0ruuugTdww7/Ew')]
    [TestCase('57.Short low size horizontal leading vertical center', ShortText + ',40,13,1,5,2,30,9,12,1,true,Trailing,Center,None,claBlack,5,2,40,18,0.97,y+/hOfjTf+5A9p97v4bzf7ROv/OWV0C+5/JvEf0npyuf+usUpH/6AFNIL4by5a8uNwYJ5H29CQ')]
    [TestCase('58.Short low size horizontal center vertical center', ShortText + ',40,13,1,5,2,30,9,12,1,true,Center,Center,None,claBlack,5,2,40,18,0.97,xi8SOvnBr04B955zbwb0fMDevvWTU3B95e2Dsfwkr28f++oWfL+6AE9UTwbz3Ls+9wQA4O38DA')]
    [TestCase('59.Short low size horizontal trailing vertical center', ShortText + ',40,13,1,5,2,30,9,12,1,true,Leading,Center,None,claBlack,5,2,40,18,0.97,zF+S+vy2wy7C+Jt3H4b3e+TuPvegS3A8596zkXsjvJd/u+gUQF/7/j1Uv4bx0oPeuAP+z+38EA')]
    {$ENDIF}
    procedure GenericTest(const AText: string; const AControlWidth, AControlHeight: Integer; const AScale, ATextLeft, ATextTop, AMaxWidth, AMaxHeight, AFontSize: Single; const AMaxLines: Integer; const ARightToLeft: Boolean; const AHorizontalAlign: TSkTextHorzAlign; const AVerticalAlign: TTextAlign; const ATrimming: TTextTrimming; const AColor: string; const AExpectedTextLeft, AExpectedTextTop, AExpectedTextRight, AExpectedTextBottom: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    {$IFDEF TEXT_RENDER}
    [Test]
    {$ENDIF}
    procedure TestJapaneseLocale;
    {$IF Defined(TEXT_RENDER) AND (CompilerVersion > 28)}
    [Test]
    {$ENDIF}
    procedure TestStyle;
    {$IFDEF TEXT_RENDER}
    [Test]
    {$ENDIF}
    procedure TestSubpixelText;
    {$IFDEF TEXT_RENDER}
    [Test]
    {$ENDIF}
    procedure TestWordsInheritHeightMultiplierAndLetterSpacing;
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
  FMX.Controls,
  FMX.Graphics,
  FMX.Forms,
  FMX.Layouts,
  FMX.Objects;

type
  TSkLabelAccess = class(TSkLabel)
  public
    procedure DrawTo(const ACanvas: ISkCanvas);
    property ParagraphBounds;
  end;

  TSkLabelDarkStyled = class(TSkLabel)
  private
    FResourceLink: TFmxObject;
  protected
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single); override;
    function GetResourceLink: TFmxObject; {$IF CompilerVersion > 28}override;{$ENDIF}
  public
    function FindStyleResource(const AStyleLookup: string; const Clone: Boolean = False): TFmxObject; override;
  end;

{ TSkLabelAccess }

procedure TSkLabelAccess.DrawTo(const ACanvas: ISkCanvas);
begin
  Draw(ACanvas, LocalRect, 1);
end;

{ TSkLabelDarkStyled }

procedure TSkLabelDarkStyled.Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  ApplyStyle;
  inherited;
end;

function TSkLabelDarkStyled.FindStyleResource(const AStyleLookup: string; const Clone: Boolean): TFmxObject;
begin
  Result := GetResourceLink.FindStyleResource(AStyleLookup, Clone);
end;

function TSkLabelDarkStyled.GetResourceLink: TFmxObject;
var
  LLayout: TLayout;
  LText: TText;
begin
  if FResourceLink = nil then
  begin
    LLayout := TLayout.Create(Self);
    LLayout.StyleName := 'Labelstyle';
    LText := TText.Create(LLayout);
    LText.StyleName := 'text';
    LText.Align := TAlignLayout.Client;
    LText.TextSettings.FontColor := TAlphaColors.White;
    LText.Parent := LLayout;
    FResourceLink := LLayout;
  end;
  Result := FResourceLink;
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
    True, TSkLabel, AMinSimilarity, AExpectedImageHash);
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
  const ACheckTextRect: Boolean; ALabelClass: TSkLabelClass; const AMinSimilarity: Double;
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
        LLabel := ALabelClass.Create(nil);
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

procedure TSkLabelTests.TestJapaneseLocale;
var
  OldTextLocale: string;
begin
  OldTextLocale := GlobalSkiaTextLocale;
  GlobalSkiaTextLocale := 'ja';
  try
    Test('直', {BitmapSize} TSize.Create(100, 100), {AScale} 1, {FontSize} 72, {TextTopLeft} PointF(10, 3),
      {MaxSize} PointF(1000, 1000), 1, {RightToLeft} False, {HorizontalAlign} TSkTextHorzAlign.Leading,
      {VerticalAlign} TTextAlign.Leading, {Trimming} TTextTrimming.None, TAlphaColors.Black,
      {ExpectedTextRect} TRectF.Create(0, 0, 0, 0), {CheckTextRect} False, {SkLabelClass} TSkLabel,
      {MinSimilarity} 0.98,
      {ExpectedImageHash} 'sD/iefuqA4+A7K5/H0b0YvDuO/mLp0D+4NpjUXsku7t/euwiVM/7/0RQb4bq5b8+OAgNlC0+Cw');
  finally
    GlobalSkiaTextLocale := OldTextLocale;
  end;
end;

procedure TSkLabelTests.TestSubpixelText;

  function CountColorFringes(const ASubpixelText: Boolean): Integer;
  var
    LLabel: TSkLabelAccess;
    LSurface: ISkSurface;
  begin
    GlobalSkiaSubpixelText := ASubpixelText;
    LSurface := TSkSurface.MakeRaster(TSkImageInfo.Create(100, 40), TSkSurfaceProperties.Create([], TSkPixelGeometry.RGBHorizontal));
    LSurface.Canvas.Clear(TAlphaColors.White);
    LLabel := TSkLabelAccess.Create(nil);
    try
      LLabel.SetBounds(0, 0, 100, 40);
      LLabel.TextSettings.Font.Families := DefaultFontFamily;
      LLabel.TextSettings.Font.Size := 14;
      LLabel.TextSettings.FontColor := TAlphaColors.Black;
      LLabel.Text := ShortText;
      LLabel.DrawTo(LSurface.Canvas);
    finally
      LLabel.Free;
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

procedure TSkLabelTests.TestStyle;
begin
  Test('A', {BitmapSize} TSize.Create(100, 100), {AScale} 1, {FontSize} 72, {TextTopLeft} PointF(10, 3),
    {MaxSize} PointF(1000, 1000), 1, {RightToLeft} False, {HorizontalAlign} TSkTextHorzAlign.Leading,
    {VerticalAlign} TTextAlign.Leading, {Trimming} TTextTrimming.None, TAlphaColors.Black,
    {ExpectedTextRect} TRectF.Create(0, 0, 0, 0), {CheckTextRect} False, {SkLabelClass} TSkLabelDarkStyled,
    {MinSimilarity} 0.98,
    {ExpectedImageHash} '2hPy+RO/r57B56u730X1lawOPvCVV1A96OCDUUIeAPQ+vugDUN/A+A1Uj4fq1LueOgboJ979FQ');
end;

procedure TSkLabelTests.TestWordsInheritHeightMultiplierAndLetterSpacing;

  function GetParagraphSize(const AWordsStyledSettings: TStyledSettings;
    const AWordsHeightMultiplier, AWordsLetterSpacing: Single): TSizeF;
  var
    LLabel: TSkLabelAccess;
  begin
    LLabel := TSkLabelAccess.Create(nil);
    try
      LLabel.TextSettings.Font.Families := DefaultFontFamily;
      LLabel.TextSettings.Font.Size := 14;
      LLabel.TextSettings.HeightMultiplier := 3;
      LLabel.TextSettings.LetterSpacing := 5;
      LLabel.Text := ShortText;
      LLabel.Words[0].HeightMultiplier := AWordsHeightMultiplier;
      LLabel.Words[0].LetterSpacing := AWordsLetterSpacing;
      LLabel.Words[0].StyledSettings := AWordsStyledSettings;
      Result := LLabel.ParagraphBounds.Size;
    finally
      LLabel.Free;
    end;
  end;

var
  LExpectedSize: TSizeF;
  LSize: TSizeF;
begin
  LExpectedSize := GetParagraphSize(AllStyledSettings, 0, 0);
  LSize := GetParagraphSize(AllStyledSettings - [TStyledSetting.Other], 0, 0);
  Assert.AreSameValue(LExpectedSize.Width, LSize.Width, TEpsilon.Position, 'Words without their own LetterSpacing should use the label one');
  Assert.AreSameValue(LExpectedSize.Height, LSize.Height, TEpsilon.Position, 'Words without their own HeightMultiplier should use the label one');

  LSize := GetParagraphSize(AllStyledSettings - [TStyledSetting.Other], 1, 1);
  Assert.IsTrue(LSize.Width < LExpectedSize.Width, 'The LetterSpacing of the words should override the label one');
  Assert.IsTrue(LSize.Height < LExpectedSize.Height, 'The HeightMultiplier of the words should override the label one');
end;

initialization
  TDUnitX.RegisterTestFixture(TSkLabelTests);
end.
