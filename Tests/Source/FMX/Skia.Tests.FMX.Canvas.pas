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
unit Skia.Tests.FMX.Canvas;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Math.Vectors,
  FMX.Types,
  FMX.Graphics,
  DUnitX.TestFramework,

  { Skia }
  System.Skia,
  FMX.Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkFMXCodecTests }

  [TestFixture]
  TSkFMXCanvasTests = class(TTestBase)
  private
    procedure ApplyGradientPoints(AGradient: TGradient; APoints: string);
    function CreateBitmap(const AImageFileName: string): TBitmap;
    function CreateMatrix(AScaleX, AScaleY, ADeltaX, ADeltaY, ARotationDegree: Single): TMatrix;
    procedure DrawChessBackground(ACanvas: TCanvas; ASquareSize: Single; AEvenSquareColor, AOddSquareColor: TAlphaColor);
    function StringToCorners(const ACornersString: string): TCorners;
    function StringToPolygon(APolygonString: string; const ABounds: TRectF): TPolygon;
    function StringToSides(const ASidesString: string): TSides;
  public
    [TestCase('1', '0.98,8DuvfukDlBDAG01D7zb8zqtf+gAlBPBG09C7Db/z6pc+QAkBvNE09G7D77z6pQ9QQgBvNA292w')]
    procedure TestClear(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,3Wvhu/PUA//AAIdvj0H3WvjuPPXAPzDA4dtj0L0WvjtPPfAPDHD49hh0r4XvzlMP/AMDHL49Bg')]
    procedure TestClear2(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,iqvuv+/KV24CHhqbv3zmAiQvgOjqXw9E3/IP37nm3Off/O8U8OCy7/57boH2tW8fwP8OkF6+Cw')]
    procedure TestClearRect(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,yR/COvnUX55B9qGnf4Xgx7uf/enO58+D2t+Trzjq7NOfu/UBbEH19fRT/wjpzCfP+QXk3x1+FA')]
    procedure TestClearRect2(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,8DuvfukDlBDAG01D7zb8zqtf+gAlBPBG09C7Db/z6pc+QAkBvNE09G7D77z6pQ9QQgBvNA292w')]
    procedure TestClipRect(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,1VeievjBV25B9aCD/0X1gsDO/vOiZ5C95umLoXwmq29vPOcVQI+6+FeQ37z6vOP/gQJduO384A')]
    procedure TestClipRect2(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,1VeyOvjAV15B9aCDD0b0jaiePvKiW2D96OSP4bwlpXPfe+cVSJ95+Vqw30P1ihexRPwCIO/+/A')]
    procedure TestClipRect3(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,ERT/gtQohA8FKy5bbzj/MQhv/+Tw/77E8PXnbjrvpxdQ/PcfzKDt4YTL34T0gyNRhfr4885/+Q')]
    procedure TestClipRect4(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,8DuvfukDlBDAG01D7zb8zqtf+gAlBPBG09C7Db/z6pc+QAkBvNE09G7D77z6pQ9QQgBvNA292w')]
    procedure TestClipRect5(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,1VeievjBV25B9aCD/0X1gsDO/vOiZ5C95umLoXwmq29vPOcVQI+6+FeQ37z6vOP/gQJduO384A')]
    procedure TestClipRect6(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,yCMSe/nUq35B9aKvj8X767Mfuv0PAGBF29L3/b7w4+cO/wgH2MG08HmHX4nqyivPeQPmAz7+Eg')]
    procedure TestClipRect7(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    procedure TestClipRect8(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,zy8SuvnDf67B9Z2nv4XxLFA/P/7F1wC957Qv4roy1+MO+fDdC2/BBjIQn0L6xTv9dwpb8Nz6/g')]
    procedure TestClipRect9(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  '3d-shapes.svg,660,343,1,0,0,660,343,0,0,660,343,1,false,false,0.98,A/jwfwDTr58C+9PTv8T3F6wvxfDVg0H/4viXYfz3DJAgv/vXO+CA+iX0j0ECi5tvu/MhEA+/8Q')]
    [TestCase('2',  '3d-shapes.svg,660,343,1,0,0,660,343,0,0,660,343,1,false,true,0.98,A/jwfwDTr58C+9PTv8T3F6wvxfDVg0H/4viXYfz3DJAgv/vXO+CA+iX0j0ECi5tvu/MhEA+/8Q')]
    [TestCase('3',  '3d-shapes.svg,660,343,1,0,0,660,343,0,0,660,343,1,true,false,0.98,A/jwfwDTr58C+9PTv8T3F6wvxfDVg0H/4viXYfz3DJAgv/vXO+CA+iX0j0ECi5tvu/MhEA+/8Q')]
    [TestCase('4',  '3d-shapes.svg,660,343,1,0,0,660,343,0,0,660,343,1,true,true,0.98,A/jwfwDTr58C+9PTv8T3F6wvxfDVg0H/4viXYfz3DJAgv/vXO+CA+iX0j0ECi5tvu/MhEA+/8Q')]
    [TestCase('5',  '3d-shapes.svg,660,343,1,0,0,960,343,0,0,660,343,1,false,false,0.98,A/jwfwDTr58C+9PTv8T3F6wvxfDVg0H/4viXYfz3DJAgv/vXO+CA+iX0j0ECi5tvu/MhEA+/8Q')]
    [TestCase('6',  '3d-shapes.svg,660,343,1,0,0,960,343,0,0,660,343,1,false,true,0.98,A/jwfwDTr58C+9PTv8T3F6wvxfDVg0H/4viXYfz3DJAgv/vXO+CA+iX0j0ECi5tvu/MhEA+/8Q')]
    [TestCase('7',  '3d-shapes.svg,660,343,1,0,0,960,343,0,0,660,343,1,true,false,0.98,A/jwfwDTr58C+9PTv8T3F6wvxfDVg0H/4viXYfz3DJAgv/vXO+CA+iX0j0ECi5tvu/MhEA+/8Q')]
    [TestCase('8',  '3d-shapes.svg,660,343,1,0,0,960,343,0,0,660,343,1,true,true,0.98,A/jwfwDTr58C+9PTv8T3F6wvxfDVg0H/4viXYfz3DJAgv/vXO+CA+iX0j0ECi5tvu/MhEA+/8Q')]
    [TestCase('9',  '3d-shapes.svg,660,343,1,0,0,660,343,0,0,960,343,1,false,false,0.98,DdQg/QteCE8A8exrAEX2Q5z/vgOC+/+9+v4nwT32vO8wwAKpg4AACRy80ATzsetvvO0ZdKA84g')]
    [TestCase('10', '3d-shapes.svg,660,343,1,0,0,660,343,0,0,960,343,1,false,true,0.98,DdQg/QteCE8A8exrAEX2Q5z/vgOC+/+9+v4nwT32vO8wwAKpg4AACRy80ATzsetvvO0ZdKA84g')]
    [TestCase('11', '3d-shapes.svg,660,343,1,0,0,660,343,0,0,960,343,1,true,false,0.98,DdQg/QteCE8A8exrAEX2Q5z/vgOC+/+9+v4nwT32vO8wwAKpg4AACRy80ATzsetvvO0ZdKA84g')]
    [TestCase('12', '3d-shapes.svg,660,343,1,0,0,660,343,0,0,960,343,1,true,true,0.98,DdQg/QteCE8A8exrAEX2Q5z/vgOC+/+9+v4nwT32vO8wwAKpg4AACRy80ATzsetvvO0ZdKA84g')]
    [TestCase('13', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,343,1,false,false,0.98,1VeyOvjAV15B9aCDDwbeUVjuA+TOKwB37Ba4oP0SE9y//wJOhC8+/vYTD3wLfpSvAiTC760BCQ')]
    [TestCase('14', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,343,1,false,true,0.98,1VeyOvjAV15B9aCDDwbeUVjuA+TOKwB37Ba4oP0SE9y//wJOhC8+/vYTD3wLfpSvAiTC760BCQ')]
    [TestCase('15', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,343,1,true,true,0.98,1VeyOvjAV15B9aCDDwbeUVjuA+TOKwB37Ba4oP0SE9y//wJOhC8+/vYTD3wLfpSvAiTC760BCQ')]
    [TestCase('16', '3d-shapes.svg,660,343,2,0,0,660,343,0,0,660,343,1,false,true,0.98,14fyd/Svxw6+75x3L4XkNgQvRQD/Q+E5/BMMMboJoaffQgcWTJ//AA1Y0YnoAryvO+zdmyA/Gw')]
    [TestCase('17', '3d-shapes.svg,660,343,2,0,0,660,343,0,0,660,343,1,true,true,0.98,14fyd/Svxw6+75x3L4XkNgQvRQD/Q+E5/BMMMboJoaffQgcWTJ//AA1Y0YnoAryvO+zdmyA/Gw')]
    [TestCase('18', '3d-shapes.svg,660,343,2,660,343,0,0,0,0,660,343,1,false,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('19', '3d-shapes.svg,660,343,2,660,343,0,0,0,0,660,343,1,true,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('20', '3d-shapes.svg,660,343,2,0,0,660,343,660,343,0,0,1,false,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('21', '3d-shapes.svg,660,343,2,0,0,660,343,660,343,0,0,1,true,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('22', '3d-shapes.svg,660,343,1,660,343,0,0,660,343,0,0,1,false,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('23', '3d-shapes.svg,660,343,1,660,343,0,0,660,343,0,0,1,true,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('24', '3d-shapes.svg,200,200,1,660,0,0,0,0,0,660,343,1,false,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('25', '3d-shapes.svg,200,200,1,660,0,0,0,0,0,660,343,1,true,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('26', '3d-shapes.svg,200,200,1,0,0,0,0,0,0,660,343,1,false,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('27', '3d-shapes.svg,200,200,1,0,0,0,0,0,0,660,343,1,true,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('28', '3d-shapes.svg,200,200,1,-100,-100,0,0,0,0,660,343,1,false,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('29', '3d-shapes.svg,200,200,1,-100,-100,0,0,0,0,660,343,1,true,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('30', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,0,1,false,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('31', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,0,1,true,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('32', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,-10,1,false,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('33', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,-10,1,true,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('34', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,1,false,true,0.98,JCjQvu7XS8+8E84Pv3/krA9hPfPngxJF0f8TIPgEnvPQfQgUCE79BTSsfXgS38cf/hgF3I3/+w')]
    [TestCase('35', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,1,true,true,0.98,JCjQvu7XS8+8E84Pv3/krA9hPfPngxJF0f8TIPgEnvPQfQgUCE79BTSsfXgS38sf/hgF3I3/+w')]
    [TestCase('36', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,1,false,true,0.98,abhfwfDS7498Ggucv/sgBHyvQ/wD+H0ACpi/PzrplKNQfA8MzJ/5/jXwr4D1IUjhxvHMQ4LECA')]
    [TestCase('37', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,1,true,true,0.98,aLhfwfDS7498Ggqcv/sgBHivQ/wD+H0ACpi/PzrplJ9QfA8MzJ/5/jXwr4D1IUjhxvHMQ4LECA')]
    [TestCase('38', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,1,false,true,0.98,A/jgvwDTr5/C+tPTv8T3F6wvxfDVg0H/4viXYfz3DJAgv/vXO+CA+iX0j0ECi5tvu/MhEA+/8Q')]
    [TestCase('39', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,1,true,true,0.98,A/jgfwDTr5/C+tPTv8T3F6wvxfDVg0H/4viXYfz3DJAgv/vXO+CA+iX0j0ECi5tvu/MhEA+/8Q')]
    [TestCase('40', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,1,false,false,0.98,JCjQvu7XS8+8E84Pv3/krA9hPfPngxJF0f8TIPgEnvPQfQgUCE79BTSsfXgS38cf/hgF3I3/+w')]
    [TestCase('41', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,1,true,false,0.98,JCjQvu7XS8+8E84Pv3/krA9hPfPngxJF0f8TIPgEnvPQfQgUCE79BTSsfXgS38sf/hgF3I3/+w')]
    [TestCase('42', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,1,false,false,0.98,abhfwfDS7498Ggucv/sgBHyvQ/wD+H0ACpi/PzrplKNQfA8MzJ/5/jXwr4D1IUjhxvHMQ4LECA')]
    [TestCase('43', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,1,true,false,0.98,aLhfwfDS7498Ggqcv/sgBHivQ/wD+H0ACpi/PzrplJ9QfA8MzJ/5/jXwr4D1IUjhxvHMQ4LECA')]
    [TestCase('44', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,1,false,false,0.98,A/jgvwDTr5/C+tPTv8T3F6wvxfDVg0H/4viXYfz3DJAgv/vXO+CA+iX0j0ECi5tvu/MhEA+/8Q')]
    [TestCase('45', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,1,true,false,0.98,A/jgfwDTr5/C+tPTv8T3F6wvxfDVg0H/4viXYfz3DJAgv/vXO+CA+iX0j0ECi5tvu/MhEA+/8Q')]
    [TestCase('46', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,0.3,false,true,0.98,7bPhe/XHo/6//q5fL0TwUGDfPfGu/6B/4ekTwbsbp/OP/O8V8A57/FPYXoT24D/PegvjU36+Dw')]
    [TestCase('47', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,0.3,true,true,0.98,7a/xO/XHo/6//q5fH0TwT2TfPfGu/7B/4eoTwbsbp/OfPPAV8A57/FLUTsT24D/PegvkU36+Dw')]
    [TestCase('48', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,0.3,false,true,0.98,Aoyx/PXF0+5/AMCL/4ICaui+//O2o09+8sr7YHwTpNcf/PETeO85+lOEzwbu9LNvvf/So/+/Ew')]
    [TestCase('49', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,0.3,true,true,0.98,Aoyx/PXG0+6/AMGL74ICaui+//O3o09+8sr7YDwTpNsfPPITeO85+lOEvwbu9LN/ff/Spw9AEw')]
    [TestCase('50', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,0.3,true,true,0.98,4+9BvPrGv74B97CbrwX2b/guQPCps/C95ueHEb0XyNP/POwDnC/8+E6E/8bxxjP/OQDsr16+DA')]
    [TestCase('51', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,0.3,false,false,0.98,7bPhe/XHo/6//q5fL0TwUGDfPfGu/6B/4ekTwbsbp/OP/O8V8A57/FPYXoT24D/PegvjU36+Dw')]
    [TestCase('52', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,0.3,true,false,0.98,7a/xO/XHo/6//q5fH0TwT2TfPfGu/7B/4eoTwbsbp/OfPPAV8A57/FLUTsT24D/PegvkU36+Dw')]
    [TestCase('53', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,0.3,false,false,0.98,Aoyx/PXF0+5/AMCL/4ICaui+//O2o09+8sr7YHwTpNcf/PETeO85+lOEzwbu9LNvvf/So/+/Ew')]
    [TestCase('54', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,0.3,true,false,0.98,Aoyx/PXG0+6/AMGL74ICaui+//O3o09+8sr7YDwTpNsfPPITeO85+lOEvwbu9LN/ff/Spw9AEw')]
    [TestCase('55', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,0.3,true,false,0.98,4+9BvPrGv74B97CbrwX2b/guQPCps/C95ueHEb0XyNP/POwDnC/8+E6E/8bxxjP/OQDsr16+DA')]
    [TestCase('56', '3d-shapes.svg,300,300,1.5,0,0,0,0,0,0,200,200,1,true,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('57', '3d-shapes.svg,300,300,1.5,0,0,0,0,0,0,200,200,1,true,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('58', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,0,0,1,true,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('59', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,0,0,1,true,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('60', '3d-shapes.svg,300,300,1.5,0,0,660,343,200,200,200,200,1,true,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('61', '3d-shapes.svg,300,300,1.5,0,0,660,343,200,200,200,200,1,true,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('62', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,0,true,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('63', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,0,true,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    procedure TestDrawBitmap(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer; ASurfaceScale, ASrcLeft, ASrcTop, ASrcRight, ASrcBottom, ADestLeft, ADestTop, ADestRight, ADestBottom, AOpacity: Single; AHighSpeed, ABlending: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [Test]
    procedure TestDrawBitmapScaledKeepsHighSampling;
    [TestCase('1', '0.25,false')]
    [TestCase('2', '0.5,false')]
    [TestCase('3', '0.75,false')]
    [TestCase('4', '0.25,true')]
    [TestCase('5', '0.5,true')]
    [TestCase('6', '0.75,true')]
    procedure TestDrawBitmapSubpixelOffset(const AOffset: Single; AHighSpeed: Boolean);
    [TestCase('1',  '3d-shapes.svg,660,343,1,0,0,660,343,0,0,660,343,1,false,false,0.98,6itOAgIPiIA/De8DEHv2drcg//4LtK8D9gc8zz7iJPAPgQncFwFB/9JzP7kMEXiAAgJVaP8A5Q')]
    [TestCase('2',  '3d-shapes.svg,660,343,1,0,0,660,343,0,0,660,343,1,false,true,0.98,6itOAgIPiIA/De8DEHv2drcg//4LtK8D9gc8zz7iJPAPgQncFwFB/9JzP7kMEXiAAgJVaP8A5Q')]
    [TestCase('3',  '3d-shapes.svg,660,343,1,0,0,660,343,0,0,660,343,1,true,false,0.98,6itOAgIPiIA/De8DEHv2drcg//4LtK8D9gc8zz7iJPAPgQncFwFB/9JzP7kMEXiAAgJVaP8A5Q')]
    [TestCase('4',  '3d-shapes.svg,660,343,1,0,0,660,343,0,0,660,343,1,true,true,0.98,6itOAgIPiIA/De8DEHv2drcg//4LtK8D9gc8zz7iJPAPgQncFwFB/9JzP7kMEXiAAgJVaP8A5Q')]
    [TestCase('5',  '3d-shapes.svg,660,343,1,0,0,960,343,0,0,660,343,1,false,false,0.98,6itOAgIPiIA/De8DEHv2drcg//4LtK8D9gc8zz7iJPAPgQncFwFB/9JzP7kMEXiAAgJVaP8A5Q')]
    [TestCase('6',  '3d-shapes.svg,660,343,1,0,0,960,343,0,0,660,343,1,false,true,0.98,6itOAgIPiIA/De8DEHv2drcg//4LtK8D9gc8zz7iJPAPgQncFwFB/9JzP7kMEXiAAgJVaP8A5Q')]
    [TestCase('7',  '3d-shapes.svg,660,343,1,0,0,960,343,0,0,660,343,1,true,false,0.98,6itOAgIPiIA/De8DEHv2drcg//4LtK8D9gc8zz7iJPAPgQncFwFB/9JzP7kMEXiAAgJVaP8A5Q')]
    [TestCase('8',  '3d-shapes.svg,660,343,1,0,0,960,343,0,0,660,343,1,true,true,0.98,6itOAgIPiIA/De8DEHv2drcg//4LtK8D9gc8zz7iJPAPgQncFwFB/9JzP7kMEXiAAgJVaP8A5Q')]
    [TestCase('9',  '3d-shapes.svg,660,343,1,0,0,660,343,0,0,960,343,1,false,false,0.98,7yPOwAIkkFA/Dd0nILvyoctwwP8s8L/C9x04rz7jKfx/wQnaLwFBANLH7zYI9S+AAv5XcK8+7g')]
    [TestCase('10', '3d-shapes.svg,660,343,1,0,0,660,343,0,0,960,343,1,false,true,0.98,7yPOwAIkkFA/Dd0nILvyoctwwP8s8L/C9x04rz7jKfx/wQnaLwFBANLH7zYI9S+AAv5XcK8+7g')]
    [TestCase('11', '3d-shapes.svg,660,343,1,0,0,660,343,0,0,960,343,1,true,false,0.98,7yPOwAIkkFA/Dd0nILvyoctwwP8s8L/C9x04rz7jKfx/wQnaLwFBANLH7zYI9S+AAv5XcK8+7g')]
    [TestCase('12', '3d-shapes.svg,660,343,1,0,0,660,343,0,0,960,343,1,true,true,0.98,7yPOwAIkkFA/Dd0nILvyoctwwP8s8L/C9x04rz7jKfx/wQnaLwFBANLH7zYI9S+AAv5XcK8+7g')]
    [TestCase('13', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,343,1,false,false,0.98,zyduQQD/p4C+D9IH0Lv1e0PAAAEqWICC9A4sDj/lIwCQAhACOMH8/Zyvn3YQMWxwxQcULK/C6g')]
    [TestCase('14', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,343,1,false,true,0.98,zyduQQD/p4C+D9IH0Lv1e0PAAAEqWICC9A4sDj/lIwCQAhACOMH8/Zyvn3YQMWxwxQcULK/C6g')]
    [TestCase('15', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,343,1,true,true,0.98,zyduQQD/p4C+D9IH0Lv1e0PAAAEqWICC9A4sDj/lIwCQAhACOMH8/Zyvn3YQMWxwxQcULK/C6g')]
    [TestCase('16', '3d-shapes.svg,660,343,2,0,0,660,343,0,0,660,343,1,false,true,0.98,11POAQD9j7B+Dd7rD/zsd2/AAgM1GKCC+zKU7r7mARjAARDhy+A++5vrPzgMCjhQQPlALGCA7A')]
    [TestCase('17', '3d-shapes.svg,660,343,2,0,0,660,343,0,0,660,343,1,true,true,0.98,11POAQD9j7B+Dd7rD/zsd2/AAgM1GKCC+zKU7r7mARjAARDhy+A++5vrPzgMCjhQQPlALGCA7A')]
    [TestCase('18', '3d-shapes.svg,660,343,2,660,343,0,0,0,0,660,343,1,false,true,0.98,0iPeQQABhFC/D9b/Dzz2iXfAPwAf4A8E9f0DX73iIAAAAAf0+yD9/8ZfvzgI/wvwwfw9TB+A8A')]
    [TestCase('19', '3d-shapes.svg,660,343,2,660,343,0,0,0,0,660,343,1,true,true,0.98,0iPeQQABhFC/D9b/Dzz2iXfAPwAf4A8E9f0DX73iIAAAAAf0+yD9/8ZfvzgI/wvwwfw9TB+A8A')]
    [TestCase('20', '3d-shapes.svg,660,343,2,0,0,660,343,660,343,0,0,1,false,true,0.98,0iPeQQABhFC/D9b/Dzz2iXfAPwAf4A8E9f0DX73iIAAAAAf0+yD9/8ZfvzgI/wvwwfw9TB+A8A')]
    [TestCase('21', '3d-shapes.svg,660,343,2,0,0,660,343,660,343,0,0,1,true,true,0.98,0iPeQQABhFC/D9b/Dzz2iXfAPwAf4A8E9f0DX73iIAAAAAf0+yD9/8ZfvzgI/wvwwfw9TB+A8A')]
    [TestCase('22', '3d-shapes.svg,660,343,1,660,343,0,0,660,343,0,0,1,false,true,0.98,0iPeQQABhFC/D9b/Dzz2iXfAPwAf4A8E9f0DX73iIAAAAAf0+yD9/8ZfvzgI/wvwwfw9TB+A8A')]
    [TestCase('23', '3d-shapes.svg,660,343,1,660,343,0,0,660,343,0,0,1,true,true,0.98,0iPeQQABhFC/D9b/Dzz2iXfAPwAf4A8E9f0DX73iIAAAAAf0+yD9/8ZfvzgI/wvwwfw9TB+A8A')]
    [TestCase('24', '3d-shapes.svg,200,200,1,660,0,0,0,0,0,660,343,1,false,true,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMCD9ALsXj3hGgyAfwf6/5A9AMJTX3gH+u9/wvxCOD8A8g')]
    [TestCase('25', '3d-shapes.svg,200,200,1,660,0,0,0,0,0,660,343,1,true,true,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMCD9ALsXj3hGgyAfwf6/5A9AMJTX3gH+u9/wvxCOD8A8g')]
    [TestCase('26', '3d-shapes.svg,200,200,1,0,0,0,0,0,0,660,343,1,false,true,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMCD9ALsXj3hGgyAfwf6/5A9AMJTX3gH+u9/wvxCOD8A8g')]
    [TestCase('27', '3d-shapes.svg,200,200,1,0,0,0,0,0,0,660,343,1,true,true,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMCD9ALsXj3hGgyAfwf6/5A9AMJTX3gH+u9/wvxCOD8A8g')]
    [TestCase('28', '3d-shapes.svg,200,200,1,-100,-100,0,0,0,0,660,343,1,false,true,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMCD9ALsXj3hGgyAfwf6/5A9AMJTX3gH+u9/wvxCOD8A8g')]
    [TestCase('29', '3d-shapes.svg,200,200,1,-100,-100,0,0,0,0,660,343,1,true,true,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMCD9ALsXj3hGgyAfwf6/5A9AMJTX3gH+u9/wvxCOD8A8g')]
    [TestCase('30', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,0,1,false,true,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMCD9ALsXj3hGgyAfwf6/5A9AMJTX3gH+u9/wvxCOD8A8g')]
    [TestCase('31', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,0,1,true,true,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMCD9ALsXj3hGgyAfwf6/5A9AMJTX3gH+u9/wvxCOD8A8g')]
    [TestCase('32', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,-10,1,false,true,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMCD9ALsXj3hGgyAfwf6/5A9AMJTX3gH+u9/wvxCOD8A8g')]
    [TestCase('33', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,-10,1,true,true,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMCD9ALsXj3hGgyAfwf6/5A9AMJTX3gH+u9/wvxCOD8A8g')]
    [TestCase('34', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,1,false,true,0.98,CvxNQ/4Q/HC9EwV8T/zvjf/gvgFdgMBD/hsAb/zgFbBQvw/+s8C/C6QzvzcS+4PwhP46NIAC8g')]
    [TestCase('35', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,1,true,true,0.98,CvxNQ/4Q/HC9EwV8T/zvjf/gvgFdgMBD/hsAb/zgFbBQvw/+s8C/C6QzvzcS+4PwhP46NIAC8g')]
    [TestCase('36', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,1,false,true,0.98,HxB+A/8YSFH8EBDYr3oMfH9QwAg2eD+DBd2rbr3fGZSQ/RMM2FAACKbrj3cLJLwgBPhArCBC6A')]
    [TestCase('37', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,1,true,true,0.98,HxB+A/8YSFH8EBDYr3oMfH9QwAg2eD+DBd2rbr3fGZSQ/RMM2FAACKbrj3cLJLwQBPhAqCBC6A')]
    [TestCase('38', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,1,false,true,0.98,5RdeAQMNwFD+C+4DcPr1bqsgvwAM7I/D9A0Ub/7hIASwAAnoA8EA/8lbb3gMEWzgwgNYcF8B5Q')]
    [TestCase('39', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,1,true,true,0.98,5RdeAQMNwFD+C+4DcPr1bqsgvwAM7I/D9A0Ub/7hIASwAAnoA8EA/8lbb3gMEWzgwgNYcF8B5Q')]
    [TestCase('40', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,1,false,false,0.98,CvxNQ/4Q/HC9EwV8T/zvjf/gvgFdgMBD/hsAb/zgFbBQvw/+s8C/C6QzvzcS+4PwhP46NIAC8g')]
    [TestCase('41', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,1,true,false,0.98,CvxNQ/4Q/HC9EwV8T/zvjf/gvgFdgMBD/hsAb/zgFbBQvw/+s8C/C6QzvzcS+4PwhP46NIAC8g')]
    [TestCase('42', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,1,false,false,0.98,HxB+A/8YSFH8EBDYr3oMfH9QwAg2eD+DBd2rbr3fGZSQ/RMM2FAACKbrj3cLJLwgBPhArCBC6A')]
    [TestCase('43', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,1,true,false,0.98,HxB+A/8YSFH8EBDYr3oMfH9QwAg2eD+DBd2rbr3fGZSQ/RMM2FAACKbrj3cLJLwQBPhAqCBC6A')]
    [TestCase('44', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,1,false,false,0.98,5RdeAQMNwFD+C+4DcPr1bqsgvwAM7I/D9A0Ub/7hIASwAAnoA8EA/8lbb3gMEWzgwgNYcF8B5Q')]
    [TestCase('45', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,1,true,false,0.98,5RdeAQMNwFD+C+4DcPr1bqsgvwAM7I/D9A0Ub/7hIASwAAnoA8EA/8lbb3gMEWzgwgNYcF8B5Q')]
    [TestCase('46', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,0.3,false,true,0.98,4Bv+gf8EvDC+EOHf/7v0iY8gvwE2MMCD9wr0Hj3hGTxw/wn760C+A7lLL7gK+xtAQ/0/hO8A8g')]
    [TestCase('47', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,0.3,true,true,0.98,4Rv+gf8EvDC+EOLf/7v0iY8gvwE2MMCD9wr0Dv3gGDxw/wn760C+A7lLL7gK+h9AQ/0/hP8A8g')]
    [TestCase('48', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,0.3,false,true,0.98,5yMOwv8G1ND9D+X7fzv9hGuQ/wMq4J+D+ffbXr3gGjTwPgv/92B+Arp/H7gIByzwQvtBpN9A7w')]
    [TestCase('49', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,0.3,true,true,0.98,5yMOwv8G1ND9D+X7f3v9hGuQ/wMq4J/D+ffbXr3gGjTgPgv/93C+Arp/H7gIBywAQ/tBqN8A7w')]
    [TestCase('50', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,0.3,true,true,0.96,1SNuQQEDrHB+DtsHYHv2f3cwfwEdBLCD9AX4rn3hHAjg/wf0A5G+/8RXX/gIARSgwv5JSJ8A7g')]
    [TestCase('51', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,0.3,false,false,0.98,4Bv+gf8EvDC+EOHf/7v0iY8gvwE2MMCD9wr0Hj3hGTxw/wn760C+A7lLL7gK+xtAQ/0/hO8A8g')]
    [TestCase('52', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,0.3,true,false,0.98,4Rv+gf8EvDC+EOLf/7v0iY8gvwE2MMCD9wr0Dv3gGDxw/wn760C+A7lLL7gK+h9AQ/0/hP8A8g')]
    [TestCase('53', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,0.3,false,false,0.98,5yMOwv8G1ND9D+X7fzv9hGuQ/wMq4J+D+ffbXr3gGjTwPgv/92B+Arp/H7gIByzwQvtBpN9A7w')]
    [TestCase('54', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,0.3,true,false,0.98,5yMOwv8G1ND9D+X7f3v9hGuQ/wMq4J/D+ffbXr3gGjTgPgv/93C+Arp/H7gIBywAQ/tBqN8A7w')]
    [TestCase('55', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,0.3,true,false,0.96,1SNuQQEDrHB+DtsHYHv2f3cwfwEdBLCD9AX4rn3hHAjg/wf0A5G+/8RXX/gIARSgwv5JSJ8A7g')]
    [TestCase('56', '3d-shapes.svg,300,300,1.5,0,0,0,0,0,0,200,200,1,true,true,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('57', '3d-shapes.svg,300,300,1.5,0,0,0,0,0,0,200,200,1,true,false,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('58', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,0,0,1,true,true,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('59', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,0,0,1,true,false,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('60', '3d-shapes.svg,300,300,1.5,0,0,660,343,200,200,200,200,1,true,true,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('61', '3d-shapes.svg,300,300,1.5,0,0,660,343,200,200,200,200,1,true,false,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('62', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,0,true,false,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('63', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,0,true,true,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    procedure TestDrawBitmapWithChessBackground(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer; ASurfaceScale, ASrcLeft, ASrcTop, ASrcRight, ASrcBottom, ADestLeft, ADestTop, ADestRight, ADestBottom, AOpacity: Single; AHighSpeed, ABlending: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,0.98,1aMi+fWli27/8rQHL0b1ZjDvPfWqH3E+6d5jAYEi0V9vAPPLMxCC/DJcz0P4DpS+Ngk5SM18AA')]
    procedure TestDrawBitmapWithClipping(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,0.98,1aMi+fWli27/8rQHL0b1ZjDvPfWqH3E+6d5jAYEi0V9vAPPLMxCC/DJcz0P4DpS+Ngk5SM18AA')]
    procedure TestDrawBitmapWithClipping2(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,0.98,06OCefWjfz4/8rcDX0b1lagOPvCVV1A96OB/Qf0lqoPfu+UYVB+6+V1UX8nq4ANfeQXVgw4+GA')]
    procedure TestDrawBitmapWithClipping3(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,1,false,true,0.98,+RdQRvMahM6G/YkPT4PrNuw+QgPnP496DR800b37AiRvwOw5+MBBBQUQ/4P4YICvwe8QxGBBCQ')]
    [TestCase('2',  '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,1,false,false,0.98,+RdQRvMahM6G/YkPT4PrNuw+QgPnP496DR800b37AiRvwOw5+MBBBQUQ/4P4YICvwe8QxGBBCQ')]
    [TestCase('3',  '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,0.5,false,true,0.98,5zeBwPXtbx6E+ZVLr0TwZswegPm+y+/7+gBckb0Q1lM/PuknKOD9/jI0r4bxIECPvfrzp7//EA')]
    [TestCase('4',  '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,0.5,false,false,0.98,5zeBwPXtbx6E+ZVLr0TwZswegPm+y+/7+gBckb0Q1lM/PuknKOD9/jI0r4bxIECPvfrzp7//EA')]
    [TestCase('5',  '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,1,true,true,0.98,+RdQRvMahM6G/YkPP4PrNuw+QgPnP396DR800b37AiRvwOw5+MBBBQUQ/4P4YICvwe8QxGBBCQ')]
    [TestCase('6',  '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,1,true,false,0.98,+RdQRvMahM6G/YkPP4PrNuw+QgPnP396DR800b37AiRvwOw5+MBBBQUQ/4P4YICvwe8QxGBBCQ')]
    [TestCase('7',  '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,0.5,true,true,0.98,5zeBwPXtbx6E+ZVHr0TwZswewPm+y+/7+gBckX0Q1lM/PuknKOD9/jI0r4bxIECPvfrzp7//EA')]
    [TestCase('8',  '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,0.5,true,false,0.98,5zeBwPXtbx6E+ZVHr0TwZswewPm+y+/7+gBckX0Q1lM/PuknKOD9/jI0r4bxIECPvfrzp7//EA')]
    [TestCase('9',  '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,0,0,200,200,1,false,true,0.98,1mNBe/PBu04ABm9rH0D1lawOPvCVV1A96OCDEX4gojffO+UcPIC58EpUX8nq4ANfeQXVgw4+GA')]
    [TestCase('10', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,0,0,200,200,1,false,false,0.98,1mNBe/PBu04ABm9rH0D1lawOPvCVV1A96OCDEX4gojffO+UcPIC58EpUX8nq4ANfeQXVgw4+GA')]
    [TestCase('11', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,0,0,200,200,0.5,false,true,0.98,1t8Bu/XBi97A/Yd3D0P1lawOPvCVV1A96OCDsf0iplv/e+UZyM959FVUX8nq4ANfeQXVgw4+GA')]
    [TestCase('12', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,0,0,200,200,0.5,false,false,0.98,1t8Bu/XBi97A/Yd3D0P1lawOPvCVV1A96OCDsf0iplv/e+UZyM959FVUX8nq4ANfeQXVgw4+GA')]
    [TestCase('13', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,0,0,200,200,1,true,true,0.98,1mNBe/PBu04ABm9rH0D1lawOPvCVV1A96OCDEX4gojffO+UcPIC58EpUX8nq4ANfeQXVgw4+GA')]
    [TestCase('14', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,0,0,200,200,1,true,false,0.98,1mNBe/PBu04ABm9rH0D1lawOPvCVV1A96OCDEX4gojffO+UcPIC58EpUX8nq4ANfeQXVgw4+GA')]
    [TestCase('15', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,0,0,200,200,0.5,true,true,0.98,1t8Bu/XBi97A/Yd3D0P1lawOPvCVV1A96OCDsf0iplv/e+UZyM959FVUX8nq4ANfeQXVgw4+GA')]
    [TestCase('16', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,0,0,200,200,0.5,true,false,0.98,1t8Bu/XBi97A/Yd3D0P1lawOPvCVV1A96OCDsf0iplv/e+UZyM959FVUX8nq4ANfeQXVgw4+GA')]
    [TestCase('17', '3d-shapes.svg,300,300,1,-2,50,500,30,0,0,660,343,-200,-50,660,343,1,false,true,0.98,wB/BuRLn/68+7dPz38DwIcQOdg7jEzBB/gIsL4ARIdyP//ArZIB/8o0rgT0WRH+wPgJCtDE8/g')]
    [TestCase('18', '3d-shapes.svg,300,300,1,-2,50,500,30,0,0,660,343,-200,-50,660,343,1,false,false,0.98,wB/BuRLn/68+7dPz38DwIcQOdg7jEzBB/gIsL4ARIdyP//ArZIB/8o0rgT0WRH+wPgJCtDE8/g')]
    [TestCase('19', '3d-shapes.svg,300,300,1,-2,50,500,30,0,0,660,343,-200,-50,660,343,0.5,false,true,0.98,y7sxegXUKw9A8bq7bwPzW7gOOv+8N0B/8/FXsH4b5rPPPesg3M889fdDcIMAksMP/AMMHBA9Cw')]
    [TestCase('20', '3d-shapes.svg,300,300,1,-2,50,500,30,0,0,660,343,-200,-50,660,343,0.5,false,false,0.98,y7sxegXUKw9A8bq7bwPzW7gOOv+8N0B/8/FXsH4b5rPPPesg3M889fdDcIMAksMP/AMMHBA9Cw')]
    [TestCase('21', '3d-shapes.svg,300,300,-1,2,-50,-100,-30,0,0,660,343,-200,-50,660,343,1,true,true,0.98,zxvxfALtl39+BtNv730KzSdwQ/T1xyBC5rkTj74I1VswRhPsG88EE+nbQQIO0EOQPfBahLA++Q')]
    [TestCase('22', '3d-shapes.svg,300,300,-1,2,-50,-100,-30,0,0,660,343,-200,-50,660,343,1,true,false,0.98,zxvxfALtl39+BtNv730KzSdwQ/T1xyBC5rkTj74I1VswRhPsG88EE+nbQQIO0EOQPfBahLA++Q')]
    [TestCase('23', '3d-shapes.svg,300,300,-1,2,-50,-100,-30,0,0,660,343,-200,-50,660,343,0.5,true,true,0.98,0rvRe/3X9+4//rp3/8H/MWjPQPLFj8A/58xL8D0XwO8vQfwBNG+/BSSYwEX82KN/u/oYhG++CA')]
    [TestCase('24', '3d-shapes.svg,300,300,-1,2,-50,-100,-30,0,0,660,343,-200,-50,660,343,0.5,true,false,0.98,0rvRe/3X9+4//rp3/8H/MWjPQPLFj8A/58xL8D0XwO8vQfwBNG+/BSSYwEX82KN/u/oYhG++CA')]
    [TestCase('25', '3d-shapes.svg,300,300,-1,2,-50,-100,-30,0,0,660,343,0,0,200,200,1,false,true,0.98,l58wve3j916G/nj3P0P1lawOPvCVV1A96OCDEf8BCOAfOec3hDB48t5XX8nq4ANfeQXVgw4+GA')]
    [TestCase('26', '3d-shapes.svg,300,300,-1,2,-50,-100,-30,0,0,660,343,0,0,200,200,1,false,false,0.98,l58wve3j916G/nj3P0P1lawOPvCVV1A96OCDEf8BCOAfOec3hDB48t5XX8nq4ANfeQXVgw4+GA')]
    [TestCase('27', '3d-shapes.svg,300,300,-1,2,-50,-100,-30,0,0,660,343,0,0,200,200,0.5,false,true,0.98,tnvx+/LSp94D+oy/n0T1lawOPvCVV1A96OCDMb4T2rOPeuYm7B859R9UX8nq4ANfeQXVgw4+GA')]
    [TestCase('28', '3d-shapes.svg,300,300,-1,2,-50,-100,-30,0,0,660,343,0,0,200,200,0.5,false,false,0.98,tnvx+/LSp94D+oy/n0T1lawOPvCVV1A96OCDMb4T2rOPeuYm7B859R9UX8nq4ANfeQXVgw4+GA')]
    [TestCase('29', '3d-shapes.svg,300,300,1,-2,-50,500,30,0,0,660,343,0,0,200,200,1,true,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDsTwZitsffu7qY577+2VEzj7sIMwOAQ8OiH3B/g')]
    [TestCase('30', '3d-shapes.svg,300,300,1,-2,-50,500,30,0,0,660,343,0,0,200,200,1,true,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDsTwZitsffu7qY577+2VEzj7sIMwOAQ8OiH3B/g')]
    [TestCase('31', '3d-shapes.svg,300,300,1,-2,-50,500,30,0,0,660,343,0,0,200,200,0.5,true,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDAT0fmq8f/ekA2M46+mLMDkTrAOg+fQryB85/Cw')]
    [TestCase('32', '3d-shapes.svg,300,300,1,-2,-50,500,30,0,0,660,343,0,0,200,200,0.5,true,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDAT0fmq8f/ekA2M46+mLMDkTrAOg+fQryB85/Cw')]
    procedure TestDrawBitmapWithMatrix(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer; ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg, ASrcLeft, ASrcTop, ASrcRight, ASrcBottom, ADestLeft, ADestTop, ADestRight, ADestBottom, AOpacity: Single; AHighSpeed, ABlending: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,1,false,true,Red,1,0.98,JpQAgecANB4EBDyLjb3rRvSOAP3wd3+99+MXMX8G/CNewO5DWJH5/P4bb4bxCNDug/INmO8ADQ')]
    [TestCase('2', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,1,true,true,Red,0.3,0.98,7dORPPPTTy7C+YLvfoPyfsDO/vOwE2C97OFj4T0cwxtffegj8O+5+UNEf8js7POO/P/m1+7+FA')]
    [TestCase('3', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,0,0,200,200,1,false,true,Red,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('4', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,0,0,200,200,1,true,true,Red,1,0.98,xG9xOvW+g+4AAVpHf0D1lawOPvCVV1A96OCDcT4gn/s+vOkUMFD48kJUX8nq4ANfeQXVgw4+GA')]
    [TestCase('5', '3d-shapes.svg,300,300,1,-2,50,500,30,0,0,660,343,-200,-50,660,343,1,false,true,Red,0.3,0.98,xf+BuvzEv86A9qCjXwXwdaw+u/igc+D+5d+jMP0gx28PfOYi1H959yG8L8fyqkefOgn+g+58Cw')]
    [TestCase('6', '3d-shapes.svg,300,300,-1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,1,true,true,Red,0.3,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('7', '3d-shapes.svg,300,300,-1,2,-50,-100,30,0,0,660,343,0,0,200,200,1,false,true,Red,0.3,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('8', '3d-shapes.svg,300,300,1,-2,-50,500,30,0,0,660,343,0,0,200,200,1,true,true,Red,0.3,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDoXwhoptffOcHIE86+GEkr8bo7Cefugj0+y2+EA')]
    procedure TestDrawBitmapWithModulateColor(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer; ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg, ASrcLeft, ASrcTop, ASrcRight, ASrcBottom, ADestLeft, ADestTop, ADestRight, ADestBottom, AOpacity: Single; AHighSpeed, ABlending: Boolean; const AModulateColor: string; AModulateColorOpacity: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,0.98,1L+RvPm9286B+5uLD4b3F7AfxfDVh0H/4vibYfoQ1+u/u+oEuI/5+kfwD8IBjrMve/MaKE+/8g')]
    procedure TestDrawBitmapWithModulateColor2(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', 'Flat,1')]
    [TestCase('2', 'Flat,4')]
    [TestCase('3', 'Round,4')]
    procedure TestDrawPathWithConsecutiveMoveTo(ACap: TStrokeCap; AThickness: Single);
    [TestCase('1',  'Bottom,AllCorners,0,Round,3,Flat')]
    [TestCase('2',  'Bottom,AllCorners,0,Round,3,Round')]
    [TestCase('3',  'Bottom,AllCorners,0,Round,1,Flat')]
    [TestCase('4',  'Top,AllCorners,0,Round,3,Flat')]
    [TestCase('5',  'Left,AllCorners,0,Round,3,Flat')]
    [TestCase('6',  'Right,AllCorners,0,Round,3,Round')]
    [TestCase('7',  'Top|Bottom,AllCorners,0,Round,6,Flat')]
    [TestCase('8',  'Left|Right,AllCorners,0,Round,6,Round')]
    [TestCase('9',  'Top|Left,AllCorners,0,Round,3,Flat')]
    [TestCase('10', 'Bottom|Right,AllCorners,0,Round,3,Round')]
    [TestCase('11', 'Top|Left|Right,AllCorners,0,Round,3,Flat')]
    [TestCase('12', 'Left|Bottom|Right,AllCorners,0,Round,3,Round')]
    [TestCase('13', 'None,AllCorners,0,Round,3,Flat')]
    [TestCase('14', 'None,None,10,Round,3,Round')]
    [TestCase('15', 'Bottom,None,0,Round,3,Flat')]
    [TestCase('16', 'Bottom,None,10,Round,3,Flat')]
    [TestCase('17', 'Bottom,TopLeft|BottomRight,10,Round,3,Flat')]
    [TestCase('18', 'Bottom,BottomLeft|BottomRight,10,Bevel,3,Flat')]
    [TestCase('19', 'Right,TopRight|BottomRight,10,InnerRound,3,Round')]
    [TestCase('20', 'Left,TopLeft|BottomLeft,10,InnerLine,3,Flat')]
    [TestCase('21', 'Top,TopLeft|TopRight,10,Round,1,Flat')]
    [TestCase('22', 'Top|Bottom,TopRight,10,Bevel,6,Round')]
    [TestCase('23', 'Bottom,AllCorners,0,Bevel,3,Flat')]
    [TestCase('24', 'AllSides,AllCorners,0,Round,3,Flat')]
    procedure TestDrawRectSides(const ASidesString, ACornersString: string; ARadius: Single; ACornerType: TCornerType; AThickness: Single; ACap: TStrokeCap);
    [TestCase('1',   'horse.webp,claWhite,Tile,0,0,1,1,0,90,0.4,0.98,1VeyOvjAV15B9aCDD4b1lagePvCVW1A96OB/UX0lqoMPfOUWVA86+F84b4fv7UNfOgfeH79/Eg')]
    [TestCase('2',   'horse.webp,claRed,Tile,0,0,1,1,0,90,0.4,0.98,1VeyOvjAV15B9aCDD0b1lagefvCVW2A96OB/UX0lqoMPfOUWWA86+GBI/wfw2S8POgfo015/Eg')]
    [TestCase('3',   'horse.webp,claWhite,Tile,0,0,1,1,0,90,1,0.98,1VeyOvjAV15B9aCDD4b1laQufvCVX1D95+B7UX0lqYcPvOUWWP85+F4If8T2AKS/uwnsB0DCCQ')]
    [TestCase('4',   'horse.webp,claRed,Tile,0,0,1,1,0,90,1,0.98,1VeyOvjAV15B9aCDD4b1lagevvCWW2D95+B/UX0lqocffOUWWP95+F8438X3zm8PuwkERE/BCQ')]
    [TestCase('5',   'horse.webp,claWhite,Tile,0.15,0.15,0.85,0.85,0,90,0.4,0.98,1VeyOvjAV15B9aCDD4b1lawOfvCWV1A96OCHUX0lq4MPfOUVVA96+GCU/4j01Pdu+QnXl749GQ')]
    [TestCase('6',   'horse.webp,claRed,Tile,0.15,0.15,0.85,0.85,0,90,0.4,0.98,1VeyOvjAV15B9aCDD0b1lagOPvCWV1B96OCDUX0lq4MPfOUVVA86+GCcb4nv0+te+Qjgew69Fg')]
    [TestCase('7',   'horse.webp,claWhite,Tile,0.15,0.15,0.85,0.85,0,90,1,0.98,1VeyOvjAV15B9aCDD4b1lKwOfvCXV1B96OGLUT0lq3//e+UVVA96+GDsX0gDw+N+uRDZtz69Gg')]
    [TestCase('8',   'horse.webp,claRed,Tile,0.15,0.15,0.85,0.85,0,90,1,0.98,1VeyOvjAV15B9aCDD0b1lKgefvCXU1C96OCHUT0lqoP/e+UVUA96+GAEYMn2wctOOQ7wb447FA')]
    [TestCase('9',   'horse.webp,claWhite,TileOriginal,0,0,1,1,0,90,0.4,0.98,1VeyOvjAV15B9aCDD4b1lagePvCVW1A96OB/UX0lqoMPfOUWVA86+F84b4fv7UNfOgfeH79/Eg')]
    [TestCase('10',  'horse.webp,claRed,TileOriginal,0,0,1,1,0,90,0.4,0.98,1VeyOvjAV15B9aCDD0b1lagefvCVW2A96OB/UX0lqoMPfOUWWA86+GBI/wfw2S8POgfo015/Eg')]
    [TestCase('11',  'horse.webp,claWhite,TileOriginal,0,0,1,1,0,90,1,0.98,1VeyOvjAV15B9aCDD4b1laQufvCVX1D95+B7UX0lqYcPvOUWWP85+F4If8T2AKS/uwnsB0DCCQ')]
    [TestCase('12',  'horse.webp,claRed,TileOriginal,0,0,1,1,0,90,1,0.98,1VeyOvjAV15B9aCDD4b1lagevvCWW2D95+B/UX0lqocffOUWWP95+F8438X3zm8PuwkERE/BCQ')]
    [TestCase('13',  'horse.webp,claWhite,TileOriginal,0.15,0.15,0.85,0.85,0,90,0.4,0.98,1VeyOvjAV15B9aCDD4b1lawOfvCWV1A96OCHUX0lq4MPfOUVVA96+GCU/4j01Pdu+QnXl749GQ')]
    [TestCase('14',  'horse.webp,claRed,TileOriginal,0.15,0.15,0.85,0.85,0,90,0.4,0.98,1VeyOvjAV15B9aCDD0b1lagOPvCWV1B96OCDUX0lq4MPfOUVVA86+GCcb4nv0+te+Qjgew69Fg')]
    [TestCase('15',  'horse.webp,claWhite,TileOriginal,0.15,0.15,0.85,0.85,0,90,1,0.98,1VeyOvjAV15B9aCDD4b1lKwOfvCXV1B96OGLUT0lq3//e+UVVA96+GDsT0gDw+N+uRDZtz69Gg')]
    [TestCase('16',  'horse.webp,claRed,TileOriginal,0.15,0.15,0.85,0.85,0,90,1,0.98,1VeyOvjAV15B9aCDD0b1lKgefvCXU1C96OCHUT0lqoP/e+UVUA96+GAEYMn2wctOOQ7wb447FA')]
    [TestCase('17',  'horse.webp,claWhite,TileStretch,0,0,1,1,0,90,0.4,0.98,1VeyOvjAV15B9aCDD4b1lagePvCVW1A96OB/UX0lqoMPfOUWVA86+F84b0fv5UdPOgfjJ7//Eg')]
    [TestCase('18',  'horse.webp,claRed,TileStretch,0,0,1,1,0,90,0.4,0.98,1VeyOvjAV15B9aCDD4b1lagefvCVW2A96OB/UX0lqoMPfOUWWA86+GBQD4jw1TMvOgfr124/Ew')]
    [TestCase('19',  'horse.webp,claWhite,TileStretch,0,0,1,1,0,90,1,0.98,1VeyOvjAV15B9aCDD4b1laQufvCVX1D95+B7Ub0lqYcPvOUWWP85+F4QbwT266ef+wn3H1BCCw')]
    [TestCase('20',  'horse.webp,claRed,TileStretch,0,0,1,1,0,90,1,0.98,1VeyOvjAV15B9aCDD4b1lagefvCWW2D95+B/UX0lqocffOUWWP95+F9E/wX5xHtf+wkMVG9BCw')]
    [TestCase('21',  'horse.webp,claWhite,TileStretch,0.15,0.15,0.85,0.85,0,90,0.4,0.98,1VeyOvjAV15B9aCDD0b1lawOfvCWV1A96OCHUX0lq4MPfOUVVA96+GCAPwn01utuOQncj+59GA')]
    [TestCase('22',  'horse.webp,claRed,TileStretch,0.15,0.15,0.85,0.85,0,90,0.4,0.98,1VeyOvjAV15B9aCDD0b1lagOPvCWV1B96OCDUX0lq4MPfOUVVA86+GCMbwnw0+teuQjjaz69FQ')]
    [TestCase('23',  'horse.webp,claWhite,TileStretch,0.15,0.15,0.85,0.85,0,90,1,0.98,1VeyOvjAV15B9aCDD4b1lKwOfvCXV1B96OGLUT0lq38PfOUVVA+6+GDA/8gBx8dueQ7nn779GA')]
    [TestCase('24',  'horse.webp,claRed,TileStretch,0.15,0.15,0.85,0.85,0,90,1,0.98,1VeyOvjAV15B9aCDD0b1lKgefvCXU1C96OCHUT0lqoP/e+UVUA96+GDYfwn4v8dOuQ34S+67EQ')]
    [TestCase('25',  'horse.webp,claWhite,Tile,0,0,1,1,90,-180,0.4,0.98,0leSuvfDXz5B9aWDz4X0M3DPgO3Fx9B+5d8DMb0lqnM/POYTUF96+FuIT8P52Xu/OwoSAL9/Bg')]
    [TestCase('26',  'horse.webp,claRed,Tile,0,0,1,1,90,-180,0.4,0.98,0FuyuvfCW17B9KOLvwX1QeQeP+6xh/C/3cPPAP0lq3sv/OUVTD96+Fykb8Xzui/Pegoe9E3+Ag')]
    [TestCase('27',  'horse.webp,claWhite,Tile,0,0,1,1,90,-180,1,0.98,zVNyuvbGY/5A9a2HfwXzoZfgxOkLZCEB4d4/8LwlqFuPPOcOSM+6+FPQH7oQzzNQ/xBtwE/C6w')]
    [TestCase('28',  'horse.webp,claRed,Tile,0,0,1,1,90,-180,1,0.98,yWPC+vbEX14B9KiXP4X0wj+/wOrbz+DDzZjDn3wmq2tffOYWQG/6+FUYcH8BgW/f/BGKJJ1+4w')]
    [TestCase('29',  'horse.webp,claWhite,Tile,0.15,0.15,0.85,0.85,90,-180,0.4,0.98,0ldyevfCaz4B9aOLn4XtSCyvffW0H8E+5fl3MX0lrHs/POYVVG/691+MP4b30dfeeAj4C+68Dg')]
    [TestCase('30',  'horse.webp,claRed,Tile,0.15,0.15,0.85,0.85,90,-180,0.4,0.98,0leS+vfAY17B9KGHz0XxU+CuvPen35C/3dAzMf0lrX8v/OUXUF86+F5szwby07ceuAcAyF08Cg')]
    [TestCase('31',  'horse.webp,claWhite,Tile,0.15,0.15,0.85,0.85,90,-180,1,0.98,zlMiOvbFgw6B9KeT/4Th1PMvPf3iR9JA4B9gAb0lrnOPPOcUVA9791zgfwELu5ceuAwsVE27AA')]
    [TestCase('32',  'horse.webp,claRed,Tile,0.15,0.15,0.85,0.85,90,-180,1,0.98,zVNievfBd25B9KOLX0Xr8TefugLDp+ECzre3AH0msndfvOYZRM86+FqI/wL9wEcu9gpBtMw59Q')]
    [TestCase('33',  'horse.webp,claWhite,TileOriginal,0,0,1,1,90,-180,0.4,0.98,0leSuvfCXz5B9aWDz4X0M3DPgO3Fx9B+5d8DMb0lrH8//OUVWH/6918IYEb40StfOQUS9I7+Bg')]
    [TestCase('34',  'horse.webp,claRed,TileOriginal,0,0,1,1,90,-180,0.4,0.98,0FuyuvfCW17B9KOLvwX1QeQe/+2xh/C/3cPPMD0mrn8//OUXUF86+F6cDwf0xc8ueQcVKP7+Aw')]
    [TestCase('35',  'horse.webp,claWhite,TileOriginal,0,0,1,1,90,-180,1,0.98,zVNyuvbGY/5A9a2HfwXzoZfgxOkLZBFB4d4/8Pwlrnuf/OYUWB87914YAcIMu2NfeQRtnD//7A')]
    [TestCase('36',  'horse.webp,claRed,TileOriginal,0,0,1,1,90,-180,1,0.98,yWPC+vbEX14B9KiXP4X0wj/PwOrbz9DDzZjDD/0msn9/fOYZSM86+FoEkAMCnX/O+Al1qF2A5Q')]
    [TestCase('37',  'horse.webp,claWhite,TileOriginal,0.15,0.15,0.85,0.85,90,-180,0.4,0.98,0ldyevfCaz4B9aOLn4XtSCyvffW0H7E+5fl3MX0lrHs/POYVVG/691+MP4b30dfeeAj4C+68Dg')]
    [TestCase('38',  'horse.webp,claRed,TileOriginal,0.15,0.15,0.85,0.85,90,-180,0.4,0.98,0leS+vfAY17B9KGHz0XxU+CuvPen35C/3dAzMf0lrX8v/OUXUF86+F5szwby07ceuAcAyF08Cg')]
    [TestCase('39',  'horse.webp,claWhite,TileOriginal,0.15,0.15,0.85,0.85,90,-180,1,0.98,zlMiOvbFgw6B9KeT/4Th1fMvPf3iR9JA4B9kAb0lrnOPPOcUVA97913gfwELu5ceuAwsVE27AA')]
    [TestCase('40',  'horse.webp,claRed,TileOriginal,0.15,0.15,0.85,0.85,90,-180,1,0.98,zVNievfBd25B9KOLX0Xr8TefugLDp+ECzre3AH0msndfvOYZRM86+FqI/wL9wEcu9gpBtMw59Q')]
    [TestCase('41',  'horse.webp,claWhite,TileStretch,0,0,1,1,90,-180,0.4,0.98,0leSevfDXy5B9aaDz8X3OHCvwO27h2D/598TIX0lqXc/POYTUF86+FtgD4T5zENf+woVxD4/CA')]
    [TestCase('42',  'horse.webp,claRed,TileStretch,0,0,1,1,90,-180,0.4,0.98,0VuyuvfCW14B9aOLv4X0QwAvv+2sa2DA3sTXEL0lq3sf/OUWUD96+Ft0b0X0uA+Pugsd/A2+BA')]
    [TestCase('43',  'horse.webp,claWhite,TileStretch,0,0,1,1,90,-180,1,0.98,zU9yuvbGY+6A9a6Hf0X7q5eQROryy3CC591v4Hwlp19vPOcPTM96+FRwD7wPrp9vPhN1KB/B7w')]
    [TestCase('44',  'horse.webp,claRed,TileStretch,0,0,1,1,90,-180,1,0.98,yl/C+vbEX15B9KiXP4XzyX/PAOrNi/DE0JnPrzwmq2tPfOYWRH/6+FSgfz8DfBs/vBSINB1+5w')]
    [TestCase('45',  'horse.webp,claWhite,TileStretch,0.15,0.15,0.85,0.85,90,-180,0.4,0.98,0ldyevfCaz4B9aOHnwXwSRyPvfSyA+H+4/F7MX0lrH8/POYUVH/6919onwb2z9fOeAb+F+58Dg')]
    [TestCase('46',  'horse.webp,claRed,TileStretch,0.15,0.15,0.85,0.85,90,-180,0.4,0.98,0leS+vfAY27B9KGHv0XxVeDe/PWg14B/3s1DMf0lrX8v/OUXUF86+F5c/4bxz79e+AYGzG18Cg')]
    [TestCase('47',  'horse.webp,claWhite,TileStretch,0.15,0.15,0.85,0.85,90,-180,1,0.98,zVcievbEgw6B9KeP/0To1sfPPPvcBzKB3QlsAX0lr3eP/OYTWB97916Ib0IHt4/udwc8dC27/w')]
    [TestCase('48',  'horse.webp,claRed,TileStretch,0.15,0.15,0.85,0.85,90,-180,1,0.98,zFNyOvfBd25B9KOLX0Xr8zcPu/6wk7HCz6/fAL0msntvfOYZRM86+FpsX8P7tl/OdglPvOy59Q')]
    [TestCase('49',  'horse.webp,claWhite,Tile,0,0,1,1,90,360,0.4,0.98,2kshPv7fU88B9NwXYMT1Mmh/wO2/t4C/5NjTsD8bxruv/Ovv4w8+9g9g78L30UsffAwVrO4/BQ')]
    [TestCase('50',  'horse.webp,claRed,Tile,0,0,1,1,90,360,0.4,0.98,258R/PbdH8+B+K+/3wP0PeQOP+6vh8BA28TDED4fuWuPuu0BHBB6+hGk/0TytRcf+wonwL09Ag')]
    [TestCase('51',  'horse.webp,claWhite,Tile,0,0,1,1,90,360,1,0.98,4btPQwcOyHDC8TX04EH2nYMQhOr9R9EC38zHP4ML8A+APfW2twBE85VzT3kLurc/QBZ08M6C6A')]
    [TestCase('52',  'horse.webp,claRed,Tile,0,0,1,1,90,360,1,0.98,44MwPvUJRHBC/cUbkEDyujefwOrXz9CFyJmjT78VzktfuPniQyH6/ZkfUL79djOfPROgpEw94Q')]
    [TestCase('53',  'horse.webp,claWhite,Tile,0.15,0.15,0.85,0.85,90,360,0.4,0.98,zReC+QXU617/8cqfr0TvRQR/ffWn//B+4ehn0f4f2F8ffej8b//9+ERgP0b30ct+uAcG3D09DQ')]
    [TestCase('54',  'horse.webp,claRed,Tile,0.15,0.15,0.85,0.85,90,360,0.4,0.98,vAsCugLG/95/966jv4XxUNzOfPagx6D/3MxLEf0jz1Pfe+cPZP+7+0tMj8byz7v+9wcNnE38CA')]
    [TestCase('55',  'horse.webp,claWhite,Tile,0.15,0.15,0.85,0.85,90,360,1,0.98,wLOxdxryw1+87AnIj8LlzI+fvP3B+2EB1/Q7AUEXHCSv/uzVj88D+ht0f8EJuXcutwpO5Pz7/A')]
    [TestCase('56',  'horse.webp,claRed,Tile,0.15,0.15,0.85,0.85,90,360,1,0.98,l5fxOBLQ/4/9+cTPT8Xr6Sv/ev+xayHDy677oLwhBQSPO+oEfN/+ACo8XwL/t0/+dQthRKz58Q')]
    [TestCase('57',  'horse.webp,claWhite,TileOriginal,0,0,1,1,90,360,0.4,0.98,2kshPv7fU88B9NwXYMT1Mmh/wO2/t4C/5NjTMEAe5E/vfOYMIHD99RbgL4b2yOvOuQcVoK5+Bg')]
    [TestCase('58',  'horse.webp,claRed,TileOriginal,0,0,1,1,90,360,0.4,0.98,258R/PbdH8+B+K+/3wP0PeQOP+6vh8CA28TDcH4f2C//OuceKEB6+w6Yn8bywat+OQgdAG6+Aw')]
    [TestCase('59',  'horse.webp,claWhite,TileOriginal,0,0,1,1,90,360,1,0.98,4btPQwcOyHDC8TX04EH2nYMQhOr9R9EC38zHf4QTOvwu/uf/V7FC8qa3YEEIpMtu+gpz0K6/6w')]
    [TestCase('60',  'horse.webp,claRed,TileOriginal,0,0,1,1,90,360,1,0.98,44MwPvUJRHAC/cUbkEDyujefwOrXz9CFyJmjLwAWHbBuuekqaJE6AJMDcML+kiueOQyKRA2/5A')]
    [TestCase('61',  'horse.webp,claWhite,TileOriginal,0.15,0.15,0.85,0.85,90,360,0.4,0.98,zReC+QXU617/8cqfr0TvRQR/ffWn//B+4ehn0f4f2F8ffej8b//9+ERgP0b30ct+uAcG3D09DQ')]
    [TestCase('62',  'horse.webp,claRed,TileOriginal,0.15,0.15,0.85,0.85,90,360,0.4,0.98,vAsCugLG/95/966jv4XxUNzOfPagx6D/3MxLEf0jz1Pfe+cPZP+7+0tMj8byz7v+9wcNnE38CA')]
    [TestCase('63',  'horse.webp,claWhite,TileOriginal,0.15,0.15,0.85,0.85,90,360,1,0.98,wLOxdxryw1+87AnIj8LlzI+fvP3B+2EB1/Q7AUEXHCSv/uzVj88D+ht0f8EJuXcutwpO5Pz7/A')]
    [TestCase('64',  'horse.webp,claRed,TileOriginal,0.15,0.15,0.85,0.85,90,360,1,0.98,l5fxOBLQ/4/9+cTPT8Xr6Sv/ev+xayHDy677oLwhBQSPO+oEfN/+ACo8XwL/t0/+dQthRKz58Q')]
    [TestCase('65',  'horse.webp,claWhite,TileStretch,0,0,1,1,90,360,0.4,0.98,31MxPv3lT4/B890DgIT2NHxfAO7Cu3B/5drrsD8ewqM//O3xyw899RWcvwL51y9PewsYeJ4/BQ')]
    [TestCase('66',  'horse.webp,claRed,TileStretch,0,0,1,1,90,360,0.4,0.98,3ZdRvPbdH99B+ay3D0T0P+jevu6vj7AA28LD0L0evGsveu0AHBB6+BWYzwT1wAc/+gohzL39Aw')]
    [TestCase('67',  'horse.webp,claWhite,TileStretch,0,0,1,1,90,360,1,0.98,7c+PQwQcxMBB8TjAUAL4or/QA+sETJHC4NEHMEMT5M9/fPq8d4CB8KQPwLgOyXMfvhR7cP7B6A')]
    [TestCase('68',  'horse.webp,claRed,TileStretch,0,0,1,1,90,360,1,0.98,6XPQPvQJRJBC/74H8ADzvUcPAOzW37CFx5Sjn34U1kdft/ngTyE6+aT7370Ejwt/+xKTvDy95Q')]
    [TestCase('69',  'horse.webp,claWhite,TileStretch,0.15,0.15,0.85,0.85,90,360,0.4,0.98,xgfSuQbWD89/8cLD/4TuSzhv/fOzA+F+4/V/IT8f1Fcv/er4e88990qIj4X30r+++AkB3L28DA')]
    [TestCase('70',  'horse.webp,claRed,TileStretch,0.15,0.15,0.85,0.85,90,360,0.4,0.98,uv8BegLID9+/9a6f78XwUuiOPPeg27A/3c83Qf0izkufe+gOfA+8+ktYPwby0qvudwcGoD38CA')]
    [TestCase('71',  'horse.webp,claWhite,TileStretch,0.15,0.15,0.85,0.85,90,360,1,0.98,r49xOBz2H1A96/YjYEPk3AuAvPne/0EB3BR0wcEVEhDP/vLNr2+D9SjQ378KvlfedxBD5Kx6+w')]
    [TestCase('72',  'horse.webp,claRed,TileStretch,0.15,0.15,0.85,0.85,90,360,1,0.98,knsB+RHTH4C99cPPz8Xp7kc/egGvmzGDzLXHMD0fA/TuOu0CuA+//ixgf8H8vCfONQpOVHw58g')]
    [TestCase('73',  'horse.webp,claWhite,Tile,0,0,1,1,-90,0,0.4,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('74',  'horse.webp,claRed,Tile,0,0,1,1,-90,0,0.4,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('75',  'horse.webp,claWhite,Tile,0,0,1,1,-90,0,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('76',  'horse.webp,claRed,Tile,0,0,1,1,-90,0,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('77',  'horse.webp,claWhite,Tile,0.15,0.15,0.85,0.85,-90,0,0.4,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('78',  'horse.webp,claRed,Tile,0.15,0.15,0.85,0.85,-90,0,0.4,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('79',  'horse.webp,claWhite,Tile,0.15,0.15,0.85,0.85,-90,0,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('80',  'horse.webp,claRed,Tile,0.15,0.15,0.85,0.85,-90,0,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('81',  'horse.webp,claWhite,TileOriginal,0,0,1,1,-90,0,0.4,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('82',  'horse.webp,claRed,TileOriginal,0,0,1,1,-90,0,0.4,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('83',  'horse.webp,claWhite,TileOriginal,0,0,1,1,-90,0,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('84',  'horse.webp,claRed,TileOriginal,0,0,1,1,-90,0,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('85',  'horse.webp,claWhite,TileOriginal,0.15,0.15,0.85,0.85,-90,0,0.4,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('86',  'horse.webp,claRed,TileOriginal,0.15,0.15,0.85,0.85,-90,0,0.4,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('87',  'horse.webp,claWhite,TileOriginal,0.15,0.15,0.85,0.85,-90,0,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('88',  'horse.webp,claRed,TileOriginal,0.15,0.15,0.85,0.85,-90,0,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('89',  'horse.webp,claWhite,TileStretch,0,0,1,1,-90,0,0.4,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('90',  'horse.webp,claRed,TileStretch,0,0,1,1,-90,0,0.4,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('91',  'horse.webp,claWhite,TileStretch,0,0,1,1,-90,0,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('92',  'horse.webp,claRed,TileStretch,0,0,1,1,-90,0,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('93',  'horse.webp,claWhite,TileStretch,0.15,0.15,0.85,0.85,-90,0,0.4,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('94',  'horse.webp,claRed,TileStretch,0.15,0.15,0.85,0.85,-90,0,0.4,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('95',  'horse.webp,claWhite,TileStretch,0.15,0.15,0.85,0.85,-90,0,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('96',  'horse.webp,claRed,TileStretch,0.15,0.15,0.85,0.85,-90,0,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('97',  'horse.webp,claWhite,Tile,0,0,1,1,180,180,0.4,0.98,2GsBvvzaO49B894DIET1OmQ/AOy6p2A/5dPHgL0lrm9f/OQVXL/5+GFgf0nr2hdPeQXWa17+Fw')]
    [TestCase('98',  'horse.webp,claRed,Tile,0,0,1,1,180,180,0.4,0.98,27sBvPXWD7/B97Gzz0P0ReDOPuysf4DA28G/gD0mrWtPfOUUWL/5+F5gjwnr2RNfOQXVb06+Fw')]
    [TestCase('99',  'horse.webp,claWhite,Tile,0,0,1,1,180,180,1,0.98,2wsAQwMCiLAB8DvEYAH1sXtwQ+bwI2FC4MCvz/0lsk/PfOQVYE85+mJwn0ns0DMveQXWS86+Fw')]
    [TestCase('100', 'horse.webp,claRed,Tile,0,0,1,1,180,180,1,0.98,48sA/vH4GzCC+8v7b0DyzDPvP+bPu1DFyZGXzz0nr0effOURWE/5+Vxw34nr0CdfuQTUS66+Fg')]
    [TestCase('101', 'horse.webp,claWhite,Tile,0.15,0.15,0.85,0.85,180,180,0.4,0.98,zS+CeQTQ0y6/8cyPbwTvTPxOPfSk++D+4eNfUT0mq3cvfOUXTO85+V9gX4nr2x8v+QTWc/49GA')]
    [TestCase('102', 'horse.webp,claRed,Tile,0.15,0.15,0.85,0.85,180,180,0.4,0.98,viMC+gDC787/9rCTn4XxWNSu/PSfw5B/3chDUX0mqncf/OUWSO/5+F5cb0nr2hsvuQTVc/79Fw')]
    [TestCase('103', 'horse.webp,claWhite,Tile,0.15,0.15,0.85,0.85,180,180,1,0.98,wu/Bdxbpi++76w2cD0Ll33MvfPq570GB2OgjUT0nq2NPfOUZQM95+l1sT8ns00vfOATXV959GA')]
    [TestCase('104', 'horse.webp,claRed,Tile,0.15,0.15,0.85,0.85,180,180,1,0.98,nM8B+Q3Fz1/9+Mir7wTs+xevOvyvZ+ECzaTnYP0nqGcvfOYYOM/5+Vtof0ns0UPvuAPWW959Fw')]
    [TestCase('105', 'horse.webp,claWhite,TileOriginal,0,0,1,1,180,180,0.4,0.98,2GsBvvzaO49B894DIET1OmQ/AOy6p1A/5dPHgL0lrm9f/OQVXL/5+GFgf0nr2hdPeQXWa17+Fw')]
    [TestCase('106', 'horse.webp,claRed,TileOriginal,0,0,1,1,180,180,0.4,0.98,27sBvPXWD7/B97Gzz0P0ReDOPuysf4AA3MG/gD0mrWtPfOUUWL/5+F5gjwnr2RNfOQXVb06+Fw')]
    [TestCase('107', 'horse.webp,claWhite,TileOriginal,0,0,1,1,180,180,1,0.98,2wsAQwMCiLAB8DvEYAH1sXtwQ+bwI2FC4MCvz/0lsk/PfOQVYE85+mJwn0ns0DMveQXWS86+Fw')]
    [TestCase('108', 'horse.webp,claRed,TileOriginal,0,0,1,1,180,180,1,0.98,48sA/vH4HzCC+8v7b0DyzDPvP+bPu1DFyZGXzz0nr0effOURWE/5+Vxw34nr0CdfuQTUS66+Fg')]
    [TestCase('109', 'horse.webp,claWhite,TileOriginal,0.15,0.15,0.85,0.85,180,180,0.4,0.98,zS+CeQTQ0y6/8cyPbwTvTPxOPfSk++D+4eNfUT0mq3cvfOUXTO85+V9gX4nr2x8v+QTWc/49GA')]
    [TestCase('110', 'horse.webp,claRed,TileOriginal,0.15,0.15,0.85,0.85,180,180,0.4,0.98,viMC+gDC787/9rCTn4XxWNSu/PSfw5B/3chDUX0mqncf/OUWSO/5+F5cb0nr2hsvuQTVc/79Fw')]
    [TestCase('111', 'horse.webp,claWhite,TileOriginal,0.15,0.15,0.85,0.85,180,180,1,0.98,wu/Bdxbpi++76w2cD0Ll33MvfPq570GB2OgjUT0nq2NPfOUZQM95+l1sT8ns00vfOATXV959GA')]
    [TestCase('112', 'horse.webp,claRed,TileOriginal,0.15,0.15,0.85,0.85,180,180,1,0.98,nM8B+Q3Fz1/9+Mir7wTs+xevOvyvZ+ECzaTnYP0nqGcvfOYYOM/5+Vtof0ns0UPvuAPWW959Fw')]
    [TestCase('113', 'horse.webp,claWhite,TileStretch,0,0,1,1,180,180,0.4,0.98,1I/RPf3PL39B8+AX0AP1OUDvAO2vi1B/5NO/gL0lr29fvOQVXM95+WFgf4nr2hcveQXWc14+GA')]
    [TestCase('114', 'horse.webp,claRed,TileStretch,0,0,1,1,180,180,0.4,0.98,08/xe/bTC3+B+KyvT8P0Q+Ae/+urV1DA2r+7gH0mrWtPfOUUVM85+V9gn0nr2hNfOQXVc06+Fw')]
    [TestCase('115', 'horse.webp,claWhite,TileStretch,0,0,1,1,180,180,1,0.98,0mOQggTnb5AB8EH0kMD0sB9AhejV00CC3r+f3z0mtU/ffOMVZH95+2Jwn8ns0jfveAXYW95+GA')]
    [TestCase('116', 'horse.webp,claRed,TileStretch,0,0,1,1,180,180,1,0.98,0fvgvfPuG5AB/b7rP//zyDO/gOXLV7AEx46T370nsEevfOURUG+5+lxw38nr0CtfuQTUV64+Fw')]
    [TestCase('117', 'horse.webp,claWhite,TileStretch,0.15,0.15,0.85,0.85,180,180,0.4,0.98,vz+yuQbJ/66/7sKzv0TuVPiOPfOr67C+4+pbYT0mrHc/POUXTP95+V9gX8nr2yMf+QTWc/59GA')]
    [TestCase('118', 'horse.webp,claRed,TileStretch,0.15,0.15,0.85,0.85,180,180,0.4,0.98,tTfyuQHD987/86+bv4XxWdB+fPWfy2C/3so3YX0mqncfvOUWSP85+V5gb4nr2h8vuQTWd/79Fw')]
    [TestCase('119', 'horse.webp,claWhite,TileStretch,0.15,0.15,0.85,0.85,180,180,1,0.98,nRtSOBzY9w995PX/zwLk8nO/fPfLy7EA3fofYT0nrWN/vOQZQM85+150T0nt01evOATXW+79GA')]
    [TestCase('120', 'horse.webp,claRed,TileStretch,0.15,0.15,0.85,0.85,180,180,1,0.98,hAPiOBDG41898cW7L4Xr/wcvev2uc4EC0KrDcD0oqmc//OUXNN95+lpwb8ns0UfPeAPWX959Fw')]
    [TestCase('121', 'horse.webp,claWhite,Tile,0,0,1,1,180,-75,0.4,0.98,1luiOvjAV16B9aCDD0b1lawOPvCVV1A96OCDkfwgp08P++MksB+69kxUX8nq4ANfeQXVgw4+GA')]
    [TestCase('122', 'horse.webp,claRed,Tile,0,0,1,1,180,-75,0.4,0.98,1luiOvjAV16B9aCDD0b1lawOPvCVV1A96OCDEf0fpTM/e+UizF959kdUX8nq4ANfeQXVgw4+GA')]
    [TestCase('123', 'horse.webp,claWhite,Tile,0,0,1,1,180,-75,1,0.98,11+i+vfBV07B9aCD/0X1lawOPvCVV1A96OCDcTsaogOPeeE7ODC69C5UX8nq4ANfeQXVgw4+GA')]
    [TestCase('124', 'horse.webp,claRed,Tile,0,0,1,1,180,-75,1,0.98,1luiOvjBV16B9aB//0X1lawOPvCVV1A96OCDobwXnLsOeuU2gFC48yJUX8nq4ANfeQXVgw4+GA')]
    [TestCase('125', 'horse.webp,claWhite,Tile,0.15,0.15,0.85,0.85,180,-75,0.4,0.98,1VeyevjAW16B9aCDD0b1lawOPvCVV1A96OCD0T0hxuNfO+UlJK/591hUX8nq4ANfeQXVgw4+GA')]
    [TestCase('126', 'horse.webp,claRed,Tile,0.15,0.15,0.85,0.85,180,-75,0.4,0.98,1VeyOvjAV16B9aCDD0b1lawOPvCVV1A96OCDAb0iudM/O+MgLI9591hUX8nq4ANfeQXVgw4+GA')]
    [TestCase('127', 'horse.webp,claWhite,Tile,0.15,0.15,0.85,0.85,180,-75,1,0.98,1lPCevjAW16B9Z+HD0b1lawOPvCVV1A96OCDgf4a729QuuQ92A5590xUX8nq4ANfeQXVgw4+GA')]
    [TestCase('128', 'horse.webp,claRed,Tile,0.15,0.15,0.85,0.85,180,-75,1,0.98,1lOyevjAW16B9Z+HD0b1lawOPvCVV1A96OCDgTwez0sA+t8w7M549kxUX8nq4ANfeQXVgw4+GA')]
    [TestCase('129', 'horse.webp,claWhite,TileOriginal,0,0,1,1,180,-75,0.4,0.98,1luiOvjAV16B9aCDD0b1lawOPvCVV1A96OCDkfwgp08P++MksB+69kxUX8nq4ANfeQXVgw4+GA')]
    [TestCase('130', 'horse.webp,claRed,TileOriginal,0,0,1,1,180,-75,0.4,0.98,1luiOvjAV16B9aCDD0b1lawOPvCVV1A96OCDEf0fpTM/e+UizF959kdUX8nq4ANfeQXVgw4+GA')]
    [TestCase('131', 'horse.webp,claWhite,TileOriginal,0,0,1,1,180,-75,1,0.98,11+i+vfBV07B9aCD/0X1lawOPvCVV1A96OCDcfsZogOPeeE7ODC69C5UX8nq4ANfeQXVgw4+GA')]
    [TestCase('132', 'horse.webp,claRed,TileOriginal,0,0,1,1,180,-75,1,0.98,1luiOvjBV16B9aB//0X1lawOPvCVV1A96OCDobwXnLsOeuU2gFC48yJUX8nq4ANfeQXVgw4+GA')]
    [TestCase('133', 'horse.webp,claWhite,TileOriginal,0.15,0.15,0.85,0.85,180,-75,0.4,0.98,1VeyevjAW16B9aCDD0b1lawOPvCVV1A96OCD0T0hxuNfO+UlJK/591hUX8nq4ANfeQXVgw4+GA')]
    [TestCase('134', 'horse.webp,claRed,TileOriginal,0.15,0.15,0.85,0.85,180,-75,0.4,0.98,1VeyOvjAV16B9aCDD0b1lawOPvCVV1A96OCDAb0iudM/O+MgLI9591hUX8nq4ANfeQXVgw4+GA')]
    [TestCase('135', 'horse.webp,claWhite,TileOriginal,0.15,0.15,0.85,0.85,180,-75,1,0.98,1lPCevjAW16B9Z+HD0b1lawOPvCVV1A96OCDgf4a729QuuQ92A5590xUX8nq4ANfeQXVgw4+GA')]
    [TestCase('136', 'horse.webp,claRed,TileOriginal,0.15,0.15,0.85,0.85,180,-75,1,0.98,1lOyevjAW16B9Z+HD0b1lawOPvCVV1A96OCDgTwez0sA+t8w7M549kxUX8nq4ANfeQXVgw4+GA')]
    [TestCase('137', 'horse.webp,claWhite,TileStretch,0,0,1,1,180,-75,0.4,0.98,1luiOvjAV16B9aCDD0b1lawOPvCVV1A96OCDsXwgqE8P++MjsF86901UX8nq4ANfeQXVgw4+GA')]
    [TestCase('138', 'horse.webp,claRed,TileStretch,0,0,1,1,180,-75,0.4,0.98,1luiOvjAV16B9aCDD0b1lawOPvCVV1A96OCDEX0fpi9PO+UhyH+59kdUX8nq4ANfeQXVgw4+GA')]
    [TestCase('139', 'horse.webp,claWhite,TileStretch,0,0,1,1,180,-75,1,0.98,11+i+vfBV07B9aCD/0X1lawOPvCVV1A96OCDsfsYpAN/eeE4OOB69TBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('140', 'horse.webp,claRed,TileStretch,0,0,1,1,180,-75,1,0.98,1luiOvjBV16B9aB//0X1lawOPvCVV1A96OCDoXwWn7M+euQxcLA49CFUX8nq4ANfeQXVgw4+GA')]
    [TestCase('141', 'horse.webp,claWhite,TileStretch,0.15,0.15,0.85,0.85,180,-75,0.4,0.98,1VeyevjAW16B9aCDD0b1lawOPvCVV1A96OCDkT0iweNfu+QiKJ/591hUX8nq4ANfeQXVgw4+GA')]
    [TestCase('142', 'horse.webp,claRed,TileStretch,0.15,0.15,0.85,0.85,180,-75,0.4,0.98,1VeyOvjAV16B9aCDD0b1lawOPvCVV1A96OCDAb0iudMvO+MfMI+591dUX8nq4ANfeQXVgw4+GA')]
    [TestCase('143', 'horse.webp,claWhite,TileStretch,0.15,0.15,0.85,0.85,180,-75,1,0.98,1lPCevjAW16B9Z+HD0b1lawOPvCVV1A96OCD8X0d4m8weuM06O5490tUX8nq4ANfeQXVgw4+GA')]
    [TestCase('144', 'horse.webp,claRed,TileStretch,0.15,0.15,0.85,0.85,180,-75,1,0.98,1lOyevjAW16B9Z+HD0b1lawOPvCVV1A96OCDcXwez0/Aud8s+L649kpUX8nq4ANfeQXVgw4+GA')]
    [TestCase('145', 'horse.webp,claWhite,Tile,0,0,1,0,180,-75,0.4,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('146', 'horse.webp,claWhite,TileStretch,0,0,1,0,180,-75,0.4,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('147', 'horse.webp,claBlue,Tile,0,0,1,0,180,-75,0.4,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('148', 'horse.webp,claBlue,TileStretch,0,0,1,0,180,-75,0.4,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    procedure TestFillArcBitmap(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode; const ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AStartAngle, ASweepAngle, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',   'horse.webp,claWhite,Tile,True,0,0,1,1,0.4,0.98,4j9hPv/kN/9B8+QvUAT4LWyPwO2vp4D/5tD/cL8azLuv/Or139/9+Bkw78P20lO/OwkewE5/Ag')]
    [TestCase('2',   'horse.webp,claRed,Tile,True,0,0,1,1,0.4,0.98,3pNBfPbeD6/B97PD70P0Pvg+P+6sX4DA27/fQD4fuW9feu0BFGB6+hNsjwXyuB8fewsryD3+AQ')]
    [TestCase('3',   'horse.webp,claWhite,Tile,True,0,0,1,1,1,0.98,9ZcPhAkahNBC8Ess0cH7kI8whOrVG7EC5bg3oIIK/guQPfPEo5AD+q/7rjsJvcc/vw6MIB9B4Q')]
    [TestCase('4',   'horse.webp,claRed,Tile,True,0,0,1,1,1,0.98,62+g/vMKICBC+88joEDyu2//wOrNa0BFyY3nv38V0FPvN/nhL/H6/aCTvz/9e0uvPRSstJx+4A')]
    [TestCase('5',   'horse.webp,claWhite,Tile,True,0.15,0.15,0.85,0.85,0.4,0.98,0RuCuQTT8x5/8s6TjwTxSRh/ffOm10C/4+djET8g1ls//ej4fy8+90ZMLwb30Mu++AgC9A09DQ')]
    [TestCase('6',   'horse.webp,claRed,Tile,True,0.15,0.15,0.85,0.85,0.4,0.98,vRcCugLEA9+/97Cbz0XxUeT+/PSdu6C/3sxPEf0jz0vvu+cOdA+8+0o8X0bxzbtO+AcMnG08CQ')]
    [TestCase('7',   'horse.webp,claWhite,Tile,True,0.15,0.15,0.85,0.85,1,0.98,y7uxtxfw17+77ROkT4Lq17+fPPjAlxHC3PE3oQEYFhz//u3Lv1/E9SA8X0EJuHOutw1FIH27/A')]
    [TestCase('8',   'horse.webp,claRed,Tile,True,0.15,0.15,0.85,0.85,1,0.98,mLPxeBLLA5D9+si7fwXr7ENPe/ypTwFD0K0DsbwhBfi+O+sDnA//ACoY30H7sFOedgtfRPx58g')]
    [TestCase('9',   'horse.webp,claWhite,TileOriginal,True,0,0,1,1,0.4,0.98,4j9hPv/kN/9B8+QvUAT4LWyPwO2vp3D/5tD/4D8e50PvfOYPHED99x+0H0f1yvOu+QQbuD6+BA')]
    [TestCase('10',  'horse.webp,claRed,TileOriginal,True,0,0,1,1,0.4,0.98,3pNBfPbeD6/B97PD70P0Pvg+P+6sX4DA27/foL4f2zPfuuceLGA6+xJsPwfywreOOQggDN6+Ag')]
    [TestCase('11',  'horse.webp,claWhite,TileOriginal,True,0,0,1,1,1,0.98,9ZcPhAkahNBC8Ess0cH7kI8whOrVG7EC5bg3wEMTQeAuPugGSCFC975DoEMFqtsO+gOECI++5w')]
    [TestCase('12',  'horse.webp,claRed,TileOriginal,True,0,0,1,1,1,0.98,62+g/vMKICBC+88joEDyu2//wOrNa0BFyY3nn8AWJLguueoraPG6/56PD0T9lEO+OQyQXA2A4g')]
    [TestCase('13',  'horse.webp,claWhite,TileOriginal,True,0.15,0.15,0.85,0.85,0.4,0.98,0RuCuQTT8x5/8s6TjwTxSRh/ffOm10C/4+djET8g1ls//ej4fy8+90ZMLwb30Mu++AgC9A09DQ')]
    [TestCase('14',  'horse.webp,claRed,TileOriginal,True,0.15,0.15,0.85,0.85,0.4,0.98,vRcCugLEA9+/97Cbz0XxUeT+/PSdu6C/3sxPEf0jz0vvu+cOdA+8+0o8X0bxzbtO+AcMnG08CQ')]
    [TestCase('15',  'horse.webp,claWhite,TileOriginal,True,0.15,0.15,0.85,0.85,1,0.98,y7uxtxfw17+77ROkT4Lq17+fPPjAlxHC3PE3oQEYFhz//u3Lv1/E9SA8X0EJuHOutw1FIH27/A')]
    [TestCase('16',  'horse.webp,claRed,TileOriginal,True,0.15,0.15,0.85,0.85,1,0.98,mLPxeBLLA5D9+si7fwXr7ENPe/ypTwFD0K0DsbwhBfi+O+sDnA//ACoY30H7sFOedgtfRPx58g')]
    [TestCase('17',  'horse.webp,claWhite,TileStretch,True,0,0,1,1,0.4,0.98,31MxPv3lT4/B890DkIT2NHxfAO7Cu3A/5drrsD8ewqM//O3xyw899RWcvwL51y9PewsYeJ4/BQ')]
    [TestCase('18',  'horse.webp,claRed,TileStretch,True,0,0,1,1,0.4,0.98,3ZdRvPbdH99B+ay3D0T0P+jefu6vj7AA28LD0L0evGsveu0AHBC6+BWYzwT1wAc/+gohzL39Aw')]
    [TestCase('19',  'horse.webp,claWhite,TileStretch,True,0,0,1,1,1,0.98,7c+PgwQcxMBB8TjAUAL4or/Qw+oETJHC4NIHMEMT5M9/fPq7c4CB8KQPwLgOyXMfvhR7cP7B6A')]
    [TestCase('20',  'horse.webp,claRed,TileStretch,True,0,0,1,1,1,0.98,6XPQPvQJRJBC/78HAMHyvUcPAOzW38CFx5Sjn34U1kdPt/ngTyE6+aT7370Ejwt/+xKTuDy95Q')]
    [TestCase('21',  'horse.webp,claWhite,TileStretch,True,0.15,0.15,0.85,0.85,0.4,0.98,xgvCuQbWD89/8cLD/4TuSzh//fOzA+F+4/V/IT8f1Fcv/er4e88990qIj4X30r+++AkB3L28DA')]
    [TestCase('22',  'horse.webp,claRed,TileStretch,True,0.15,0.15,0.85,0.85,0.4,0.98,ugMCegLID9+/9a6f78XwUuiOPPeg27A/3c87Uf0izkufe+gOfA+8+ktYPwby0qvudwcGoD38CA')]
    [TestCase('23',  'horse.webp,claWhite,TileStretch,True,0.15,0.15,0.85,0.85,1,0.98,r5NxeBz2H1D96vYjUEPk3AuAvPne/0EB3BR4wcEVEgzP/vLNr2+D9SjQ378KvlfOdxBD4Kx6+w')]
    [TestCase('24',  'horse.webp,claRed,TileStretch,True,0.15,0.15,0.85,0.85,1,0.98,kn8B+RHTH4C99cTPz8Xp7kc/egGvmzGDzLXLMD0fA/T+Ou0CuA+//i1gf8H8vCe+NQpOVHz58Q')]
    [TestCase('25',  'horse.webp,claWhite,Tile,False,0,0,1,1,0.4,0.98,4j9hPv/kN/9B8+QvUAT4LWyPwO2vp4D/5tD/cL8azLuv/Or139/9+Bkw78P20lO/OwkewE5/Ag')]
    [TestCase('26',  'horse.webp,claRed,Tile,False,0,0,1,1,0.4,0.98,3pNBfPbeD6/B97PD70P0Pvg+P+6sX4DA27/fQD4fuW9feu0BFGB6+hNsjwXyuB8fewsryD3+AQ')]
    [TestCase('27',  'horse.webp,claWhite,Tile,False,0,0,1,1,1,0.98,9ZcPhAkahNBC8Ess0cH7kI8whOrVG7EC5bg3oIIK/guQPfPEo5AD+q/7rjsJvcc/vw6MIB9B4Q')]
    [TestCase('28',  'horse.webp,claRed,Tile,False,0,0,1,1,1,0.98,62+g/vMKICBC+88joEDyu2//wOrNa0BFyY3nv38V0FPvN/nhL/H6/aCTvz/9e0uvPRSstJx+4A')]
    [TestCase('29',  'horse.webp,claWhite,Tile,False,0.15,0.15,0.85,0.85,0.4,0.98,0RuCuQTT8x5/8s6TjwTxSRh/ffOm10C/4+djET8g1ls//ej4fy8+90ZMLwb30Mu++AgC9A09DQ')]
    [TestCase('30',  'horse.webp,claRed,Tile,False,0.15,0.15,0.85,0.85,0.4,0.98,vRcCugLEA9+/97Cbz0XxUeT+/PSdu6C/3sxPEf0jz0vvu+cOdA+8+0o8X0bxzbtO+AcMnG08CQ')]
    [TestCase('31',  'horse.webp,claWhite,Tile,False,0.15,0.15,0.85,0.85,1,0.98,y7uxtxfw17+77ROkT4Lq17+fPPjAlxHC3PE3oQEYFhz//u3Lv1/E9SA8X0EJuHOutw1FIH27/A')]
    [TestCase('32',  'horse.webp,claRed,Tile,False,0.15,0.15,0.85,0.85,1,0.98,mLPxeBLLA5D9+si7fwXr7ENPe/ypTwFD0K0DsbwhBfi+O+sDnA//ACoY30H7sFOedgtfRPx58g')]
    [TestCase('33',  'horse.webp,claWhite,TileOriginal,False,0,0,1,1,0.4,0.98,4j9hPv/kN/9B8+QvUAT4LWyPwO2vp3D/5tD/4D8e50PvfOYPHED99x+0H0f1yvOu+QQbuD6+BA')]
    [TestCase('34',  'horse.webp,claRed,TileOriginal,False,0,0,1,1,0.4,0.98,3pNBfPbeD6/B97PD70P0Pvg+P+6sX4DA27/foL4f2zPfuuceLGA6+xJsPwfywreOOQggDN6+Ag')]
    [TestCase('35',  'horse.webp,claWhite,TileOriginal,False,0,0,1,1,1,0.98,9ZcPhAkahNBC8Ess0cH7kI8whOrVG7EC5bg3wEMTQeAuPugGSCFC975DoEMFqtsO+gOECI++5w')]
    [TestCase('36',  'horse.webp,claRed,TileOriginal,False,0,0,1,1,1,0.98,62+g/vMKICBC+88joEDyu2//wOrNa0BFyY3nn8AWJLguueoraPG6/56PD0T9lEO+OQyQXA2A4g')]
    [TestCase('37',  'horse.webp,claWhite,TileOriginal,False,0.15,0.15,0.85,0.85,0.4,0.98,0RuCuQTT8x5/8s6TjwTxSRh/ffOm10C/4+djET8g1ls//ej4fy8+90ZMLwb30Mu++AgC9A09DQ')]
    [TestCase('38',  'horse.webp,claRed,TileOriginal,False,0.15,0.15,0.85,0.85,0.4,0.98,vRcCugLEA9+/97Cbz0XxUeT+/PSdu6C/3sxPEf0jz0vvu+cOdA+8+0o8X0bxzbtO+AcMnG08CQ')]
    [TestCase('39',  'horse.webp,claWhite,TileOriginal,False,0.15,0.15,0.85,0.85,1,0.98,y7uxtxfw17+77ROkT4Lq17+fPPjAlxHC3PE3oQEYFhz//u3Lv1/E9SA8X0EJuHOutw1FIH27/A')]
    [TestCase('40',  'horse.webp,claRed,TileOriginal,False,0.15,0.15,0.85,0.85,1,0.98,mLPxeBLLA5D9+si7fwXr7ENPe/ypTwFD0K0DsbwhBfi+O+sDnA//ACoY30H7sFOedgtfRPx58g')]
    [TestCase('41',  'horse.webp,claWhite,TileStretch,False,0,0,1,1,0.4,0.98,31MxPv3lT4/B890DkIT2NHxfAO7Cu3A/5drrsD8ewqM//O3xyw899RWcvwL51y9PewsYeJ4/BQ')]
    [TestCase('42',  'horse.webp,claRed,TileStretch,False,0,0,1,1,0.4,0.98,3ZdRvPbdH99B+ay3D0T0P+jefu6vj7AA28LD0L0evGsveu0AHBC6+BWYzwT1wAc/+gohzL39Aw')]
    [TestCase('43',  'horse.webp,claWhite,TileStretch,False,0,0,1,1,1,0.98,7c+PgwQcxMBB8TjAUAL4or/Qw+oETJHC4NIHMEMT5M9/fPq7c4CB8KQPwLgOyXMfvhR7cP7B6A')]
    [TestCase('44',  'horse.webp,claRed,TileStretch,False,0,0,1,1,1,0.98,6XPQPvQJRJBC/78HAMHyvUcPAOzW38CFx5Sjn34U1kdPt/ngTyE6+aT7370Ejwt/+xKTuDy95Q')]
    [TestCase('45',  'horse.webp,claWhite,TileStretch,False,0.15,0.15,0.85,0.85,0.4,0.98,xgvCuQbWD89/8cLD/4TuSzh//fOzA+F+4/V/IT8f1Fcv/er4e88990qIj4X30r+++AkB3L28DA')]
    [TestCase('46',  'horse.webp,claRed,TileStretch,False,0.15,0.15,0.85,0.85,0.4,0.98,ugMCegLID9+/9a6f78XwUuiOPPeg27A/3c87Uf0izkufe+gOfA+8+ktYPwby0qvudwcGoD38CA')]
    [TestCase('47',  'horse.webp,claWhite,TileStretch,False,0.15,0.15,0.85,0.85,1,0.98,r5NxeBz2H1D96vYjUEPk3AuAvPne/0EB3BR4wcEVEgzP/vLNr2+D9SjQ378KvlfOdxBD4Kx6+w')]
    [TestCase('48',  'horse.webp,claRed,TileStretch,False,0.15,0.15,0.85,0.85,1,0.98,kn8B+RHTH4C99cTPz8Xp7kc/egGvmzGDzLXLMD0fA/T+Ou0CuA+//i1gf8H8vCe+NQpOVHz58Q')]
    [TestCase('49',  'horse.webp,claWhite,Tile,True,0,0.2,1,0.8,0.4,0.98,to+h+gPkI08C77uzLwXyWtDuf/e/z9B85vI/Mf8c4I9f/eoA4E/8+i2s38X00M9u+QMROO5+Cw')]
    [TestCase('50',  'horse.webp,claRed,Tile,True,0,0.2,1,0.8,0.4,0.98,y7dxuv3fC0/B85dDP8XzY6A+vvi6g3A+3s4jQX4f1H/PO+kP4I86/DBkr8bxx6MeeQUR4M0+CQ')]
    [TestCase('51',  'horse.webp,claWhite,Tile,True,0,0.2,1,0.8,1,0.98,h1+AuhUbTLBD5eT/78PsAAjPQgL+gxH84w7U8AEQL6Bf//Lhs6D//uEvgMADuH9u+QFqyD2A+A')]
    [TestCase('52',  'horse.webp,claRed,Tile,True,0,0.2,1,0.8,1,0.98,u78Q+gUMFCBB8YvfDoTxGZCOfgXyvyAAz7SXoD8WEXhv++4FtED7Aed/nwL8og++OAVs8Oz/8g')]
    [TestCase('53',  'horse.webp,claWhite,TileOriginal,True,0,0.2,1,0.8,0.4,0.98,to+h+gPkI08C77uzLwXyWtDuf/e/z9B85vI/Mf8c4I9f/eoA4E/8+i2s38X00M9u+QMROO5+Cw')]
    [TestCase('54',  'horse.webp,claRed,TileOriginal,True,0,0.2,1,0.8,0.4,0.98,y7dxuv3fC0/B85dDP8XzY6A+vvi6g3A+3s4jQX4f1H/PO+kP4I86/DBkr8bxx6MeeQUR4M0+CQ')]
    [TestCase('55',  'horse.webp,claWhite,TileOriginal,True,0,0.2,1,0.8,1,0.98,h1+AuhUbTLBD5eT/78PsAAjPQgL+gxH84w7U8AEQL6Bf//Lhs6D//uEvgMADuH9u+QFqyD2A+A')]
    [TestCase('56',  'horse.webp,claRed,TileOriginal,True,0,0.2,1,0.8,1,0.98,u78Q+gUMFCBB8YvfDoTxGZCOfgXyvyAAz7SXoD8WEXhv++4FtED7Aed/nwL8og++OAVs8Oz/8g')]
    [TestCase('57',  'horse.webp,claWhite,TileStretch,True,0,0.2,1,0.8,0.4,0.98,vJ/h+gHbJ29C8LmTzwXxW/x+P/fJz+A85/tzAQAd14fPvez4A2D89S/Mf0X1yMPeOQYOTL6+DA')]
    [TestCase('58',  'horse.webp,claRed,TileStretch,True,0,0.2,1,0.8,0.4,0.98,y7vBOv3aB3+B85g7v0XzYbDuvfm5fyD+3tArUf4e0W/P++kLBFC6+y+Mb4bwxZde+QUS6H1+CQ')]
    [TestCase('59',  'horse.webp,claWhite,TileStretch,True,0,0.2,1,0.8,1,0.98,lY8gexAFWPBD6N6vf8XqA3ivgQEXhDG85SNYEcQQGoxfAPfLBwGA8uZ/oH8FpF+eegdj/K1/+w')]
    [TestCase('60',  'horse.webp,claRed,TileStretch,True,0,0.2,1,0.8,1,0.98,u8vgugQADLDB8I3PPoXwE7iuvQfwt2D/0Lej0P8UCkh/u/D7C8E6AeXj/4H5nfdduQZtBB2/8w')]
    [TestCase('61',  'horse.webp,claWhite,Tile,True,0,0.2,1,0.8,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('62',  'horse.webp,claRed,Tile,True,0,0.2,1,0.8,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('63',  'horse.webp,claWhite,TileOriginal,True,0,0.2,1,0.8,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('64',  'horse.webp,claRed,TileOriginal,True,0,0.2,1,0.8,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('65',  'horse.webp,claWhite,TileStretch,True,0,0.2,1,0.8,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('66',  'horse.webp,claRed,TileStretch,True,0,0.2,1,0.8,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('67',  'horse.webp,claWhite,Tile,True,0,0.2,1,0.2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('68',  'horse.webp,claRed,Tile,True,0,0.2,1,0.2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('69',  'horse.webp,claWhite,TileOriginal,True,0,0.2,1,0.2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('70',  'horse.webp,claRed,TileOriginal,True,0,0.2,1,0.2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('71',  'horse.webp,claWhite,TileStretch,True,0,0.2,1,0.2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('72',  'horse.webp,claRed,TileStretch,True,0,0.2,1,0.2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('73',  'horse.webp,claWhite,Tile,True,0.5,0.2,0.5,0.8,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('74',  'horse.webp,claRed,Tile,True,0.5,0.2,0.5,0.8,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('75',  'horse.webp,claWhite,TileOriginal,True,0.5,0.2,0.5,0.8,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('76',  'horse.webp,claRed,TileOriginal,True,0.5,0.2,0.5,0.8,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('77',  'horse.webp,claWhite,TileStretch,True,0.5,0.2,0.5,0.8,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('78',  'horse.webp,claRed,TileStretch,True,0.5,0.2,0.5,0.8,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('79',  'horse.webp,claWhite,Tile,True,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('80',  'horse.webp,claRed,Tile,True,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('81',  'horse.webp,claWhite,TileOriginal,True,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('82',  'horse.webp,claRed,TileOriginal,True,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('83',  'horse.webp,claWhite,TileStretch,True,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('84',  'horse.webp,claRed,TileStretch,True,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('85',  'horse.webp,claWhite,Tile,True,0,-0.2,1,1.2,1,0.98,yO/eAgw1uDAB9O93Qb/3hIvwQPYi3D8E5eYj4P7wLFxgQAHwg/AABNebILgQ85/APgZRpP899A')]
    [TestCase('86',  'horse.webp,claWhite,TileOriginal,True,0,-0.2,1,1.2,1,0.98,yO/eAgw1uDAB9O93Qb/3hIvwQPYi3D8E5eYjsP4QxjuvPuvqbyA7+ih4T8Tv1J/fOvwlqA4+CQ')]
    [TestCase('87',  'horse.webp,claWhite,TileStretch,True,0,-0.2,1,1.2,1,0.98,0SdPgwsC0ODC9dTTQP/5ddcAgvwXXBDE3t8vYH32EQRQPQYNdKD+/dvnb3YRNOjffg0wpN6B7g')]
    [TestCase('88',  'horse.webp,claWhite,Tile,False,0,-0.2,1,1.2,1,0.98,yO/eAgw1uDAB9O93Qb/3hIvwQPYi3D8E5eYj4P7wLFxgQAHwg/AABNebILgQ85/APgZRpP899A')]
    [TestCase('89',  'horse.webp,claWhite,TileOriginal,False,0,-0.2,1,1.2,1,0.98,yO/eAgw1uDAB9O93Qb/3hIvwQPYi3D8E5eYjsP4QxjuvPuvqbyA7+ih4T8Tv1J/fOvwlqA4+CQ')]
    [TestCase('90',  'horse.webp,claWhite,TileStretch,False,0,-0.2,1,1.2,1,0.98,0SdPgwsC0ODC9dTTQP/5ddcAgvwXXBDE3t8vYH32EQRQPQYNdKD+/dvnb3YRNOjffg0wpN6B7g')]
    [TestCase('91',  'horse.webp,claWhite,Tile,True,0,-0.2,1,1.2,0.85,0.98,ym+fAQkjYEBB9OMrQUD3rUOAgPUN8D+D5eVXsP74GDzAP/32V/B/AuxrsDoL8GPw/QU+eP+9+Q')]
    [TestCase('92',  'horse.webp,claWhite,TileOriginal,True,0,-0.2,1,1.2,0.85,0.98,ym+fAQkjYEBB9OMrQUD3rUOAgPUN8D+D5eVXgP4TwkdPPurwQ/D6+TBwDwXv1oefev0ZpA5+Cw')]
    [TestCase('93',  'horse.webp,claWhite,TileStretch,True,0,-0.2,1,1.2,0.85,0.98,0qMPggj4c6DC9cyfQAD5oINggfoEXBAD4N9jYH39AfAvfQEOTPA9/e/TP7kLJ8QPPgwjnE7B9A')]
    [TestCase('94',  'horse.webp,claWhite,Tile,False,0,-0.2,1,1.2,0.85,0.98,ym+fAQkjYEBB9OMrQUD3rUOAgPUN8D+D5eVXsP74GDzAP/32V/B/AuxrsDoL8GPw/QU+eP+9+Q')]
    [TestCase('95',  'horse.webp,claWhite,TileOriginal,False,0,-0.2,1,1.2,0.85,0.98,ym+fAQkjYEBB9OMrQUD3rUOAgPUN8D+D5eVXgP4TwkdPPurwQ/D6+TBwDwXv1oefev0ZpA5+Cw')]
    [TestCase('96',  'horse.webp,claWhite,TileStretch,False,0,-0.2,1,1.2,0.85,0.98,0qMPggj4c6DC9cyfQAD5oINggfoEXBAD4N9jYH39AfAvfQEOTPA9/e/TP7kLJ8QPPgwjnE7B9A')]
    [TestCase('97',  'horse.webp,$7F0000FF,Tile,True,0,-0.2,1,1.2,1,0.98,o7dg/fq6y74A+qX734PrBnDf/uuwF9DA29jjoDkL1P8/+uf3/0/4ACG4zkD67Ne+uQIL2H0/CQ')]
    [TestCase('98',  'horse.webp,$7F0000FF,TileOriginal,True,0,-0.2,1,1.2,1,0.98,o7dg/fq6y74A+qX734PrBnDf/uuwF9DA29jj8Psbt5Pvu+cVrL93+y0AD4ft5fv+eQXw781+Cw')]
    [TestCase('99',  'horse.webp,$7F0000FF,TileStretch,True,0,-0.2,1,1.2,1,0.98,pbOQ/fu81+4A+Znvf8PrB5wfP+yuH5AB2NbXYDkM1PNvuuf+C1D4/ye0/sD659PO+QQQ+L0+Cw')]
    [TestCase('100', 'horse.webp,$7F0000FF,Tile,False,0,-0.2,1,1.2,1,0.98,o7dg/fq6y74A+qX734PrBnDf/uuwF9DA29jjoDkL1P8/+uf3/0/4ACG4zkD67Ne+uQIL2H0/CQ')]
    [TestCase('101', 'horse.webp,$7F0000FF,TileOriginal,False,0,-0.2,1,1.2,1,0.98,o7dg/fq6y74A+qX734PrBnDf/uuwF9DA29jj8Psbt5Pvu+cVrL93+y0AD4ft5fv+eQXw781+Cw')]
    [TestCase('102', 'horse.webp,$7F0000FF,TileStretch,False,0,-0.2,1,1.2,1,0.98,pbOQ/fu81+4A+Znvf8PrB5wfP+yuH5AB2NbXYDkM1PNvuuf+C1D4/ye0/sD659PO+QQQ+L0+Cw')]
    [TestCase('103', 'horse.webp,$7F0000FF,Tile,True,0,-0.2,1,1.2,0.85,0.98,q/fwfPq7u95A+aXnLwTtG1S/vuysH1DA3dn7MDoPzu9/euf85494/yvQDgL46t+uOQMD9D1/Cw')]
    [TestCase('104', 'horse.webp,$7F0000FF,TileOriginal,True,0,-0.2,1,1.2,0.85,0.98,q/fwfPq6u95A+aXnLwTtG1S/vuysH1DA3dn7IHwdtZP/e+cVoB/4+jUQX0ft5P/ueQXsA74+DQ')]
    [TestCase('105', 'horse.webp,$7F0000FF,TileStretch,True,0,-0.2,1,1.2,0.85,0.98,rO8gffu9w/6A+Jrf30PtHHjvvuyqJ/CA2tfv8DkQzuOvOucC7J/4/i/MPoL45tu+OQUHDJ4+DQ')]
    [TestCase('106', 'horse.webp,$7F0000FF,Tile,False,0,-0.2,1,1.2,0.85,0.98,q/fwfPq7u95A+aXnLwTtG1S/vuysH1DA3dn7MDoPzu9/euf85494/yvQDgL46t+uOQMD9D1/Cw')]
    [TestCase('107', 'horse.webp,$7F0000FF,TileOriginal,False,0,-0.2,1,1.2,0.85,0.98,q/fwfPq6u95A+aXnLwTtG1S/vuysH1DA3dn7IHwdtZP/e+cVoB/4+jUQX0ft5P/ueQXsA74+DQ')]
    [TestCase('108', 'horse.webp,$7F0000FF,TileStretch,False,0,-0.2,1,1.2,0.85,0.98,rO8gffu9w/6A+Jrf30PtHHjvvuyqJ/CA2tfv8DkQzuOvOucC7J/4/i/MPoL45tu+OQUHDJ4+DQ')]
    procedure TestFillEllipseBitmap(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode; ABlending: Boolean; const ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',   'horse.webp,claWhite,Tile,True,0,0,1,1,0.85,0.98,4sNtQwURkABBB+VfoDv+gI9QfwIQAMCD+dw7j77cICAAQgn/BzF/AcHL7jgCAzTww/xHmE+A5w')]
    [TestCase('2',   'horse.webp,claRed,Tile,True,0,0,1,1,0.85,0.98,2Hve/vIEOHCAEHx7rzr2pJuP/AIKaP8F4rj3Dvzl+IcvfQ4YfNG3BLRLX/z3y8+vQgFiiC2+5g')]
    [TestCase('3',   'horse.webp,claWhite,Tile,True,0,0,1,1,1,0.98,5bO9AwYUjHDBBehvoHv/f5dQvwIM/M+D+tZHv/7bISRwggkABHG/AcGzDjkBBEAwxPxIqF+A5Q')]
    [TestCase('4',   'horse.webp,claRed,Tile,True,0,0,1,1,1,0.98,2otufvAFJNDAEGxjf/r1qncPPAMFTF/G3qv3zvvm82+/vA8dkNF2BbJLHz31wsevQgJoPM295A')]
    [TestCase('5',   'horse.webp,claWhite,Tile,True,0.15,0.15,0.85,0.85,0.85,0.98,9Bv+P/0VeAA9EvCzX/r8jH+APv0V3I8E9efjbv/gItivgAjlN9F++r/3XrkI/Puvgv48bN/A8A')]
    [TestCase('6',   'horse.webp,claRed,Tile,True,0.15,0.15,0.85,0.85,0.85,0.98,yRMOwfj2n6B+HbDD/7z9nhNwvQABnF+F6q23PvvoE7jfPQYUHFG6A8fbvrn89eO/gfxSsJ4/6A')]
    [TestCase('7',   'horse.webp,claWhite,Tile,True,0.15,0.15,0.85,0.85,1,0.98,+xe+v/wZcMC8EvWjDzr+jYdgfvwS0K9E9eLfzr/gI9DfwAjhPxF/+b7rfvkI/P+vAv87eO+A8A')]
    [TestCase('8',   'horse.webp,claRed,Tile,True,0.15,0.15,0.85,0.85,1,0.98,yA/+gPf0n6D+H6q3P73+ogcgfQD7i6/F6J6r7nrqEqiP/QUYILF5BMjH/vn69N+fgfxUmH5/5g')]
    [TestCase('9',   'horse.webp,claWhite,TileOriginal,True,0,0,1,1,0.85,0.98,4sNtQwURkABBB+VfoDv+gI9QfwIQAMCD+dw7Pz/hJ8ifwQr0GzE//dVDTzoD8+8Pw/pLeN/+7Q')]
    [TestCase('10',  'horse.webp,claRed,TileOriginal,True,0,0,1,1,0.85,0.98,2Hve/vIEOHCAEHx7rzr2pJuP/AIKaP8F4rj3fjzkD6hP/QwUNCE5BLqrnnr84G+/AgJVCB5A6Q')]
    [TestCase('11',  'horse.webp,claWhite,TileOriginal,True,0,0,1,1,1,0.98,5bO9AwYUjHDBBehvoHv/f5dQvwIM/M+D+tZHjz/hKrz/QQvzH4G//Nk/r7oC8u8vg/pMhJ/+7A')]
    [TestCase('12',  'horse.webp,claRed,TileOriginal,True,0,0,1,1,1,0.98,2otuvvAEJNDAEGxjf/r1qncPPAMFTF/G3qv3XrzkDZTv/A0YPFH4BLmLDnv63FfPwgJZ1B3A5w')]
    [TestCase('13',  'horse.webp,claWhite,TileOriginal,True,0.15,0.15,0.85,0.85,0.85,0.98,9Bv+P/0VeAA9EvCzX/r8jH+APv0V3I8E9efjbv/gItivgAjlN9F++r/3XrkI/Puvgv48bN/A8A')]
    [TestCase('14',  'horse.webp,claRed,TileOriginal,True,0.15,0.15,0.85,0.85,0.85,0.98,yRMOwfj2n6B+HbDD/7z9nhNwvQABnF+F6q23PvvoE7jfPQYUHFG6A8fbvrn89eO/gfxSsJ4/6A')]
    [TestCase('15',  'horse.webp,claWhite,TileOriginal,True,0.15,0.15,0.85,0.85,1,0.98,+xe+v/wZcMC8EvWjDzr+jYdgfvwS0K9E9eLfzr/gI9DfwAjhPxF/+b7rfvkI/P+vAv87eO+A8A')]
    [TestCase('16',  'horse.webp,claRed,TileOriginal,True,0.15,0.15,0.85,0.85,1,0.98,yA/+gPf0n6D+H6q3P73+ogcgfQD7i6/F6J6r7nrqEqiP/QUYILF5BMjH/vn69N+fgfxUmH5/5g')]
    [TestCase('17',  'horse.webp,claWhite,TileStretch,True,0,0,1,1,0.85,0.98,2vf9AgESxCAACNUDIPz6j7fw/gI4LKAD9vITDz/kCuwPQQ/333B9+bizf7YGDewPwwE5AA/B7Q')]
    [TestCase('18',  'horse.webp,claRed,TileStretch,True,0,0,1,1,0.85,0.98,13/+PvMDWNAAFG5j/3r2pnfPuwMRzF+G4L67Hvvk/nuvvA4XlCG3ALijz3r+25fPQABNjA096w')]
    [TestCase('19',  'horse.webp,claWhite,TileStretch,True,0,0,1,1,1,0.98,3Os9AwEWyHCABtUDILz7kMfgPgM8MKBD9u8XT7/kB+hPwRD312B9+LbDH7YGEOwfgwI3+C4B7Q')]
    [TestCase('20',  'horse.webp,claRed,TileStretch,True,0,0,1,1,1,0.98,2Y+e/vADTDDBFFxH33r2q08vOwQNwN8G3bKzrrrl+WMvPBAcrAH2ALazP7v81od/wABPRG386Q')]
    [TestCase('21',  'horse.webp,claWhite,TileStretch,True,0.15,0.15,0.85,0.85,0.85,0.98,3fedAAEbtHD+D9cbMPv3kL+Afv4vNNCD9AQYj//eHsx/gAznKwE++sZ3D7gJAOTPwgA5OC+A7w')]
    [TestCase('22',  'horse.webp,claRed,TileStretch,True,0.15,0.15,0.85,0.85,0.85,0.98,xOcNQfj9t5D+GKzTT338oBuA/AQH4H9F57OHrvvmEbg/vQcTMFH6AcoXb/n9/78PgftDvC6/5w')]
    [TestCase('23',  'horse.webp,claWhite,TileStretch,True,0.15,0.15,0.85,0.85,1,0.98,4O9tQAEftHD+D9gfEDv4ks9g/v0wPNCD9AQg/7/eH8CvgA3jMyE++cd//zcKAeDPggE4OC8A7w')]
    [TestCase('24',  'horse.webp,claRed,TileStretch,True,0.15,0.15,0.85,0.85,1,0.98,wtv9APf9u6C+GqXLj339pA8QfAUB2M8F5aZzbvvnEKjf/AcYPME5AssPn3n8/7O/QPtDqP7+5Q')]
    [TestCase('25',  'horse.webp,claWhite,Tile,False,0,0,1,1,0.85,0.98,4sNtQwURkABBB+VfoDv+gI9QfwIQAMCD+dw7j77cICAAQgn/BzF/AcHL7jgCAzTww/xHmE+A5w')]
    [TestCase('26',  'horse.webp,claRed,Tile,False,0,0,1,1,0.85,0.98,2Hve/vIEOHCAEHx7rzr2pJuP/AIKaP8F4rj3Dvzl+IcvfQ4YfNG3BLRLX/z3y8+vQgFiiC2+5g')]
    [TestCase('27',  'horse.webp,claWhite,Tile,False,0,0,1,1,1,0.98,5bO9AwYUjHDBBehvoHv/f5dQvwIM/M+D+tZHv/7bISRwggkABHG/AcGzDjkBBEAwxPxIqF+A5Q')]
    [TestCase('28',  'horse.webp,claRed,Tile,False,0,0,1,1,1,0.98,2otufvAFJNDAEGxjf/r1qncPPAMFTF/G3qv3zvvm82+/vA8dkNF2BbJLHz31wsevQgJoPM295A')]
    [TestCase('29',  'horse.webp,claWhite,Tile,False,0.15,0.15,0.85,0.85,0.85,0.98,9Bv+P/0VeAA9EvCzX/r8jH+APv0V3I8E9efjbv/gItivgAjlN9F++r/3XrkI/Puvgv48bN/A8A')]
    [TestCase('30',  'horse.webp,claRed,Tile,False,0.15,0.15,0.85,0.85,0.85,0.98,yRMOwfj2n6B+HbDD/7z9nhNwvQABnF+F6q23PvvoE7jfPQYUHFG6A8fbvrn89eO/gfxSsJ4/6A')]
    [TestCase('31',  'horse.webp,claWhite,Tile,False,0.15,0.15,0.85,0.85,1,0.98,+xe+v/wZcMC8EvWjDzr+jYdgfvwS0K9E9eLfzr/gI9DfwAjhPxF/+b7rfvkI/P+vAv87eO+A8A')]
    [TestCase('32',  'horse.webp,claRed,Tile,False,0.15,0.15,0.85,0.85,1,0.98,yA/+gPf0n6D+H6q3P73+ogcgfQD7i6/F6J6r7nrqEqiP/QUYILF5BMjH/vn69N+fgfxUmH5/5g')]
    [TestCase('33',  'horse.webp,claWhite,TileOriginal,False,0,0,1,1,0.85,0.98,4sNtQwURkABBB+VfoDv+gI9QfwIQAMCD+dw7Pz/hJ8ifwQr0GzE//dVDTzoD8+8Pw/pLeN/+7Q')]
    [TestCase('34',  'horse.webp,claRed,TileOriginal,False,0,0,1,1,0.85,0.98,2Hve/vIEOHCAEHx7rzr2pJuP/AIKaP8F4rj3fjzkD6hP/QwUNCE5BLqrnnr84G+/AgJVCB5A6Q')]
    [TestCase('35',  'horse.webp,claWhite,TileOriginal,False,0,0,1,1,1,0.98,5bO9AwYUjHDBBehvoHv/f5dQvwIM/M+D+tZHjz/hKrz/QQvzH4G//Nk/r7oC8u8vg/pMhJ/+7A')]
    [TestCase('36',  'horse.webp,claRed,TileOriginal,False,0,0,1,1,1,0.98,2otuvvAEJNDAEGxjf/r1qncPPAMFTF/G3qv3XrzkDZTv/A0YPFH4BLmLDnv63FfPwgJZ1B3A5w')]
    [TestCase('37',  'horse.webp,claWhite,TileOriginal,False,0.15,0.15,0.85,0.85,0.85,0.98,9Bv+P/0VeAA9EvCzX/r8jH+APv0V3I8E9efjbv/gItivgAjlN9F++r/3XrkI/Puvgv48bN/A8A')]
    [TestCase('38',  'horse.webp,claRed,TileOriginal,False,0.15,0.15,0.85,0.85,0.85,0.98,yRMOwfj2n6B+HbDD/7z9nhNwvQABnF+F6q23PvvoE7jfPQYUHFG6A8fbvrn89eO/gfxSsJ4/6A')]
    [TestCase('39',  'horse.webp,claWhite,TileOriginal,False,0.15,0.15,0.85,0.85,1,0.98,+xe+v/wZcMC8EvWjDzr+jYdgfvwS0K9E9eLfzr/gI9DfwAjhPxF/+b7rfvkI/P+vAv87eO+A8A')]
    [TestCase('40',  'horse.webp,claRed,TileOriginal,False,0.15,0.15,0.85,0.85,1,0.98,yA/+gPf0n6D+H6q3P73+ogcgfQD7i6/F6J6r7nrqEqiP/QUYILF5BMjH/vn69N+fgfxUmH5/5g')]
    [TestCase('41',  'horse.webp,claWhite,TileStretch,False,0,0,1,1,0.85,0.98,2vf9AgESxCAACNUDIPz6j7fw/gI4LKAD9vITDz/kCuwPQQ/333B9+bizf7YGDewPwwE5AA/B7Q')]
    [TestCase('42',  'horse.webp,claRed,TileStretch,False,0,0,1,1,0.85,0.98,13/+PvMDWNAAFG5j/3r2pnfPuwMRzF+G4L67Hvvk/nuvvA4XlCG3ALijz3r+25fPQABNjA096w')]
    [TestCase('43',  'horse.webp,claWhite,TileStretch,False,0,0,1,1,1,0.98,3Os9AwEWyHCABtUDILz7kMfgPgM8MKBD9u8XT7/kB+hPwRD312B9+LbDH7YGEOwfgwI3+C4B7Q')]
    [TestCase('44',  'horse.webp,claRed,TileStretch,False,0,0,1,1,1,0.98,2Y+e/vADTDDBFFxH33r2q08vOwQNwN8G3bKzrrrl+WMvPBAcrAH2ALazP7v81od/wABPRG386Q')]
    [TestCase('45',  'horse.webp,claWhite,TileStretch,False,0.15,0.15,0.85,0.85,0.85,0.98,3fedAAEbtHD+D9cbMPv3kL+Afv4vNNCD9AQYj//eHsx/gAznKwE++sZ3D7gJAOTPwgA5OC+A7w')]
    [TestCase('46',  'horse.webp,claRed,TileStretch,False,0.15,0.15,0.85,0.85,0.85,0.98,xOcNQfj9t5D+GKzTT338oBuA/AQH4H9F57OHrvvmEbg/vQcTMFH6AcoXb/n9/78PgftDvC6/5w')]
    [TestCase('47',  'horse.webp,claWhite,TileStretch,False,0.15,0.15,0.85,0.85,1,0.98,4O9tQAEftHD+D9gfEDv4ks9g/v0wPNCD9AQg/7/eH8CvgA3jMyE++cd//zcKAeDPggE4OC8A7w')]
    [TestCase('48',  'horse.webp,claRed,TileStretch,False,0.15,0.15,0.85,0.85,1,0.98,wtv9APf9u6C+GqXLj339pA8QfAUB2M8F5aZzbvvnEKjf/AcYPME5AssPn3n8/7O/QPtDqP7+5Q')]
    [TestCase('49',  'horse.webp,claWhite,Tile,True,0,0.2,1,0.8,0.85,0.98,tbO9wAQezMD/DuVPULr5fWtgPwMeVECD8AR8fv3gKtxPAAv909A9AsFXj7gFB/Tfwfs8IK8A7w')]
    [TestCase('50',  'horse.webp,claRed,Tile,True,0,0.2,1,0.8,0.85,0.98,4gNegPcSnKD9GJlff3r9kgPAuwUUrM8G37dHjvvlELzv/Acc0BD6BMbDTnr/9ZdPgf4+aF5A6g')]
    [TestCase('51',  'horse.webp,claWhite,Tile,True,0,0.2,1,0.8,1,0.98,sZudwAUk1ADADuhfEDr6fGtgfwMdYDAD8ARojv3gLNRvwAv9x+C9AsFbj3gFCfTPgfs7HL+A7g')]
    [TestCase('52',  'horse.webp,claRed,Tile,True,0,0.2,1,0.8,1,0.98,5fstAPYVnHC9Go8/P7r+lPMvewYRnE9H26onPrvmDqx//AciyIC5Bcernvr99IcPAf89RF7A6A')]
    [TestCase('53',  'horse.webp,claWhite,TileOriginal,True,0,0.2,1,0.8,0.85,0.98,tbO9wAQezMD/DuVPULr5fWtgPwMeVECD8AR8fv3gKtxPAAv909A9AsFXj7gFB/Tfwfs8IK8A7w')]
    [TestCase('54',  'horse.webp,claRed,TileOriginal,True,0,0.2,1,0.8,0.85,0.98,4gNegPcSnKD9GJlff3r9kgPAuwUUrM8G37dHjvvlELzv/Acc0BD6BMbDTnr/9ZdPgf4+aF5A6g')]
    [TestCase('55',  'horse.webp,claWhite,TileOriginal,True,0,0.2,1,0.8,1,0.98,sZudwAUk1ADADuhfEDr6fGtgfwMdYDAD8ARojv3gLNRvwAv9x+C9AsFbj3gFCfTPgfs7HL+A7g')]
    [TestCase('56',  'horse.webp,claRed,TileOriginal,True,0,0.2,1,0.8,1,0.98,5fstAPYVnHC9Go8/P7r+lPMvewYRnE9H26onPrvmDqx//AciyIC5Bcernvr99IcPAf89RF7A6A')]
    [TestCase('57',  'horse.webp,claWhite,TileStretch,True,0,0.2,1,0.8,0.85,0.98,wdc9gQAL2ABAEeAPoPv3f8twfgIzVGBD8hXoTn/hGMwfwQ7qGyG+98WfvzcH99vfggA2TC+A8Q')]
    [TestCase('58',  'horse.webp,claRed,TileStretch,True,0,0.2,1,0.8,0.85,0.98,4g8OgfYIlBC+GJtPb7v8jScQuwcSpC+G4LpTvvvkCpTvPAkUGLH5A8QXvzn98IPPwf8/eK7/6g')]
    [TestCase('59',  'horse.webp,claWhite,TileStretch,True,0,0.2,1,0.8,1,0.98,v8s9gQAO5ECAEeIPkPv3fttAvgI2ZFDD8Rnorn/hF8BvARDoH0E+9sWrrzcH9tfvQgE0UC+A8Q')]
    [TestCase('60',  'horse.webp,claRed,TileStretch,True,0,0.2,1,0.8,1,0.98,5Qf+wPQKlAA+GpEvX7v9jhtQuggPlI8G3a03bnvlB3x/vAkYHAG5BMUP/3n77m+vQQA+VI6/6Q')]
    [TestCase('61',  'horse.webp,claWhite,Tile,True,0,0.2,1,0.8,0,0.98,zyduQQD/o4B+D9IH0Lv2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('62',  'horse.webp,claRed,Tile,True,0,0.2,1,0.8,0,0.98,zyduQQD/o4B+D9IH0Lv2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('63',  'horse.webp,claWhite,TileOriginal,True,0,0.2,1,0.8,0,0.98,zyduQQD/o4B+D9IH0Lv2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('64',  'horse.webp,claRed,TileOriginal,True,0,0.2,1,0.8,0,0.98,zyduQQD/o4B+D9IH0Lv2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('65',  'horse.webp,claWhite,TileStretch,True,0,0.2,1,0.8,0,0.98,zyduQQD/o4B+D9IH0Lv2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('66',  'horse.webp,claRed,TileStretch,True,0,0.2,1,0.8,0,0.98,zyduQQD/o4B+D9IH0Lv2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('67',  'horse.webp,claWhite,Tile,True,0,0.2,1,0.2,1,0.98,zyduQQD/o4B+D9IH0Lv2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('68',  'horse.webp,claRed,Tile,True,0,0.2,1,0.2,1,0.98,zyduQQD/o4B+D9IH0Lv2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('69',  'horse.webp,claWhite,TileOriginal,True,0,0.2,1,0.2,1,0.98,zyduQQD/o4B+D9IH0Lv2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('70',  'horse.webp,claRed,TileOriginal,True,0,0.2,1,0.2,1,0.98,zyduQQD/o4B+D9IH0Lv2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('71',  'horse.webp,claWhite,TileStretch,True,0,0.2,1,0.2,1,0.98,zyduQQD/o4B+D9IH0Lv2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('72',  'horse.webp,claRed,TileStretch,True,0,0.2,1,0.2,1,0.98,zyduQQD/o4B+D9IH0Lv2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('73',  'horse.webp,claWhite,Tile,True,0.5,0.2,0.5,0.8,1,0.98,zyduQQD/o4B+D9IH0Lv2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('74',  'horse.webp,claRed,Tile,True,0.5,0.2,0.5,0.8,1,0.98,zyduQQD/o4B+D9IH0Lv2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('75',  'horse.webp,claWhite,TileOriginal,True,0.5,0.2,0.5,0.8,1,0.98,zyduQQD/o4B+D9IH0Lv2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('76',  'horse.webp,claRed,TileOriginal,True,0.5,0.2,0.5,0.8,1,0.98,zyduQQD/o4B+D9IH0Lv2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('77',  'horse.webp,claWhite,TileStretch,True,0.5,0.2,0.5,0.8,1,0.98,zyduQQD/o4B+D9IH0Lv2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('78',  'horse.webp,claRed,TileStretch,True,0.5,0.2,0.5,0.8,1,0.98,zyduQQD/o4B+D9IH0Lv2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('79',  'horse.webp,claWhite,Tile,True,0.5,0.5,0.5,0.5,1,0.98,zyduQQD/o4B+D9IH0Lv2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('80',  'horse.webp,claRed,Tile,True,0.5,0.5,0.5,0.5,1,0.98,zyduQQD/o4B+D9IH0Lv2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('81',  'horse.webp,claWhite,TileOriginal,True,0.5,0.5,0.5,0.5,1,0.98,zyduQQD/o4B+D9IH0Lv2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('82',  'horse.webp,claRed,TileOriginal,True,0.5,0.5,0.5,0.5,1,0.98,zyduQQD/o4B+D9IH0Lv2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('83',  'horse.webp,claWhite,TileStretch,True,0.5,0.5,0.5,0.5,1,0.98,zyduQQD/o4B+D9IH0Lv2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('84',  'horse.webp,claRed,TileStretch,True,0.5,0.5,0.5,0.5,1,0.98,zyduQQD/o4B+D9IH0Lv2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('85',  'horse.webp,claWhite,Tile,True,0,-0.2,1,1.2,1,0.98,xJ9tggUx0NCAB8DfsHv0hHNgv/8xmB/E9wMcbwDeNwjAwgn+n3C/A8VvYDgI37PgwfpI6L898A')]
    [TestCase('86',  'horse.webp,claWhite,TileOriginal,True,0,-0.2,1,1.2,1,0.98,xJ9tggUx0NCAB8DfsHv0hHNgv/8xmB/E9wMcD77hGuhvAAjvBzG//s1zL7gG+BtgwvtPeK9/9A')]
    [TestCase('87',  'horse.webp,claWhite,TileStretch,True,0,-0.2,1,1.2,1,0.98,zdfdAgX+53ACCaU7sPv2db9gwAUnGOCD8fwv377jHLS//w4blBB9/ci7r7YIIPj/AQIn6K5B6g')]
    [TestCase('88',  'horse.webp,claWhite,Tile,False,0,-0.2,1,1.2,1,0.98,xJ9tggUx0NCAB8DfsHv0hHNgv/8xmB/E9wMcbwDeNwjAwgn+n3C/A8VvYDgI37PgwfpI6L898A')]
    [TestCase('89',  'horse.webp,claWhite,TileOriginal,False,0,-0.2,1,1.2,1,0.98,xJ9tggUx0NCAB8DfsHv0hHNgv/8xmB/E9wMcD77hGuhvAAjvBzG//s1zL7gG+BtgwvtPeK9/9A')]
    [TestCase('90',  'horse.webp,claWhite,TileStretch,False,0,-0.2,1,1.2,1,0.98,zdfdAgX+53ACCaU7sPv2db9gwAUnGOCD8fwv377jHLS//w4blBB9/ci7r7YIIPj/AQIn6K5B6g')]
    [TestCase('91',  'horse.webp,claWhite,Tile,True,0,-0.2,1,1.2,0.85,0.98,xrM9wgQpyHCACMO/sPv0hG9g//8vrA9E9wMY/7/eMwhAggn+ryA/A8RDYPgH45fwAftHzB9+8A')]
    [TestCase('92',  'horse.webp,claWhite,TileOriginal,True,0,-0.2,1,1.2,0.85,0.98,xrM9wgQpyHCACMO/sPv0hG9g//8vrA9E9wMY/73hGuxPAAjxB/H+/strP/gG+BNgwvtNcL//8w')]
    [TestCase('93',  'horse.webp,claWhite,TileStretch,True,0,-0.2,1,1.2,0.85,0.98,zeOtQgT+3+ABCqwzsPv2eK8wAAUnGNAD8v0nr37jHMCvvw0WpCD9/cir77YIGvgPQgEr9G6B6w')]
    [TestCase('94',  'horse.webp,claWhite,Tile,False,0,-0.2,1,1.2,0.85,0.98,xrM9wgQpyHCACMO/sPv0hG9g//8vrA9E9wMY/7/eMwhAggn+ryA/A8RDYPgH45fwAftHzB9+8A')]
    [TestCase('95',  'horse.webp,claWhite,TileOriginal,False,0,-0.2,1,1.2,0.85,0.98,xrM9wgQpyHCACMO/sPv0hG9g//8vrA9E9wMY/73hGuxPAAjxB/H+/strP/gG+BNgwvtNcL//8w')]
    [TestCase('96',  'horse.webp,claWhite,TileStretch,False,0,-0.2,1,1.2,0.85,0.98,zeOtQgT+3+ABCqwzsPv2eK8wAAUnGNAD8v0nr37jHMCvvw0WpCD9/cir77YIGvgPQgEr9G6B6w')]
    [TestCase('97',  'horse.webp,$7F0000FF,Tile,True,0,-0.2,1,1.2,1,0.98,nveNgPvX/x+/EKfv77zqfkOgPvkA0P+D6/gbX3rfERgQPf3w40C5BMmfXngE71ffv/g+WH7A8w')]
    [TestCase('98',  'horse.webp,$7F0000FF,TileOriginal,True,0,-0.2,1,1.2,1,0.98,nveNgPvX/x+/EKfv77zqfkOgPvkA0P+D6/gbnzviGTCAPgcKzIC7AbH/bngHBLBfwgA7sL6A7Q')]
    [TestCase('99',  'horse.webp,$7F0000FF,TileStretch,True,0,-0.2,1,1.2,1,0.98,oO/NwPzZC0D/D5vnjzzrf2/gfvn927/E5/ULL7rgEhBA/fz371C5A86bjjgF6lPvP/tCdL7/9Q')]
    [TestCase('100', 'horse.webp,$7F0000FF,Tile,False,0,-0.2,1,1.2,1,0.98,nveNgPvX/x+/EKfv77zqfkOgPvkA0P+D6/gbX3rfERgQPf3w40C5BMmfXngE71ffv/g+WH7A8w')]
    [TestCase('101', 'horse.webp,$7F0000FF,TileOriginal,False,0,-0.2,1,1.2,1,0.98,nveNgPvX/x+/EKfv77zqfkOgPvkA0P+D6/gbnzviGTCAPgcKzIC7AbH/bngHBLBfwgA7sL6A7Q')]
    [TestCase('102', 'horse.webp,$7F0000FF,TileStretch,False,0,-0.2,1,1.2,1,0.98,oO/NwPzZC0D/D5vnjzzrf2/gfvn927/E5/ULL7rgEhBA/fz371C5A86bjjgF6lPvP/tCdL7/9Q')]
    [TestCase('103', 'horse.webp,$7F0000FF,Tile,True,0,-0.2,1,1.2,0.85,0.98,pf+tQPzdFwC/EK7zz3zsf0ewfvoF2P/D7PkT37rfExhwvf7y5/D5A8i7XvgE8W9PQPk+eG6A8w')]
    [TestCase('104', 'horse.webp,$7F0000FF,TileOriginal,True,0,-0.2,1,1.2,0.85,0.98,pf+tQPzdFwC/EK7zz3zsf0ewfvoF2P/D7PkT3/vhGSiwPgcH1NB7AbMLb3gHA7hfQgA8xK5A7g')]
    [TestCase('105', 'horse.webp,$7F0000FF,TileStretch,True,0,-0.2,1,1.2,0.85,0.98,p/fdQP3fHyD/D6Prf/zsgGvwvvoD4J+E6fcHn7rgExCgff747/A5A8y3jngF7WtPQPtClM5/9Q')]
    [TestCase('106', 'horse.webp,$7F0000FF,Tile,False,0,-0.2,1,1.2,0.85,0.98,pf+tQPzdFwC/EK7zz3zsf0ewfvoF2P/D7PkT37rfExhwvf7y5/D5A8i7XvgE8W9PQPk+eG6A8w')]
    [TestCase('107', 'horse.webp,$7F0000FF,TileOriginal,False,0,-0.2,1,1.2,0.85,0.98,pf+tQPzdFwC/EK7zz3zsf0ewfvoF2P/D7PkT3/vhGSiwPgcH1NB7AbMLb3gHA7hfQgA8xK5A7g')]
    [TestCase('108', 'horse.webp,$7F0000FF,TileStretch,False,0,-0.2,1,1.2,0.85,0.98,p/fdQP3fHyD/D6Prf/zsgGvwvvoD4J+E6fcHn7rgExCgff747/A5A8y3jngF7WtPQPtClM5/9Q')]
    procedure TestFillEllipseBitmapWithChessBackground(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode; ABlending: Boolean; const ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  '3d-shapes.svg,claWhite,Tile,true,1,0.98,yd+g+wQYhD9B8rTrrwTuSyi/Q+DvA0E+3t0zQf0c49NfOt//X7889RU0UIAKvC8POQQ4CL9+Bg')]
    [TestCase('2',  '3d-shapes.svg,claWhite,Tile,true,0.5,0.98,z5sxu/7s707B86q3X8XxcOjuAOjCr9A9495bUT0hx6svO+IKWG+79jvE34T6zhc/+QQHxG4+Dw')]
    [TestCase('3',  '3d-shapes.svg,claWhite,Tile,false,1,0.98,yd+g+wQYhD9B8rTrrwTuSyi/Q+DvA0E+3t0zQf0c49NfOt//X7889RU0UIAKvC8POQQ4CL9+Bg')]
    [TestCase('4',  '3d-shapes.svg,claWhite,Tile,false,0.5,0.98,z5sxu/7s707B86q3X8XxcOjuAOjCr9A9495bUT0hx6svO+IKWG+79jvE34T6zhc/+QQHxG4+Dw')]
    [TestCase('5',  '3d-shapes.svg,claRed,Tile,true,1,0.98,0oNgfPQWQH/C+4FXv4XySmw+A+jUoyBA0tcTYbwh9a8PuOX8h6958BIcX4P8qgs/NgZFBB6//w')]
    [TestCase('6',  '3d-shapes.svg,claRed,Tile,true,0.5,0.98,02+RO/brz+6B+JBv7wX0cIyuAOy1f8A+3dtL0bwj0JsPuuUJbN859Dk4X8bzxQfP9wUNRI7+Cw')]
    [TestCase('7',  '3d-shapes.svg,claRed,Tile,false,1,0.98,0oNgfPQWQH/C+4FXv4XySmw+A+jUoyBA0tcTYbwh9a8PuOX8h6958BIcX4P8qgs/NgZFBB6//w')]
    [TestCase('8',  '3d-shapes.svg,claRed,Tile,false,0.5,0.98,02+RO/brz+6B+JBv7wX0cIyuAOy1f8A+3dtL0bwj0JsPuuUJbN859Dk4X8bzxQfP9wUNRI7+Cw')]
    [TestCase('9',  '3d-shapes.svg,claWhite,TileOriginal,true,1,0.98,yd+g+wQYhD9B8rTrrwTuSyi/Q+DvA0E+3t0zIX0c6bc/et77U//89Bv8D8EIvfsOeQI1CF/+BQ')]
    [TestCase('10', '3d-shapes.svg,claWhite,TileOriginal,true,0.5,0.98,z5sxu/7s707B86q3X8XxcOjuAOjCr9A9495bQf0gyp8f++EIVI+79j2oP8X5z/8++QMFxD7+Dg')]
    [TestCase('11', '3d-shapes.svg,claWhite,TileOriginal,false,1,0.98,yd+g+wQYhD9B8rTrrwTuSyi/Q+DvA0E+3t0zIX0c6bc/et77U//89Bv8D8EIvfsOeQI1CF/+BQ')]
    [TestCase('12', '3d-shapes.svg,claWhite,TileOriginal,false,0.5,0.98,z5sxu/7s707B86q3X8XxcOjuAOjCr9A9495bQf0gyp8f++EIVI+79j2oP8X5z/8++QMFxD7+Dg')]
    [TestCase('13', '3d-shapes.svg,claRed,TileOriginal,true,1,0.98,0oNgfPQWQH/C+4FXv4XySmw+A+jUoyBA0tcTUXwh95vP9+T7d/858Bfo/kP7sc9utgVEEC4/Ag')]
    [TestCase('14', '3d-shapes.svg,claRed,TileOriginal,true,0.5,0.98,02+RO/brz+6B+JBv7wX0cIyuAOy1f8A+3dtL0Xwj0Y/vOeUIZA869DscrwbzyOfutwUNSJ4+DQ')]
    [TestCase('15', '3d-shapes.svg,claRed,TileOriginal,false,1,0.98,0oNgfPQWQH/C+4FXv4XySmw+A+jUoyBA0tcTUXwh95vP9+T7d/858Bfo/kP7sc9utgVEEC4/Ag')]
    [TestCase('16', '3d-shapes.svg,claRed,TileOriginal,false,0.5,0.98,02+RO/brz+6B+JBv7wX0cIyuAOy1f8A+3dtL0Xwj0Y/vOeUIZA869DscrwbzyOfutwUNSJ4+DQ')]
    [TestCase('17', '3d-shapes.svg,claWhite,TileStretch,true,1,0.98,/8dwgf7aY98D/M7vj4TyNXiPRenGOzG/4QSUgT4U7zsw+ujW/19/5ibw34EGcRPguwggJL+BAQ')]
    [TestCase('18', '3d-shapes.svg,claWhite,TileStretch,true,0.5,0.98,6o8RfvvN356C+Le3TwX0ZRDPgeyuy0D+5PKL8f0czd8fO+f2q68870Okn4X4qIuvOgf70+7/DA')]
    [TestCase('19', '3d-shapes.svg,claWhite,TileStretch,false,1,0.98,/8dwgf7aY98D/M7vj4TyNXiPRenGOzG/4QSUgT4U7zsw+ujW/19/5ibw34EGcRPguwggJL+BAQ')]
    [TestCase('20', '3d-shapes.svg,claWhite,TileStretch,false,0.5,0.98,6o8RfvvN356C+Le3TwX0ZRDPgeyuy0D+5PKL8f0czd8fO+f2q68870Okn4X4qIuvOgf70+7/DA')]
    [TestCase('21', '3d-shapes.svg,claRed,TileStretch,true,1,0.98,7/9gv/TQI0/DAYlf30TvStAOxefOpwCA4eUj4X0X8Bcgd+/hY3C75hfor0T6UbevOQ9D/J2//g')]
    [TestCase('22', '3d-shapes.svg,claRed,TileStretch,true,0.5,0.98,4qsBffbIv16C+5Vzb0XycLyOweuyf6D+5OJTkX0ezc+feer738967zycD4fymV9/eQoMPM5+Cw')]
    [TestCase('23', '3d-shapes.svg,claRed,TileStretch,false,1,0.98,7/9gv/TQI0/DAYlf30TvStAOxefOpwCA4eUj4X0X8Bcgd+/hY3C75hfor0T6UbevOQ9D/J2//g')]
    [TestCase('24', '3d-shapes.svg,claRed,TileStretch,false,0.5,0.98,4qsBffbIv16C+5Vzb0XycLyOweuyf6D+5OJTkX0ezc+feer738967zycD4fymV9/eQoMPM5+Cw')]
    procedure TestFillEllipseBitmapWithClipping(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode; ABlending: Boolean; const AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  '3d-shapes.svg,300,300,Tile,1,2,-50,-100,30,-200,-50,660,343,1,true,0.98,4lvR/eTym7+A/EH0L8H5Dzzvu/WnK7DGz7tfr7wDx08gRQqziz9CDiNkv8Hx6jvgPBASqB0+Eg')]
    [TestCase('2',  '3d-shapes.svg,300,300,Tile,1,2,-50,-100,30,-200,-50,660,343,0.6,true,0.98,3b+RvOzeG//A+QDEHwP4RQDPfPOgP/CC2co74DwRvPuPgfvad+9+BTtczwTv5r9/+wv6Aw6+FA')]
    [TestCase('3',  '3d-shapes.svg,300,300,Tile,1,2,-50,-100,30,0,0,200,200,1,true,0.98,qxcBvuruNw+E9afzb0T1lawOPvCVV1A96OCDYf0kq38ffOUVXP85+GFUX8nq4ANfeQXVgw4+GA')]
    [TestCase('4',  '3d-shapes.svg,300,300,Tile,1,2,-50,-100,30,0,0,200,200,0.6,true,0.98,vJexPPDc3/6C9aTHD0X1lawOPvCVV1A96OCDYT0lq38ffOUVWA86+GFUX8nq4ANfeQXVgw4+GA')]
    [TestCase('5',  '3d-shapes.svg,300,300,Tile,1,-2,50,500,30,-200,-50,660,343,1,true,0.98,ryNReBzC0+9/46M30Ifj9PufxO3mT6B58TZYAQD+B+xeQxMCoKCBA+a7bsL6MSCPfAB7k/+9+A')]
    [TestCase('6',  '3d-shapes.svg,300,300,Tile,-1,2,-50,-100,30,-200,-50,660,343,0.6,true,0.98,3mNievrOT97A9KJ7D0X1lawOPvCVV1A96OCDsb0kp18PvOUZoN+59VlUX8nq4ANfeQXVgw4+GA')]
    [TestCase('7',  '3d-shapes.svg,300,300,Tile,-1,2,250,-50,30,0,0,200,200,1,true,0.98,yP8fvv2z094AEjez70H1lawOPvCVV1A96OCDcf8I+oe/+d813OC25wBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('8',  '3d-shapes.svg,300,300,Tile,1,-2,-50,200,30,0,0,200,200,0.6,true,0.98,04/i+fe+b55B9pvb74QAXChg/PuW8/A86OBbQT0mrmt/POIRaL+7+VdYf8fp3gN/uQMJRE58EA')]
    [TestCase('9',  '3d-shapes.svg,300,300,TileOriginal,1,2,150,20,-90,-200,-50,660,343,1,true,0.98,0l/yuvfEP05B965/TwYGU6CwAPp6EwEE7rhLX30lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('10', '3d-shapes.svg,300,300,TileOriginal,1,2,-50,100,-90,-200,50,660,343,0.6,true,0.98,1k/SuvXXa+5/9cVX/0TsA6zfvADmPwHC5BgMUb0lq4MPfOUVWA86+GBUL0ju1ZfP+APkk14/GA')]
    [TestCase('11', '3d-shapes.svg,300,300,TileOriginal,1,2,50,100,20,0,0,200,200,1,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDgT0ogmefe+zxO1+5+FHIX4bv4M+/OwrLv05+FQ')]
    [TestCase('12', '3d-shapes.svg,300,300,TileOriginal,1,2,50,100,20,0,0,200,200,0.6,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDcT0nknPPu+kARJ95+Fecn4ft4HvPegjPpz6+Fg')]
    [TestCase('13', '3d-shapes.svg,300,300,TileOriginal,1,-2,50,500,30,-200,-50,660,343,1,true,0.98,ryNReBzC0+9/46M30Ifj9PufxO3mT6B58TZYAQD+B+xeQxMCoKCBA+a7bsL6MSCPfAB7k/+9+A')]
    [TestCase('14', '3d-shapes.svg,300,300,TileOriginal,-1,2,-50,-100,60,-200,-50,660,343,0.6,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('15', '3d-shapes.svg,300,300,TileOriginal,-1,2,-50,-100,-60,0,0,200,200,1,true,0.98,aj8APPv6qyCD7pDDcEXh/cMP/P714+G/2xfwQIAgr9vuQuvGM9+++FXoH4P4xlc+eQMsFN75EA')]
    [TestCase('16', '3d-shapes.svg,300,300,TileOriginal,1,-2,-50,300,10,0,0,200,200,0.6,true,0.98,/+9BPfjQ6058+eLXDkX2dZBOveuWa1AA5ccfgT8lpUd//+rk42//+zZQf8fm1qvOuQn4X168Ew')]
    [TestCase('17', '3d-shapes.svg,300,300,TileStretch,1,2,-50,-100,30,-200,-50,660,343,1,true,0.98,+RdQRvMahM6G/YkPT4PrNuw+QgPnP496DR800b37AiRvwOw5+MBBBQUQ/4P4YICvwe8QxGBBCQ')]
    [TestCase('18', '3d-shapes.svg,300,300,TileStretch,1,2,-50,-100,30,-200,-50,660,343,0.6,true,0.98,6v+gQfX2d55E+pI/X4TvXNSOgPvGr697/gZUoX0M30uv/ukrUKA+ACksHwbzLUxffvj43w9ADw')]
    [TestCase('19', '3d-shapes.svg,300,300,TileStretch,1,2,-50,-100,30,0,0,200,200,1,true,0.98,1mNBe/PBu04ABm9rH0D1lawOPvCVV1A96OCDEX4gojffO+UcPIC58EpUX8nq4ANfeQXVgw4+GA')]
    [TestCase('20', '3d-shapes.svg,300,300,TileStretch,1,2,-50,-100,30,0,0,200,200,0.6,true,0.98,1scBO/XBk75A/4Jzf0L1lawOPvCVV1A96OCDwX0ipVPve+UZ4L+581NUX8nq4ANfeQXVgw4+GA')]
    [TestCase('21', '3d-shapes.svg,300,300,TileStretch,1,-2,50,500,30,-200,-50,660,343,1,true,0.98,vysB+RLq+z9+7dL3z8DwIcQOdg7jEzBB/gIsL4ARIdyP//ArZIB/8o0rgT0WRH+wPgJCtDE8/g')]
    [TestCase('22', '3d-shapes.svg,300,300,TileStretch,-1,2,-50,-100,-60,-200,-50,660,343,0.6,true,0.98,5M/Bu/zch16C9btLUIUBTjjPA/Pdq2+8+B5wAP3/+TfQgADYQ3B+9XHI/kD9C6ROfQfWc47ADw')]
    [TestCase('23', '3d-shapes.svg,300,300,TileStretch,-1,2,-50,-100,-60,0,0,200,200,1,true,0.98,E+RP/fsZWAC7+CgUcAPs7gevffXIe0HA4+YfYf4U/z/PvOf/M7D7+2OMb4Xv8/O+uQULjI18CQ')]
    [TestCase('24', '3d-shapes.svg,300,300,TileStretch,1,-2,-50,500,30,0,0,200,200,0.6,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDIb0dl6ffPOsB0I56+2Sw3oPw9i/P/A/mW76AEg')]
    procedure TestFillEllipseBitmapWithMatrix(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer; AWrapMode: TWrapMode; ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg, ADestLeft, ADestTop, ADestRight, ADestBottom, AOpacity: Single; ABlending: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',   '[claBlack;0 claWhite;1],claWhite,Linear,False,1,1,0.98,ixexPRC48/++6vA/IIbsyndvwfbHqxHB2v3bAEMaDKDvPfHCT7+C/Oi7j34UqHuu/BNkzA39/g')]
    [TestCase('2',   '[$7F000000;0 $7FFFFFFF;1],claWhite,Linear,False,1,1,0.98,sLcxPAS8Iy8A8MjfHwbxMBC/f/OuAzF/4e8vMcAf25P/POvsU19++iSE/4P/xL8OuwwcKI69Cw')]
    [TestCase('3',   '[claBlack;0 claWhite;1],claWhite,Linear,False,1,0.5,0.98,sLcxPAS8Jy8A8MjfHwbxLxC/v/OuAzF/4e8vMcAf25P/fOvrU29++iSI74P/xL8OuwwdKI59Cw')]
    [TestCase('4',   '[claBlack;0 claWhite;1],claWhite,Linear,True,1,1,0.98,ixexPRC48/++6vA/IIbsyndvwfbHqxHB2v3bAEMaDKDvPfHCT7+C/Oi7j34UqHuu/BNkzA39/g')]
    [TestCase('5',   '[$7F000000;0 $7FFFFFFF;1],claWhite,Linear,True,1,1,0.98,sLcxPAS8Iy8A8MjfHwbxMBC/f/OuAzF/4e8vMcAf25P/POvsU19++iSE/4P/xL8OuwwcKI69Cw')]
    [TestCase('6',   '[claBlack;0 claWhite;1],claWhite,Linear,True,1,0.5,0.98,sLcxPAS8Jy8A8MjfHwbxLxC/v/OuAzF/4e8vMcAf25P/fOvrU29++iSI74P/xL8OuwwdKI59Cw')]
    [TestCase('7',   '[claBlack;0 claWhite;1],claWhite,Linear,False,2,1,0.98,2FfiuPm/pw7B+pCX30TpQvC+Qvrvl9A67cnnUbwgldfwOwnAF/+CCy+kvjkCAXgfgQA2fH6A8A')]
    [TestCase('8',   '[$7F000000;0 $7FFFFFFF;1],claWhite,Linear,False,2,1,0.98,11fC+fjAfz4B+JiLb0XvbMxuAPXCdxC86tW30fwioC/wO/frN3/+AUj8jkH28D8/PQMFfE5/BA')]
    [TestCase('9',   '[claBlack;0 claWhite;1],claWhite,Linear,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('10',  '[claBlack;0 claWhite;1],claWhite,Linear,True,0.5,1,0.98,riPhgApzK67EAck7EEH1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('11',  '[$7F000000;0 $7FFFFFFF;1],claWhite,Linear,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('12',  '[claBlack;0 claWhite;1],claWhite,Linear,True,3,0.5,0.98,1VeyOvjAV15B9aCDD0b2lniOfvGjWyD+5uBzYb0knX/f++YPbG+5+VMcXwX06Tt/PQXnE88/Eg')]
    [TestCase('13',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,False,1,1,0.98,gSvxPRCr5x//6uU74Ebtz1MPAfW7i5EB2e77kAQZEJAfv/K6S78D++LnXz4ZpIOOfRVe/G09/w')]
    [TestCase('14',  '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Linear,False,1,1,0.98,rMNRPAS1Hz8A8MLff0bxM/yOv/Ko83C/4Oc/8UAf3YuPPezoU9+++SGc78MBwsNuew0ZQL69Cw')]
    [TestCase('15',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,False,1,0.5,0.98,q8NRPAS1Iz8A8MPff0bxMvyOv/Ko83C/4Oc/8QAf3YuPPeznU++++SGc3wMCwsN+ew0aPL69Cw')]
    [TestCase('16',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,True,1,1,0.98,gSvxPRCr5x//6uU74Ebtz1MPAfW7i5EB2e77kAQZEJAfv/K6S78D++LnXz4ZpIOOfRVe/G09/w')]
    [TestCase('17',  '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Linear,True,1,1,0.98,rMNRPAS1Hz8A8MLff0bxM/yOv/Ko83C/4Oc/8UAf3YuPPezoU9+++SGc78MBwsNuew0ZQL69Cw')]
    [TestCase('18',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,True,1,0.5,0.98,q8NRPAS1Iz8A8MPff0bxMvyOv/Ko83C/4Oc/8QAf3YuPPeznU++++SGc3wMCwsN+ew0aPL69Cw')]
    [TestCase('19',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,False,2,1,0.98,2EvyePnAoy7B+46Xv0TqQ/Duwfrkf7B767r7IXshk8fg+gjO/44BDTk0Tnr+ASBvwAIw8M3A8A')]
    [TestCase('20',  '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Linear,False,2,1,0.98,11PS+fjAez6B+JePb8XvbMz+v/W8a4C86c2/QXwjnyNwO/fyK8+9AkzE3oH08RPfPAQCOG6/BA')]
    [TestCase('21',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('22',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,True,0.5,1,0.98,mPsgQA1zHx7FAsZfkED1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('23',  '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Linear,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('24',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,True,3,0.5,0.98,1VeyOvjAV15B9aCDD0b2lXh+fvGiWzC+5uBvUX0knYPfu+YQcF/5+VP8nkXy5A/vfAXuyz7/Ew')]
    [TestCase('25',  '[claBlack;-1 claWhite;0.8],claWhite,Linear,True,1,1,0.98,stvQ/BTfX5A+5htc4MPpvt//QvgAKDL/4TN0YEYVJqC/gPK0T0/F+ukngP0cosPOfRVmRE69Ag')]
    [TestCase('26',  '[claBlack;0 claWhite;2],claWhite,Linear,True,1,1,0.98,iyex/Qy3x0++7ukLoMbu01cvQPazaxGC2N77sL0f979feu/eT1//AP1f338FrjO+ORBkAF37/Q')]
    [TestCase('27',  '[claBlack;0 claWhite;1],claRed,Linear,False,1,1,0.98,iSMR/Qi6u5998dPbHwby2fu+PvmsLyEC08GD4H0i76tPuO4FtL/8BACwj0H+llseuhFxIMx48g')]
    [TestCase('28',  '[$7F000000;0 $7FFFFFFF;1],claRed,Linear,False,1,1,0.98,r7/hewC9B39/87mvD8bzONBevvShw8C/3dADof0jzZcv+ukNhF97/jCAf4X0u6++uQsjUG17BQ')]
    [TestCase('29',  '[claBlack;0 claWhite;1],claRed,Linear,False,1,0.5,0.98,r7/huwC9C39/87qvD8bzN9BevvShw8C/3dADof0jzZcvOuoNhF97/jCEb4X0u6++uQsjUG07BQ')]
    [TestCase('30',  '[claBlack;0 claWhite;1],claRed,Linear,True,1,1,0.98,iSMR/Qi6u5998dPbHwby2fu+PvmsLyEC08GD4H0i76tPuO4FtL/8BACwj0H+llseuhFxIMx48g')]
    [TestCase('31',  '[$7F000000;0 $7FFFFFFF;1],claRed,Linear,True,1,1,0.98,r7/hewC9B39/87mvD8bzONBevvShw8C/3dADof0jzZcv+ukNhF97/jCAf4X0u6++uQsjUG17BQ')]
    [TestCase('32',  '[claBlack;0 claWhite;1],claRed,Linear,True,1,0.5,0.98,r7/huwC9C39/87qvD8bzN9BevvShw8C/3dADof0jzZcvOuoNhF97/jCEb4X0u6++uQsjUG07BQ')]
    [TestCase('33',  '[claBlack;0 claWhite;1],claRed,Linear,False,2,1,0.98,1FMiufnBp/5A+46XzwTqRdzugfvnc1B76rrP4fkhlVugegjLcw+ADEZobvv40FsPfwBZBI097A')]
    [TestCase('34',  '[$7F000000;0 $7FFFFFFF;1],claRed,Linear,False,2,1,0.98,1Vfi+fjAfy5B+JeLb8XvbcT+//W+Z1B86c2robsjoO9f+/bwZw99AlPgbsLx2C8//AIXxM09Ag')]
    [TestCase('35',  '[claBlack;0 claWhite;1],claRed,Linear,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('36',  '[claBlack;0 claWhite;1],claRed,Linear,True,0.5,1,0.98,zXehvv1pM+6DDoNjr0H1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('37',  '[$7F000000;0 $7FFFFFFF;1],claRed,Linear,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('38',  '[claBlack;0 claWhite;1],claRed,Linear,True,3,0.5,0.98,1VeyOvjAV15B9aCDDwb2lXyOfvGjVyD+5uBvIb0kn4Pvu+YQdE+5+VQYr4Xx2zsPPQXyt97+EA')]
    [TestCase('39',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,False,1,1,0.98,gjdBvQm1p4+979vnf8bx2vvOfvemHxGC1cS7kP4g8Ze/eO8IyJ88BP/Dj0H/kWd+ehNxEHx48A')]
    [TestCase('40',  '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Linear,False,1,1,0.98,rMfx+wC6/35/8r2zT4bzONBuvvOeu7D/3tIf8T0jzotfeuoPkF87/jCIfwX1ubfueQwjSE17BA')]
    [TestCase('41',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,False,1,0.5,0.98,rMfx+wC6/35/8r2zT4bzN9RuvvOeu7D/3tIf8T0jzo9feuoPkF87/i+MbwX1ubPueQwjSE07BA')]
    [TestCase('42',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,True,1,1,0.98,gjdBvQm1p4+979vnf8bx2vvOfvemHxGC1cS7kP4g8Ze/eO8IyJ88BP/Dj0H/kWd+ehNxEHx48A')]
    [TestCase('43',  '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Linear,True,1,1,0.98,rMfx+wC6/35/8r2zT4bzONBuvvOeu7D/3tIf8T0jzotfeuoPkF87/jCIfwX1ubfueQwjSE17BA')]
    [TestCase('44',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,True,1,0.5,0.98,rMfx+wC6/35/8r2zT4bzN9RuvvOeu7D/3tIf8T0jzo9feuoPkF87/i+MbwX1ubPueQwjSE07BA')]
    [TestCase('45',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,False,2,1,0.98,2EvyePnAoy7B+46Xv0TqQ/Duwfrkf7B767r7Ybkhk2Mw+gfLY+//C0kYHvv32T9/vgBT7Jx97Q')]
    [TestCase('46',  '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Linear,False,2,1,0.98,11PS+fjAez6B+JePb8XvbMz+v/W8a4C86c2/Ybsjn/Mfu/bwX//8AVW4TkLx3SPv+wIUuN39Ag')]
    [TestCase('47',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('48',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,True,0.5,1,0.98,yGsh/v1qP86DD3xvH0H1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('49',  '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Linear,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('50',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,True,3,0.5,0.98,1VeyOvjAV15B9aCDD0b2lXh+fvGiWzC+5uBvQT0knoPfu+YQdF/5+VQAr8Xx3xe/vATxv94+Ew')]
    [TestCase('51',  '[claBlack;-1 claWhite;0.8],claRed,Linear,True,1,1,0.98,ldtQPAXMA9C99rzLvwT03L8+Pv3DY0FCzLizDwAi/Zcvue4FrI/9Bf8TsMEAi1/eOhJySFy48Q')]
    [TestCase('52',  '[claBlack;0 claWhite;2],claRed,Linear,True,1,1,0.98,iithfQm4q5898trXr4bx2xvPvvemK5HC1MDPMLsj6cOPN+7/g2/8BAlYX0H6pCN+OA9rKDy59w')]
    [TestCase('53',  '[claRed;0 claBlue;1],claWhite,Linear,False,1,1,0.98,l59QfQTc+899/MgfcIXz0yuPvfzBf5FDzsPb7/kf6SNw9+jrU66+DQN0z74Dol/NdwxkODy6+w')]
    [TestCase('54',  '[$7FFF0000;0 $7F0000FF;1],claWhite,Linear,False,1,1,0.98,tnsBPP7OK5//+LTPz4X0NejOffar63AA29GvoLsiytPPOecA1F78AjJkHwT3wTOe+AgcXC38CQ')]
    [TestCase('55',  '[claRed;0 claBlue;1],claWhite,Linear,False,1,0.5,0.98,tnsBfP7OK5//+LTTz4X0NOjOffar63AA29GvoLsiytO/OecA1F78AjFkH0T3wS+e+AgdXC38CQ')]
    [TestCase('56',  '[claRed;0 claBlue;1],claWhite,Linear,True,1,1,0.98,l59QfQTc+899/MgfcIXz0yuPvfzBf5FDzsPb7/kf6SNw9+jrU66+DQN0z74Dol/NdwxkODy6+w')]
    [TestCase('57',  '[$7FFF0000;0 $7F0000FF;1],claWhite,Linear,True,1,1,0.98,tnsBPP7OK5//+LTPz4X0NejOffar63AA29GvoLsiytPPOecA1F78AjJkHwT3wTOe+AgcXC38CQ')]
    [TestCase('58',  '[claRed;0 claBlue;1],claWhite,Linear,True,1,0.5,0.98,tnsBfP7OK5//+LTTz4X0NOjOffar63AA29GvoLsiytO/OecA1F78AjFkH0T3wS+e+AgdXC38CQ')]
    [TestCase('59',  '[claRed;0 claBlue;1],claWhite,Linear,False,2,1,0.98,xneyefrIu24A+oqPL8XuPHReQQQERBA847Kz4LojpG+QfAbEm7+BFT8kv/oCt6sPQP1hsK2/5w')]
    [TestCase('60',  '[$7FFF0000;0 $7F0000FF;1],claWhite,Linear,False,2,1,0.98,zmcyevnEi+6A95WHnwXyaZCuf/rNT7C85ckbIXwkp/df/PXte9/9BlA8H8L2zFe/fAEbGN4+AA')]
    [TestCase('61',  '[claRed;0 claBlue;1],claWhite,Linear,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('62',  '[claRed;0 claBlue;1],claWhite,Linear,True,0.5,1,0.98,1ydxgvpQi81CCbarT0T1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('63',  '[$7FFF0000;0 $7F0000FF;1],claWhite,Linear,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('64',  '[claRed;0 claBlue;1],claWhite,Linear,True,3,0.5,0.98,1VeyOvjAV15B9aCDDwb0mZiuPvKmO+D95uBz8XwloYMPfOcObD95+VeEr4XzzcPvPQbv466/Cg')]
    [TestCase('65',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,False,1,1,0.98,o5dAPQTjKxC+/7nz/4Tz2RPvff/Kn6HDyrqXD7kg4kfwdufmE84+EANsf74Doz+d9wlkNMx6/Q')]
    [TestCase('66',  '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Linear,False,1,1,0.98,vHvxO/7SP79/+qy7j0X0N9z+vfew+4CA2c2PMDsjxuOPeeb+t248BDJg7wP3wSN+uAcdXG38Cg')]
    [TestCase('67',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,False,1,0.5,0.98,vHfxO/7SQ7+/+q27j0X0N9z+vfew+4CA2c2LIDsjx+N/eeb+t248BDFg7wP3wR9+uAcdWG28Cg')]
    [TestCase('68',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,True,1,1,0.98,o5dAPQTjKxC+/7nz/4Tz2RPvff/Kn6HDyrqXD7kg4kfwdufmE84+EANsf74Doz+d9wlkNMx6/Q')]
    [TestCase('69',  '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Linear,True,1,1,0.98,vHvxO/7SP79/+qy7j0X0N9z+vfew+4CA2c2PMDsjxuOPeeb+t248BDJg7wP3wSN+uAcdXG38Cg')]
    [TestCase('70',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,True,1,0.5,0.98,vHfxO/7SQ7+/+q27j0X0N9z+vfew+4CA2c2LIDsjx+N/eeb+t248BDFg7wP3wR9+uAcdWG28Cg')]
    [TestCase('71',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,False,2,1,0.98,w3viefrIwz6A+YiHL4XvQ0h+gQQIPLC74Kl3cPsjqE/gfAfGu2+BFUGEj/sBpsvPgP1s1F0/6A')]
    [TestCase('72',  '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Linear,False,2,1,0.98,zGtCevnEj86A95SDn4XybHy+f/rPS4B85MX/YLwkqud/fPbui7/9BlFsf0L2w2cfvQEgLK4+AA')]
    [TestCase('73',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('74',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,True,0.5,1,0.98,2ScxA/lSW73CCcafX0X1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('75',  '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Linear,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('76',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,True,3,0.5,0.98,1VeyOvjAV15B9aCDD8bzmZiuPvKmN9D95uBv0bwloYcfvOcNbB85+Vio/8XyyeuPPgf1vy5/CA')]
    [TestCase('77',  '[claRed;-1 claBlue;0.8],claWhite,Linear,True,1,1,0.98,fs/w/QnPZ7+88/a78EbytdN/PPanD9GD1OLHYDcd3XewNujxG949Dvz7zr0EtDs9dwxkzMs6+w')]
    [TestCase('78',  '[claRed;0 claBlue;2],claWhite,Linear,True,1,1,0.98,m6OQfAPbG+C9+7rvr0T02d++Pf7JhwHDyrp/X30h9ttvuOv3+15+CgHYX4ACld9t+Q5sUEz59g')]
    [TestCase('79',  '[claRed;0 $7F000000;0.5 claBlue;1],claWhite,Linear,True,1,1,0.98,u2OhvfzEax9//MyrL8X0JOS+vfCgWyED1LxLQDkmyMtvN+fsp379AyokXwL3qd8N9ww4HE17Bg')]
    [TestCase('80',  '[claRed;0 $7F000000;1 claBlue;0.5],claWhite,Linear,True,1,1,0.98,j5vQ/gXdu4+9/eN/EMfxxrsv/fivb6HE1NWLoPgb4VOwOOwUBO86BQ3I/sD84ffNdwpJ/Au7/g')]
    [TestCase('81',  '[claRed;0 $7F000000;-1 claBlue;0.5],claWhite,Linear,True,1,1,0.98,j5fQPgbev4+9/eODIMfxxr8v/fivb6HE1NWLkLYb24+QNuj2G249DfnDfj0FvTv9dg1jmMs6+g')]
    [TestCase('82',  '[claRed;1 $7F000000;0.5 claBlue;0],claWhite,Linear,True,1,1,0.98,oL/BfASvy8597fMrcMX0A0CPvOuQt1AC1djrAPwkzssPeOoMiI87/hsor4P2rJ9OuRFFmEx6/A')]
    [TestCase('83',  '[claRed;1 $7F000000;0 claBlue;0.5],claWhite,Linear,True,1,1,0.98,hROxPQvRVx++9Ndz4Ajw4btPfvqn14BB4ee3IToa5nOQ9+wZLH+7BevXHn8Evg9++RNxJGs57w')]
    [TestCase('84',  '[claRed;1 $7F000000;0 claBlue;0.5 claWhite;0.49],claWhite,Linear,True,1,1,0.98,jTPhexXR758A5t1zIAbn4Gufw/7mrwG+4ygUgboZ7V9weOkWKJ87CfAD7z4GuT++uBJxPNu58Q')]
    [TestCase('85',  '[claRed;1 $7F000000;0 claBlue;0.5 claWhite;0.49],claWhite,Linear,False,1,1,0.98,jTPhexXR758A5t1zIAbn4Gufw/7mrwG+4ygUgboZ7V9weOkWKJ87CfAD7z4GuT++uBJxPNu58Q')]
    [TestCase('86',  '[claRed;1 $7F000000;0 claBlue;0.5 claWhite;0.49],claWhite,Linear,True,2,0.5,0.98,118S+vjAdz7B9puHr4Xxdrze//O3b2C86d+j4T0inXvQvPXrD58+AD1ADwL3BYCvPgTqG19ABQ')]
    [TestCase('87',  '[],claWhite,Linear,True,1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('88',  '[claBlack;0 claWhite;1],claWhite,Radial,False,1,1,0.98,pV8hPQ+4oz896v3bj4XqzYtPQfTie6HB4ArAEP4e/9vfeu7Ikw8C/w98/0D9p/+N+A580Gy4AQ')]
    [TestCase('89',  '[$7F000000;0 $7FFFFFFF;1],claWhite,Radial,False,1,1,0.98,vdvhuwO8+07/786vz8XvMRivf/K853B/5PUjsT0i1a9v++nvdw+++zhoLwX0w4P+OAopqD37DA')]
    [TestCase('90',  '[claBlack;0 claWhite;1],claWhite,Radial,False,1,0.5,0.98,vdvhuwO8/07/78+vz8XvMRivf/K864B/5PUjsT0i1a9v++nvdw+++zdoLwX0w4P+OAopqD37DA')]
    [TestCase('91',  '[claBlack;0 claWhite;1],claWhite,Radial,True,1,1,0.98,pV8hPQ+4oz896v3bj4XqzYtPQfTie6HB4ArAEP4e/9vfeu7Ikw8C/w98/0D9p/+N+A580Gy4AQ')]
    [TestCase('92',  '[$7F000000;0 $7FFFFFFF;1],claWhite,Radial,True,1,1,0.98,vdvhuwO8+07/786vz8XvMRivf/K853B/5PUjsT0i1a9v++nvdw+++zhoLwX0w4P+OAopqD37DA')]
    [TestCase('93',  '[claBlack;0 claWhite;1],claWhite,Radial,True,1,0.5,0.98,vdvhuwO8/07/78+vz8XvMRivf/K864B/5PUjsT0i1a9v++nvdw+++zdoLwX0w4P+OAopqD37DA')]
    [TestCase('94',  '[claBlack;0 claWhite;1],claWhite,Radial,False,2,1,0.98,2FPiuPm/ox5B+4+XvwTqRPD+gvnup1A67crjcfskk1uwegi4V19CB05YX7kECgSgAf0gMF8A7A')]
    [TestCase('95',  '[$7F000000;0 $7FFFFFFF;1],claWhite,Radial,False,2,1,0.98,11PS+fjAfz5B+JeLb8Xvbcx+wPTBf9C76tWzYTwln+9f+/bnVy++/1dYb4H39YN/fQH72z4/Ag')]
    [TestCase('96',  '[claBlack;0 claWhite;1],claWhite,Radial,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('97',  '[claBlack;0 claWhite;1],claWhite,Radial,True,0.5,1,0.98,1TcxAgNy+w0EALrPz0P1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('98',  '[$7F000000;0 $7FFFFFFF;1],claWhite,Radial,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('99',  '[claBlack;0 claWhite;1],claWhite,Radial,True,3,0.5,0.98,1VeyOvjAV15B9aCDD0b2lXiOfvGjWyD+5uBvUT0knn/fe+YRdF/5+VMon0Xy5i8PfQTr+w5/Eg')]
    [TestCase('100', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,False,1,1,0.98,pY+BPQ6pd+986PvLb0bpz3d/QfPkQ/GB4gjQMD0h+N+Peu2/r28C/hJYjwH3p+PtNw2KsDy3AA')]
    [TestCase('101', '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Radial,False,1,1,0.98,vfMRPAO15y7/7s2nP0bvMhC/v/G8y6B/5fQrQX0j0a9Pe+nqgz8++zlYf8XwxHOuOAkwmJ16DA')]
    [TestCase('102', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,False,1,0.5,0.98,vfMRPAO15y7/7s6nP0bvMhDPv/G9z6B/5fQrQX0j0a9Pe+nqgz8++zlYbwXxw3OueAkwmJ16DA')]
    [TestCase('103', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,True,1,1,0.98,pY+BPQ6pd+986PvLb0bpz3d/QfPkQ/GB4gjQMD0h+N+Peu2/r28C/hJYjwH3p+PtNw2KsDy3AA')]
    [TestCase('104', '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Radial,True,1,1,0.98,vfMRPAO15y7/7s2nP0bvMhC/v/G8y6B/5fQrQX0j0a9Pe+nqgz8++zlYf8XwxHOuOAkwmJ16DA')]
    [TestCase('105', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,True,1,0.5,0.98,vfMRPAO15y7/7s6nP0bvMhDPv/G9z6B/5fQrQX0j0a9Pe+nqgz8++zlYbwXxw3OueAkwmJ16DA')]
    [TestCase('106', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,False,2,1,0.98,2EvyePnAoy7B+46Xv8TqR/wuwvnnjwD7678LEnokl0cQegnEVy+BCVZYb7kCEQhAwf0TGL9A6Q')]
    [TestCase('107', '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Radial,False,2,1,0.98,11PS+fjAez6B+JePbwXwbtQeAPW+czD86c/HsfskoeMfe/ftV4/9AFtYb4H2+IdPvQH0z27/AA')]
    [TestCase('108', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('109', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,True,0.5,1,0.98,0isxggJ0410EALLHb0T1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('110', '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Radial,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('111', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,True,3,0.5,0.98,1VeyOvjAV15B9aCDD0b2lXh+fvGiWzC+5uBvUT0knn/fe+YReF/5+VMM30Xy4AuvfATuz94+FA')]
    [TestCase('112', '[claBlack;-1 claWhite;0.8],claWhite,Radial,True,1,1,0.98,wf+QPBTfL5A95iIkkIPov+vfAvcQDIL/5DpowMMXHbwP//C2d9/E+/v7z74Qon++uxJ0vO16Aw')]
    [TestCase('113', '[claBlack;0 claWhite;2],claWhite,Radial,True,1,1,0.98,mUtxfQy3n2997u/Xb8bt1WMfAPXAU1GC2+TvQPsh8dvPOO7hbw8/AhA8DwH6rfe9tw1wgAw5/w')]
    [TestCase('114', '[claBlack;0 claWhite;1],claRed,Radial,False,1,1,0.98,fktR/Qqxkx/98tfHPwb05wt/fvu5N5HC0rdTwDsn95t/eO/1X+/9CBBQ74H2ncNdeAx3XLy4+w')]
    [TestCase('115', '[$7F000000;0 $7FFFFFFF;1],claRed,Radial,False,1,1,0.98,qtMBvAG5804/9LujH8b0Ptg+vvWnx/C/3czrkHwm0Y9PeuoFWP97ADhUr4Xwv2Pe+AgmcG37CQ')]
    [TestCase('116', '[claBlack;0 claWhite;1],claRed,Radial,False,1,0.5,0.98,qtMBvAG59z4/9LujH8b0Ptw+/vWnx/B/3cvrkDwm0Y8/euoFWP+7ADhUr8Xwv2Pe+AgmbF37CQ')]
    [TestCase('117', '[claBlack;0 claWhite;1],claRed,Radial,True,1,1,0.98,fktR/Qqxkx/98tfHPwb05wt/fvu5N5HC0rdTwDsn95t/eO/1X+/9CBBQ74H2ncNdeAx3XLy4+w')]
    [TestCase('118', '[$7F000000;0 $7FFFFFFF;1],claRed,Radial,True,1,1,0.98,qtMBvAG5804/9LujH8b0Ptg+vvWnx/C/3czrkHwm0Y9PeuoFWP97ADhUr4Xwv2Pe+AgmcG37CQ')]
    [TestCase('119', '[claBlack;0 claWhite;1],claRed,Radial,True,1,0.5,0.98,qtMBvAG59z4/9LujH8b0Ptw+/vWnx/B/3cvrkDwm0Y8/euoFWP+7ADhUr8Xwv2Pe+AgmbF37CQ')]
    [TestCase('120', '[claBlack;0 claWhite;1],claRed,Radial,False,2,1,0.98,108CufnAox6B+46XzwTpRtj+gfrkczA76rrPofkjmlMgOgjJf7+AClOcfnv5y49PfwBc/Kz96A')]
    [TestCase('121', '[$7F000000;0 $7FFFFFFF;1],claRed,Radial,False,2,1,0.98,1lPS+fjAfz6B+JeLbwXvbsAOQPW8Z0A86c2ngbskousf+/bva299AVr4fgLy1ktf/AIZwN29AA')]
    [TestCase('122', '[claBlack;0 claWhite;1],claRed,Radial,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('123', '[claBlack;0 claWhite;1],claRed,Radial,True,0.5,1,0.98,11cxQP5hCy4DCocvn0P1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('124', '[$7F000000;0 $7FFFFFFF;1],claRed,Radial,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('125', '[claBlack;0 claWhite;1],claRed,Radial,True,3,0.5,0.98,1VeyOvjAV15B9aCDD0b2lXiOfvGjWyC+5uBvQT0knn/fe+YReF/5+VMEn8Xx3SPfvAT0t95+Eg')]
    [TestCase('126', '[claBlack;0.2 claWhite;0.8],claRed,Radial,False,1,1,0.98,dG+RvQyoa8988N7Lb4b06guPvvu4H4HC1LhjcLso+JOP+O/yVz8+ChI0D4L0nqNNuAp8ZKz4/A')]
    [TestCase('127', '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Radial,False,1,1,0.98,peMhfAK03x7/8r+nPwb1QNxO/vWmu+C/3szzYDwn0YtPuuoEVC88ATlEv4Xvv1PeOAgodF27Cg')]
    [TestCase('128', '[claBlack;0.2 claWhite;0.8],claRed,Radial,False,1,0.5,0.98,peMhfAK04w7/8r+nPwb1P9xO/vWmu/B/3szzYDwn0YtPuuoEVC88ATlEr4Xvv1POOAgpcF17Cg')]
    [TestCase('129', '[claBlack;0.2 claWhite;0.8],claRed,Radial,True,1,1,0.98,dG+RvQyoa8988N7Lb4b06guPvvu4H4HC1LhjcLso+JOP+O/yVz8+ChI0D4L0nqNNuAp8ZKz4/A')]
    [TestCase('130', '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Radial,True,1,1,0.98,peMhfAK03x7/8r+nPwb1QNxO/vWmu+C/3szzYDwn0YtPuuoEVC88ATlEv4Xvv1PeOAgodF27Cg')]
    [TestCase('131', '[claBlack;0.2 claWhite;0.8],claRed,Radial,True,1,0.5,0.98,peMhfAK04w7/8r+nPwb1P9xO/vWmu/B/3szzYDwn0YtPuuoEVC88ATlEr4Xvv1POOAgpcF17Cg')]
    [TestCase('132', '[claBlack;0.2 claWhite;0.8],claRed,Radial,False,2,1,0.98,2EvyePnAoy7B+46Xv4ToQ/zuwfrjb0B767z3gTkkm1sQugjLZ68AClp8Dvv3z4/vfgFc4Nz95w')]
    [TestCase('133', '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Radial,False,2,1,0.98,11PS+fjAez6B+JePbwXvbNT+f/W8Y1C86c6/cfsko+8PO/fxX189AV3sPkLx2EsvfAMZsO09AA')]
    [TestCase('134', '[claBlack;0.2 claWhite;0.8],claRed,Radial,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('135', '[claBlack;0.2 claWhite;0.8],claRed,Radial,True,0.5,1,0.98,1ENBwP5h/93CCIMj30P1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('136', '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Radial,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('137', '[claBlack;0.2 claWhite;0.8],claRed,Radial,True,3,0.5,0.98,1VeyOvjAV15B9aCDD0b2lXh+fvGiWzC+5uBvUT0knn/fe+YReF/5+VP0vkXy4A+ffATyw97+Ew')]
    [TestCase('138', '[claBlack;-1 claWhite;0.8],claRed,Radial,True,1,1,0.98,j+9wfAbH6599977DzwT148cOfv7Ka4ECzLOX774kAIwfOe/8gy8+CAbc/4H8kBP+OQ92aGw49g')]
    [TestCase('139', '[claBlack;0 claWhite;2],claRed,Radial,True,1,1,0.98,hT+BfQqzl1+98tzPv4by4SOvvvisL8HC1Lu3IPol7buft+73V//8BhEoj4H2qNuddwxuSDw5/A')]
    [TestCase('140', '[claRed;0 claBlue;1],claWhite,Radial,False,1,1,0.98,oa/AfALkCwC++ss70IXytxsfvfi7Y8GC0srr/7wd71cAuOsNJN+8BO3XTwAJqGuueRBwuGt58A')]
    [TestCase('141', '[$7FFF0000;0 $7F0000FF;1],claWhite,Radial,False,1,1,0.98,u4OxO/3SL78/+LXf/wX0J+CeffSo3wBA3dW3IL0hzOsPeugRPG97/ieU38T5xLeO+QoiIL17BA')]
    [TestCase('142', '[claRed;0 claBlue;1],claWhite,Radial,False,1,0.5,0.98,u4Oxe/3SM68/+Lbf/wX0JuCOffSo3xBA3dW3IH0hze8PeugRPH97/iaUzwT6xLd++QojHL07BA')]
    [TestCase('143', '[claRed;0 claBlue;1],claWhite,Radial,True,1,1,0.98,oa/AfALkCwC++ss70IXytxsfvfi7Y8GC0srr/7wd71cAuOsNJN+8BO3XTwAJqGuueRBwuGt58A')]
    [TestCase('144', '[$7FFF0000;0 $7F0000FF;1],claWhite,Radial,True,1,1,0.98,u4OxO/3SL78/+LXf/wX0J+CeffSo3wBA3dW3IL0hzOsPeugRPG97/ieU38T5xLeO+QoiIL17BA')]
    [TestCase('145', '[claRed;0 claBlue;1],claWhite,Radial,True,1,0.5,0.98,u4Oxe/3SM68/+Lbf/wX0JuCOffSo3xBA3dW3IH0hze8PeugRPH97/iaUzwT6xLd++QojHL07BA')]
    [TestCase('146', '[claRed;0 claBlue;1],claWhite,Radial,False,2,1,0.98,xXvSefrIvz7A+YmLL4XvP4RuAQYGSGC847W/gPshl1PgfAjIg4/AEzHAbjoDxoP/fvxR7O2A6w')]
    [TestCase('147', '[$7FFF0000;0 $7F0000FF;1],claWhite,Radial,False,2,1,0.98,zWdCevnEi86A95WHn4Xyapi+P/vNU+D85csjcbwjoet//Pbvb0/9BUkM78H200Mv/AATNH7/AQ')]
    [TestCase('148', '[claRed;0 claBlue;1],claWhite,Radial,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('149', '[claRed;0 claBlue;1],claWhite,Radial,True,0.5,1,0.98,2ENh//pNC07EDJivb0L1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('150', '[$7FFF0000;0 $7F0000FF;1],claWhite,Radial,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('151', '[claRed;0 claBlue;1],claWhite,Radial,True,3,0.5,0.98,1VeyOvjAV15B9aCDD8bzmZiePvKmN9D95uBzwfwloocfvOcNbA/5+FiYDwbzzOcvvgbv255/CQ')]
    [TestCase('152', '[claRed;0.2 claBlue;0.8],claWhite,Radial,False,1,1,0.98,rJ+QfADtR3B+/sEf8EXxswM/ffjCh0HC0sXPnz0d7msw+OoRQI88Aun7j0ALqqu+eRFzsFs57g')]
    [TestCase('153', '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Radial,False,1,1,0.98,wXuhO/zWT+//+bHTD0bzJdSePfSs79C/3dKrcH0hzPcvOugTTE87/SWk/wT7xdeOeQskGL07Aw')]
    [TestCase('154', '[claRed;0.2 claBlue;0.8],claWhite,Radial,False,1,0.5,0.98,wXuhO/zWT+//+bHTD0bzJNSeffSs79C/3dKncD0hzPcfOugTTE87/SSo7wT7xdeOeQslGK37Ag')]
    [TestCase('155', '[claRed;0.2 claBlue;0.8],claWhite,Radial,True,1,1,0.98,rJ+QfADtR3B+/sEf8EXxswM/ffjCh0HC0sXPnz0d7msw+OoRQI88Aun7j0ALqqu+eRFzsFs57g')]
    [TestCase('156', '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Radial,True,1,1,0.98,wXuhO/zWT+//+bHTD0bzJdSePfSs79C/3dKrcH0hzPcvOugTTE87/SWk/wT7xdeOeQskGL07Aw')]
    [TestCase('157', '[claRed;0.2 claBlue;0.8],claWhite,Radial,True,1,0.5,0.98,wXuhO/zWT+//+bHTD0bzJNSeffSs79C/3dKncD0hzPcfOugTTE87/SSo7wT7xdeOeQslGK37Ag')]
    [TestCase('158', '[claRed;0.2 claBlue;0.8],claWhite,Radial,False,2,1,0.98,w3viefrIwz6A+YiHLwXxRkiugQUKXEC84K2PwLsgljPAfAnJpx+AEinQTrsDwYvv/vpNLD4B7g')]
    [TestCase('159', '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Radial,False,2,1,0.98,zGtCevnEj86A95SDn0Xzbnje//rPW9B85McHkTwjoNtvfPfvfw99BUQUXwL30EcfPAARVJ4/Aw')]
    [TestCase('160', '[claRed;0.2 claBlue;0.8],claWhite,Radial,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('161', '[claRed;0.2 claBlue;0.8],claWhite,Radial,True,0.5,1,0.98,308Bf/pML85ED5izf0L1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('162', '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Radial,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('163', '[claRed;0.2 claBlue;0.8],claWhite,Radial,True,3,0.5,0.98,1VeyOvjAV15B9aCDD8bzmZiuPvKmN9D95uBvwfwloocfvOcNaA/5+Fi0HwbyyPOffgf1uz7/Bw')]
    [TestCase('164', '[claRed;-1 claBlue;0.8],claWhite,Radial,True,1,1,0.98,g9eg/QjUc9+88vjHMMfxpsdP/POk/0AD1+bLADkc4YsQd+kCgA49CfIzj74HtsMNuA5plEu69Q')]
    [TestCase('165', '[claRed;0 claBlue;2],claWhite,Radial,True,1,1,0.98,oKdQfALfI/C9+rz738Tzy9t+ffzGd6ECzb2H3z4g+fev+OwIYG+9BfYLAEEFmGNeOhFxENx48Q')]
    [TestCase('166', '[claRed;0 $7F000000;0.5 claBlue;1],claWhite,Radial,True,1,1,0.98,vOdwvfrhu98//bTjf0X5CKhuf/Gl7wAB3cZvkPshzlNgd+gXQD/6AAO474P8rNdeeg1EwLx6/w')]
    [TestCase('167', '[claRed;0 $7F000000;1 claBlue;0.5],claWhite,Radial,True,1,1,0.98,k9fQPgLkk6/99dxHAEfwwn8f/vGbDzED2Nt3wDke3Ncw9uULgP56A/aTfz8HnGPueQ9J2Iu6+A')]
    [TestCase('168', '[claRed;0 $7F000000;-1 claBlue;0.5],claWhite,Radial,True,1,1,0.98,kt8QfgPsy0/99+FzcMfypY8PPfSjH9EC29lnsDkd2+cAd+cRiN77BuCL374LqWuO+Q1rXDs68g')]
    [TestCase('169', '[claRed;1 $7F000000;0.5 claBlue;0],claWhite,Radial,True,1,1,0.98,kBfhfQS8Ex8+8thDkAX6FTTffvGeh2DB19cLgbkkzteft+f5y667CSAk38Lyo8cdeQw9QM36CA')]
    [TestCase('170', '[claRed;1 $7F000000;0 claBlue;0.5],claWhite,Radial,True,1,1,0.98,d/+xPAzBJ6877esLkEjx2sdPvPud33DB3tMDEbkm0guw+en+l359DAwA77/7tQc+eAVh8Ls6+w')]
    [TestCase('171', '[claRed;1 $7F000000;0 claBlue;0.5 claWhite;0.49],claWhite,Radial,True,1,1,0.98,lY+BehHY0y+97d3Xr8Tp0MM/PvjUu+H+3vlrYD8i6IdP/e7yE6+ABhHoXwAHsldeegtQnCz7+w')]
    [TestCase('172', '[claRed;1 $7F000000;0 claBlue;0.5 claWhite;0.49],claWhite,Radial,False,1,1,0.98,lY+BehHY0y+97d3Xr8Tp0MM/PvjUu+H+3vlrYD8i6IdP/e7yE6+ABhHoXwAHsldeegtQnCz7+w')]
    [TestCase('173', '[claRed;1 $7F000000;0 claBlue;0.5 claWhite;0.49],claWhite,Radial,True,2,0.5,0.98,1lcyuvjAb04B95uHr8XxdrBuAPO8a8D76eKvYX0mnb8P/PDpP2+9+lP4ngH/8GePvPz2Rw9BCQ')]
    [TestCase('174', '[],claWhite,Radial,True,1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('175', '[claRed;0.3],claWhite,Radial,True,1,1,0.98,n6PguwLaO/D9+q2/30P14Jfu/f/Sj4GCx7Env4AiA5Bfee4DnP/9Bv870EEBiFsOuxFzaEw48g')]
    [TestCase('176', '[claRed;0.3],claBlue,Radial,True,1,1,0.98,jDfB/Qm1n5+98uHTPwfx3DvvPvafKwGD1sAbcfgk49+/9u35Sx88BRIAL4H2s+/ddgxlNKz5/A')]
    [TestCase('177', '[claRed;0.3],claWhite,Linear,True,1,1,0.98,n6PguwLaO/D9+q2/30P14Jfu/f/Sj4GCx7Env4AiA5Bfee4DnP/9Bv870EEBiFsOuxFzaEw48g')]
    [TestCase('178', '[claRed;0.3],claBlue,Linear,True,1,1,0.98,jDfB/Qm1n5+98uHTPwfx3DvvPvafKwGD1sAbcfgk49+/9u35Sx88BRIAL4H2s+/ddgxlNKz5/A')]
    [TestCase('179', '[claRed;0 claBlue;1 claBlue;1],claWhite,Radial,False,1,1,0.98,oa/AfALkCwC++ss70IXytxsfvfi7Y8GC0srr/7wd71cAuOsNJN+8BO3XTwAJqGuueRBwuGt58A')]
    [TestCase('180', '[claRed;0 claRed;0 claBlue;1],claWhite,Radial,False,1,1,0.98,oa/AfALkCwC++ss70IXytxsfvfi7Y8GC0srr/7wd71cAuOsNJN+8BO3XTwAJqGuueRBwuGt58A')]
    [TestCase('181', '[claRed;0 claBlue;1 claBlue;1],claWhite,Linear,False,1,1,0.98,l59QfQTc+899/MgfcIXz0yuPvfzBf5FDzsPb7/kf6SNw9+jrU66+DQN0z74Dol/NdwxkODy6+w')]
    [TestCase('182', '[claRed;0 claRed;0 claBlue;1],claWhite,Linear,False,1,1,0.98,l59QfQTc+899/MgfcIXz0yuPvfzBf5FDzsPb7/kf6SNw9+jrU66+DQN0z74Dol/NdwxkODy6+w')]
    [TestCase('183', '[$00FF0000;0 $FF0000FF;1],claBlack,Linear,True,1,1,0.98,vcehegDH5y6A9KOXX4fzV+CuvveodzD+5NW/8fkm2NvPd+r/G088ACoQr8PyuDdetw078Iy6BQ')]
    [TestCase('184', '[$00FF0000;0 $FF0000FF;1],claBlack,Radial,True,1,1,0.98,wx8iOQLEL7/+8bKHnwbzQARvPPyVzzA/3c6DobsmtRvPO+sNfL/7AkEsjwTw4ceeNwUXUK28BQ')]
    [TestCase('185', '[$00FF0000;0 $FF0000FF;1],claBlack,Linear,True,1,0.5,0.98,yQ+yevzEn84A9aKPv4b0dsRe/vOeZ8B95tujoTsmwa/v+ecKOC87/EU0f8buzJteuAkJuE38Dg')]
    [TestCase('186', '[$00FF0000;0 $FF0000FF;1],claBlack,Radial,True,1,0.5,0.98,zDviOf3Cww6A86mHT0b0atg+PfaVk0C+4teDgfwlsE/ve+gRaO+6/VBA70bt4eN+eAX26139Dg')]
    procedure TestFillEllipseGradient(const APoints, AModulateColor: string; AGradientStyle: TGradientStyle; ABlending: Boolean; const AScale, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  '[claBlack;0 claWhite;1],claWhite,Linear,False,1,1,0.98,pwPheA/J30/96JYn0Mnq9hsPAAPm2zA93+GbssAS4/c+P/LFF8C+/ErAL8L24Kee+gIsaG16CA')]
    [TestCase('2',  '[$7F000000;0 $7FFFFFFF;1],claWhite,Linear,False,1,1,0.98,vq/BuQPEG18/75vX70fwRuAOv/m9m0C94+APAj8cxzuf/evtt298+lWIz8Xw4NP+OQQB9D08EA')]
    [TestCase('3',  '[claBlack;0 claWhite;1],claWhite,Linear,False,1,0.5,0.98,vq/B+QPEG08/75vX7wfwReQOv/m+m0C94+EPAj8cxzuv/evtt298+lWIz8Xw4NP+OQQB9D08EA')]
    [TestCase('4',  '[claBlack;0 claWhite;1],claWhite,Linear,True,1,1,0.98,pwPheA/J30/96JYn0Mnq9hsPAAPm2zA93+GbssAS4/c+P/LFF8C+/ErAL8L24Kee+gIsaG16CA')]
    [TestCase('5',  '[$7F000000;0 $7FFFFFFF;1],claWhite,Linear,True,1,1,0.98,vq/BuQPEG18/75vX70fwRuAOv/m9m0C94+APAj8cxzuf/evtt298+lWIz8Xw4NP+OQQB9D08EA')]
    [TestCase('6',  '[claBlack;0 claWhite;1],claWhite,Linear,True,1,0.5,0.98,vq/B+QPEG08/75vX7wfwReQOv/m+m0C94+EPAj8cxzuv/evtt298+lWIz8Xw4NP+OQQB9D08EA')]
    [TestCase('7',  '[claBlack;0 claWhite;1],claWhite,Linear,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDEbskgh9x+/vS+51892qEvr4ABTzvPgL3I99/Cg')]
    [TestCase('8',  '[$7F000000;0 $7FFFFFFF;1],claWhite,Linear,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDMTwll0+wu/D0q07792XsHoT18iMvvAPm0/5+EQ')]
    [TestCase('9',  '[claBlack;0 claWhite;1],claWhite,Linear,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('10', '[claBlack;0 claWhite;1],claWhite,Linear,True,0.5,1,0.98,78OC/vqZp/3E+9mrP0H1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('11', '[$7F000000;0 $7FFFFFFF;1],claWhite,Linear,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('12', '[claBlack;0 claWhite;1],claWhite,Linear,True,3,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBob4np5hOveQbebw4+Fw')]
    [TestCase('13', '[claBlack;0.2 claWhite;0.8],claWhite,Linear,False,1,1,0.98,qwcReQ3J4x996pQXQEru/SNv/wHdz3D93dO7soER5v+ef/S9F2A/+krMP8L22p8+uwIteP25CA')]
    [TestCase('14', '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Linear,False,1,1,0.98,wK/huQLEGz//75rPL8jxSui+Pvm5k2D94tkfgr8byD/P/ezpt788+VWQz8Xw3dNOOgQB/P17EA')]
    [TestCase('15', '[claBlack;0.2 claWhite;0.8],claWhite,Linear,False,1,0.5,0.98,wK/huQLEHz//75rPL8jxSei+Pvm5k2D94tkfgn8byT/P/ezpt788+VWQz8Xw3dNOOgQB/P17EA')]
    [TestCase('16', '[claBlack;0.2 claWhite;0.8],claWhite,Linear,True,1,1,0.98,qwcReQ3J4x996pQXQEru/SNv/wHdz3D93dO7soER5v+ef/S9F2A/+krMP8L22p8+uwIteP25CA')]
    [TestCase('17', '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Linear,True,1,1,0.98,wK/huQLEGz//75rPL8jxSui+Pvm5k2D94tkfgr8byD/P/ezpt788+VWQz8Xw3dNOOgQB/P17EA')]
    [TestCase('18', '[claBlack;0.2 claWhite;0.8],claWhite,Linear,True,1,0.5,0.98,wK/huQLEHz//75rPL8jxSei+Pvm5k2D94tkfgn8byT/P/ezpt788+VWQz8Xw3dNOOgQB/P17EA')]
    [TestCase('19', '[claBlack;0.2 claWhite;0.8],claWhite,Linear,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDofokiBtRu/zW+028+HNQHn/+BAg/PgL80z4/Dg')]
    [TestCase('20', '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Linear,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDATwlmU+w+/D2qy57+GnUToT08gfP+wPpq54+Ew')]
    [TestCase('21', '[claBlack;0 claWhite;1],claWhite,Radial,False,1,1,0.98,rz/BOA/Jr2+85p8D4Inp+QNfgALxqzA94uOHUvwW4PO+fenY+999A0hErwLy/8seuAQ1OO07CA')]
    [TestCase('22', '[$7F000000;0 $7FFFFFFF;1],claWhite,Radial,False,1,1,0.98,wsvBuQPEA+8+7qDD/4fvSNQuf/nDg0A95eIH0jwexTvffOf3q+/7/VRMD0bu8Oe+OAUF3P08EA')]
    [TestCase('23', '[claBlack;0 claWhite;1],claWhite,Radial,False,1,0.5,0.98,wsuxuQPFA+/+7aDD/4fvR9Q+f/nEg0A95eIH0jwexTvffOf2q+/7/VRM/0Xu8Oe+OAUF3P08EA')]
    [TestCase('24', '[claBlack;0 claWhite;1],claWhite,Radial,True,1,1,0.98,rz/BOA/Jr2+85p8D4Inp+QNfgALxqzA94uOHUvwW4PO+fenY+999A0hErwLy/8seuAQ1OO07CA')]
    [TestCase('25', '[$7F000000;0 $7FFFFFFF;1],claWhite,Radial,True,1,1,0.98,wsvBuQPEA+8+7qDD/4fvSNQuf/nDg0A95eIH0jwexTvffOf3q+/7/VRMD0bu8Oe+OAUF3P08EA')]
    [TestCase('26', '[claBlack;0 claWhite;1],claWhite,Radial,True,1,0.5,0.98,wsuxuQPFA+/+7aDD/4fvR9Q+f/nEg0A95eIH0jwexTvffOf2q+/7/VRM/0Xu8Oe+OAUF3P08EA')]
    [TestCase('27', '[claBlack;0 claWhite;1],claWhite,Radial,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDAfslhdOQe/zEO9589HuY3v7/EmAf/wLiW29ABw')]
    [TestCase('28', '[$7F000000;0 $7FFFFFFF;1],claWhite,Radial,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDIbwlmCvA+/Dty2579m34HkT1+TM/PATc7z6/Dw')]
    [TestCase('29', '[claBlack;0 claWhite;1],claWhite,Radial,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('30', '[claBlack;0 claWhite;1],claWhite,Radial,True,0.5,1,0.98,77sCf/qXrz2F+dKzb0L1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('31', '[$7F000000;0 $7FFFFFFF;1],claWhite,Radial,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('32', '[claBlack;0 claWhite;1],claWhite,Radial,True,3,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBob4np5hOveQbebw4+Fw')]
    [TestCase('33', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,False,1,1,0.98,s1fROA3Lq/875qTvr8rrAPQ+gAHqj0A94terkrsX4+uO/eba/w/+BEcoz8LwBtSe9wQ7QA28Bw')]
    [TestCase('34', '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Radial,False,1,1,0.98,xNfBuQLG/67+7aK3X4jwS9Ae//jAc1A95dwXcrwexzfPPOb4qw98/lNAH8bt8+t+OAUI4A39Dw')]
    [TestCase('35', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,False,1,0.5,0.98,xNfBuQLGA6++7aK3X4jwStAe//jAc1A95dwXcrwexzfPPOb4qw98/lNAD8bt8+t+OAUJ4A39Dw')]
    [TestCase('36', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,True,1,1,0.98,s1fROA3Lq/875qTvr8rrAPQ+gAHqj0A94terkrsX4+uO/eba/w/+BEcoz8LwBtSe9wQ7QA28Bw')]
    [TestCase('37', '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Radial,True,1,1,0.98,xNfBuQLG/67+7aK3X4jwS9Ae//jAc1A95dwXcrwexzfPPOb4qw98/lNAH8bt8+t+OAUI4A39Dw')]
    [TestCase('38', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,True,1,0.5,0.98,xNfBuQLGA6++7aK3X4jwStAe//jAc1A95dwXcrwexzfPPOb4qw98/lNAD8bt8+t+OAUJ4A39Dw')]
    [TestCase('39', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDkXolhMfQu/3GT1589H1gHr/9GSCPfgTaFy8ACA')]
    [TestCase('40', '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Radial,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCD8XslmCfwe/Hu0z579m/cPgT0/BPv+wTYyx4/EA')]
    [TestCase('41', '[claRed;0 claBlue;1],claBlack,Linear,False,1,1,0.98,rf9weAnXo8878JbT3wnt/QN/vgTMh4C+3bej8jkbxNduO+zw3y96BEAc/4Lx/ZseOAQa5Bw9Aw')]
    [TestCase('42', '[$7FFF0000;0 $7F0000FF;1],claBlack,Linear,False,1,1,0.98,wauRuQDL/57+8pur/0fxStROfvqxb/D94swTonsgty+/u+gDmB86/lA4Pwbu7s+++AT3t429DQ')]
    [TestCase('43', '[claRed;0 claBlue;1],claBlack,Linear,False,1,0.5,0.98,wauR+QDL/57+8pur/0fxSdROfvqxb/D94swTojsguCu/++gDmB86/lA4Lwbu7s+++AT4s429DQ')]
    [TestCase('44', '[claRed;0 claBlue;1],claBlack,Linear,True,1,1,0.98,rf9weAnXo8878JbT3wnt/QN/vgTMh4C+3bej8jkbxNduO+zw3y96BEAc/4Lx/ZseOAQa5Bw9Aw')]
    [TestCase('45', '[$7FFF0000;0 $7F0000FF;1],claBlack,Linear,True,1,1,0.98,wauRuQDL/57+8pur/0fxStROfvqxb/D94swTonsgty+/u+gDmB86/lA4Pwbu7s+++AT3t429DQ')]
    [TestCase('46', '[claRed;0 claBlue;1],claBlack,Linear,True,1,0.5,0.98,wauR+QDL/57+8pur/0fxSdROfvqxb/D94swTojsguCu/++gDmB86/lA4Lwbu7s+++AT4s429DQ')]
    [TestCase('47', '[claRed;0 claBlue;1],claBlack,Linear,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDgfojhsvQu/zJZ0489X2ALr/94TOPPP4diM68EA')]
    [TestCase('48', '[$7FFF0000;0 $7F0000FF;1],claBlack,Linear,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCD8bskmCfwO/Hw3y679m7sTgT04Bvv+gH5h259FA')]
    [TestCase('49', '[claRed;0 claBlue;1],claBlack,Linear,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('50', '[claRed;0 claBlue;1],claBlack,Linear,True,0.5,1,0.98,39NyPftsu03EAMxPP0L1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('51', '[$7FFF0000;0 $7F0000FF;1],claBlack,Linear,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('52', '[claRed;0 claBlue;1],claBlack,Linear,True,3,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBob4np5ROveQbebw4+Fw')]
    [TestCase('53', '[claRed;0.2 claBlue;0.8],claBlack,Linear,False,1,1,0.98,rf9weAnXo8878JbT3wnt/QN/vgTMh4C+3bej8jkbxNduO+zw3y96BEAc/4Lx/ZseOAQa5Bw9Aw')]
    [TestCase('54', '[$7FFF0000;0.2 $7F0000FF;0.8],claBlack,Linear,False,1,1,0.98,wauRuQDL/57+8pur/0fxStROfvqxb/D94swTonsgty+/u+gDmB86/lA4Pwbu7s+++AT3t429DQ')]
    [TestCase('55', '[claRed;0.2 claBlue;0.8],claBlack,Linear,False,1,0.5,0.98,wauR+QDL/57+8pur/0fxSdROfvqxb/D94swTojsguCu/++gDmB86/lA4Lwbu7s+++AT4s429DQ')]
    [TestCase('56', '[claRed;0.2 claBlue;0.8],claBlack,Linear,True,1,1,0.98,rf9weAnXo8878JbT3wnt/QN/vgTMh4C+3bej8jkbxNduO+zw3y96BEAc/4Lx/ZseOAQa5Bw9Aw')]
    [TestCase('57', '[$7FFF0000;0.2 $7F0000FF;0.8],claBlack,Linear,True,1,1,0.98,wauRuQDL/57+8pur/0fxStROfvqxb/D94swTonsgty+/u+gDmB86/lA4Pwbu7s+++AT3t429DQ')]
    [TestCase('58', '[claRed;0.2 claBlue;0.8],claBlack,Linear,True,1,0.5,0.98,wauR+QDL/57+8pur/0fxSdROfvqxb/D94swTojsguCu/++gDmB86/lA4Lwbu7s+++AT4s429DQ')]
    [TestCase('59', '[claRed;0.2 claBlue;0.8],claBlack,Linear,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDgfojhsvQu/zJZ0489X2ALr/94TOPPP4diM68EA')]
    [TestCase('60', '[$7FFF0000;0.2 $7F0000FF;0.8],claBlack,Linear,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCD8bskmCfwO/Hw3y679m7sTgT04Bvv+gH5h259FA')]
    [TestCase('61', '[claRed;0 claBlue;1],claBlack,Radial,False,1,1,0.98,rf9weAnXo8878JbT3wnt/QN/vgTMh4C+3bej8jkbxNduO+zw3y96BEAc/4Lx/ZseOAQa5Bw9Aw')]
    [TestCase('62', '[$7FFF0000;0 $7F0000FF;1],claBlack,Radial,False,1,1,0.98,wauRuQDL/57+8pur/0fxStROfvqxb/D94swTonsgty+/u+gDmB86/lA4Pwbu7s+++AT3t429DQ')]
    [TestCase('63', '[claRed;0 claBlue;1],claBlack,Radial,False,1,0.5,0.98,wauR+QDL/57+8pur/0fxSdROfvqxb/D94swTojsguCu/++gDmB86/lA4Lwbu7s+++AT4s429DQ')]
    [TestCase('64', '[claRed;0 claBlue;1],claBlack,Radial,True,1,1,0.98,rf9weAnXo8878JbT3wnt/QN/vgTMh4C+3bej8jkbxNduO+zw3y96BEAc/4Lx/ZseOAQa5Bw9Aw')]
    [TestCase('65', '[$7FFF0000;0 $7F0000FF;1],claBlack,Radial,True,1,1,0.98,wauRuQDL/57+8pur/0fxStROfvqxb/D94swTonsgty+/u+gDmB86/lA4Pwbu7s+++AT3t429DQ')]
    [TestCase('66', '[claRed;0 claBlue;1],claBlack,Radial,True,1,0.5,0.98,wauR+QDL/57+8pur/0fxSdROfvqxb/D94swTojsguCu/++gDmB86/lA4Lwbu7s+++AT4s429DQ')]
    [TestCase('67', '[claRed;0 claBlue;1],claBlack,Radial,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDgfojhsvQu/zJZ0489X2ALr/94TOPPP4diM68EA')]
    [TestCase('68', '[$7FFF0000;0 $7F0000FF;1],claBlack,Radial,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCD8bskmCfwO/Hw3y679m7sTgT04Bvv+gH5h259FA')]
    [TestCase('69', '[claRed;0 claBlue;1],claBlack,Radial,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('70', '[claRed;0 claBlue;1],claBlack,Radial,True,0.5,1,0.98,39NyPftsu03EAMxPP0L1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('71', '[$7FFF0000;0 $7F0000FF;1],claBlack,Radial,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('72', '[claRed;0 claBlue;1],claBlack,Radial,True,3,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBob4np5ROveQbebw4+Fw')]
    [TestCase('73', '[claRed;0.2 claBlue;0.8],claBlack,Radial,False,1,1,0.98,rf9weAnXo8878JbT3wnt/QN/vgTMh4C+3bej8jkbxNduO+zw3y96BEAc/4Lx/ZseOAQa5Bw9Aw')]
    [TestCase('74', '[$7FFF0000;0.2 $7F0000FF;0.8],claBlack,Radial,False,1,1,0.98,wauRuQDL/57+8pur/0fxStROfvqxb/D94swTonsgty+/u+gDmB86/lA4Pwbu7s+++AT3t429DQ')]
    [TestCase('75', '[claRed;0.2 claBlue;0.8],claBlack,Radial,False,1,0.5,0.98,wauR+QDL/57+8pur/0fxSdROfvqxb/D94swTojsguCu/++gDmB86/lA4Lwbu7s+++AT4s429DQ')]
    [TestCase('76', '[claRed;0.2 claBlue;0.8],claBlack,Radial,True,1,1,0.98,rf9weAnXo8878JbT3wnt/QN/vgTMh4C+3bej8jkbxNduO+zw3y96BEAc/4Lx/ZseOAQa5Bw9Aw')]
    [TestCase('77', '[$7FFF0000;0.2 $7F0000FF;0.8],claBlack,Radial,True,1,1,0.98,wauRuQDL/57+8pur/0fxStROfvqxb/D94swTonsgty+/u+gDmB86/lA4Pwbu7s+++AT3t429DQ')]
    [TestCase('78', '[claRed;0.2 claBlue;0.8],claBlack,Radial,True,1,0.5,0.98,wauR+QDL/57+8pur/0fxSdROfvqxb/D94swTojsguCu/++gDmB86/lA4Lwbu7s+++AT4s429DQ')]
    [TestCase('79', '[claRed;0.2 claBlue;0.8],claBlack,Radial,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDgfojhsvQu/zJZ0489X2ALr/94TOPPP4diM68EA')]
    [TestCase('80', '[$7FFF0000;0.2 $7F0000FF;0.8],claBlack,Radial,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCD8bskmCfwO/Hw3y679m7sTgT04Bvv+gH5h259FA')]
    procedure TestFillEllipseGradient2(const APoints, AModulateColor: string; AGradientStyle: TGradientStyle; ABlending: Boolean; const AScale, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,False,0.9,1,0.98,ydd/Pvv/D8+D/Grr/kH1lawOPvCVV1A96OCDQX/6Jkz+eu9P6NFu6rBXX8nq4ANfeQXVgw4+GA')]
    [TestCase('2',  '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,False,0.9,1,0.98,ydd/Pvv/D8+D/Grr/kH1lawOPvCVV1A96OCDQX/6Jkz+eu9P6NFu6rBXX8nq4ANfeQXVgw4+GA')]
    [TestCase('3',  '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,False,0.9,1,0.98,ydd/Pvv/D8+D/Grr/kH1lawOPvCVV1A96OCDQX/6Jkz+eu9P6NFu6rBXX8nq4ANfeQXVgw4+GA')]
    [TestCase('4',  '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,False,0.9,1,0.98,ydd/Pvv/D8+D/Grr/kH1lawOPvCVV1A96OCDQX/6Jkz+eu9P6NFu6rBXX8nq4ANfeQXVgw4+GA')]
    [TestCase('5',  '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,False,0.9,0.5,0.98,zxeRvPngs54C+YU3D0T1lawOPvCVV1A96OCDUf4P6Ot+e+oyoGA08QhUX8nq4ANfeQXVgw4+GA')]
    [TestCase('6',  '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,False,0.9,0.5,0.98,zxeRvPngs54C+YU3D0T1lawOPvCVV1A96OCDUf4P6Ot+e+oyoGA08QhUX8nq4ANfeQXVgw4+GA')]
    [TestCase('7',  '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,False,0.9,0.5,0.98,zxeRvPngs54C+YU3D0T1lawOPvCVV1A96OCDUf4P6Ot+e+oyoGA08QhUX8nq4ANfeQXVgw4+GA')]
    [TestCase('8',  '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,False,0.9,0.5,0.98,zxeRvPngs54C+YU3D0T1lawOPvCVV1A96OCDUf4P6Ot+e+oyoGA08QhUX8nq4ANfeQXVgw4+GA')]
    [TestCase('9',  '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,False,0.9,1,0.98,4j+w+/oVhF9D+ju3Pn/1lawOPvCVV1A96OCDwQEDCNRN+/VU/OEx5bNXX8nq4ANfeQXVgw4+GA')]
    [TestCase('10', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,False,0.9,1,0.98,4j+w+/oVhF9D+ju3Pn/1lawOPvCVV1A96OCDwQEDCNRN+/VU/OEx5bNXX8nq4ANfeQXVgw4+GA')]
    [TestCase('11', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,False,0.9,1,0.98,4j+w+/oVhF9D+ju3Pn/1lawOPvCVV1A96OCDwQEDCNRN+/VU/OEx5bNXX8nq4ANfeQXVgw4+GA')]
    [TestCase('12', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,False,0.9,1,0.98,4j+w+/oVhF9D+ju3Pn/1lawOPvCVV1A96OCDwQEDCNRN+/VU/OEx5bNXX8nq4ANfeQXVgw4+GA')]
    [TestCase('13', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,False,0.9,0.5,0.98,3Esxe/nr717C920bn0L1lawOPvCVV1A96OCDkT8U2auuu+01qPC17glUX8nq4ANfeQXVgw4+GA')]
    [TestCase('14', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,False,0.9,0.5,0.98,3Esxe/nr717C920bn0L1lawOPvCVV1A96OCDkT8U2auuu+01qPC17glUX8nq4ANfeQXVgw4+GA')]
    [TestCase('15', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,False,0.9,0.5,0.98,3Esxe/nr717C920bn0L1lawOPvCVV1A96OCDkT8U2auuu+01qPC17glUX8nq4ANfeQXVgw4+GA')]
    [TestCase('16', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,False,0.9,0.5,0.98,3Esxe/nr717C920bn0L1lawOPvCVV1A96OCDkT8U2auuu+01qPC17glUX8nq4ANfeQXVgw4+GA')]
    [TestCase('17', '[claRed;0 claBlue;1],claBlack,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,False,0.9,0.5,0.98,0nPxO/vQpy6C+o4Ln0b1lawOPvCVV1A96OCDoXwSxQvPeeQwjKC18SdUX8nq4ANfeQXVgw4+GA')]
    [TestCase('18', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,0,False,False,False,0.9,1,0.98,upPxPP3Um7C99ANMkYX3zE/fd/m4xxHHv7dfb/0cCUjvPOsIrH///iXUrsHz+ksutgNZaEx89w')]
    [TestCase('19', '[claRed;0 claNull;0.5 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,0,False,False,False,0.9,1,0.98,0gti++28jy/A8qxX0Mb3Scievuy15wCC0eGDgDsg5/NfuuIidG879EXkngXv8Kv+9QUHhI19Cg')]
    [TestCase('20', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,True,0.9,1,0.98,1ffdv+4Q8L+8GZzTHrn2h2NAvwEkEMBD9ALsfn7oH7TPfwT6M0G1841Pb3gH+u9/gvxCPD/A8Q')]
    [TestCase('21', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,True,0.9,1,0.98,1ffdv+4Q8L+8GZzTHrn2h2NAvwEkEMBD9ALsfn7oH7TPfwT6M0G1841Pb3gH+u9/gvxCPD/A8Q')]
    [TestCase('22', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,True,0.9,1,0.98,1ffdv+4Q8L+8GZzTHrn2h2NAvwEkEMBD9ALsfn7oH7TPfwT6M0G1841Pb3gH+u9/gvxCPD/A8Q')]
    [TestCase('23', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,True,0.9,1,0.98,1ffdv+4Q8L+8GZzTHrn2h2NAvwEkEMBD9ALsfn7oH7TPfwT6M0G1841Pb3gH+u9/gvxCPD/A8Q')]
    [TestCase('24', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,True,0.9,0.5,0.98,0g+uQPcHSKC9FLdrf7r2h2NAvwEkEMBD9ALs7r3kHeCv/wX6F2H5+adPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('25', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,True,0.9,0.5,0.98,0g+uQPcHSKC9FLdrf7r2h2NAvwEkEMBD9ALs7r3kHeCv/wX6F2H5+adPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('26', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,True,0.9,0.5,0.98,0g+uQPcHSKC9FLdrf7r2h2NAvwEkEMBD9ALs7r3kHeCv/wX6F2H5+adPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('27', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,True,0.9,0.5,0.98,0g+uQPcHSKC9FLdrf7r2h2NAvwEkEMBD9ALs7r3kHeCv/wX6F2H5+adPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('28', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,True,0.9,1,0.98,7WMuPe4maDB8F26bXrb2h2NAvwEkEMBD9ALs7sDwATwfwAoAQFG47pBPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('29', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,True,0.9,1,0.98,7WMuPe4maDB8F26bXrb2h2NAvwEkEMBD9ALs7sDwATwfwAoAQFG47pBPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('30', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,True,0.9,1,0.98,7WMuPe4maDB8F26bXrb2h2NAvwEkEMBD9ALs7sDwATwfwAoAQFG47pBPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('31', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,True,0.9,1,0.98,7WMuPe4maDB8F26bXrb2h2NAvwEkEMBD9ALs7sDwATwfwAoAQFG47pBPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('32', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,True,0.9,0.5,0.98,3kNOf/cSiGC9E6BTH7n2h2NAvwEkEMBD9ALsLv/oDqTfPwn9I+F696lPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('33', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,True,0.9,0.5,0.98,3kNOf/cSiGC9E6BTH7n2h2NAvwEkEMBD9ALsLv/oDqTfPwn9I+F696lPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('34', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,True,0.9,0.5,0.98,3kNOf/cSiGC9E6BTH7n2h2NAvwEkEMBD9ALsLv/oDqTfPwn9I+F696lPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('35', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,True,0.9,0.5,0.98,3kNOf/cSiGC9E6BTH7n2h2NAvwEkEMBD9ALsLv/oDqTfPwn9I+F696lPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('36', '[claRed;0 claBlue;1],claBlack,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,True,0.9,0.5,0.98,1WsOwPj3PzA9FsE/H732h2NAvwEkEMBD9ALsPjzn+gfw/f/4A6F6+sdPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('37', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,0,False,False,True,0.9,1,0.98,xYsuQPMOpDB/Ep2bQP0DffP/+gYmBIBF5MlnjvvkHRjQvQUazOB5A7cL3/gJD5SPgfxNwG7A6Q')]
    [TestCase('38', '[claRed;0 claNull;0.5 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,0,False,False,True,0.9,1,0.98,5H/eP/oT2NB+DrQbALz+fBfAvQIvHFAE6/AT7zvjFfjPvgX7B0F7/8IDn/gFCMg/gf5FEN+/7g')]
    procedure TestFillEllipseGradientRadial(const APoints, AModulateColor: string; AGradientScaleX, AGradientScaleY, AGradientPosX, AGradientPosY, AGradientRotationDegree, AGradientRotationCenterX, AGradientRotationCenterY, AGradientSkewX, AGradientSkewY, ADestLeftPercent, ADestTopPercent, ADestRightPercent, ADestBottomPercent, ACanvasScaleX, ACanvasScaleY, ACanvasOffsetX, ACanvasOffsetY, ACanvasRotationDeg: Single; AApplyClip, ABlending, ADrawBackgroundChess: Boolean; const ABitmapScale, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  '[claRed;0 claBlue;1],claWhite,0,0,1,1,0,0,1,1,1,1,0,0,0,False,True,1,1,False,0.98,31tgP/MYPMBBAdZjgH/toOsvgOjOa8CFzL+n//wQ7I+fN+/PoxD8BqnXzrkOlkeOuQ5sYL1A5A')]
    [TestCase('2',  '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0,1,1,1,1,0,0,0,False,True,1,1,False,0.98,n7PP//rOd18A+urj0IHrlg/gQOi+RyAFxr9X37sO9udP9+7Ss8B5A5UPAL0Jf1vPfBGXuMx+3Q')]
    [TestCase('3',  '[claRed;0 claBlue;1],claWhite,1,0,0,1,0,0,1,1,1,1,0,0,0,False,True,1,1,False,0.98,x/dfQPTw3/9AAOq3sAHypq/ff+zVi+CFxqY/DzsMANyPNujIiwA7Cq+fH/sOe8++eg6DRH0A5Q')]
    [TestCase('4',  '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0,0,1,1,1,1,0,0,0,False,True,1,1,False,0.98,7YNAv/EoZADCAtFLAL/touP/v+jSb/AFzr+7P30R6n+vN+/On4C8B66TDvkPmgve+A1jiC0B5g')]
    [TestCase('5',  '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0,0,1,1,1,1,0,0,0,False,True,1,1,False,0.98,kYvff/y+Tw9A+O//UALrlBMQAei7Q/CExL9DnzsO+Ps/9+7St0C5ApBTsL0IepeP/RGglFz+2w')]
    [TestCase('6',  '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0,1,1,1,1,0,0,0,False,True,1,1,False,0.98,yu+/QPPv4/9AAe+/MEL0q4+PP+7bnyCGxZsjb7oKBuwvNuXFexA7DLWz/zoQdcuOug2EXM3A5g')]
    [TestCase('7',  '[claRed;0 claBlue;1],claWhite,0,0,1,1,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,False,0.98,iNPBOBTV35899swTsAXq0rff+f+mR7FD0sznUDsg/pP/uuYCzL7/BRsMH8ACxMMNNgZVuGt69Q')]
    [TestCase('8',  '[claRed;0 claBlue;1],claWhite,1,1,0,0,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,False,0.98,avMR+hbKW6987faP8AbsyHMvuf6uI9GC0tTzcDocApjPuuoPGL9+AxyMn4ECuCvetglcKJz58Q')]
    [TestCase('9',  '[claRed;0 claBlue;1],claWhite,1,0,0,1,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,False,0.98,e88BOhXTr0/99eFDoMbq11O/eQK+a9FCzsOf4Hgd+NMfeugKoB7/BhiY/0ADuQNudgVYJBx69Q')]
    [TestCase('10', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,False,0.98,js+BuBPY+889+MP3b8Xp1Mf/OQCkT+FD0srjcPsg/ZP/uuUAuP5/Bhvszv8Cxq/ddQVToIt69g')]
    [TestCase('11', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,False,0.98,Y/tRuhfHP3986/+nQEfsxmP/eP6vG6GC0tX3UDobA5i/uusSKI/+Ahys74ECtkMOdwpeQHz58A')]
    [TestCase('12', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,False,0.98,fMdR+hTUu1/99+A7wIbq3DPPuQPHf7HCzL17EDgd9e/PeegLgA7/Bxa8D4EDtwtuNgRYPCw69g')]
    [TestCase('13', '[claRed;0 claBlue;1],claWhite,0,0,1,1,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,False,0.98,uNNwewMYvF/B86c3r4Lu90OPPf3Hh6DB18sXcT4ZDqyP+OsAMJC7B7tPf//+rMuduQp8jPz/5A')]
    [TestCase('14', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,False,0.98,lqcQPQXpF0/A7ujLnwbv8gc/vfvMY6BA1MOf8DwVHQAweOoJUDA5B6n7j4EDioueeQyFpKz/5A')]
    [TestCase('15', '[claRed;0 claBlue;1],claWhite,1,0,0,1,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,False,0.98,o4sgPQP4Y++A9NKT3wXv++/O/f7Sp+CA0LVHsLsVFhiAt+cA8G95Cqr7L8ECjk9OeQmAxFyA5w')]
    [TestCase('16', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,False,0.98,wNsQOwMj4I/B9JoX34Hu+E+fff3Hj9DB2M0vwf4ZC5ifOOz+LxC8B78nD//9s5+deQp6hAzA5A')]
    [TestCase('17', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,False,0.98,j59gfQXf8x7A7fXrfwfv8fsuffvNW3CA08KHkDwUIBAg+OkLWLA4B6Uf8IEEg7eeuQyGrJz/5A')]
    [TestCase('18', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,False,0.98,oHeAvQL1W/8A9tebb0bv/tvuPQDWv8BAzq4H4PoUFzQgd+b/z/+4C6YfcIEDiF8ueQh/2IzA6A')]
    [TestCase('19', '[claRed;0 claBlue;1],claWhite,0,0,1,1,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,wKMA/Pu3+04CA25TX0H3i6gOPfSlT5D95dRHcfwB8wsv++kwsFD2//prT0nr3BNP+QTfY879Fg')]
    [TestCase('20', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,tG/w/P2tv26BAG+L34H3i6gePfSlT5A95tRDoX0G5Qef/O07yLD0/O1rTwnr2hNfOQXgY879Fg')]
    [TestCase('21', '[claRed;0 claBlue;1],claWhite,1,0,0,1,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,v4tgvPy15/6BAmdPX0H3j6hO/fSnR8C95NQzob0E7u9O/Os1xID1/vdvT8nq2RNvOQXgZ779Fg')]
    [TestCase('22', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,wqvQe/u5B3+CA25HT0H3i6gOPfSlT5D95dVHMfwA9gvvOukuqLC2AP1rT0nr3BNP+QTfY879Fg')]
    [TestCase('23', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,smcgPf6rsz4BAG+X/4H3jKgeffSlT4A95tRD4T0H4gfvvO490GA0/OprT8nq2hNfOQXgY879Fg')]
    [TestCase('24', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,wo9QvPy26w4CA2RDP0H3kaRuPfWoR9A95NQv8f0E7+du/Os0yIA1//lvT4nq2BN/OQXgZ779Fg')]
    [TestCase('25', '[claRed;0 claBlue;1],claWhite,0,0,1,1,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,0kNhvPnjy57C+YA3f0T1lawOPvCVV1A96OCDUb0M4OceuecwgAD28xpUX8nq4ANfeQXVgw4+GA')]
    [TestCase('26', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,0huhfPfPp57B+JJrz0X1lawOPvCVV1A96OCDET4Q3+v+eus1nPB08AlUX8nq4ANfeQXVgw4+GA')]
    [TestCase('27', '[claRed;0 claBlue;1],claWhite,1,0,0,1,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,1R8hfPjcvz6C+IVDT0T1lawOPvCVV1A96OCDcf4O5tduOukwjMB18hRUX8nq4ANfeQXVgw4+GA')]
    [TestCase('28', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,0ktRPPro094C+nwrP0T1lawOPvCVV1A96OCDIb0L4Oe+uOYueDC29B1UX8nq4ANfeQXVgw4+GA')]
    [TestCase('29', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,0g+x/PbLn17B+JZzD0b1lawOPvCVV1A96OCDQf4Q3+tuO+w3pMC07wZUX8nq4ANfeQXVgw4+GA')]
    [TestCase('30', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,1xvxe/jdv14C+IM7/0P1lawOPvCVV1A96OCDwT4P6M+O+ugviOB18hZUX8nq4ANfeQXVgw4+GA')]
    [TestCase('31', '[claRed;0 claBlue;1],claWhite,0,0,1,1,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,zAuR+/jYtz5C/nI770P2lqT+vfGST2C9595vEXwG7f/ueektlPA2/v5rTwnr3Q9fOQXeZ979Fg')]
    [TestCase('32', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,ytPQ+/bKqy6B/Hljv0T2l6QOvvGSS1C9595rob0L4PPee+41yGD1+vJrT4nq2w9veQXeZ879Fg')]
    [TestCase('33', '[claRed;0 claBlue;1],claWhite,1,0,0,1,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,zOuA+/fSs77B/HE/z0P2mKQevvGTS1C9595nsX0K6ON+e+wwuAC2/PtrT0nq2g9/eQXfa879Fg')]
    [TestCase('34', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,zBeBe/nbu37C/nEvv0P2lqD+vfGST2C9599zwXsF8AN/OegrjED3/gFoTwnr3Q9POQXeZ979Fg')]
    [TestCase('35', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,ysvgu/bHp+4A/Htr70T2l6QevvGSS1C9595r8b0M3e9OfO831BA1+u9rT4nq2g9veQXfa879Fg')]
    [TestCase('36', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,zOdw+/fSs76B/G87j0P2mKQu/vGTS0C9591nEf4K6Ne+u+wwwPD1/PxrT0nq2Q+PeQXfa779Fg')]
    [TestCase('37', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,0.5,False,0.98,zOdw+/fSs76B/G87j0P2mKQu/vGTS0C9591nEf4K6Ne+u+wwwPD1/PxrT0nq2Q+PeQXfa779Fg')]
    [TestCase('38', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,1,False,0.98,xHs/vPfjCx/CAz7zDkH3mpxOfvORP0A959tLwb7wJSx++/NJJPKxAZh7P8np0RuveQXpV369FQ')]
    [TestCase('39', '[claRed;0 claBlue;1],claGreen,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,1,False,0.98,w/Ove/7Wy44CBlmz/sX2lqT+PfOPU5A95t5n8Xn29b8+d+hOHOKy+tiDPwnr2R9POQXiU569FQ')]
    [TestCase('40', '[claRed;0 claBlue;1],claGreen,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,0.5,False,0.98,zCMhe/vLk/7B/X0b/wX2lqj+vfGSV3A95993ofsN0COf+eYyvHB2+RxsT8nq3BNfeQXca979Fg')]
    [TestCase('41', '[claRed;0 claBlue;1],claWhite,1.15,-0.15,-0.15,1.15,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,1,False,0.98,woN/vPfjCx+CBEUHz0H3mZwuPvOQP1A959xTof3uJEiuOvNLFCIyAZV7P0nq1BuPOQXoU469FQ')]
    [TestCase('42', '[claRed;0 claBlue;1],claWhite,1.15,-0.15,-0.15,1.15,0,-0.15,1.15,0.85,1,1.3,0,-20,30,True,True,0.9,1,False,0.98,qJtuP/+sN3+BEy07/7z4gpgOPPyzdyB+4LfnYPzhNqAeO/JTECJxB5aDPwnr1CNfuQTqR469FQ')]
    [TestCase('43', '[claRed;0.3 claNull;0.5 claBlue;0.7],claWhite,1.15,-0.15,-0.15,1.15,0,-0.15,1.15,0.85,1,1.3,0,-20,30,True,True,0.9,1,False,0.98,zU8BO/LMj17AAHcX30L3ipC+PPuuhzC+377/QD0NxLNuPeckiCA2/Rt0T8nq1htf+QTiY669Fg')]
    [TestCase('44', '[claRed;0 claBlue;1],claWhite,0,0,1,1,0,0,1,1,1,1,0,0,0,False,True,1,1,True,0.98,z3se/+8TRGAAF3OrT/nwjvM/+wAGSO8G4t23HjniD6xvvAULBOF3DrqLHrcG3MOOvvwo7O2/6A')]
    [TestCase('45', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0,1,1,1,1,0,0,0,False,True,1,1,True,0.98,j899f/fIfw+/D4cnoDvvhBcAfAD2K0+G291n7vffGQAQfAUOFJH1CqXHb7oBxtfPQf9TQP294Q')]
    [TestCase('46', '[claRed;0 claBlue;1],claWhite,1,0,0,1,0,0,1,1,1,1,0,0,0,False,True,1,1,True,0.98,txMeAPHq56//FYf7j7v1lLvvugQNbB8H3MRPHnfdIvxPu/4F7NC2EcBXf/gGwke/f/w/zK1/6Q')]
    [TestCase('47', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0,0,1,1,1,1,0,0,0,False,True,1,1,True,0.98,3Z8OP+4jbLCAGG6PzzjxkOsP+wAKUB+H493LXrniDZh/vAUKAGE4D79LbrYH4YfePfwfEF4A6g')]
    [TestCase('48', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0,0,1,1,1,1,0,0,0,False,True,1,1,True,0.98,gqudP/m4U78+DoxDILzugh8gfADzJx8G2t1TrnffGxQAfAUPGBE1CqELEHsAwRNwAgBcHJ094A')]
    [TestCase('49', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0,1,1,1,1,0,0,0,False,True,1,1,True,0.98,ugtuAPDp56//FowD8Dv4mZevOgYTgE/H2rkzfvbbKQzguvsC3OD2E8ZrXzgIvEePv/tA5P0/6w')]
    [TestCase('50', '[claRed;0 claBlue;1],claWhite,0,0,1,1,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,True,0.98,uC/eQPkAeKA+G64TcP39iH+w+wP4h08G67yLfrnoC0jAfAEYSGC6Cbm3TngCBkwPgfdKDN5/6Q')]
    [TestCase('51', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,True,0.98,mksuAvz09899EtiPsL7/fjcAuwIAYH9F68SbrrjkD0ygfAUllGD5Bro7v3kC+7ffQftSfB6/5Q')]
    [TestCase('52', '[claRed;0 claBlue;1],claWhite,1,0,0,1,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,True,0.98,rCceAvr9T2A+G8JDYH7+jReQewYQpH8F57NDHvflBYTwOwMgHMC5CrVHLzkD+49vwfZOeI5/6Q')]
    [TestCase('53', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,True,0.98,vyeegPgClNA+HaX3L339io/QOwT2j38G67qLrnnpCkTQfAAVNKA6Crmb/rcCCTjfwPZJ9P0/6g')]
    [TestCase('54', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,True,0.98,lFNugvzy2599EOGnAD8AfCfQegICWE9F68abfrjjEEyQfAYopDB5BrpXH3oC+M8PAvxUlO7+5A')]
    [TestCase('55', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,True,0.98,rR9uwvn+V3D+HMI7gH7+kvuvuwcZvF9F5a4jPrblAqCQOwMh/L+5C7RnP3kD+ZdvgfVOkJ4/6g')]
    [TestCase('56', '[claRed;0 claBlue;1],claWhite,0,0,1,1,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,True,0.98,0Qeef/IRPMA9GJFrH/r4e3fA+gT+Z5+H6NMDvznkBBSQewYdeFC5Crpznrf9DPgugv45pF1A5Q')]
    [TestCase('57', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,True,0.98,r9s9AfTil888E9H/D375djtwugMDQJ/G5MuPPjjgEmhA+wQnmPA2Cqgfv3kC6r8vQgBCvA1A5Q')]
    [TestCase('58', '[claRed;0 claBlue;1],claWhite,1,0,0,1,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,True,0.98,u8NNAfLx32/9GLzHT335fyPwugYJiN8G4b03DnfgDICQOgIeOBB3DakfX7kB7YPfQf083K0A6A')]
    [TestCase('59', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,True,0.98,2BM+P/IbYPA9GYNLT/n4fIPQOgX9b89H6dQfH/rkAASguwYbdNC5Cr1PLrf8E9A+Qv43nG1A5Q')]
    [TestCase('60', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,True,0.98,qNONQfTXc488Et8f4H75dS9gOgMEOG8G5Ml37jffFXwwewQpnHA2CqRDL3oD4+cvggBDxP1/5Q')]
    [TestCase('61', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,True,0.98,uautgfHu2389GsDP3735gg8Q+wcMnL/G3rX3LfbfDKAwugAcFKC2DqVDrzkC6JO/Qfw88N1A6Q')]
    [TestCase('62', '[claRed;0 claBlue;1],claWhite,0,0,1,1,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,xkMOwPgBWBD+FajDLzv3imPA/gEhHKAD8/j/vjzhHZQfvgHsDzF7/bFPP7gH+/OPAvxDKE+A8Q')]
    [TestCase('63', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,uhP+wPr2H0B9E6n7r3v3imPA/gEhGJBD8/j77r3lDpCPfwX2K5E5+qRPT3gH+vOfAvxELE+A8Q')]
    [TestCase('64', '[claRed;0 claBlue;1],claWhite,1,0,0,1,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,xS9uwPn+R9B9FaHDLzv3jmMAvwIjFMAD8vfr7j3kF3w/vwPwI2F6/K5PTzgH+POvAvxELE+A8Q')]
    [TestCase('65', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,yE/ev/gDZEB+Fqi3Hzv3imPA/gEhHKAD8/j/fjzgIJTfvQDpB4E7/rRPP7gH+/OPAvxDKE+A8Q')]
    [TestCase('66', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,twsuQfv0ExD9EqkHwHv3i2PA/gEhGJBD8/j7Lr7mC5Dffwb4LzF5+aFPT3gH+fOfAvxELE+A8Q')]
    [TestCase('67', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,xzNegPkASOC9FZ6zDzv3kGMg/wIkENCD8ffnLn7kGHBfvwPvJ2G6/LBPT/gG9/OvAvxEMD/A8Q')]
    [TestCase('68', '[claRed;0 claBlue;1],claWhite,0,0,1,1,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,1T9+QPcLYKB9FbJr/7r2h2NAvwEkEMBD9ALs7nzhFNxP/QL49/C6/LlPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('69', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,1RO+APX3P5C8FMSfP7z2h2NAvwEkEMBD9ALsrv3kFOAvvwb+E/E5+alPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('70', '[claRed;0 claBlue;1],claWhite,1,0,0,1,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,2Bs+APYEVEA9FLd3z7r2h2NAvwEkEMBD9ALsDr7jGsyffgT4A8H6+rRPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('71', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,1UduwPcQaNC9Fa5fr7r2h2NAvwEkEMBD9ALsvrzgFNzfPAL37zA7/b1Pb3gH+u9/gvxCPD/A8Q')]
    [TestCase('72', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,1QvOgPTzN2B8FMirj7z2h2NAvwEkEMBD9ALs3r3lE+CPfwf/G8F5+KVPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('73', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,2RMOAPYFWFC9E7Vzb7r2h2NAvwEkEMBD9ALsXv7jHci/fgT4A+H6+rVPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('74', '[claRed;0 claBlue;1],claWhite,0,0,1,1,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,0CeuwPcJTAB+F7R/T7v2hmcg/wElEMBD9AH0TnzgILCPfQH09/C6/rNPP7gH/O9/AvxDLF/A8Q')]
    [TestCase('75', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,z/PtwPX7Q+C8FbynH7z2h2cw/wEmELBD9ADw3r3lE6R/fwb8J2F5+6dPT3gH+u+fAvxEME/A8Q')]
    [TestCase('76', '[claRed;0 claBlue;1],claWhite,1,0,0,1,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,0AeewPYCSID9FbOHH/v2iGdA/wEmEKBD9ADs7n3kG5AffwT3GxE6/bBPTzgH+O+fQvxEME/A8Q')]
    [TestCase('77', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,0TOeQPgMUED+F7N3H7v2hmcgvwElEMBD9AH0/nvfI7AffQDy60B7/7ZPP7gH/O9/AvxDLF/A8Q')]
    [TestCase('78', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,zuf9QPX4P6B8Fb2zT7z2h2cw/wEmELBD9ADsLr7mEKDvvwf+MxG5+qVPTzgH+e+fQvxEME/A8Q')]
    [TestCase('79', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,0QeOgPYDSIC9FbF/7/r2iGdA/wEmEKBD9ADoTj7lG4hf/wT3HwF6/bFPT/gG+O+vQvxEMD/A8Q')]
    [TestCase('80', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,0.5,True,0.98,0QeOgPYDSIC9FbF/7/r2iGdA/wEmEKBD9ADoTj7lG4hf/wT3HwF6/bFPT/gG+O+vQvxEMD/A8Q')]
    [TestCase('81', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,1,True,0.98,0+OdP+0G8H/8G5D77vn2iWtAfwIoEIAD9P7nLj/pHAQvPwLzP3G2+qBPL7gG9e/fwvtHKD+A8Q')]
    [TestCase('82', '[claRed;0 claBlue;1],claGreen,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,1,True,0.98,0lsOP/T5s+88Hqu73n72hXPwPgImJNAD8wEAX7ru7Jvv+vb3N2G38+BXL/gH/Pd/gvtAKG+A8Q')]
    [TestCase('83', '[claRed;0 claBlue;1],claGreen,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,0.5,True,0.98,0D8+APr8K7D9Fr9fX732hmsg/wElHMDD8wH43vvnA9A/Pf/5G3H6+dFTT7gH+/N/AvxBMF+A8Q')]
    [TestCase('84', '[claRed;0 claBlue;1],claWhite,1.15,-0.15,-0.15,1.15,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,1,True,0.98,0evdf+0G7H+8HJYLr/r2iGsgfwInEJAD9P/vHn7nGiRffgH1M6E2+p1PLzgH9++/wvtGJE+A8Q')]
    [TestCase('85', '[claRed;0 claBlue;1],claWhite,1.15,-0.15,-0.15,1.15,0,-0.15,1.15,0.85,1,1.3,0,-20,30,True,True,0.9,1,True,0.98,sBsuAPT0y698G3qzL3v4k1NQ/gMYHJBD8OrjXv3jGhgfPv7oMwE3+p1PLzgH+POvgvtFHE9A8Q')]
    [TestCase('86', '[claRed;0.3 claNull;0.5 claBlue;0.7],claWhite,1.15,-0.15,-0.15,1.15,0,-0.15,1.15,0.85,1,1.3,0,-20,30,True,True,0.9,1,True,0.98,ux/ugf/9TyB/Eqrrz7z3lUfQ/gQbGNDD7vDP3rzhH9Bf/wL88xA7AMNTTzgH9/OvAvxDLD+A8Q')]
    procedure TestFillEllipseGradientStartEndPoint(const APoints, AModulateColor: string; AStartX, AStartY, AStopX, AStopY, ADestLeftPercent, ADestTopPercent, ADestRightPercent, ADestBottomPercent, ACanvasScaleX, ACanvasScaleY, ACanvasOffsetX, ACanvasOffsetY, ACanvasRotationDeg: Single; AApplyClip, ABlending: Boolean; const ABitmapScale, AOpacity: Single; ADrawBackgroundChess: Boolean; AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  '[claRed;0 claBlue;1],claWhite,Linear,False,1,1,0.98,w3Pev/wLbBA+GdD/Lzz+lAOAPAoXsK/E7NKHjnrnHoyQvQEXNHA7BqL3LrgKD6TvAfw1ZH5B6g')]
    [TestCase('2',  '[$7FFF0000;0 $7F0000FF;1],claWhite,Linear,False,1,1,0.98,yU+ugP4FiFB+FNEDAHz6jTPg/QUd4D+E8Oq7/jvkHEyQfgQImIA8A7IjT/gIBcgvQvw70N4A7g')]
    [TestCase('3',  '[claRed;0 claBlue;1],claWhite,Linear,False,1,0.5,0.98,yU+ugP4FiFB+FNEDAHz6jTPg/QUd4D+E8Oq7/jvkHEyQfgQJmIA8A7IjT/gIBcgvQvw70N4A7g')]
    [TestCase('4',  '[claRed;0 claBlue;1],claWhite,Linear,True,1,1,0.98,w3Pev/wLbBA+GdD/Lzz+lAOAPAoXsK/E7NKHjnrnHoyQvQEXNHA7BqL3LrgKD6TvAfw1ZH5B6g')]
    [TestCase('5',  '[$7FFF0000;0 $7F0000FF;1],claWhite,Linear,True,1,1,0.98,yU+ugP4FiFB+FNEDAHz6jTPg/QUd4D+E8Oq7/jvkHEyQfgQImIA8A7IjT/gIBcgvQvw70N4A7g')]
    [TestCase('6',  '[claRed;0 claBlue;1],claWhite,Linear,True,1,0.5,0.98,yU+ugP4FiFB+FNEDAHz6jTPg/QUd4D+E8Oq7/jvkHEyQfgQJmIA8A7IjT/gIBcgvQvw70N4A7g')]
    [TestCase('7',  '[claRed;0 claBlue;1],claWhite,Linear,False,2,1,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALsfrvgFti//wP4gzH8Bd7LfngEuT/gPwOO1A196w')]
    [TestCase('8',  '[$7FFF0000;0 $7F0000FF;1],claWhite,Linear,False,2,1,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALsbvzgGPSvvwX5P+H8AtAPb/gF2hcwwf9oiK6+7g')]
    [TestCase('9',  '[claRed;0 claBlue;1],claWhite,Linear,False,-2,0.5,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALsXv3gGgyAfwf6/5A9AMJPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('10', '[claRed;0 claBlue;1],claWhite,Linear,True,0.5,1,0.98,vN/t//rnV5C+E7/f77z2h2NAvwEkEMBD9ALsXv3gGgyAfwf6/5A9AMJPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('11', '[$7FFF0000;0 $7F0000FF;1],claWhite,Linear,True,-1,1,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALsXv3gGgyAfwf6/5A9AMJPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('12', '[claRed;0 claBlue;1],claWhite,Linear,True,3,0.5,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALsXv3gGgyAfwf6/5A9AMI7jzgI/Pu/gvs+OC8A8Q')]
    [TestCase('13', '[claRed;0.2 claBlue;0.8],claWhite,Linear,False,1,1,0.98,xIc+v/0IbHD+GMvz7zv/m9Ov/AsesK+E685z7rnmG6CgPQAaJCD7Bp7L3ncMEqxvAfwwPK4B6g')]
    [TestCase('14', '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Linear,False,1,1,0.98,yldOAP8EiIA+FM/77/v6kRvwvQYh4D8E8OizrrvjG1iQvgMKkGB8A7APH/gJBsz/gfw5vP7A7Q')]
    [TestCase('15', '[claRed;0.2 claBlue;0.8],claWhite,Linear,False,1,0.5,0.98,yldOwP4EiIB+FM/77/v6kRvwvQYh4D8E8OizrrvjG1iQvgMKkFB8A7APH/gJBsz/Qfw5vP7A7Q')]
    [TestCase('16', '[claRed;0.2 claBlue;0.8],claWhite,Linear,True,1,1,0.98,xIc+v/0IbHD+GMvz7zv/m9Ov/AsesK+E685z7rnmG6CgPQAaJCD7Bp7L3ncMEqxvAfwwPK4B6g')]
    [TestCase('17', '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Linear,True,1,1,0.98,yldOAP8EiIA+FM/77/v6kRvwvQYh4D8E8OizrrvjG1iQvgMKkGB8A7APH/gJBsz/gfw5vP7A7Q')]
    [TestCase('18', '[claRed;0.2 claBlue;0.8],claWhite,Linear,True,1,0.5,0.98,yldOwP4EiIB+FM/77/v6kRvwvQYh4D8E8OizrrvjG1iQvgMKkFB8A7APH/gJBsz/Qfw5vP7A7Q')]
    [TestCase('19', '[claRed;0.2 claBlue;0.8],claWhite,Linear,False,2,1,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALs3nvgFOSPPwX9n+F7BdrjnjgEuXOgAAabiD285w')]
    [TestCase('20', '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Linear,False,2,1,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALsnrzgF/iPfwb8T7G8As4bf7gF2jOQQQFvYD6+7A')]
    [TestCase('21', '[claRed;0 claBlue;1],claWhite,Radial,False,1,1,0.98,z3eOv/sKdKA+G9EboLz7f/cvfAcWoP+D79mPLrziJhRAfQkUvFD8ALWrLzoJE6Bvwv1IJN9A6g')]
    [TestCase('22', '[$7FFF0000;0 $7F0000FF;1],claWhite,Radial,False,1,1,0.98,z09+wP0FjJB+FdIPQDz5gyuwfQQd2N8D8u2/zvzhIBBgfggH3PB8ALt/TzkIB8h/Qv1FMI8A7g')]
    [TestCase('23', '[claRed;0 claBlue;1],claWhite,Radial,False,1,0.5,0.98,z09+wP0FjJB+FdIPQDz5gyuwfQQd2N8D8u2/zvzhIBBgfggH3PC8ALt7TzkIB8R/Qv1FMI8A7g')]
    [TestCase('24', '[claRed;0 claBlue;1],claWhite,Radial,True,1,1,0.98,z3eOv/sKdKA+G9EboLz7f/cvfAcWoP+D79mPLrziJhRAfQkUvFD8ALWrLzoJE6Bvwv1IJN9A6g')]
    [TestCase('25', '[$7FFF0000;0 $7F0000FF;1],claWhite,Radial,True,1,1,0.98,z09+wP0FjJB+FdIPQDz5gyuwfQQd2N8D8u2/zvzhIBBgfggH3PB8ALt/TzkIB8h/Qv1FMI8A7g')]
    [TestCase('26', '[claRed;0 claBlue;1],claWhite,Radial,True,1,0.5,0.98,z09+wP0FjJB+FdIPQDz5gyuwfQQd2N8D8u2/zvzhIBBgfggH3PC8ALt7TzkIB8R/Qv1FMI8A7g')]
    [TestCase('27', '[claRed;0 claBlue;1],claWhite,Radial,False,2,1,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALsbnvdBcg/PwX9X7G7BdC/njgHvDsg/wKNJB5+8A')]
    [TestCase('28', '[$7FFF0000;0 $7F0000FF;1],claWhite,Radial,False,2,1,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALsbjzfEOxvfwb8L6H8AskLfzgH2xfQwP9nsC4/8Q')]
    [TestCase('29', '[claRed;0 claBlue;1],claWhite,Radial,False,-2,0.5,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALsXv3gGgyAfwf6/5A9AMJPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('30', '[claRed;0 claBlue;1],claWhite,Radial,True,0.5,1,0.98,xuOt//rwz5B+F6Tvv7v2h2NAvwEkEMBD9ALsXv3gGgyAfwf6/5A9AMJPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('31', '[$7FFF0000;0 $7F0000FF;1],claWhite,Radial,True,-1,1,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALsXv3gGgyAfwf6/5A9AMJPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('32', '[claRed;0 claBlue;1],claWhite,Radial,True,3,0.5,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALsXv3gGgyAfwf6/5A9AMI7jzgI/Pu/gvs+OB8A8Q')]
    [TestCase('33', '[claRed;0.2 claBlue;0.8],claWhite,Radial,False,1,1,0.98,03/efvwGfHD/G8wrsDz7fM/fOwgfpO8D79qHjrzhJxAgvQoT3HA8/7bXr/oIFqRfAv5PPM9A6Q')]
    [TestCase('34', '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Radial,False,1,1,0.98,0VMuQP4CkAC/Fc8XQPz4gheQ/QQi2N/D8e67/nzhIQxQPgkH7AC9/7yTjzkICMhvQv1JPH+A7Q')]
    [TestCase('35', '[claRed;0.2 claBlue;0.8],claWhite,Radial,False,1,0.5,0.98,0VMuQP4DkAD/Fc8XQPz4gheQ/QQi2N/D8e67/nzhIQxQPgkH7AC9/7yTjzkICchvQv1JPH+A7Q')]
    [TestCase('36', '[claRed;0.2 claBlue;0.8],claWhite,Radial,True,1,1,0.98,03/efvwGfHD/G8wrsDz7fM/fOwgfpO8D79qHjrzhJxAgvQoT3HA8/7bXr/oIFqRfAv5PPM9A6Q')]
    [TestCase('37', '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Radial,True,1,1,0.98,0VMuQP4CkAC/Fc8XQPz4gheQ/QQi2N/D8e67/nzhIQxQPgkH7AC9/7yTjzkICMhvQv1JPH+A7Q')]
    [TestCase('38', '[claRed;0.2 claBlue;0.8],claWhite,Radial,True,1,0.5,0.98,0VMuQP4DkAD/Fc8XQPz4gheQ/QQi2N/D8e67/nzhIQxQPgkH7AC9/7yTjzkICchvQv1JPH+A7Q')]
    [TestCase('39', '[claRed;0.2 claBlue;0.8],claWhite,Radial,False,2,1,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALsXjvdAdTPvgT8U6G7BMfPfjgKtoPQPwWfBM698A')]
    [TestCase('40', '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Radial,False,2,1,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALsXvzeDvAv/wX7J5F8AsQPb7gI2DsgwQBxoA4/8Q')]
    [TestCase('41', '[claRed;0 claBlue;1],claBlack,Linear,False,1,1,0.98,1TNugfYOgDC9GN2jf73/lz/QPAHxrw/F7bpH7/joAeyv/AQR0CC6ArvjPvkCC9hvAf043O5B7A')]
    [TestCase('42', '[$7FFF0000;0 $7F0000FF;1],claBlack,Linear,False,1,1,0.98,0i9ugfsHkOA9FNjXrzz7j08QfgEL3G8E8d4bL/vkDvwfPgYG5NB7Ab4bzzgFA+D/wfw9DB8B7w')]
    [TestCase('43', '[claRed;0 claBlue;1],claBlack,Linear,False,1,0.5,0.98,0i9uQfsHkOA9FNjXrzz7j08AfgEK3G8E8d4bH/vkDvwfPgYG5NB7Ab4b3zgFA+D/wfw9DB8B7w')]
    [TestCase('44', '[claRed;0 claBlue;1],claBlack,Linear,True,1,1,0.98,1TNugfYOgDC9GN2jf73/lz/QPAHxrw/F7bpH7/joAeyv/AQR0CC6ArvjPvkCC9hvAf043O5B7A')]
    [TestCase('45', '[$7FFF0000;0 $7F0000FF;1],claBlack,Linear,True,1,1,0.98,0i9ugfsHkOA9FNjXrzz7j08QfgEL3G8E8d4bL/vkDvwfPgYG5NB7Ab4bzzgFA+D/wfw9DB8B7w')]
    [TestCase('46', '[claRed;0 claBlue;1],claBlack,Linear,True,1,0.5,0.98,0i9uQfsHkOA9FNjXrzz7j08AfgEK3G8E8d4bH/vkDvwfPgYG5NB7Ab4b3zgFA+D/wfw9DB8B7w')]
    [TestCase('47', '[claRed;0 claBlue;1],claBlack,Linear,False,2,1,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALsPjzeHrCPwAf4PyE8AeZfDvr/3ltP/fxsBN78/A')]
    [TestCase('48', '[$7FFF0000;0 $7F0000FF;1],claBlack,Linear,False,2,1,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALszrzfHNwPgAf5H9G8ANTXPrkD7Kfv//xXoI5+9w')]
    [TestCase('49', '[claRed;0 claBlue;1],claBlack,Linear,False,-2,0.5,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALsXv3gGgyAfwf6/5A9AMJPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('50', '[claRed;0 claBlue;1],claBlack,Linear,True,0.5,1,0.98,sT8ufwLPv8C9Fb+nT7z2h2NAvwEkEMBD9ALsXv3gGgyAfwf6/5A9AMJPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('51', '[$7FFF0000;0 $7F0000FF;1],claBlack,Linear,True,-1,1,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALsXv3gGgyAfwf6/5A9AMJPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('52', '[claRed;0 claBlue;1],claBlack,Linear,True,3,0.5,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALsXv3gGgyAfwf6/5A9AMJTbzgH+vOPwvxCNC8A8Q')]
    [TestCase('53', '[claRed;0.2 claBlue;0.8],claBlack,Linear,False,1,1,0.98,1TNugfYOgDC9GN2jf73/lz/QPAHxrw/F7bpH7/joAeyv/AQR0CC6ArvjPvkCC9hvAf043O5B7A')]
    [TestCase('54', '[$7FFF0000;0.2 $7F0000FF;0.8],claBlack,Linear,False,1,1,0.98,0i9ugfsHkOA9FNjXrzz7j08QfgEL3G8E8d4bL/vkDvwfPgYG5NB7Ab4bzzgFA+D/wfw9DB8B7w')]
    [TestCase('55', '[claRed;0.2 claBlue;0.8],claBlack,Linear,False,1,0.5,0.98,0i9uQfsHkOA9FNjXrzz7j08AfgEK3G8E8d4bH/vkDvwfPgYG5NB7Ab4b3zgFA+D/wfw9DB8B7w')]
    [TestCase('56', '[claRed;0.2 claBlue;0.8],claBlack,Linear,True,1,1,0.98,1TNugfYOgDC9GN2jf73/lz/QPAHxrw/F7bpH7/joAeyv/AQR0CC6ArvjPvkCC9hvAf043O5B7A')]
    [TestCase('57', '[$7FFF0000;0.2 $7F0000FF;0.8],claBlack,Linear,True,1,1,0.98,0i9ugfsHkOA9FNjXrzz7j08QfgEL3G8E8d4bL/vkDvwfPgYG5NB7Ab4bzzgFA+D/wfw9DB8B7w')]
    [TestCase('58', '[claRed;0.2 claBlue;0.8],claBlack,Linear,True,1,0.5,0.98,0i9uQfsHkOA9FNjXrzz7j08AfgEK3G8E8d4bH/vkDvwfPgYG5NB7Ab4b3zgFA+D/wfw9DB8B7w')]
    [TestCase('59', '[claRed;0.2 claBlue;0.8],claBlack,Linear,False,2,1,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALsPjzeHrCPwAf4PyE8AeZfDvr/3ltP/fxsBN78/A')]
    [TestCase('60', '[$7FFF0000;0.2 $7F0000FF;0.8],claBlack,Linear,False,2,1,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALszrzfHNwPgAf5H9G8ANTXPrkD7Kfv//xXoI5+9w')]
    [TestCase('61', '[claRed;0 claBlue;1],claBlack,Radial,False,1,1,0.98,1TNugfYOgDC9GN2jf73/lz/QPAHxrw/F7bpH7/joAeyv/AQR0CC6ArvjPvkCC9hvAf043O5B7A')]
    [TestCase('62', '[$7FFF0000;0 $7F0000FF;1],claBlack,Radial,False,1,1,0.98,0i9ugfsHkOA9FNjXrzz7j08QfgEL3G8E8d4bL/vkDvwfPgYG5NB7Ab4bzzgFA+D/wfw9DB8B7w')]
    [TestCase('63', '[claRed;0 claBlue;1],claBlack,Radial,False,1,0.5,0.98,0i9uQfsHkOA9FNjXrzz7j08AfgEK3G8E8d4bH/vkDvwfPgYG5NB7Ab4b3zgFA+D/wfw9DB8B7w')]
    [TestCase('64', '[claRed;0 claBlue;1],claBlack,Radial,True,1,1,0.98,1TNugfYOgDC9GN2jf73/lz/QPAHxrw/F7bpH7/joAeyv/AQR0CC6ArvjPvkCC9hvAf043O5B7A')]
    [TestCase('65', '[$7FFF0000;0 $7F0000FF;1],claBlack,Radial,True,1,1,0.98,0i9ugfsHkOA9FNjXrzz7j08QfgEL3G8E8d4bL/vkDvwfPgYG5NB7Ab4bzzgFA+D/wfw9DB8B7w')]
    [TestCase('66', '[claRed;0 claBlue;1],claBlack,Radial,True,1,0.5,0.98,0i9uQfsHkOA9FNjXrzz7j08AfgEK3G8E8d4bH/vkDvwfPgYG5NB7Ab4b3zgFA+D/wfw9DB8B7w')]
    [TestCase('67', '[claRed;0 claBlue;1],claBlack,Radial,False,2,1,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALsPjzeHrCPwAf4PyE8AeZfDvr/3ltP/fxsBN78/A')]
    [TestCase('68', '[$7FFF0000;0 $7F0000FF;1],claBlack,Radial,False,2,1,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALszrzfHNwPgAf5H9G8ANTXPrkD7Kfv//xXoI5+9w')]
    [TestCase('69', '[claRed;0 claBlue;1],claBlack,Radial,False,-2,0.5,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALsXv3gGgyAfwf6/5A9AMJPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('70', '[claRed;0 claBlue;1],claBlack,Radial,True,0.5,1,0.98,sT8ufwLPv8C9Fb+nT7z2h2NAvwEkEMBD9ALsXv3gGgyAfwf6/5A9AMJPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('71', '[$7FFF0000;0 $7F0000FF;1],claBlack,Radial,True,-1,1,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALsXv3gGgyAfwf6/5A9AMJPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('72', '[claRed;0 claBlue;1],claBlack,Radial,True,3,0.5,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALsXv3gGgyAfwf6/5A9AMJTbzgH+vOPwvxCNC8A8Q')]
    [TestCase('73', '[claRed;0.2 claBlue;0.8],claBlack,Radial,False,1,1,0.98,1TNugfYOgDC9GN2jf73/lz/QPAHxrw/F7bpH7/joAeyv/AQR0CC6ArvjPvkCC9hvAf043O5B7A')]
    [TestCase('74', '[$7FFF0000;0.2 $7F0000FF;0.8],claBlack,Radial,False,1,1,0.98,0i9ugfsHkOA9FNjXrzz7j08QfgEL3G8E8d4bL/vkDvwfPgYG5NB7Ab4bzzgFA+D/wfw9DB8B7w')]
    [TestCase('75', '[claRed;0.2 claBlue;0.8],claBlack,Radial,False,1,0.5,0.98,0i9uQfsHkOA9FNjXrzz7j08AfgEK3G8E8d4bH/vkDvwfPgYG5NB7Ab4b3zgFA+D/wfw9DB8B7w')]
    [TestCase('76', '[claRed;0.2 claBlue;0.8],claBlack,Radial,True,1,1,0.98,1TNugfYOgDC9GN2jf73/lz/QPAHxrw/F7bpH7/joAeyv/AQR0CC6ArvjPvkCC9hvAf043O5B7A')]
    [TestCase('77', '[$7FFF0000;0.2 $7F0000FF;0.8],claBlack,Radial,True,1,1,0.98,0i9ugfsHkOA9FNjXrzz7j08QfgEL3G8E8d4bL/vkDvwfPgYG5NB7Ab4bzzgFA+D/wfw9DB8B7w')]
    [TestCase('78', '[claRed;0.2 claBlue;0.8],claBlack,Radial,True,1,0.5,0.98,0i9uQfsHkOA9FNjXrzz7j08AfgEK3G8E8d4bH/vkDvwfPgYG5NB7Ab4b3zgFA+D/wfw9DB8B7w')]
    [TestCase('79', '[claRed;0.2 claBlue;0.8],claBlack,Radial,False,2,1,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALsPjzeHrCPwAf4PyE8AeZfDvr/3ltP/fxsBN78/A')]
    [TestCase('80', '[$7FFF0000;0.2 $7F0000FF;0.8],claBlack,Radial,False,2,1,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALszrzfHNwPgAf5H9G8ANTXPrkD7Kfv//xXoI5+9w')]
    procedure TestFillEllipseGradientWithChessBackground(const APoints, AModulateColor: string; AGradientStyle: TGradientStyle; ABlending: Boolean; const AScale, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,0Dsy+fzE776+7qtXH4fxXOiOvPqVqxB+3dKX4bwmrgP/POwGkF97AU8UP8Xr8tO+dwUDqL29DA')]
    procedure TestFillEllipseSolid(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,0Dsy+fzE686+7qtXD8fxXOSOvPqVpwB+3dKX4bwmrgP/POwGkF97AU8UP8Xr8tO+dwUCqL29DA')]
    procedure TestFillEllipseSolid2(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,0UNSOADFW99757kLb8j0k6je/fCVR2C95eJvYb0no2+/++caPL95+VtYb8nq3wdfeQXWf/79Fw')]
    procedure TestFillEllipseSolid3(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,sFOitgWmE2975LVHT8bvL9x+Og/AC6G+z8K7sMAq0fP9P/ntDxCAD0U0PwD+8iteOwBVPN56Bg')]
    procedure TestFillEllipseSolid4(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,yxuytwHIgy/857UrH4jtHyBP+wWX+7D+0sa3Yfwns4PuPfP4x698Czy43j/76vMtOAJLOD07BA')]
    procedure TestFillEllipseSolid5(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,sFOitgWmE2975LVHT8bvL9x+Og/AC6G+z8K7sMAq0fP9P/ntDxCAD0U0PwD+8iteOwBVPN56Bg')]
    procedure TestFillEllipseSolid6(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  '300,300,1,2,-50,-100,60,-200,-50,660,343,1,true,0.98,+9+e/uz0w7B/HTqfbnj9XexuO/68fxC/2q2fAL/z6pc+QAkBvNE09G6nX3/xZE/AehR83Mt56w')]
    [TestCase('2',  '300,300,1,2,-50,-100,60,-200,-50,660,343,1,false,0.98,+9+e/uz0w7B/HTqfbnj9XexuO/68fxC/2q2fAL/z6pc+QAkBvNE09G6nX3/xZE/AehR83Mt56w')]
    [TestCase('3',  '300,300,1,2,-50,-100,60,-200,-50,660,343,0.5,true,0.98,6JugfPLaj4+ACW0PL3/5ecy+PPepazB+4ccPMX4MywsfvvcLjGA39uZ/XwTuoqsf+gwpLO27AQ')]
    [TestCase('4',  '300,300,1,2,-50,-100,60,-200,-50,660,343,0.5,false,0.98,6JugfPLaj4+ACW0PL3/5ecy+PPepazB+4ccPMX4MywsfvvcLjGA39uZ/XwTuoqsf+gwpLO27AQ')]
    [TestCase('5',  '300,300,1,2,-50,-100,60,-200,-50,660,343,1,true,0.98,+9+e/uz0w7B/HTqfbnj9XexuO/68fxC/2q2fAL/z6pc+QAkBvNE09G6nX3/xZE/AehR83Mt56w')]
    [TestCase('6',  '300,300,1,2,-50,-100,60,-200,-50,660,343,1,false,0.98,+9+e/uz0w7B/HTqfbnj9XexuO/68fxC/2q2fAL/z6pc+QAkBvNE09G6nX3/xZE/AehR83Mt56w')]
    [TestCase('7',  '300,300,1,2,-50,-100,60,-200,-50,660,343,0.5,true,0.98,6JugfPLaj4+ACW0PL3/5ecy+PPepazB+4ccPMX4MywsfvvcLjGA39uZ/XwTuoqsf+gwpLO27AQ')]
    [TestCase('8',  '300,300,1,2,-50,-100,60,-200,-50,660,343,0.5,false,0.98,6JugfPLaj4+ACW0PL3/5ecy+PPepazB+4ccPMX4MywsfvvcLjGA39uZ/XwTuoqsf+gwpLO27AQ')]
    [TestCase('9',  '300,300,1,2,-50,-100,60,0,0,200,200,1,true,0.98,3T8y+vTBd47B+pRfP0X1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('10', '300,300,1,2,-50,-100,30,0,0,200,200,1,false,0.98,678v/ePpH56FGvYKfnn1lawOPvCVV1A96OCDYT4dod8uPOsOYAA48jlUX8nq4ANfeQXVgw4+GA')]
    [TestCase('11', '300,300,1,2,-50,-100,30,0,0,200,200,0.5,true,0.98,4Avh++3UO34DCEvHvn/1lawOPvCVV1A96OCD4X0hpjMffOgS3A859UxUX8nq4ANfeQXVgw4+GA')]
    [TestCase('12', '300,300,1,2,-50,-100,30,0,0,200,200,0.5,false,0.98,4Avh++3UO34DCEvHvn/1lawOPvCVV1A96OCD4X0hpjMffOgS3A859UxUX8nq4ANfeQXVgw4+GA')]
    [TestCase('13', '300,300,1,2,-50,-100,30,0,0,200,200,1,true,0.98,678v/ePpH56FGvYKfnn1lawOPvCVV1A96OCDYT4dod8uPOsOYAA48jlUX8nq4ANfeQXVgw4+GA')]
    [TestCase('14', '300,300,1,2,-50,-100,30,0,0,200,200,1,false,0.98,678v/ePpH56FGvYKfnn1lawOPvCVV1A96OCDYT4dod8uPOsOYAA48jlUX8nq4ANfeQXVgw4+GA')]
    [TestCase('15', '300,300,1,2,-50,-100,30,0,0,200,200,0.5,true,0.98,4Avh++3UO34DCEvHvn/1lawOPvCVV1A96OCD4X0hpjMffOgS3A859UxUX8nq4ANfeQXVgw4+GA')]
    [TestCase('16', '300,300,1,2,-50,-100,30,0,0,200,200,0.5,false,0.98,4Avh++3UO34DCEvHvn/1lawOPvCVV1A96OCD4X0hpjMffOgS3A859UxUX8nq4ANfeQXVgw4+GA')]
    [TestCase('17', '300,300,1,-2,50,500,30,-200,-50,660,343,1,true,0.98,muNw+gfzC9A8+8oTYAP50JPPe/0mLGAF1tvPDT727g+PfwcGHAG293/D77z6pQ9QQgBvNA292w')]
    [TestCase('18', '300,300,1,-2,50,500,30,-200,-50,660,343,1,false,0.98,muNw+gfzC9A8+8oTYAP50JPPe/0mLGAF1tvPDT727g+PfwcGHAG293/D77z6pQ9QQgBvNA292w')]
    [TestCase('19', '300,300,1,-2,50,500,30,-200,-50,660,343,0.5,true,0.98,uJuROgDaMx9/+LXLv0T3MiDv/PbeQ2AB396nr70NzEfPvfYOOAD49++PH8Pywofv/QIj2I29+Q')]
    [TestCase('20', '300,300,1,-2,50,500,30,-200,-50,660,343,0.5,false,0.98,uJuROgDaMx9/+LXLv0T3MiDv/PbeQ2AB396nr70NzEfPvfYOOAD49++PH8Pywofv/QIj2I29+Q')]
    [TestCase('21', '300,300,-1,2,-50,-100,30,-200,-50,660,343,1,true,0.98,2WNheefYu24BEEdjPn/1lawOPvCVV1A96OCDQT4inxNPfOgXBJC480lUX8nq4ANfeQXVgw4+GA')]
    [TestCase('22', '300,300,-1,2,-50,-100,30,-200,-50,660,343,1,false,0.98,2WNheefYu24BEEdjPn/1lawOPvCVV1A96OCDQT4inxNPfOgXBJC480lUX8nq4ANfeQXVgw4+GA')]
    [TestCase('23', '300,300,-1,2,-50,-100,30,-200,-50,660,343,0.5,true,0.98,198Buu/Mi27BAnPznkL1lawOPvCVV1A96OCD0f0jpUsv/OYWrE/59VRUX8nq4ANfeQXVgw4+GA')]
    [TestCase('24', '300,300,-1,2,-50,-100,30,-200,-50,660,343,0.5,false,0.98,198Buu/Mi27BAnPznkL1lawOPvCVV1A96OCD0f0jpUsv/OYWrE/59VRUX8nq4ANfeQXVgw4+GA')]
    [TestCase('25', '300,300,-1,2,-50,-100,-60,0,0,200,200,1,true,0.98,+QOPPusBjHC/G07vfvfs27POeALV77FEwrOzHwAaBVzu/fLKRzA9BjP8/8L33xMu+AJCfO05AA')]
    [TestCase('26', '300,300,-1,2,-50,-100,-60,0,0,200,200,1,false,0.98,+QOPPusBjHC/G07vfvfs27POeALV77FEwrOzHwAaBVzu/fLKRzA9BjP8/8L33xMu+AJCfO05AA')]
    [TestCase('27', '300,300,-1,2,-50,-100,-60,0,0,200,200,0.5,true,0.98,56ugvPHgd2+ACHc7vz7xOKxue/m1IwEB1cmbsP4f2PP+POzvz687/0moL0bx34u+OAQM/P07DA')]
    [TestCase('28', '300,300,-1,2,-50,-100,-60,0,0,200,200,0.5,false,0.98,56ugvPHgd2+ACHc7vz7xOKxue/m1IwEB1cmbsP4f2PP+POzvz687/0moL0bx34u+OAQM/P07DA')]
    [TestCase('29', '300,300,1,-2,-50,500,30,0,0,200,200,1,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDkXojdTdw+/HHZ1778mZwHwDx42swPRXq3xy7+g')]
    [TestCase('30', '300,300,1,-2,-50,500,30,0,0,200,200,1,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDkXojdTdw+/HHZ1778mZwHwDx42swPRXq3xy7+g')]
    [TestCase('31', '300,300,1,-2,-50,500,30,0,0,200,200,0.5,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCD8XskkN/Pu+vu37569WNkv8Tt4bdPew3gr518CQ')]
    [TestCase('32', '300,300,1,-2,-50,500,30,0,0,200,200,0.5,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCD8XskkN/Pu+vu37569WNkv8Tt4bdPew3gr518CQ')]
    [TestCase('33', '80,80,1,1,0,0,0,0,0,60,60,0,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('34', '80,80,1,1,0,0,0,60,0,60,60,1,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('35', '80,80,1,1,0,0,0,0,60,60,60,1,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('36', '80,80,1,1,0,0,0,0,0,60,60,0,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('37', '80,80,1,1,0,0,0,60,0,60,60,1,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('38', '80,80,1,1,0,0,0,0,60,60,60,1,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('39', '80,80,1,1,0,0,10,0,0,60,60,0,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('40', '80,80,1,1,0,0,10,60,0,60,60,1,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('41', '80,80,1,1,0,0,10,0,60,60,60,1,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('42', '80,80,1,1,0,0,10,0,0,60,60,0,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('43', '80,80,1,1,0,0,10,60,0,60,60,1,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('44', '80,80,1,1,0,0,10,0,60,60,60,1,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    procedure TestFillEllipseSolidWithMatrix(ASurfaceWidth, ASurfaceHeight: Integer; ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg, ADestLeft, ADestTop, ADestRight, ADestBottom, AOpacity: Single; ABlending: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,zzPi+P3FCz9+7a1PTwfxUPA+vPyVuzB+28+b0fwmr+sufe0DnJ87A0sEbwTs9stedwUMfK19Cg')]
    procedure TestFillEllipseSolidWithModulateColor(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,w2tC+f7J8x497r2PH0nvLEwPe/6fv5B949t/sfwmqF+vfewETE88BTUA3wL0/deO9wIm3Iy9Aw')]
    procedure TestFillEllipseSolidWithResource(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',   'horse.webp,claWhite,Tile,True,0,0,1,1,0.4,0.98,5mchv/XTy35D+sLL78XxdZAewPGtW0A+5uCPQb4fv1uweuUSQL/59yxc/0fu2z/feQTPgz5+GQ')]
    [TestCase('2',   'horse.webp,claRed,Tile,True,0,0,1,1,0.4,0.98,5ptx/e/Rh45D/JuPHwXydIyeP/GyN/D95eKHAb4fsiOQueULjH/49yp4f8jt1zOf+QTSc649GA')]
    [TestCase('3',   'horse.webp,claWhite,Tile,True,0,0,1,1,1,0.98,AADghfHvd6+GAfQ7sEXsRWgeg/TRW5B/4+Gbkf8W3KPROOUNJD95999n3wX00pef+gLGf34+Gw')]
    [TestCase('4',   'horse.webp,claRed,Tile,True,0,0,1,1,1,0.98,/4OgAePry74GB5SjrwPtQ1zuQfLcB+B+4uWLET8XvBPhNeb82y+299mvL0fyy3vfOQTNVw49GA')]
    [TestCase('5',   'horse.webp,claWhite,Tile,True,0.15,0.15,0.85,0.85,0.4,0.98,yBsiugTQo14B9rLHvwTzfMyefvCYb/C95OmL4T0h1H9/O+MSTO+7814kX8jr4g9P+QTbU549Fg')]
    [TestCase('6',   'horse.webp,claRed,Tile,True,0.15,0.15,0.85,0.85,0.4,0.98,yB/COgHDs44B+aa7/wXyfcCOfvGQZ/C95OaTIX0ky38v++ESXA978mAYr0jr4QP/OATbR269FQ')]
    [TestCase('7',   'horse.webp,claWhite,Tile,True,0.15,0.15,0.85,0.85,1,0.98,s7thORfpD08B980roMLvVQB/v/Ccj+A+3/WbsX4aEXi/et8NPL++7FzY3kbt5iMv+QPkC/58Ew')]
    [TestCase('8',   'horse.webp,claRed,Tile,True,0.15,0.15,0.85,0.85,1,0.98,s8fhug7IO8/B/q8T4EXtV+Q+P/OIe/B+3++v4bwi/XfPedwMaH/86V/Ajkfs4Qdv+AHj6208Eg')]
    [TestCase('9',   'horse.webp,claWhite,TileOriginal,True,0,0,1,1,0.4,0.98,5mchv/XTy35D+sLL78XxdZAewPGtW0A+5uCPET4fzgPwuuIqhB+5+C1Ur0jt2At/eQTVe/69GQ')]
    [TestCase('10',  'horse.webp,claRed,TileOriginal,True,0,0,1,1,0.4,0.98,5ptx/e/Rh45D/JuPHwXydIyeP/GyN/D95eKHEb4exfO/eeMjqG/4+CpYD8ns2f9ueQTUe+59GQ')]
    [TestCase('11',  'horse.webp,claWhite,TileOriginal,True,0,0,1,1,1,0.98,AADghfHve6+GAfQ7sEXsRWgeg/TRW5B/4+GbMb8VBMRgud5IyL93+eBXr4fxzRe/eQLUb849HA')]
    [TestCase('12',  'horse.webp,claRed,TileOriginal,True,0,0,1,1,1,0.98,/oOgAePry74GB5SjrwPtQ1zuQfLcB+B+4uWLEX8U7JdAtuA4IPA1+tlbn8jvz/duuQLTa749Gw')]
    [TestCase('13',  'horse.webp,claWhite,TileOriginal,True,0.15,0.15,0.85,0.85,0.4,0.98,yBsiugTQo14B9rLHvwTzfMyefvCYb/C95OmL4T0h1H9/O+MSTO+7814kX8jr4g9P+QTbU549Fg')]
    [TestCase('14',  'horse.webp,claRed,TileOriginal,True,0.15,0.15,0.85,0.85,0.4,0.98,yB/COgHDs44B+aa7/wXyfcCOfvGQZ/C95OaTIX0ky38v++ESXA978mAYr0jr4QP/OATbR269FQ')]
    [TestCase('15',  'horse.webp,claWhite,TileOriginal,True,0.15,0.15,0.85,0.85,1,0.98,s7thORfpD08B980roMLvVQB/v/Ccj+A+3/WbsX4aEXS/et8NPL++7FzY3kbt5iMv+QPkC/58Ew')]
    [TestCase('16',  'horse.webp,claRed,TileOriginal,True,0.15,0.15,0.85,0.85,1,0.98,s8fhug7IO8/B/q8T4EXtV+Q+P/OIe/B+3++v4bwi/XfPedwMaH/86V/Ajkfs4Qdv+AHj6208Eg')]
    [TestCase('17',  'horse.webp,claWhite,TileStretch,True,0,0,1,1,0.4,0.98,4G8BP/TUw54D+r3HL8bxdJQuAPGxc/B95eSDcf4fwEvgOuYUQH+59yqAz4fu20vf+QTPgx5+Gg')]
    [TestCase('18',  'horse.webp,claRed,TileStretch,True,0,0,1,1,0.4,0.98,5Zthve/Ri27D/JuLDwXycoRuv/CyR+B95eSDIb4ftBvAOeYNjH84+CuYf0ju2SuPeQTSe469GA')]
    [TestCase('19',  'horse.webp,claWhite,TileStretch,True,0,0,1,1,1,0.98,7w+Qxe3yZ+9GAekrUEbsQnBOQ/Lbm/B+4el/IcAX4HdBeecSJK8499nDb0X01LOPugPHgy7+HQ')]
    [TestCase('20',  'horse.webp,claRed,TileStretch,True,0,0,1,1,1,0.98,/X+AweLr247GB5Kbj0PtPEh+gfHcL6A+4eqHUf8WwgNRNucB4C/299v/L8fzzmfP+QLMa858GQ')]
    [TestCase('21',  'horse.webp,claWhite,TileStretch,True,0.15,0.15,0.85,0.85,0.4,0.98,xhtCOgXOq46B9a/Tz8TyfNSuvu+Zd/B95OmTMb4g1X9/e+MTRP9782EsTwjs5Rs/eQXbT569Fg')]
    [TestCase('22',  'horse.webp,claRed,TileStretch,True,0.15,0.15,0.85,0.85,0.4,0.98,xhvCegHCr56B+Ka//0XyfcB+fvGQa/B95OaTcb0kzH8P++ERWB878mEgj8jr4v/u+APZS269FQ')]
    [TestCase('23',  'horse.webp,claWhite,TileStretch,True,0.15,0.15,0.85,0.85,1,0.98,r7uheRjiI8+B9cVL4MLuVRCv/+6gp+C+3vincb8ZFXifeuAPLM/+62Lwrkbu6zv/eAXk//08FA')]
    [TestCase('24',  'horse.webp,claRed,TileStretch,True,0.15,0.15,0.85,0.85,1,0.98,rrvROg/GN+9B/a4T8MXtV+AuP/OJh+C+3vCvoT0j/3uPudwJYK886WHQXkft5fcueAHf9308Eg')]
    [TestCase('25',  'horse.webp,claWhite,Tile,False,0,0,1,1,0.4,0.98,5mchv/XTy35D+sLL78XxdZAewPGtW0A+5uCPQb4fv1uweuUSQL/59yxc/0fu2z/feQTPgz5+GQ')]
    [TestCase('26',  'horse.webp,claRed,Tile,False,0,0,1,1,0.4,0.98,5ptx/e/Rh45D/JuPHwXydIyeP/GyN/D95eKHAb4fsiOQueULjH/49yp4f8jt1zOf+QTSc649GA')]
    [TestCase('27',  'horse.webp,claWhite,Tile,False,0,0,1,1,1,0.98,AADghfHvd6+GAfQ7sEXsRWgeg/TRW5B/4+Gbkf8W3KPROOUNJD95999n3wX00pef+gLGf34+Gw')]
    [TestCase('28',  'horse.webp,claRed,Tile,False,0,0,1,1,1,0.98,/4OgAePry74GB5SjrwPtQ1zuQfLcB+B+4uWLET8XvBPhNeb82y+299mvL0fyy3vfOQTNVw49GA')]
    [TestCase('29',  'horse.webp,claWhite,Tile,False,0.15,0.15,0.85,0.85,0.4,0.98,yBsiugTQo14B9rLHvwTzfMyefvCYb/C95OmL4T0h1H9/O+MSTO+7814kX8jr4g9P+QTbU549Fg')]
    [TestCase('30',  'horse.webp,claRed,Tile,False,0.15,0.15,0.85,0.85,0.4,0.98,yB/COgHDs44B+aa7/wXyfcCOfvGQZ/C95OaTIX0ky38v++ESXA978mAYr0jr4QP/OATbR269FQ')]
    [TestCase('31',  'horse.webp,claWhite,Tile,False,0.15,0.15,0.85,0.85,1,0.98,s7thORfpD08B980roMLvVQB/v/Ccj+A+3/WbsX4aEXi/et8NPL++7FzY3kbt5iMv+QPkC/58Ew')]
    [TestCase('32',  'horse.webp,claRed,Tile,False,0.15,0.15,0.85,0.85,1,0.98,s8fhug7IO8/B/q8T4EXtV+Q+P/OIe/B+3++v4bwi/XfPedwMaH/86V/Ajkfs4Qdv+AHj6208Eg')]
    [TestCase('33',  'horse.webp,claWhite,TileOriginal,False,0,0,1,1,0.4,0.98,5mchv/XTy35D+sLL78XxdZAewPGtW0A+5uCPET4fzgPwuuIqhB+5+C1Ur0jt2At/eQTVe/69GQ')]
    [TestCase('34',  'horse.webp,claRed,TileOriginal,False,0,0,1,1,0.4,0.98,5ptx/e/Rh45D/JuPHwXydIyeP/GyN/D95eKHEb4exfO/eeMjqG/4+CpYD8ns2f9ueQTUe+59GQ')]
    [TestCase('35',  'horse.webp,claWhite,TileOriginal,False,0,0,1,1,1,0.98,AADghfHve6+GAfQ7sEXsRWgeg/TRW5B/4+GbMb8VBMRgud5IyL93+eBXr4fxzRe/eQLUb849HA')]
    [TestCase('36',  'horse.webp,claRed,TileOriginal,False,0,0,1,1,1,0.98,/oOgAePry74GB5SjrwPtQ1zuQfLcB+B+4uWLEX8U7JdAtuA4IPA1+tlbn8jvz/duuQLTa749Gw')]
    [TestCase('37',  'horse.webp,claWhite,TileOriginal,False,0.15,0.15,0.85,0.85,0.4,0.98,yBsiugTQo14B9rLHvwTzfMyefvCYb/C95OmL4T0h1H9/O+MSTO+7814kX8jr4g9P+QTbU549Fg')]
    [TestCase('38',  'horse.webp,claRed,TileOriginal,False,0.15,0.15,0.85,0.85,0.4,0.98,yB/COgHDs44B+aa7/wXyfcCOfvGQZ/C95OaTIX0ky38v++ESXA978mAYr0jr4QP/OATbR269FQ')]
    [TestCase('39',  'horse.webp,claWhite,TileOriginal,False,0.15,0.15,0.85,0.85,1,0.98,s7thORfpD08B980roMLvVQB/v/Ccj+A+3/WbsX4aEXS/et8NPL++7FzY3kbt5iMv+QPkC/58Ew')]
    [TestCase('40',  'horse.webp,claRed,TileOriginal,False,0.15,0.15,0.85,0.85,1,0.98,s8fhug7IO8/B/q8T4EXtV+Q+P/OIe/B+3++v4bwi/XfPedwMaH/86V/Ajkfs4Qdv+AHj6208Eg')]
    [TestCase('41',  'horse.webp,claWhite,TileStretch,False,0,0,1,1,0.4,0.98,4G8BP/TUw54D+r3HL8bxdJQuAPGxc/B95eSDcf4fwEvgOuYUQH+59yqAz4fu20vf+QTPgx5+Gg')]
    [TestCase('42',  'horse.webp,claRed,TileStretch,False,0,0,1,1,0.4,0.98,5Zthve/Ri27D/JuLDwXycoRuv/CyR+B95eSDIb4ftBvAOeYNjH84+CuYf0ju2SuPeQTSe469GA')]
    [TestCase('43',  'horse.webp,claWhite,TileStretch,False,0,0,1,1,1,0.98,7w+Qxe3yZ+9GAekrUEbsQnBOQ/Lbm/B+4el/IcAX4HdBeecSJK8499nDb0X01LOPugPHgy7+HQ')]
    [TestCase('44',  'horse.webp,claRed,TileStretch,False,0,0,1,1,1,0.98,/X+AweLr247GB5Kbj0PtPEh+gfHcL6A+4eqHUf8WwgNRNucB4C/299v/L8fzzmfP+QLMa858GQ')]
    [TestCase('45',  'horse.webp,claWhite,TileStretch,False,0.15,0.15,0.85,0.85,0.4,0.98,xhtCOgXOq46B9a/Tz8TyfNSuvu+Zd/B95OmTMb4g1X9/e+MTRP9782EsTwjs5Rs/eQXbT569Fg')]
    [TestCase('46',  'horse.webp,claRed,TileStretch,False,0.15,0.15,0.85,0.85,0.4,0.98,xhvCegHCr56B+Ka//0XyfcB+fvGQa/B95OaTcb0kzH8P++ERWB878mEgj8jr4v/u+APZS269FQ')]
    [TestCase('47',  'horse.webp,claWhite,TileStretch,False,0.15,0.15,0.85,0.85,1,0.98,r7uheRjiI8+B9cVL4MLuVRCv/+6gp+C+3vincb8ZFXifeuAPLM/+62Lwrkbu6zv/eAXk//08FA')]
    [TestCase('48',  'horse.webp,claRed,TileStretch,False,0.15,0.15,0.85,0.85,1,0.98,rrvROg/GN+9B/a4T8MXtV+AuP/OJh+C+3vCvoT0j/3uPudwJYK886WHQXkft5fcueAHf9308Eg')]
    [TestCase('49',  'horse.webp,claWhite,Tile,True,0,0.2,1,0.8,0.4,0.98,sruhO/vVx47D9bHDHwX0frzefvGicyD95e2nAb4c1A/AO+UbjA969kdQ74jr1TNPeQTPZy6+GA')]
    [TestCase('50',  'horse.webp,claRed,Tile,True,0,0.2,1,0.8,0.4,0.98,z+cBPPjQs/7C+Jl3b8XxfqyufvGcV3D95eSjMb0exwcge+MZlE959klQb4nq1RsPOQTRX649Fw')]
    [TestCase('51',  'horse.webp,claWhite,Tile,True,0,0.2,1,0.8,1,0.98,fs8g/f/za8+G9swnoAPyW9geAPO2l8B84gHYAb8PEuBwu+Ql5A+68yBMPwjtxXc/OQPFP16+GQ')]
    [TestCase('52',  'horse.webp,claRed,Tile,True,0,0.2,1,0.8,1,0.98,xTsB/vfpO1/F/Y5jf8TsWrSOf/OmT4C94urP8XwU88vQOeAe9B/48yhEj4nqwz9/+AHLKx59FQ')]
    [TestCase('53',  'horse.webp,claWhite,TileOriginal,True,0,0.2,1,0.8,0.4,0.98,sruhO/vVx47D9bHDHwX0frzefvGicyD95e2nAb4c1A/AO+UbjA969kdQ74jr1TNPeQTPZy6+GA')]
    [TestCase('54',  'horse.webp,claRed,TileOriginal,True,0,0.2,1,0.8,0.4,0.98,z+cBPPjQs/7C+Jl3b8XxfqyufvGcV3D95eSjMb0exwcge+MZlE959kpQb4nq1RsPOQTRX649Fw')]
    [TestCase('55',  'horse.webp,claWhite,TileOriginal,True,0,0.2,1,0.8,1,0.98,fs8g/f/za8+G9swnoAPyW9geAPO2l8B84gHYAb8PEuRwu+Ql5A+68yBMPwjtxXc/OQPFP16+GQ')]
    [TestCase('56',  'horse.webp,claRed,TileOriginal,True,0,0.2,1,0.8,1,0.98,xjsB/vfpN1/F/Y5jf8TsWrSOf/OmT4C94urP8XwU88vQOeAe9B/48yhEj4nqwz9/+AHLKx59FQ')]
    [TestCase('57',  'horse.webp,claWhite,TileStretch,True,0,0.2,1,0.8,0.4,0.98,tsPxO/vOu66D9rKrjwX0fsTe/vCjcyD95e2nQb4c1g/ge+UamC+69UhY34jr1zdPuQTPax7+GA')]
    [TestCase('58',  'horse.webp,claRed,TileStretch,True,0,0.2,1,0.8,0.4,0.98,zusxfPjMq/5C+JtvnwXyfayefvGcV3D95eSjUf0eyQfweuMXlF+59kpYb8nq1hf/uAPRY649Fw')]
    [TestCase('59',  'horse.webp,claWhite,TileStretch,True,0,0.2,1,0.8,1,0.98,iOfQff/iTw9H+M3r38TxW+guwPG4m+B84gDcoX8PF+Swe+UiAEC68SVcD4jsyYc/eQPHQy7+GQ')]
    [TestCase('60',  'horse.webp,claRed,TileStretch,True,0,0.2,1,0.8,1,0.98,w0OBvvjeI2+F/JNX/wTtWbSOP/OmV5B94uvTQf0U9dNgeeAZ9E849CpgbwnrxzdvOAHKMx79FQ')]
    [TestCase('61',  'horse.webp,claWhite,Tile,True,0,0.2,1,0.8,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('62',  'horse.webp,claRed,Tile,True,0,0.2,1,0.8,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('63',  'horse.webp,claWhite,TileOriginal,True,0,0.2,1,0.8,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('64',  'horse.webp,claRed,TileOriginal,True,0,0.2,1,0.8,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('65',  'horse.webp,claWhite,TileStretch,True,0,0.2,1,0.8,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('66',  'horse.webp,claRed,TileStretch,True,0,0.2,1,0.8,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('67',  'horse.webp,claWhite,Tile,True,0,0.2,1,0.2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('68',  'horse.webp,claRed,Tile,True,0,0.2,1,0.2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('69',  'horse.webp,claWhite,TileOriginal,True,0,0.2,1,0.2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('70',  'horse.webp,claRed,TileOriginal,True,0,0.2,1,0.2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('71',  'horse.webp,claWhite,TileStretch,True,0,0.2,1,0.2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('72',  'horse.webp,claRed,TileStretch,True,0,0.2,1,0.2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('73',  'horse.webp,claWhite,Tile,True,0.5,0.2,0.5,0.8,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('74',  'horse.webp,claRed,Tile,True,0.5,0.2,0.5,0.8,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('75',  'horse.webp,claWhite,TileOriginal,True,0.5,0.2,0.5,0.8,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('76',  'horse.webp,claRed,TileOriginal,True,0.5,0.2,0.5,0.8,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('77',  'horse.webp,claWhite,TileStretch,True,0.5,0.2,0.5,0.8,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('78',  'horse.webp,claRed,TileStretch,True,0.5,0.2,0.5,0.8,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('79',  'horse.webp,claWhite,Tile,True,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('80',  'horse.webp,claRed,Tile,True,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('81',  'horse.webp,claWhite,TileOriginal,True,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('82',  'horse.webp,claRed,TileOriginal,True,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('83',  'horse.webp,claWhite,TileStretch,True,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('84',  'horse.webp,claRed,TileStretch,True,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('85',  'horse.webp,claWhite,Tile,True,0,-0.2,1,1.2,1,0.98,u6ffRPXnF1/I+3crsEfsUsBOQ/LgZ3C96f4zQvf2/kehPOto9P+1ADPA74b5zhOvuQPc1w7+Hg')]
    [TestCase('86',  'horse.webp,claWhite,TileOriginal,True,0,-0.2,1,1.2,1,0.98,u6ffRPXnF1+I+3cvsEfsUsBOQ/LgZ3C96f4zEn0OzXNAu+IM9A+29CGY34b4zwffuQLYs669Hw')]
    [TestCase('87',  'horse.webp,claWhite,TileStretch,True,0,-0.2,1,1.2,1,0.98,3L+fBfbS907J92W3X0frSJjew/Phl/D85ggE0rf4+08hfO1v2K91ADDob8b5x/MeOgPcy879Hw')]
    [TestCase('88',  'horse.webp,claWhite,Tile,False,0,-0.2,1,1.2,1,0.98,u6ffRPXnF1/I+3crsEfsUsBOQ/LgZ3C96f4zQvf2/kehPOto9P+1ADPA74b5zhOvuQPc1w7+Hg')]
    [TestCase('89',  'horse.webp,claWhite,TileOriginal,False,0,-0.2,1,1.2,1,0.98,u6ffRPXnF1+I+3cvsEfsUsBOQ/LgZ3C96f4zEn0OzXNAu+IM9A+29CGY34b4zwffuQLYs669Hw')]
    [TestCase('90',  'horse.webp,claWhite,TileStretch,False,0,-0.2,1,1.2,1,0.98,3L+fBfbS907J92W3X0frSJjew/Phl/D85ggE0rf4+08hfO1v2K91ADDob8b5x/MeOgPcy879Hw')]
    [TestCase('91',  'horse.webp,claWhite,Tile,True,0,-0.2,1,1.2,0.85,0.98,vw9Qg/Xh+07H+n0TcMftXLx+AvLVY3B96fobMvj98gOBfOpc3J92/zmwT0f30A+f+QPbyw7+HQ')]
    [TestCase('92',  'horse.webp,claWhite,TileOriginal,True,0,-0.2,1,1.2,0.85,0.98,vw9Qg/Xh+07H+n0TcMftXLx+AvLVY3B96fobEv0RyE9gO+MN3K829SuMP4f20QfPOQPYq759Hg')]
    [TestCase('93',  'horse.webp,claWhite,TileStretch,True,0,-0.2,1,1.2,0.85,0.98,2yPwg/bQ3x5I922vL8fsVJj+AvPWiwA95wLwoXj/7wshfOxhxF82/zfQ34b3y/f+eQPbv869Hg')]
    [TestCase('94',  'horse.webp,claWhite,Tile,False,0,-0.2,1,1.2,0.85,0.98,vw9Qg/Xh+07H+n0TcMftXLx+AvLVY3B96fobMvj98gOBfOpc3J92/zmwT0f30A+f+QPbyw7+HQ')]
    [TestCase('95',  'horse.webp,claWhite,TileOriginal,False,0,-0.2,1,1.2,0.85,0.98,vw9Qg/Xh+07H+n0TcMftXLx+AvLVY3B96fobEv0RyE9gO+MN3K829SuMP4f20QfPOQPYq759Hg')]
    [TestCase('96',  'horse.webp,claWhite,TileStretch,False,0,-0.2,1,1.2,0.85,0.98,2yPwg/bQ3x5I922vL8fsVJj+AvPWiwA95wLwoXj/7wshfOxhxF82/zfQ34b3y/f+eQPbv869Hg')]
    [TestCase('97',  'horse.webp,$7F0000FF,Tile,True,0,-0.2,1,1.2,1,0.98,vuPQvfauB96D+W1DD8bxdaxOgPO1N/B96erXwXoMzxtwet8bGJB2+UkgXwjv3MN+eQTbX949GQ')]
    [TestCase('98',  'horse.webp,$7F0000FF,TileOriginal,True,0,-0.2,1,1.2,1,0.98,vuPQvfauB+6D+W1DD8bxdaxOgPO1N/B96erXgbwZvQNQO+IUkF+3+DgYX4jv2stuuQTXY579GQ')]
    [TestCase('99',  'horse.webp,$7F0000FF,TileStretch,True,0,-0.2,1,1.2,1,0.98,vt/QvfewD/5D+GNLv4XxdbROgPOzNxA+6evXwToOzjegOuEeFKD2+EoYT0jv3L9u+QTaV859GQ')]
    [TestCase('100', 'horse.webp,$7F0000FF,Tile,False,0,-0.2,1,1.2,1,0.98,vuPQvfauB96D+W1DD8bxdaxOgPO1N/B96erXwXoMzxtwet8bGJB2+UkgXwjv3MN+eQTbX949GQ')]
    [TestCase('101', 'horse.webp,$7F0000FF,TileOriginal,False,0,-0.2,1,1.2,1,0.98,vuPQvfauB+6D+W1DD8bxdaxOgPO1N/B96erXgbwZvQNQO+IUkF+3+DgYX4jv2stuuQTXY579GQ')]
    [TestCase('102', 'horse.webp,$7F0000FF,TileStretch,False,0,-0.2,1,1.2,1,0.98,vt/QvfewD/5D+GNLv4XxdbROgPOzNxA+6evXwToOzjegOuEeFKD2+EoYT0jv3L9u+QTaV859GQ')]
    [TestCase('103', 'horse.webp,$7F0000FF,Tile,True,0,-0.2,1,1.2,0.85,0.98,whth/faxE37D+HVLD0byeqz+P/OxO9A96ejLITsQygeweuAa/B83+U0of0ju3ct+uQTaY949GQ')]
    [TestCase('104', 'horse.webp,$7F0000FF,TileOriginal,True,0,-0.2,1,1.2,0.85,0.98,whth/faxE37D+HVLD0byeqz+P/OxO9A96ejLoXwbuu9/u+IUhM+3+D4gj8ju29Nu+QTXZ669GQ')]
    [TestCase('105', 'horse.webp,$7F0000FF,TileStretch,True,0,-0.2,1,1.2,0.85,0.98,whdhvfeyF57D92xTz0XyerD+P/OuP/D96OnLIbsRyB/Q+uEd+C+3+E4kf4ju3ctuOQXZX949GQ')]
    [TestCase('106', 'horse.webp,$7F0000FF,Tile,False,0,-0.2,1,1.2,0.85,0.98,whth/faxE37D+HVLD0byeqz+P/OxO9A96ejLITsQygeweuAa/B83+U0of0ju3ct+uQTaY949GQ')]
    [TestCase('107', 'horse.webp,$7F0000FF,TileOriginal,False,0,-0.2,1,1.2,0.85,0.98,whth/faxE37D+HVLD0byeqz+P/OxO9A96ejLoXwbuu9/u+IUhM+3+D4gj8ju29Nu+QTXZ669GQ')]
    [TestCase('108', 'horse.webp,$7F0000FF,TileStretch,False,0,-0.2,1,1.2,0.85,0.98,whdhvfeyF57D92xTz0XyerD+P/OuP/D96OnLIbsRyB/Q+uEd+C+3+E4kf4ju3ctuOQXZX949GQ')]
    procedure TestFillPathBitmap(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode; ABlending: Boolean; const ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',   'horse.webp,claWhite,Tile,True,0,0,1,1,0.4,0.98,3T9i+fq+qx5/9LpX70X1ceBOvvebq8D94+F7oT4kulO/POj+iz/8+VV4Hwns2Q8/+QPwM359EA')]
    [TestCase('2',   'horse.webp,claRed,Tile,True,0,0,1,1,0.4,0.98,yUvC+fm5uz6/9ql7L0b2ccTe/fiWowA+4tZr8b0luVsPvOgPfE87/liQj4jr3fNeuQX0J769Dw')]
    [TestCase('3',   'horse.webp,claWhite,Tile,True,0,0,1,1,1,0.98,6BNi9/68I5978uAXrwX1OjCvPgOjI2F+3eNzoQAi0g/PPeza03///ESsv8jtzicfuQEXvJ38BA')]
    [TestCase('4',   'horse.webp,claRed,Tile,True,0,0,1,1,1,0.98,tzNyePyuTw98+LdzX4b3OuyO/QWYEwH/2MdL4T4mzh8PfO0FvC89B03sP4fs2t9euQUimC09Aw')]
    [TestCase('5',   'horse.webp,claWhite,Tile,True,0.15,0.15,0.85,0.85,0.4,0.98,2lOi+fi9h77/861LD4b2gcxufvWaf3C95uJrwf0lq2Pf/OcIgD87+1dor0jq3AtPuQPqX549Ew')]
    [TestCase('6',   'horse.webp,claRed,Tile,True,0.15,0.15,0.85,0.85,0.4,0.98,zWOCOfq+g/4/9aRbDwb4hLzuffaWf5D95Nhfcb0mq1d/vOgScJ86/lZ0n8jq2wOP+QPpS+79Eg')]
    [TestCase('7',   'horse.webp,claWhite,Tile,True,0.15,0.15,0.85,0.85,1,0.98,4E8C+Pm5yy598b/7Hkb4YvwOP/2iu6B95OVLcX4mrC8f/uv0w/+8/0qAn8fp1hM/+QAIJN78Cw')]
    [TestCase('8',   'horse.webp,claRed,Tile,True,0.15,0.15,0.85,0.85,1,0.98,wXPSd/26w9799KkjD0b8ati+vf+Yu+A94Mwvob0orBcvPe0OmI87B0ekbwfr1APf+QEH+M39Cg')]
    [TestCase('9',   'horse.webp,claWhite,TileOriginal,True,0,0,1,1,0.4,0.98,3T9i+fq+qx5/9LpX70X1ceBOvvebq8D94+F7oT4kulO/POj+iz/8+VV4Hwns2Q8/+QPwM359EA')]
    [TestCase('10',  'horse.webp,claRed,TileOriginal,True,0,0,1,1,0.4,0.98,yUvC+fm5uz6/9ql7L0b2ccTe/fiWowA+4tZr8b0luVsPvOgPgE87/liQj4jr3fNeuQX0J769Dw')]
    [TestCase('11',  'horse.webp,claWhite,TileOriginal,True,0,0,1,1,1,0.98,6BNi9/68I5978uAXrwX1OjCvPgOjI2F+3eNzoQAi0g/PPeza03///ESsv8jtzicfuQEXvJ38BA')]
    [TestCase('12',  'horse.webp,claRed,TileOriginal,True,0,0,1,1,1,0.98,tzNyePyuTw98+LdzX4b3OuyO/QWYEwH/2MdL4T4mzh8PfO0FvC89B03sP4fs2t9euQUimC09Aw')]
    [TestCase('13',  'horse.webp,claWhite,TileOriginal,True,0.15,0.15,0.85,0.85,0.4,0.98,2lOi+fi9h77/861LD4b2gcxufvWaf3C95uJrwf0lq2Pf/OcIgD87+1dor0jq3AtPuQPqX549Ew')]
    [TestCase('14',  'horse.webp,claRed,TileOriginal,True,0.15,0.15,0.85,0.85,0.4,0.98,zWOCOfq+g/4/9aRbDwb4hLzuffaWf5D95Nhfcb0mq1d/vOgScJ86/lZ0n8jq2wOP+QPpS+79Eg')]
    [TestCase('15',  'horse.webp,claWhite,TileOriginal,True,0.15,0.15,0.85,0.85,1,0.98,4E8C+Pm5yy598b/7Hkb4YvwOP/2iu6B95OVLcX4mrC8f/uv0w/+8/0qAn8fp1hM/+QAIJN78Cw')]
    [TestCase('16',  'horse.webp,claRed,TileOriginal,True,0.15,0.15,0.85,0.85,1,0.98,wXPSd/26w9799KkjD0b8ati+vf+Yu+A94Mwvob0orBcvPe0OmI87B0ekbwfr1APf+QEH+M39Cg')]
    [TestCase('17',  'horse.webp,claWhite,TileStretch,True,0,0,1,1,0.4,0.98,0i9y+fu/v47/87F774XzcghPvveiy3B95OuLwT4ktVOv/On7h897+FeMb8js3AvvuAXxJ479EA')]
    [TestCase('18',  'horse.webp,claRed,TileStretch,True,0,0,1,1,0.4,0.98,yj+S+fm5x35/9amDH0b2ccSe/fmWt+C94dlf0b0ktlfve+kOkA87/ViYT0js3vP+OAXyI669Dw')]
    [TestCase('19',  'horse.webp,claWhite,TileStretch,True,0,0,1,1,1,0.98,zPOhtwG9V9988clzvwXxPZS/PgO0d6G93vuX0cAhxA+vffDTz2+++EnkD8fv1xs/uAUalM38BQ')]
    [TestCase('20',  'horse.webp,claRed,TileStretch,True,0,0,1,1,1,0.98,uhfy9/yva5989bZ/P0b3OujefAiXQ7G+184nkb4jyBe/O+8D6J/8BEwAoMbu29tuuAQckA29Ag')]
    [TestCase('21',  'horse.webp,claWhite,TileStretch,True,0.15,0.15,0.85,0.85,0.4,0.98,1UvC+fnBi/7/86ljP8b1gdxevvSfj4A95uN74T0lrF///OgGfE+7+lhsj4jr3g8/eQToT679Eg')]
    [TestCase('22',  'horse.webp,claRed,TileStretch,True,0.15,0.15,0.85,0.85,0.4,0.98,z0+iufnBi95/9KVnD8b3g8DOffaXi5C95NlfoX0mrFdvPOkSdM+6/Vd4f8jr3gNfeQTnS+69Eg')]
    [TestCase('23',  'horse.webp,claWhite,TileStretch,True,0.15,0.15,0.85,0.85,1,0.98,1TNiePzD1/598bc3j0b2YiTfvvut49B94+dzsX4kridvPu7vuy+9/k2UP8fs2yMfuQID/B09Cw')]
    [TestCase('24',  'horse.webp,claRed,TileStretch,True,0.15,0.15,0.85,0.85,1,0.98,xUMS+PvC15698qs7D0b7Z9x+vf+Z0+C939ArIT4orQ//vO4NoO87BkmoH0ft2wdP+QIB9L09Cg')]
    [TestCase('25',  'horse.webp,claWhite,Tile,False,0,0,1,1,0.4,0.98,3T9i+fq+qx5/9LpX70X1ceBOvvebq8D94+F7oT4kulO/POj+iz/8+VV4Hwns2Q8/+QPwM359EA')]
    [TestCase('26',  'horse.webp,claRed,Tile,False,0,0,1,1,0.4,0.98,yUvC+fm5uz6/9ql7L0b2ccTe/fiWowA+4tZr8b0luVsPvOgPfE87/liQj4jr3fNeuQX0J769Dw')]
    [TestCase('27',  'horse.webp,claWhite,Tile,False,0,0,1,1,1,0.98,6BNi9/68I5978uAXrwX1OjCvPgOjI2F+3eNzoQAi0g/PPeza03///ESsv8jtzicfuQEXvJ38BA')]
    [TestCase('28',  'horse.webp,claRed,Tile,False,0,0,1,1,1,0.98,tzNyePyuTw98+LdzX4b3OuyO/QWYEwH/2MdL4T4mzh8PfO0FvC89B03sP4fs2t9euQUimC09Aw')]
    [TestCase('29',  'horse.webp,claWhite,Tile,False,0.15,0.15,0.85,0.85,0.4,0.98,2lOi+fi9h77/861LD4b2gcxufvWaf3C95uJrwf0lq2Pf/OcIgD87+1dor0jq3AtPuQPqX549Ew')]
    [TestCase('30',  'horse.webp,claRed,Tile,False,0.15,0.15,0.85,0.85,0.4,0.98,zWOCOfq+g/4/9aRbDwb4hLzuffaWf5D95Nhfcb0mq1d/vOgScJ86/lZ0n8jq2wOP+QPpS+79Eg')]
    [TestCase('31',  'horse.webp,claWhite,Tile,False,0.15,0.15,0.85,0.85,1,0.98,4E8C+Pm5yy598b/7Hkb4YvwOP/2iu6B95OVLcX4mrC8f/uv0w/+8/0qAn8fp1hM/+QAIJN78Cw')]
    [TestCase('32',  'horse.webp,claRed,Tile,False,0.15,0.15,0.85,0.85,1,0.98,wXPSd/26w9799KkjD0b8ati+vf+Yu+A94Mwvob0orBcvPe0OmI87B0ekbwfr1APf+QEH+M39Cg')]
    [TestCase('33',  'horse.webp,claWhite,TileOriginal,False,0,0,1,1,0.4,0.98,3T9i+fq+qx5/9LpX70X1ceBOvvebq8D94+F7oT4kulO/POj+iz/8+VV4Hwns2Q8/+QPwM359EA')]
    [TestCase('34',  'horse.webp,claRed,TileOriginal,False,0,0,1,1,0.4,0.98,yUvC+fm5uz6/9ql7L0b2ccTe/fiWowA+4tZr8b0luVsPvOgPgE87/liQj4jr3fNeuQX0J769Dw')]
    [TestCase('35',  'horse.webp,claWhite,TileOriginal,False,0,0,1,1,1,0.98,6BNi9/68I5978uAXrwX1OjCvPgOjI2F+3eNzoQAi0g/PPeza03///ESsv8jtzicfuQEXvJ38BA')]
    [TestCase('36',  'horse.webp,claRed,TileOriginal,False,0,0,1,1,1,0.98,tzNyePyuTw98+LdzX4b3OuyO/QWYEwH/2MdL4T4mzh8PfO0FvC89B03sP4fs2t9euQUimC09Aw')]
    [TestCase('37',  'horse.webp,claWhite,TileOriginal,False,0.15,0.15,0.85,0.85,0.4,0.98,2lOi+fi9h77/861LD4b2gcxufvWaf3C95uJrwf0lq2Pf/OcIgD87+1dor0jq3AtPuQPqX549Ew')]
    [TestCase('38',  'horse.webp,claRed,TileOriginal,False,0.15,0.15,0.85,0.85,0.4,0.98,zWOCOfq+g/4/9aRbDwb4hLzuffaWf5D95Nhfcb0mq1d/vOgScJ86/lZ0n8jq2wOP+QPpS+79Eg')]
    [TestCase('39',  'horse.webp,claWhite,TileOriginal,False,0.15,0.15,0.85,0.85,1,0.98,4E8C+Pm5yy598b/7Hkb4YvwOP/2iu6B95OVLcX4mrC8f/uv0w/+8/0qAn8fp1hM/+QAIJN78Cw')]
    [TestCase('40',  'horse.webp,claRed,TileOriginal,False,0.15,0.15,0.85,0.85,1,0.98,wXPSd/26w9799KkjD0b8ati+vf+Yu+A94Mwvob0orBcvPe0OmI87B0ekbwfr1APf+QEH+M39Cg')]
    [TestCase('41',  'horse.webp,claWhite,TileStretch,False,0,0,1,1,0.4,0.98,0i9y+fu/v47/87F774XzcghPvveiy3B95OuLwT4ktVOv/On7h897+FeMb8js3AvvuAXxJ479EA')]
    [TestCase('42',  'horse.webp,claRed,TileStretch,False,0,0,1,1,0.4,0.98,yj+S+fm5x35/9amDH0b2ccSe/fmWt+C94dlf0b0ktlfve+kOkA87/ViYT0js3vP+OAXyI669Dw')]
    [TestCase('43',  'horse.webp,claWhite,TileStretch,False,0,0,1,1,1,0.98,zPOhtwG9V9988clzvwXxPZS/PgO0d6G93vuX0cAhxA+vffDTz2+++EnkD8fv1xs/uAUalM38BQ')]
    [TestCase('44',  'horse.webp,claRed,TileStretch,False,0,0,1,1,1,0.98,uhfy9/yva5989bZ/P0b3OujefAiXQ7G+184nkb4jyBe/O+8D6J/8BEwAoMbu29tuuAQckA29Ag')]
    [TestCase('45',  'horse.webp,claWhite,TileStretch,False,0.15,0.15,0.85,0.85,0.4,0.98,1UvC+fnBi/7/86ljP8b1gdxevvSfj4A95uN74T0lrF///OgGfE+7+lhsj4jr3g8/eQToT679Eg')]
    [TestCase('46',  'horse.webp,claRed,TileStretch,False,0.15,0.15,0.85,0.85,0.4,0.98,z0+iufnBi95/9KVnD8b3g8DOffaXi5C95NlfoX0mrFdvPOkSdM+6/Vd4f8jr3gNfeQTnS+69Eg')]
    [TestCase('47',  'horse.webp,claWhite,TileStretch,False,0.15,0.15,0.85,0.85,1,0.98,1TNiePzD1/598bc3j0b2YiTfvvut49B94+dzsX4kridvPu7vuy+9/k2UP8fs2yMfuQID/B09Cw')]
    [TestCase('48',  'horse.webp,claRed,TileStretch,False,0.15,0.15,0.85,0.85,1,0.98,xUMS+PvC15698qs7D0b7Z9x+vf+Z0+C939ArIT4orQ//vO4NoO87BkmoH0ft2wdP+QIB9L09Cg')]
    [TestCase('49',  'horse.webp,claWhite,Tile,True,0,0.2,1,0.8,0.4,0.98,zUNyufy+mz7A8KRrz0b0dNz+/fSekzC95uOb4b0kplPffOcKaO86/FBcn0js5xdfeQTjM769Ew')]
    [TestCase('50',  'horse.webp,claRed,Tile,True,0,0.2,1,0.8,0.4,0.98,wzdSOfvCk/5/86Bjv8b1dNS+/fWZh0A95dmPcT0mpUdv/OcSYD96/lBkj8jr5Qt/+QPhM859Ew')]
    [TestCase('51',  'horse.webp,claWhite,Tile,True,0,0.2,1,0.8,1,0.98,wR+CdwO6A2/+6atH34fyQyTvPfys7wB95Oe/wb4joA8ffur5iz98Ajdsf4fu8jc/uQL3u009DQ')]
    [TestCase('52',  'horse.webp,claRed,Tile,True,0,0.2,1,0.8,1,0.98,qANCt//G787976Evv0f2QhQ//f6fxzB94M+fof0mnu/ufOsMcJ86CDl0T0ft7BOfuQHxu319DA')]
    [TestCase('53',  'horse.webp,claWhite,TileOriginal,True,0,0.2,1,0.8,0.4,0.98,zUNyufy+mz7A8KRrz0b0dNz+/fSekzC95uOb4b0kplPffOcKaO86/FBcn0js5xdfeQTjM769Ew')]
    [TestCase('54',  'horse.webp,claRed,TileOriginal,True,0,0.2,1,0.8,0.4,0.98,wzdSOfvCk/5/86Bjv8b1dNS+/fWZh0A95dmPcT0mpUdv/OcSYD96/lBkj8jr5Qt/+QPhM859Ew')]
    [TestCase('55',  'horse.webp,claWhite,TileOriginal,True,0,0.2,1,0.8,1,0.98,wR+CdwO6A2/+6atH34fyQyTvPfys7wB95Oe/wb4joA8ffur5iz98Ajdsf4fu8jc/uQL3u009DQ')]
    [TestCase('56',  'horse.webp,claRed,TileOriginal,True,0,0.2,1,0.8,1,0.98,qANCt//G787976Evv0f2QhQ//f6fxzB94M+fof0mnu/ufOsMcJ86CDl0T0ft7BOfuQHxu319DA')]
    [TestCase('57',  'horse.webp,claWhite,TileStretch,True,0,0.2,1,0.8,0.4,0.98,zStyefvFry4A8p9vH0fzc+ge/vSjn0A95uWfIT4kplfv/OgIZN86+1Fgn4js6B9fuQTiN669Ew')]
    [TestCase('58',  'horse.webp,claRed,TileStretch,True,0,0.2,1,0.8,0.4,0.98,xydyufrFp+5/859fD8f1dNSuPfaai1B95NmLkf0lpkdPPOgQZD/6/VBkjwjs5gtf+QPgM949Ew')]
    [TestCase('59',  'horse.webp,claWhite,TileStretch,True,0,0.2,1,0.8,1,0.98,wOuhdwDNN2/+7J5PnwjwQEAvvvu2AxE94+zLUT8inxMv/u30dw+8/zt0b0fv9EdPuQP0wx39DA')]
    [TestCase('60',  'horse.webp,claRed,TileStretch,True,0,0.2,1,0.8,1,0.98,st+Rt/7NG8898J0rf8j2QRAvPf+i00D93s+XAT4mnu+u/OsIdI+6Bjd8T8ft8BNfeQHwu429Cw')]
    [TestCase('61',  'horse.webp,claWhite,Tile,True,0,0.2,1,0.8,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('62',  'horse.webp,claRed,Tile,True,0,0.2,1,0.8,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('63',  'horse.webp,claWhite,TileOriginal,True,0,0.2,1,0.8,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('64',  'horse.webp,claRed,TileOriginal,True,0,0.2,1,0.8,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('65',  'horse.webp,claWhite,TileStretch,True,0,0.2,1,0.8,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('66',  'horse.webp,claRed,TileStretch,True,0,0.2,1,0.8,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('67',  'horse.webp,claWhite,Tile,True,0,0.2,1,0.2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('68',  'horse.webp,claRed,Tile,True,0,0.2,1,0.2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('69',  'horse.webp,claWhite,TileOriginal,True,0,0.2,1,0.2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('70',  'horse.webp,claRed,TileOriginal,True,0,0.2,1,0.2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('71',  'horse.webp,claWhite,TileStretch,True,0,0.2,1,0.2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('72',  'horse.webp,claRed,TileStretch,True,0,0.2,1,0.2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('73',  'horse.webp,claWhite,Tile,True,0.5,0.2,0.5,0.8,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('74',  'horse.webp,claRed,Tile,True,0.5,0.2,0.5,0.8,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('75',  'horse.webp,claWhite,TileOriginal,True,0.5,0.2,0.5,0.8,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('76',  'horse.webp,claRed,TileOriginal,True,0.5,0.2,0.5,0.8,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('77',  'horse.webp,claWhite,TileStretch,True,0.5,0.2,0.5,0.8,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('78',  'horse.webp,claRed,TileStretch,True,0.5,0.2,0.5,0.8,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('79',  'horse.webp,claWhite,Tile,True,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('80',  'horse.webp,claRed,Tile,True,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('81',  'horse.webp,claWhite,TileOriginal,True,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('82',  'horse.webp,claRed,TileOriginal,True,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('83',  'horse.webp,claWhite,TileStretch,True,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('84',  'horse.webp,claRed,TileStretch,True,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('85',  'horse.webp,claWhite,Tile,True,0,-0.2,1,1.2,1,0.98,AmxSNwXca1988yC0P3/q90dfgP+uL2E/1hU4UcEm6atuPPDC7y8E8lZw70b0pSd/NQci7I08Bw')]
    [TestCase('86',  'horse.webp,claWhite,TileOriginal,True,0,-0.2,1,1.2,1,0.98,AmxSNwXca1988yC0P3/q90dfgP+uL2E/1hU4AYEd7ceOPe/kL3A/AVCYX0bry0PP9wEn2I27Bw')]
    [TestCase('87',  'horse.webp,claWhite,TileStretch,True,0,-0.2,1,1.2,1,0.98,52sCOALXh+998fw/wMDs/ZsPQAG3H4F/1xI0IQEm4Z8+fPXK4y8C8ldsP8Xyr/+e9QcjjL08Aw')]
    [TestCase('88',  'horse.webp,claWhite,Tile,False,0,-0.2,1,1.2,1,0.98,AmxSNwXca1988yC0P3/q90dfgP+uL2E/1hU4UcEm6atuPPDC7y8E8lZw70b0pSd/NQci7I08Bw')]
    [TestCase('89',  'horse.webp,claWhite,TileOriginal,False,0,-0.2,1,1.2,1,0.98,AmxSNwXca1988yC0P3/q90dfgP+uL2E/1hU4AYEd7ceOPe/kL3A/AVCYX0bry0PP9wEn2I27Bw')]
    [TestCase('90',  'horse.webp,claWhite,TileStretch,False,0,-0.2,1,1.2,1,0.98,52sCOALXh+998fw/wMDs/ZsPQAG3H4F/1xI0IQEm4Z8+fPXK4y8C8ldsP8Xyr/+e9QcjjL08Aw')]
    [TestCase('91',  'horse.webp,claWhite,Tile,True,0,-0.2,1,1.2,0.85,0.98,/GvSNwPYQx/98w2sT8DrDzAPQP2qDxG/2A1EsYAm4M9evO7P26/C8ldsT8fyrh8P9gYXAL68CQ')]
    [TestCase('92',  'horse.webp,claWhite,TileOriginal,True,0,-0.2,1,1.2,0.85,0.98,/GvSNwPYQx/98w2sT8DrDzAPQP2qDxG/2A1EccAe4+NOve3rD6D+/1KMz0brzjsPeAIb9O07Cg')]
    [TestCase('93',  'horse.webp,claWhite,TileStretch,True,0,-0.2,1,1.2,0.85,0.98,5GdiuADTV28+8u4jgAHuFHS///6yAzH/2QtAkcAl2cM+PPPWz//A8lho34Xxt/8utgcXsO08Bg')]
    [TestCase('94',  'horse.webp,claWhite,Tile,False,0,-0.2,1,1.2,0.85,0.98,/GvSNwPYQx/98w2sT8DrDzAPQP2qDxG/2A1EsYAm4M9evO7P26/C8ldsT8fyrh8P9gYXAL68CQ')]
    [TestCase('95',  'horse.webp,claWhite,TileOriginal,False,0,-0.2,1,1.2,0.85,0.98,/GvSNwPYQx/98w2sT8DrDzAPQP2qDxG/2A1EccAe4+NOve3rD6D+/1KMz0brzjsPeAIb9O07Cg')]
    [TestCase('96',  'horse.webp,claWhite,TileStretch,False,0,-0.2,1,1.2,0.85,0.98,5GdiuADTV28+8u4jgAHuFHS///6yAzH/2QtAkcAl2cM+PPPWz//A8lho34Xxt/8utgcXsO08Bg')]
    [TestCase('97',  'horse.webp,$7F0000FF,Tile,True,0,-0.2,1,1.2,1,0.98,z2fiOfzB057/89EHgETyPdhe/vmQfyD/3udjofslto+PO+oFQH/7/FlgD0fvx9tuNwQB3C39DA')]
    [TestCase('98',  'horse.webp,$7F0000FF,TileOriginal,True,0,-0.2,1,1.2,1,0.98,z2fiOfzB057/89EHgETyPdhe/vmQfyD/3udjsTwivXN/O+gSlB96/0xgP8ft2wePuAP24219DQ')]
    [TestCase('99',  'horse.webp,$7F0000FF,TileStretch,True,0,-0.2,1,1.2,1,0.98,0Wfi+fq9157/9MwDgMTyPeBO/vmNf/D+3uZroTsmtotf++kHPA/7/FpU30buyOdu9wT/5z09DQ')]
    [TestCase('100', 'horse.webp,$7F0000FF,Tile,False,0,-0.2,1,1.2,1,0.98,z2fiOfzB057/89EHgETyPdhe/vmQfyD/3udjofslto+PO+oFQH/7/FlgD0fvx9tuNwQB3C39DA')]
    [TestCase('101', 'horse.webp,$7F0000FF,TileOriginal,False,0,-0.2,1,1.2,1,0.98,z2fiOfzB057/89EHgETyPdhe/vmQfyD/3udjsTwivXN/O+gSlB96/0xgP8ft2wePuAP24219DQ')]
    [TestCase('102', 'horse.webp,$7F0000FF,TileStretch,False,0,-0.2,1,1.2,1,0.98,0Wfi+fq9157/9MwDgMTyPeBO/vmNf/D+3uZroTsmtotf++kHPA/7/FpU30buyOdu9wT/5z09DQ')]
    [TestCase('103', 'horse.webp,$7F0000FF,Tile,True,0,-0.2,1,1.2,0.85,0.98,0GcCevvBv94/9Mnzv8TyStRevviQd9B+4OZn4fsltYufu+kHRD87/Fpgb4fuy9+udwT69029Dg')]
    [TestCase('104', 'horse.webp,$7F0000FF,TileOriginal,True,0,-0.2,1,1.2,0.85,0.98,0GcCevvBv94/9Mnzv8TyStRevviQd9B+4OZn0bwiunePu+cTjB96/k9cj0ft3Aev+APx+339Dg')]
    [TestCase('105', 'horse.webp,$7F0000FF,TileStretch,True,0,-0.2,1,1.2,0.85,0.98,0mcCuvq9x97/9MXvvwTzS9g+fviOe7A+4OVr4TsmtYt/O+kJQO86/FtUP8fty+u+9wT5/139Dg')]
    [TestCase('106', 'horse.webp,$7F0000FF,Tile,False,0,-0.2,1,1.2,0.85,0.98,0GcCevvBv94/9Mnzv8TyStRevviQd9B+4OZn4fsltYufu+kHRD87/Fpgb4fuy9+udwT69029Dg')]
    [TestCase('107', 'horse.webp,$7F0000FF,TileOriginal,False,0,-0.2,1,1.2,0.85,0.98,0GcCevvBv94/9Mnzv8TyStRevviQd9B+4OZn0bwiunePu+cTjB96/k9cj0ft3Aev+APx+339Dg')]
    [TestCase('108', 'horse.webp,$7F0000FF,TileStretch,False,0,-0.2,1,1.2,0.85,0.98,0mcCuvq9x97/9MXvvwTzS9g+fviOe7A+4OVr4TsmtYt/O+kJQO86/FtUP8fty+u+9wT5/139Dg')]
    procedure TestFillPathBitmap2(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode; ABlending: Boolean; const ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',   'horse.webp,claWhite,Tile,True,0,0,1,1,0.4,0.98,6C8yPPvDl37B9MbTDwX8hCD/Pu2UpxD96t8Hgb8j0euf+uUfQK+7+j14T4j42dtOOg7sp27+Eg')]
    [TestCase('2',   'horse.webp,claRed,Tile,True,0,0,1,1,0.4,0.98,4BsivPa4n44B+5+Xf8X7gOR+PuuRt1D+59UDUb4kvt/POeUiaD+6+0WYT0jy0s/eeQ34Ox69Dw')]
    [TestCase('3',   'horse.webp,claWhite,Tile,True,0,0,1,1,1,0.98,A/CBvv/H854B9P5LcEMGadBfQOiSH6G87t5LwIIgDIiAeOYtJA9+/gisv4YNzZue+xoN3A4/Cw')]
    [TestCase('4',   'horse.webp,claRed,Tile,True,0,0,1,1,1,0.98,8b9RPvSrC7+BA56zr4QFYTg/v+OKR9G/58U/wD8j3GtQ9uQ2iH/6ABz4n4b9voOOOhkszK27Ag')]
    [TestCase('5',   'horse.webp,claWhite,Tile,True,0.15,0.15,0.85,0.85,0.4,0.98,24OyuQK1d34A769nz0TugfxOffGYs2C95eeTcf4jwTd/vOcGzI98+V1gr8ft689eeAXqRx4+EQ')]
    [TestCase('6',   'horse.webp,claRed,Tile,True,0.15,0.15,0.85,0.85,0.4,0.98,2ZMSuv6vk57A8qWHr4Xwg8yufPOPr7B94dJbwb0kwS8fPOgN0I97+mJMj8fs7sMOuAbvOy5+EQ')]
    [TestCase('7',   'horse.webp,claWhite,Tile,True,0.15,0.15,0.85,0.85,1,0.98,5MdCeBKknx5/5cQ778LjYng/PPOcN3E94vGrIYAh4scu/erwf0BA+1h0D8Xy/Xu+dgUK6D3+Bg')]
    [TestCase('8',   'horse.webp,claRed,Tile,True,0.15,0.15,0.85,0.85,1,0.98,3u8yuQiW535/762LD4XpZgCveviGKyE+17wfUb4j4bM+/OsAiMD9/WZEv0TwA2QedggWyE3+Bg')]
    [TestCase('9',   'horse.webp,claWhite,TileOriginal,True,0,0,1,1,0.4,0.98,6C8yPPvDl37B9MbTDwX8hCD/Pu2UpxD96t8Hgb8j0euf+uUfQK+7+j14T4j42dtOOg7sp27+Eg')]
    [TestCase('10',  'horse.webp,claRed,TileOriginal,True,0,0,1,1,0.4,0.98,4BsivPa4n44B+5+Xf8X7gOR+PuuRt1D+59UDUb4kvt/POeUiaD+6+0WYT0jy0s/eeQ34Ox69Dw')]
    [TestCase('11',  'horse.webp,claWhite,TileOriginal,True,0,0,1,1,1,0.98,A/CBvv/H854B9P5LcEMGadBfQOiSH6G87t5LwIIgDIiAeOYtJA9+/gisv4YNzZue+xoN3A4/Cw')]
    [TestCase('12',  'horse.webp,claRed,TileOriginal,True,0,0,1,1,1,0.98,8b9RPvSrC7+BA56zr4QFYTg/v+OKR9G/58U/wD8j3GtQ9uQ2iH/6ABz4n4b9voOOOhkszK27Ag')]
    [TestCase('13',  'horse.webp,claWhite,TileOriginal,True,0.15,0.15,0.85,0.85,0.4,0.98,24OyuQK1d34A765nz0TugfxeffGYs2C95eeTcf4jwTd/vOcGzI98+V1gr8ft689eeAXqRx4+EQ')]
    [TestCase('14',  'horse.webp,claRed,TileOriginal,True,0.15,0.15,0.85,0.85,0.4,0.98,2ZMSuv6vk57A8qWHr4Xwg8yufPOPr7B94dJbwb0kwS8fPOgN0I97+mJMj8fs7sMOuAbvOy5+EQ')]
    [TestCase('15',  'horse.webp,claWhite,TileOriginal,True,0.15,0.15,0.85,0.85,1,0.98,5MdCeBKknx5/5cQ778LjYng/PPOcN3E94vGrIYAh4scu/erwf0BA+1h0D8Xy/Xu+dgUK6D3+Bg')]
    [TestCase('16',  'horse.webp,claRed,TileOriginal,True,0.15,0.15,0.85,0.85,1,0.98,3u8yuQiW535/762LD4XpZgCveviGKyE+17wfUb4j4bM+/OsAiMD9/WZEv0TwA2QedggWyE3+Bg')]
    [TestCase('17',  'horse.webp,claWhite,TileStretch,True,0,0,1,1,0.4,0.98,5CtSPPy/o65B9cHXLwX7iUCv/u2Yx+C86+QjcX8j0O+fuuYgRF97+j+QDwj32cP++Q3uq/79Eg')]
    [TestCase('18',  'horse.webp,claRed,TileStretch,True,0,0,1,1,0.4,0.98,3h8SfPe1p45B+qCTf4X7g/Q+PuyTx0C+59T7QD4kvuOvueUibC86+0WkP8jx07/OuQz2Sw69EA')]
    [TestCase('19',  'horse.webp,claWhite,TileStretch,True,0,0,1,1,1,0.98,+ePBPgK9Ey8C9fJT0EMDdyCQP+qcZzH88OqXkEIgB4hgOOgvLF+9/Qzo/8UJz2PuehoU4N79Cg')]
    [TestCase('20',  'horse.webp,claRed,TileStretch,True,0,0,1,1,1,0.98,68sxfvalG9/BAaCvrwQFaGCPPuaPa6G/5sMzoH8i228QNuY1lF96/xwYcIb8wFdeOhcn+H17BQ')]
    [TestCase('21',  'horse.webp,claWhite,TileStretch,True,0.15,0.15,0.85,0.85,0.4,0.98,3H/CeQG4f36A765r/0TtgPROffGcv2D95emP0f4jwjePvOgDxM88+V5oz8fv6tdeeAblRy5+EQ')]
    [TestCase('22',  'horse.webp,claRed,TileStretch,True,0.15,0.15,0.85,0.85,0.4,0.98,24siOv6xl54A86aDz0Xwgsie/POSs8A94dNbEf4kwi8f/OgMzL/7+WNQj0ft7sv+dwfsNy4+EQ')]
    [TestCase('23',  'horse.webp,claWhite,TileStretch,True,0.15,0.15,0.85,0.85,1,0.98,5rtieA+ttx6/5sNHbwPhYGQf/PKnX4F94vafEUEh5Mtefe3ob9CA+luEb0X3+ZfONgj+701+Bw')]
    [TestCase('24',  'horse.webp,claRed,TileStretch,True,0.15,0.15,0.85,0.85,1,0.98,5NdieQeZ725/77CDXwXpZfhu+vmNP2G+1r8fIT8k5a8u/O39gzB+/GhI30TxA3jOtQoOwG1+Bg')]
    [TestCase('25',  'horse.webp,claWhite,Tile,False,0,0,1,1,0.4,0.98,6C8yPPvDl37B9MbTDwX8hCD/Pu2UpxD96t8Hgb8j0euf+uUfQK+7+j14T4j42dtOOg7sp27+Eg')]
    [TestCase('26',  'horse.webp,claRed,Tile,False,0,0,1,1,0.4,0.98,4BsivPa4n44B+5+Xf8X7gOR+PuuRt1D+59UDUb4kvt/POeUiaD+6+0WYT0jy0s/eeQ34Ox69Dw')]
    [TestCase('27',  'horse.webp,claWhite,Tile,False,0,0,1,1,1,0.98,A/CBvv/H854B9P5LcEMGadBfQOiSH6G87t5LwIIgDIiAeOYtJA9+/gisv4YNzZue+xoN3A4/Cw')]
    [TestCase('28',  'horse.webp,claRed,Tile,False,0,0,1,1,1,0.98,8b9RPvSrC7+BA56zr4QFYTg/v+OKR9G/58U/wD8j3GtQ9uQ2iH/6ABz4n4b9voOOOhkszK27Ag')]
    [TestCase('29',  'horse.webp,claWhite,Tile,False,0.15,0.15,0.85,0.85,0.4,0.98,24OyuQK1d34A769nz0TugfxOffGYs2C95eeTcf4jwTd/vOcGzI98+V1gr8ft689eeAXqRx4+EQ')]
    [TestCase('30',  'horse.webp,claRed,Tile,False,0.15,0.15,0.85,0.85,0.4,0.98,2ZMSuv6vk57A8qWHr4Xwg8yufPOPr7B94dJbwb0kwS8fPOgN0I97+mJMj8fs7sMOuAbvOy5+EQ')]
    [TestCase('31',  'horse.webp,claWhite,Tile,False,0.15,0.15,0.85,0.85,1,0.98,5MdCeBKknx5/5cQ778LjYng/PPOcN3E94vGrIYAh4scu/erwf0BA+1h0D8Xy/Xu+dgUK6D3+Bg')]
    [TestCase('32',  'horse.webp,claRed,Tile,False,0.15,0.15,0.85,0.85,1,0.98,3u8yuQiW535/762LD4XpZgCveviGKyE+17wfUb4j4bM+/OsAiMD9/WZEv0TwA2QedggWyE3+Bg')]
    [TestCase('33',  'horse.webp,claWhite,TileOriginal,False,0,0,1,1,0.4,0.98,6C8yPPvDl37B9MbTDwX8hCD/Pu2UpxD96t8Hgb8j0euf+uUfQK+7+j14T4j42dtOOg7sp27+Eg')]
    [TestCase('34',  'horse.webp,claRed,TileOriginal,False,0,0,1,1,0.4,0.98,4BsivPa4n44B+5+Xf8X7gOR+PuuRt1D+59UDUb4kvt/POeUiaD+6+0WYT0jy0s/eeQ34Ox69Dw')]
    [TestCase('35',  'horse.webp,claWhite,TileOriginal,False,0,0,1,1,1,0.98,A/CBvv/H854B9P5LcEMGadBfQOiSH6G87t5LwIIgDIiAeOYtJA9+/gisv4YNzZue+xoN3A4/Cw')]
    [TestCase('36',  'horse.webp,claRed,TileOriginal,False,0,0,1,1,1,0.98,8b9RPvSrC7+BA56zr4QFYTg/v+OKR9G/58U/wD8j3GtQ9uQ2iH/6ABz4n4b9voOOOhkszK27Ag')]
    [TestCase('37',  'horse.webp,claWhite,TileOriginal,False,0.15,0.15,0.85,0.85,0.4,0.98,24OyuQK1d34A765nz0TugfxeffGYs2C95eeTcf4jwTd/vOcGzI98+V1gr8ft689eeAXqRx4+EQ')]
    [TestCase('38',  'horse.webp,claRed,TileOriginal,False,0.15,0.15,0.85,0.85,0.4,0.98,2ZMSuv6vk57A8qWHr4Xwg8yufPOPr7B94dJbwb0kwS8fPOgN0I97+mJMj8fs7sMOuAbvOy5+EQ')]
    [TestCase('39',  'horse.webp,claWhite,TileOriginal,False,0.15,0.15,0.85,0.85,1,0.98,5MdCeBKknx5/5cQ778LjYng/PPOcN3E94vGrIYAh4scu/erwf0BA+1h0D8Xy/Xu+dgUK6D3+Bg')]
    [TestCase('40',  'horse.webp,claRed,TileOriginal,False,0.15,0.15,0.85,0.85,1,0.98,3u8yuQiW535/762LD4XpZgCveviGKyE+17wfUb4j4bM+/OsAiMD9/WZEv0TwA2QedggWyE3+Bg')]
    [TestCase('41',  'horse.webp,claWhite,TileStretch,False,0,0,1,1,0.4,0.98,5CtSPPy/o65B9cHXLwX7iUCv/u2Yx+C86+QjcX8j0O+fuuYgRF97+j+QDwj32cP++Q3uq/79Eg')]
    [TestCase('42',  'horse.webp,claRed,TileStretch,False,0,0,1,1,0.4,0.98,3h8SfPe1p45B+qCTf4X7g/Q+PuyTx0C+59T7QD4kvuOvueUibC86+0WkP8jx07/OuQz2Sw69EA')]
    [TestCase('43',  'horse.webp,claWhite,TileStretch,False,0,0,1,1,1,0.98,+ePBPgK9Ey8C9fJT0EMDdyCQP+qcZzH88OqXkEIgB4hgOOgvLF+9/Qzo/8UJz2PuehoU4N79Cg')]
    [TestCase('44',  'horse.webp,claRed,TileStretch,False,0,0,1,1,1,0.98,68sxfvalG9/BAaCvrwQFaGCPPuaPa6G/5sMzoH8i228QNuY1lF96/xwYcIb8wFdeOhcn+H17BQ')]
    [TestCase('45',  'horse.webp,claWhite,TileStretch,False,0.15,0.15,0.85,0.85,0.4,0.98,3H/CeQG4f36A765r/0TtgPROffGcv2D95emP0f4jwjePvOgDxM88+V5oz8fv6tdeeAblRy5+EQ')]
    [TestCase('46',  'horse.webp,claRed,TileStretch,False,0.15,0.15,0.85,0.85,0.4,0.98,24siOv6xl54A86aDz0Xwgsie/POSs8A94dNbEf4kwi8f/OgMzL/7+WNQj0ft7sv+dwfsNy4+EQ')]
    [TestCase('47',  'horse.webp,claWhite,TileStretch,False,0.15,0.15,0.85,0.85,1,0.98,5rtieA+ttx6/5sNHbwPhYGQf/PKnX4F94vafEUEh5Mtefe3ob9CA+luEb0X3+ZfONgj+701+Bw')]
    [TestCase('48',  'horse.webp,claRed,TileStretch,False,0.15,0.15,0.85,0.85,1,0.98,5NdieQeZ725/77CDXwXpZfhu+vmNP2G+1r8fIT8k5a8u/O39gzB+/GhI30TxA3jOtQoOwG1+Bg')]
    [TestCase('49',  'horse.webp,claWhite,Tile,True,0,0.2,1,0.8,0.4,0.98,txMSegG2ly7B8auzf4Xvb8iefvOinyD96ueXIT4eyKdfu+cRwO86+16Afwfy4a+e+QPtU14+Fw')]
    [TestCase('50',  'horse.webp,claRed,Tile,True,0,0.2,1,0.8,0.4,0.98,yVMyuvy2r44A9Z6T/4Xze5yOffOjW3C95tFvQb0fv5v/eucTvB96+2OI70fw459u+QTuVy7+Fg')]
    [TestCase('51',  'horse.webp,claWhite,Tile,True,0,0.2,1,0.8,1,0.98,iasxeQ+o886A7Lv3n4TmNvR+P/i2C+H87vCzQT8T9Ntf+uoKZDC8/1rEn8T85DfueQEQDN7+FQ')]
    [TestCase('52',  'horse.webp,claRed,Tile,True,0,0.2,1,0.8,1,0.98,uEdyeQOmM1+/9Juv78XwVIi+fPi4X4C95LxLEf0W38NveeoQWDC6AGfQz4X46Adu+QMTEG4+FQ')]
    [TestCase('53',  'horse.webp,claWhite,TileOriginal,True,0,0.2,1,0.8,0.4,0.98,txMSegG2ly7B8auzf4Xvb8iefvOinyD96ueXIT4eyKdfu+cRwO86+16Afwfy4a+e+QPtU14+Fw')]
    [TestCase('54',  'horse.webp,claRed,TileOriginal,True,0,0.2,1,0.8,0.4,0.98,yVMyuvy2r44A9Z6T/4Xze5yOffOjW3C95tFvQb0fv5v/eucTvB96+2OI70fw459u+QTuVy7+Fg')]
    [TestCase('55',  'horse.webp,claWhite,TileOriginal,True,0,0.2,1,0.8,1,0.98,iqsxeQ+o886A7Lv3n4TmNvR+P/i2C+H87vCzQT8T9Ntf+uoKZDC8/1rEn8T85DfueQEQDN7+FQ')]
    [TestCase('56',  'horse.webp,claRed,TileOriginal,True,0,0.2,1,0.8,1,0.98,uEdyeQOmM1+/9Juv78XwVIi+fPi4X4C95LxPEf0W38NveeoQWDC6AGfQz4X46Adu+QMTEG4+FQ')]
    [TestCase('57',  'horse.webp,claWhite,TileStretch,True,0,0.2,1,0.8,0.4,0.98,vBMiOgC5m/6A8qafn8Xucche/vKqm0A96+aTgb4dzadPO+kTtP+6+l+snwfz37PeuQToZw5+Fw')]
    [TestCase('58',  'horse.webp,claRed,TileStretch,True,0,0.2,1,0.8,0.4,0.98,y09Suvy1t44A9Z2PPwb0fKxe/fOmY5C95tFvcb0fwZ/f+ucUsD86+2OU/0fw5Kd+uQXsV/59Fw')]
    [TestCase('59',  'horse.webp,claWhite,TileStretch,True,0,0.2,1,0.8,1,0.98,ladBOQyvA1+A7q7P7wTlO/C+/vbI+zD97++rUYARANwveu4QREC8/l4w8MT/3TuOegMFOB5+Fg')]
    [TestCase('60',  'horse.webp,claRed,TileStretch,True,0,0.2,1,0.8,1,0.98,vD/ieQOlR0//9Jifn8bxV7BOvPm+c+A95LpTof0W4ssPuesROHC6/2fw74X46R+eOQYPEO79FQ')]
    [TestCase('61',  'horse.webp,claWhite,Tile,True,0,0.2,1,0.8,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('62',  'horse.webp,claRed,Tile,True,0,0.2,1,0.8,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('63',  'horse.webp,claWhite,TileOriginal,True,0,0.2,1,0.8,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('64',  'horse.webp,claRed,TileOriginal,True,0,0.2,1,0.8,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('65',  'horse.webp,claWhite,TileStretch,True,0,0.2,1,0.8,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('66',  'horse.webp,claRed,TileStretch,True,0,0.2,1,0.8,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('67',  'horse.webp,claWhite,Tile,True,0,0.2,1,0.2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('68',  'horse.webp,claRed,Tile,True,0,0.2,1,0.2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('69',  'horse.webp,claWhite,TileOriginal,True,0,0.2,1,0.2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('70',  'horse.webp,claRed,TileOriginal,True,0,0.2,1,0.2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('71',  'horse.webp,claWhite,TileStretch,True,0,0.2,1,0.2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('72',  'horse.webp,claRed,TileStretch,True,0,0.2,1,0.2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('73',  'horse.webp,claWhite,Tile,True,0.5,0.2,0.5,0.8,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('74',  'horse.webp,claRed,Tile,True,0.5,0.2,0.5,0.8,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('75',  'horse.webp,claWhite,TileOriginal,True,0.5,0.2,0.5,0.8,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('76',  'horse.webp,claRed,TileOriginal,True,0.5,0.2,0.5,0.8,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('77',  'horse.webp,claWhite,TileStretch,True,0.5,0.2,0.5,0.8,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('78',  'horse.webp,claRed,TileStretch,True,0.5,0.2,0.5,0.8,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('79',  'horse.webp,claWhite,Tile,True,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('80',  'horse.webp,claRed,Tile,True,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('81',  'horse.webp,claWhite,TileOriginal,True,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('82',  'horse.webp,claRed,TileOriginal,True,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('83',  'horse.webp,claWhite,TileStretch,True,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('84',  'horse.webp,claRed,TileStretch,True,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('85',  'horse.webp,claWhite,Tile,True,0,-0.2,1,1.2,1,0.98,FZwhQu6jL3+C8iSgAAUII/iwxOWHD0FB6r9TIIEV4pfyu/g3nG6+BxjwzoAJBQA/viAK6M6BCQ')]
    [TestCase('86',  'horse.webp,claWhite,TileOriginal,True,0,-0.2,1,1.2,1,0.98,FZwhQu6jL3+C8iSgAAUII/iwxOWHD0FB6r9TcIIfCEgA/OY3gM97AAp4MIkD4XeOeg//N59+DA')]
    [TestCase('87',  'horse.webp,claWhite,TileStretch,True,0,-0.2,1,1.2,1,0.98,DDixAu67W6+D7wz40MUHNzhBhOqjH/F/6rpL8EEY3lOye/0ylG69AxksH8EHB5ye/SMGYP6ADA')]
    [TestCase('88',  'horse.webp,claWhite,Tile,False,0,-0.2,1,1.2,1,0.98,FZwhQu6jL3+C8iSgAAUII/iwxOWHD0FB6r9TIIEV4pfyu/g3nG6+BxjwzoAJBQA/viAK6M6BCQ')]
    [TestCase('89',  'horse.webp,claWhite,TileOriginal,False,0,-0.2,1,1.2,1,0.98,FZwhQu6jL3+C8iSgAAUII/iwxOWHD0FB6r9TcIIfCEgA/OY3gM97AAp4MIkD4XeOeg//N59+DA')]
    [TestCase('90',  'horse.webp,claWhite,TileStretch,False,0,-0.2,1,1.2,1,0.98,DDixAu67W6+D7wz40MUHNzhBhOqjH/F/6rpL8EEY3lOye/0ylG69AxksH8EHB5ye/SMGYP6ADA')]
    [TestCase('91',  'horse.webp,claWhite,Tile,True,0,-0.2,1,1.2,0.85,0.98,DLgBwe+nD08C8xB0IEUFNKCwQ+eJ86DA6cR/kAAY2SPyu/UyuL59BSMAD8IE/wN/vRwD2D7BCw')]
    [TestCase('92',  'horse.webp,claWhite,TileOriginal,True,0,-0.2,1,1.2,0.85,0.98,DLgBwe+nD08C8xB0IEUFNKCwQ+eJ86DA6cR/sIEg+isAvOYyfI87/xdMMMn/4Ytu+g35H48+Dg')]
    [TestCase('93',  'horse.webp,claWhite,TileStretch,True,0,-0.2,1,1.2,0.85,0.98,BGCBge+8M1+D8PzD4AUFRdRQQ+uh/5A/6sB7QAEa1ufB+/kusO78ASQ0X4IDAaz+fB/+Z45ADg')]
    [TestCase('94',  'horse.webp,claWhite,Tile,False,0,-0.2,1,1.2,0.85,0.98,DLgBwe+nD08C8xB0IEUFNKCwQ+eJ86DA6cR/kAAY2SPyu/UyuL59BSMAD8IE/wN/vRwD2D7BCw')]
    [TestCase('95',  'horse.webp,claWhite,TileOriginal,False,0,-0.2,1,1.2,0.85,0.98,DLgBwe+nD08C8xB0IEUFNKCwQ+eJ86DA6cR/sIEg+isAvOYyfI87/xdMMMn/4Ytu+g35H48+Dg')]
    [TestCase('96',  'horse.webp,claWhite,TileStretch,False,0,-0.2,1,1.2,0.85,0.98,BGCBge+8M1+D8PzD4AUFRdRQQ+uh/5A/6sB7QAEa1ufB+/kusO78ASQ0X4IDAaz+fB/+Z45ADg')]
    [TestCase('97',  'horse.webp,$7F0000FF,Tile,True,0,-0.2,1,1.2,1,0.98,vfch/PaJr17C9aT3jwX4aKg/gOGGi4A/5srjoP0ewYMweuoNpE97AGko70T63FcOOQ0CXK7+Eg')]
    [TestCase('98',  'horse.webp,$7F0000FF,TileOriginal,True,0,-0.2,1,1.2,1,0.98,vfch/PaJr17C9aT3jwX4aKg/gOGGi4A/5srjIP4gvO/fOeQjiA96/j+c78jx5iuOOQn2c/59EA')]
    [TestCase('99',  'horse.webp,$7F0000FF,TileStretch,True,0,-0.2,1,1.2,1,0.98,uucRfPaGr36C9Z4D0AX3aLQvgOGEkyC/4svPAD4fuXvwuewLjP/6/mJEL8X44Ftu+Q0AVF4+FA')]
    [TestCase('100', 'horse.webp,$7F0000FF,Tile,False,0,-0.2,1,1.2,1,0.98,vfch/PaJr17C9aT3jwX4aKg/gOGGi4A/5srjoP0ewYMweuoNpE97AGko70T63FcOOQ0CXK7+Eg')]
    [TestCase('101', 'horse.webp,$7F0000FF,TileOriginal,False,0,-0.2,1,1.2,1,0.98,vfch/PaJr17C9aT3jwX4aKg/gOGGi4A/5srjIP4gvO/fOeQjiA96/j+c78jx5iuOOQn2c/59EA')]
    [TestCase('102', 'horse.webp,$7F0000FF,TileStretch,False,0,-0.2,1,1.2,1,0.98,uucRfPaGr36C9Z4D0AX3aLQvgOGEkyC/4svPAD4fuXvwuewLjP/6/mJEL8X44Ftu+Q0AVF4+FA')]
    [TestCase('103', 'horse.webp,$7F0000FF,Tile,True,0,-0.2,1,1.2,0.85,0.98,wQfiO/eRoz7C9aTjn4X3b4Tvv+OIgzB/5s77kP0fvltwuukOmB87/2gwnwX43HMe+Qv7Y57+Ew')]
    [TestCase('104', 'horse.webp,$7F0000FF,TileOriginal,True,0,-0.2,1,1.2,0.85,0.98,wQfiO/eRoz7C9aTjn4X3b4Tvv+OIgzB/5s77AL4hut8veuQhhA+6/USQ/8jw5UuOuQjxc/59EQ')]
    [TestCase('105', 'horse.webp,$7F0000FF,TileStretch,True,0,-0.2,1,1.2,0.85,0.98,vvfhu/aPo06C9Z/v38X2b4zfv+OGi+B+487r8D0gt1dAuusNhN/6/WJIz8X24HNuuQz6W16+FA')]
    [TestCase('106', 'horse.webp,$7F0000FF,Tile,False,0,-0.2,1,1.2,0.85,0.98,wQfiO/eRoz7C9aTjn4X3b4Tvv+OIgzB/5s77kP0fvltwuukOmB87/2gwnwX43HMe+Qv7Y57+Ew')]
    [TestCase('107', 'horse.webp,$7F0000FF,TileOriginal,False,0,-0.2,1,1.2,0.85,0.98,wQfiO/eRoz7C9aTjn4X3b4Tvv+OIgzB/5s77AL4hut8veuQhhA+6/USQ/8jw5UuOuQjxc/59EQ')]
    [TestCase('108', 'horse.webp,$7F0000FF,TileStretch,False,0,-0.2,1,1.2,0.85,0.98,vvfhu/aPo06C9Z/v38X2b4zfv+OGi+B+487r8D0gt1dAuusNhN/6/WJIz8X24HNuuQz6W16+FA')]
    procedure TestFillPathBitmap3(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode; ABlending: Boolean; const ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  'horse.webp,claWhite,Tile,true,1,0.98,+yOegQIknAD+C+kX0Hr+l3fwPgEk4L8C8/DHXv/fLvzfgAb/7/A9/rcLvzgOAFgggwE2WG9B8A')]
    [TestCase('2',  'horse.webp,claWhite,Tile,true,0.5,0.98,5SeOQQERoEC+Dd4PUHv6j2sQfwEk+E/D8/nbXn7gJAQwwAb9+8A9/70vj/gK/SfQAv88SN8A8Q')]
    [TestCase('3',  'horse.webp,claWhite,Tile,false,1,0.98,+yOegQIknAD+C+kX0Hr+l3fwPgEk4L8C8/DHXv/fLvzfgAb/7/A9/rcLvzgOAFgggwE2WG9B8A')]
    [TestCase('4',  'horse.webp,claWhite,Tile,false,0.5,0.98,5SeOQQERoEC+Dd4PUHv6j2sQfwEk+E/D8/nbXn7gJAQwwAb9+8A9/70vj/gK/SfQAv88SN8A8Q')]
    [TestCase('5',  'horse.webp,claRed,Tile,true,1,0.98,6fNtwfYIuCA+G4l//7v9jt/fPfwbCPDF69e/Tnzi/tu/PgUJUGF6AMtXrzj+8UMQwv9VSB7+5w')]
    [TestCase('6',  'horse.webp,claRed,Tile,true,0.5,0.98,3A9ugfsDrFB+Fa7D7zv6ih+A/v4gDNBE8OzX3rzhDPQfPwYBKAF8AMZTf/gC9htAQv5LwC7/7A')]
    [TestCase('7',  'horse.webp,claRed,Tile,false,1,0.98,6fNtwfYIuCA+G4l//7v9jt/fPfwbCPDF69e/Tnzi/tu/PgUJUGF6AMtXrzj+8UMQwv9VSB7+5w')]
    [TestCase('8',  'horse.webp,claRed,Tile,false,0.5,0.98,3A9ugfsDrFB+Fa7D7zv6ih+A/v4gDNBE8OzX3rzhDPQfPwYBKAF8AMZTf/gC9htAQv5LwC7/7A')]
    [TestCase('9',  'horse.webp,claWhite,TileOriginal,true,1,0.98,+yOegQIknAD+C+kX0Hr+l3fwPgEk4L8C8/DHXv/fLvzfgAb/7/A9/rcLvzgOAFgggwE2WG9B8A')]
    [TestCase('10', 'horse.webp,claWhite,TileOriginal,true,0.5,0.98,5SeOQQERoEC+Dd4PUHv6j2sQfwEk+E/D8/nbXn7gJAQwwAb9+8A9/70vj/gK/SfQAv88SN8A8Q')]
    [TestCase('11', 'horse.webp,claWhite,TileOriginal,false,1,0.98,+yOegQIknAD+C+kX0Hr+l3fwPgEk4L8C8/DHXv/fLvzfgAb/7/A9/rcLvzgOAFgggwE2WG9B8A')]
    [TestCase('12', 'horse.webp,claWhite,TileOriginal,false,0.5,0.98,5SeOQQERoEC+Dd4PUHv6j2sQfwEk+E/D8/nbXn7gJAQwwAb9+8A9/70vj/gK/SfQAv88SN8A8Q')]
    [TestCase('13', 'horse.webp,claRed,TileOriginal,true,1,0.98,6fNtwfYIuCA+G4l//7v9jt/fPfwbCPDF69e/Tnzi/tu/PgUJUGF6AMtXrzj+8UMQwv9VSB7+5w')]
    [TestCase('14', 'horse.webp,claRed,TileOriginal,true,0.5,0.98,3A9ugfsDrFB+Fa7D7zv6ih+A/v4gDNBE8OzX3rzhDPQfPwYBKAF8AMZTf/gC9htAQv5LwC7/7A')]
    [TestCase('15', 'horse.webp,claRed,TileOriginal,false,1,0.98,6fNtwfYIuCA+G4l//7v9jt/fPfwbCPDF69e/Tnzi/tu/PgUJUGF6AMtXrzj+8UMQwv9VSB7+5w')]
    [TestCase('16', 'horse.webp,claRed,TileOriginal,false,0.5,0.98,3A9ugfsDrFB+Fa7D7zv6ih+A/v4gDNBE8OzX3rzhDPQfPwYBKAF8AMZTf/gC9htAQv5LwC7/7A')]
    [TestCase('17', 'horse.webp,claWhite,TileStretch,true,1,0.98,8RfewQQavJC+DN0fIDv7pccw/gItLEBC9fwTH7/fKvy/QAgB9EA9/btH/3cKAiBwAgE9XE8A8A')]
    [TestCase('18', 'horse.webp,claWhite,TileStretch,true,0.5,0.98,4B+egQIMsIA+DtgTcPv4lpOwPgIpHADD9P8DP37gIgQgwAf+/2C9/r5LL/gI/gtwwv5ASE8A8Q')]
    [TestCase('19', 'horse.webp,claWhite,TileStretch,false,1,0.98,8RfewQQavJC+DN0fIDv7pccw/gItLEBC9fwTH7/fKvy/QAgB9EA9/btH/3cKAiBwAgE9XE8A8A')]
    [TestCase('20', 'horse.webp,claWhite,TileStretch,false,0.5,0.98,4B+egQIMsIA+DtgTcPv4lpOwPgIpHADD9P8DP37gIgQgwAf+/2C9/r5LL/gI/gtwwv5ASE8A8Q')]
    [TestCase('21', 'horse.webp,claRed,TileStretch,true,1,0.98,4/tNQfkBxEC+GYt7/zv9lgsgvf4hLLAF69WvPrzh/eNvPgYHXEE6/8t3fzj98xfgwf1QcO696g')]
    [TestCase('22', 'horse.webp,claRed,TileStretch,true,0.5,0.98,2RNewfwAtGB+FK/D7/v5jjcwPgAjHLDE7+zPznzhDPT//gYBLPG7/8Zjb3gC9wMwQv1J1B5/7g')]
    [TestCase('23', 'horse.webp,claRed,TileStretch,false,1,0.98,4/tNQfkBxEC+GYt7/zv9lgsgvf4hLLAF69WvPrzh/eNvPgYHXEE6/8t3fzj98xfgwf1QcO696g')]
    [TestCase('24', 'horse.webp,claRed,TileStretch,false,0.5,0.98,2RNewfwAtGB+FK/D7/v5jjcwPgAjHLDE7+zPznzhDPT//gYBLPG7/8Zjb3gC9wMwQv1J1B5/7g')]
    procedure TestFillPathBitmapWithChessBackground(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode; ABlending: Boolean; const AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  'horse.webp,claWhite,Tile,true,1,0.98,1FeSuvfBVx4B9aJ/D0YGbNRPwOiSG4F87+BPUP0krH8PPOUUUB96+GCw3wYOz6N+uxoK3A7/Cw')]
    [TestCase('2',  'horse.webp,claWhite,Tile,true,0.5,0.98,1Vei+vfAVz5B9aF/Dwb+gEAvf+yUu/C86+DnUD0lq4MPfOUVVB96+GCEH0j819NuOhDws47+EQ')]
    [TestCase('3',  'horse.webp,claWhite,Tile,false,1,0.98,1FeSuvfBVx4B9aJ/D0YGbNRPwOiSG4F87+BPUP0krH8PPOUUUB96+GCw3wYOz6N+uxoK3A7/Cw')]
    [TestCase('4',  'horse.webp,claWhite,Tile,false,0.5,0.98,1Vei+vfAVz5B9aF/Dwb+gEAvf+yUu/C86+DnUD0lq4MPfOUVVB96+GCEH0j819NuOhDws47+EQ')]
    [TestCase('5',  'horse.webp,claRed,Tile,true,1,0.98,1Fei+vfAVz4B9aF//4UFYjgvf+OJQ7E/6MdHUD0lq4P/O+UVUB96+GD4vwb+wIde+hgp2J17Aw')]
    [TestCase('6',  'horse.webp,claRed,Tile,true,0.5,0.98,1Fei+vfAV05B9aGD/4X9fPSe/umPz4A+6NPjUH0lq4MPfOUVUB86+GCoD4j00MfeOQ//L968DQ')]
    [TestCase('7',  'horse.webp,claRed,Tile,false,1,0.98,1Fei+vfAVz4B9aF//4UFYjgvf+OJQ7E/6MdHUD0lq4P/O+UVUB96+GD4vwb+wIde+hgp2J17Aw')]
    [TestCase('8',  'horse.webp,claRed,Tile,false,0.5,0.98,1Fei+vfAV05B9aGD/4X9fPSe/umPz4A+6NPjUH0lq4MPfOUVUB86+GCoD4j00MfeOQ//L968DQ')]
    [TestCase('9',  'horse.webp,claWhite,TileOriginal,true,1,0.98,1FeSuvfBVx4B9aJ/D0YGbNRPwOiSG4F87+BPUP0krH8PPOUUUB96+GCw3wYOz6N+uxoK3A7/Cw')]
    [TestCase('10', 'horse.webp,claWhite,TileOriginal,true,0.5,0.98,1Vei+vfAVz5B9aF/D8b9gEAvf+yUu/C86+DnUD0lq4MPfOUVVB96+GCEH0j819NuOhDws47+EQ')]
    [TestCase('11', 'horse.webp,claWhite,TileOriginal,false,1,0.98,1FeSuvfBVx4B9aJ/D0YGbNRPwOiSG4F87+BPUP0krH8PPOUUUB96+GCw3wYOz6N+uxoK3A7/Cw')]
    [TestCase('12', 'horse.webp,claWhite,TileOriginal,false,0.5,0.98,1Vei+vfAVz5B9aF/D8b9gEAvf+yUu/C86+DnUD0lq4MPfOUVVB96+GCEH0j819NuOhDws47+EQ')]
    [TestCase('13', 'horse.webp,claRed,TileOriginal,true,1,0.98,1Fei+vfAVz4B9aF//4UFYjgvf+OJQ7E/6MdHUD0lq4P/O+UVUB96+GD4vwb+wIde+hgp2J17Aw')]
    [TestCase('14', 'horse.webp,claRed,TileOriginal,true,0.5,0.98,1Fei+vfAV05B9aGD/4X9fPSe/umPz4A+6NPjUH0lq4MPfOUVUB86+GCoD4j00MfeOQ//L968DQ')]
    [TestCase('15', 'horse.webp,claRed,TileOriginal,false,1,0.98,1Fei+vfAVz4B9aF//4UFYjgvf+OJQ7E/6MdHUD0lq4P/O+UVUB96+GD4vwb+wIde+hgp2J17Aw')]
    [TestCase('16', 'horse.webp,claRed,TileOriginal,false,0.5,0.98,1Fei+vfAV05B9aGD/4X9fPSe/umPz4A+6NPjUH0lq4MPfOUVUB86+GCoD4j00MfeOQ//L968DQ')]
    [TestCase('17', 'horse.webp,claWhite,TileStretch,true,1,0.98,1FeSuvfBVx4B9aJ//0UDeiSAv+qbZxG88eybUP0krH8PPOUUUB96+GDsH0YK0GfOOhoS4N69Cw')]
    [TestCase('18', 'horse.webp,claWhite,TileStretch,true,0.5,0.98,1Vei+vfAVz5B9aF/D0b8iGjPfu2Y37D87OYPUT0lq4MPfOUVVB96+GCgv4f62LcO+g/0s/79EQ')]
    [TestCase('19', 'horse.webp,claWhite,TileStretch,false,1,0.98,1FeSuvfBVx4B9aJ//0UDeiSAv+qbZxG88eybUP0krH8PPOUUUB96+GDsH0YK0GfOOhoS4N69Cw')]
    [TestCase('20', 'horse.webp,claWhite,TileStretch,false,0.5,0.98,1Vei+vfAVz5B9aF/D0b8iGjPfu2Y37D87OYPUT0lq4MPfOUVVB96+GCgv4f62LcO+g/0s/79EQ')]
    [TestCase('21', 'horse.webp,claRed,TileStretch,true,1,0.98,1FOi+vfAVz4B9aF//wUFamSP/uWOY4E/58U3UD0lq4P/O+UVUB96+GAYkAb9wl8u+hYkAG47Bg')]
    [TestCase('22', 'horse.webp,claRed,TileStretch,true,0.5,0.98,1Fei+vfAV05B9aGD/0X9fwhPPuuS33C+59LfUH0lq4P/e+UVVB86+GC4/8fz0a++OQ79Q748Dw')]
    [TestCase('23', 'horse.webp,claRed,TileStretch,false,1,0.98,1FOi+vfAVz4B9aF//wUFamSP/uWOY4E/58U3UD0lq4P/O+UVUB96+GAYkAb9wl8u+hYkAG47Bg')]
    [TestCase('24', 'horse.webp,claRed,TileStretch,false,0.5,0.98,1Fei+vfAV05B9aGD/0X9fwhPPuuS33C+59LfUH0lq4P/e+UVVB86+GC4/8fz0a++OQ79Q748Dw')]
    procedure TestFillPathBitmapWithClipping(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode; ABlending: Boolean; const AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  'horse.webp,300,300,Tile,1,2,-50,-100,30,-200,-50,660,343,1,true,0.98,1NstRf8j2FABDulf4D3n1Oefu/m0j/FGx+S/73voC5yvPf/2J+G699l7n7/y4K9/OAdi5Cz79A')]
    [TestCase('2',  'horse.webp,300,300,Tile,1,2,-50,-100,30,-200,-50,660,343,0.6,true,0.98,1Kf/QPz7218BBMwHIMHsImif/PWoExFD1ONzcPwA5ZMP/fQCbJD69w9sf0Pv4GvPuAYqiF38Ag')]
    [TestCase('3',  'horse.webp,300,300,Tile,1,2,-50,-100,30,0,0,200,200,1,true,0.98,/w/xeufJr24AHG+zrn31lawOPvCVV1A96OCDIb4ltGtvvOIWuN/59VlUX8nq4ANfeQXVgw4+GA')]
    [TestCase('4',  'horse.webp,300,300,Tile,1,2,-50,-100,30,0,0,200,200,0.6,true,0.98,7pPROu7Fi86ADIMHD0H1lawOPvCVV1A96OCD0b0lsHdP/OMWkO+59lxUX8nq4ANfeQXVgw4+GA')]
    [TestCase('5',  'horse.webp,300,300,Tile,1,-2,50,500,30,-200,-50,660,343,1,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBAD4vh3SOfPfO2j9/6FA')]
    [TestCase('6',  'horse.webp,300,300,Tile,-1,2,-50,-100,30,-200,-50,660,343,0.6,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('7',  'horse.webp,300,300,Tile,-1,2,250,-50,30,0,0,200,200,1,true,0.98,ExjOhQEosOC/Cimgvz72WojuPOuNb6AC46/vIP0ExJe/+t0SyCD07wlUX8nq4ANfeQXVgw4+GA')]
    [TestCase('8',  'horse.webp,300,300,Tile,1,-2,-50,200,30,0,0,200,200,0.6,true,0.98,0EfxPQHV46+A6wJw4MTxEBQAvPOcI2GB3shLQIIWBoDfvuz3zw99AEN8P0jr1gs/eQb1Rz69Ew')]
    [TestCase('9',  'horse.webp,300,300,TileOriginal,1,2,150,20,-90,-200,-50,660,343,1,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('10', 'horse.webp,300,300,TileOriginal,1,2,-50,100,-90,-200,50,660,343,0.6,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('11', 'horse.webp,300,300,TileOriginal,1,2,50,100,20,0,0,200,200,1,true,0.98,1leiuvjBW36B9Z+DD0b1lawOPvCVV1A96OCD8UH/ItSQAwz31+7GBgqAf8Tqwg+vfAgTJD79Cg')]
    [TestCase('12', 'horse.webp,300,300,TileOriginal,1,2,50,100,20,0,0,200,200,0.6,true,0.98,1leievjBW26B9aCDD0b1lawOPvCVV1A96OCDEYAO8k+QgPwDCM/BACxwf8bqzgtfOwf7S449EA')]
    [TestCase('13', 'horse.webp,300,300,TileOriginal,1,-2,50,500,30,-200,-50,660,343,1,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBAD4vh3SOfPfO2j9/6FA')]
    [TestCase('14', 'horse.webp,300,300,TileOriginal,-1,2,-50,-100,60,-200,-50,660,343,0.6,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('15', 'horse.webp,300,300,TileOriginal,-1,2,-50,-100,-60,0,0,200,200,1,true,0.98,xbudhQDzd8FAC9i3z/7f70OfPP25c7GA0utL0T0auee+f+nlt088/l1AP4XrAwyfeQMhcO07Bg')]
    [TestCase('16', 'horse.webp,300,300,TileOriginal,1,-2,-50,300,10,0,0,200,200,0.6,true,0.98,1ncAfwPks49A99VHEIL2b3TefPCjYyBA483jwP0FFnRfPe4voPA8BPRzLwnr3Qc/uQbmZ569Fg')]
    [TestCase('17', 'horse.webp,300,300,TileStretch,1,2,-50,-100,30,-200,-50,660,343,1,true,0.98,8L++wfzwQ+B8GwtEr/zi0ydw+vjkW+FG1AbQX/zhJfC//wD250C8/sBXf3/v22//dwlk+Ny69Q')]
    [TestCase('18', 'horse.webp,300,300,TileStretch,1,2,-50,-100,30,-200,-50,660,343,0.6,true,0.98,5S/g/vrdf68+DOBfb0DqIYzve/XF8xAD3Pd/wPz89Mc//vUDRGA7/ABUb4Pt3UOP+AcrlB18Aw')]
    [TestCase('19', 'horse.webp,300,300,TileStretch,1,2,-50,-100,30,0,0,200,200,1,true,0.98,4+Pw+enVjx7AG2azzn31lawOPvCVV1A96OCDMT4lsVufvOMTpH/59VZUX8nq4ANfeQXVgw4+GA')]
    [TestCase('20', 'horse.webp,300,300,TileStretch,1,2,-50,-100,30,0,0,200,200,0.6,true,0.98,3ndBuu/Ne55ADH0HH0H1lawOPvCVV1A96OCD0T0lr2tffOQUhL/59lpUX8nq4ANfeQXVgw4+GA')]
    [TestCase('21', 'horse.webp,300,300,TileStretch,1,-2,50,500,30,-200,-50,660,343,1,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBAD0vh3iOPPfO1k+/6FA')]
    [TestCase('22', 'horse.webp,300,300,TileStretch,-1,2,-50,-100,-60,-200,-50,660,343,0.6,true,0.98,tb+Qu/7HS89C9Yy3TwTxSAivP/vEy8D84gGswfz/88Nvfvruk+C7+wvED/8A+7e/wAEN2F5+BA')]
    [TestCase('23', 'horse.webp,300,300,TileStretch,-1,2,-50,-100,-60,0,0,200,200,1,true,0.98,6xcuA//rpyC9E88jf/zd6Ffv/Pral6GA1v0rof0avdOe/+rjq0+8/llQr8TqAwAf+QIjgL27Bw')]
    [TestCase('24', 'horse.webp,300,300,TileStretch,1,-2,-50,500,30,0,0,200,200,0.6,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCD8f0jkZOv/O3zE+96+G98LoPx86/P/BLaN77AFw')]
    procedure TestFillPathBitmapWithMatrix(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer; AWrapMode: TWrapMode; ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg, ADestLeft, ADestTop, ADestRight, ADestBottom, AOpacity: Single; ABlending: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',   '[claBlack;0 claWhite;1],claWhite,Linear,False,1,1,0.98,xxeiuP3GGz8+8blTn0bxQUivfPqn91C94OO3sT4kwM/effLgB1AA+UScrwXw1QOPuAQuVI27Bw')]
    [TestCase('2',   '[$7F000000;0 $7FFFFFFF;1],claWhite,Linear,False,1,1,0.98,zjei+frDt85/86xrX0bza/hePfWep1B95OKbAf4ktSfv/Ov7ry+9+FJ4j0ft2wP/OAUB6M38Dw')]
    [TestCase('3',   '[claBlack;0 claWhite;1],claWhite,Linear,False,1,0.5,0.98,zjei+frDu85/861rX0bza/hePfWep1B95OKbAf4ktSfv/Ov7rz+9+FJ4j0ft2wP/OAUC6M38Dw')]
    [TestCase('4',   '[claBlack;0 claWhite;1],claWhite,Linear,True,1,1,0.98,xxeiuP3GGz8+8blTn0bxQUivfPqn91C94OO3sT4kwM/effLgB1AA+UScrwXw1QOPuAQuVI27Bw')]
    [TestCase('5',   '[$7F000000;0 $7FFFFFFF;1],claWhite,Linear,True,1,1,0.98,zjei+frDt85/86xrX0bza/hePfWep1B95OKbAf4ktSfv/Ov7ry+9+FJ4j0ft2wP/OAUB6M38Dw')]
    [TestCase('6',   '[claBlack;0 claWhite;1],claWhite,Linear,True,1,0.5,0.98,zjei+frDu85/861rX0bza/hePfWep1B95OKbAf4ktSfv/Ov7rz+9+FJ4j0ft2wP/OAUC6M38Dw')]
    [TestCase('7',   '[claBlack;0 claWhite;1],claWhite,Linear,False,2,1,0.98,1VeyOvjAV15B9aCDD8b5poBuf/O3R3D96dGr4fwmsH8fvOgcTA+7+FasncPj7rtvgPzSO08BDA')]
    [TestCase('8',   '[$7F000000;0 $7FFFFFFF;1],claWhite,Linear,False,2,1,0.98,1VeyOvjAV15B9aCDD4b3nZS+/vGmT2A96dmXET0mrYMPPOcYUI96+FuAfkbn51/fPAHU364/Eg')]
    [TestCase('9',   '[claBlack;0 claWhite;1],claWhite,Linear,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('10',  '[claBlack;0 claWhite;1],claWhite,Linear,True,0.5,1,0.98,4GtCfQGZXy6E9bmrP0P1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('11',  '[$7F000000;0 $7FFFFFFF;1],claWhite,Linear,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('12',  '[claBlack;0 claWhite;1],claWhite,Linear,True,3,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('13',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,False,1,1,0.98,xReyuP3GG18+8bZXr8byQ0CvPPqh82D93t2/MT8kvscOvvPaG9BA90Sc74Xw0P++uAQsVG17Bw')]
    [TestCase('14',  '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Linear,False,1,1,0.98,zTey+frDu95/86trXwb0bPRePfWbp2C949+jQf4ktCMPvez4u2+991J4r4ft2AMPOQUB7L28Dw')]
    [TestCase('15',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,False,1,0.5,0.98,zTey+frDu95/86trXwb0bPRePfWbp2C949+jQf4ktCMPvez3u3/991J4n4ft2AMPOQUB6L28Dw')]
    [TestCase('16',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,True,1,1,0.98,xReyuP3GG18+8bZXr8byQ0CvPPqh82D93t2/MT8kvscOvvPaG9BA90Sc74Xw0P++uAQsVG17Bw')]
    [TestCase('17',  '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Linear,True,1,1,0.98,zTey+frDu95/86trXwb0bPRePfWbp2C949+jQf4ktCMPvez4u2+991J4r4ft2AMPOQUB7L28Dw')]
    [TestCase('18',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,True,1,0.5,0.98,zTey+frDu95/86trXwb0bPRePfWbp2C949+jQf4ktCMPvez3u3/991J4n4ft2AMPOQUB6L28Dw')]
    [TestCase('19',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,False,2,1,0.98,1VeyOvjAV15B9aCDDwb6pYBO//O4R4B96dGnwTwnsIMPvOgdSN86+VeAzUPk7I/vf//L587BDg')]
    [TestCase('20',  '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Linear,False,2,1,0.98,1VeyOvjAV15B9aCDD8b3nZSu/vGnT3C96NmTEX0mrYMP/OYZUH+6+Fxsnobn5keffALQt+5/Ew')]
    [TestCase('21',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('22',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,True,0.5,1,0.98,22fiPAKaWx4E9ran70L1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('23',  '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Linear,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('24',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,True,3,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('25',  '[claBlack;-1 claWhite;0.8],claWhite,Linear,True,1,1,0.98,ziuCOP/GD8+978VHn8btOlz/vPnBB/H85fyf4b8jxc++PvPPNzDC9kSwv8Xx0iO/+AQ3eD27CQ')]
    [TestCase('26',  '[claBlack;0 claWhite;2],claWhite,Linear,True,1,1,0.98,xwuiePzHHz++8rVPfwbyQzxv/Pqa75C93tm7If0kvt8OvfD2xy++/UeMbwXu3u8eOAQpNP07Bg')]
    [TestCase('27',  '[claBlack;0 claWhite;1],claRed,Linear,False,1,1,0.98,uBuS+PzHF7/98bM/T8b0RCjv+/yZ24B93MyPAb0mx8uePPMNuO+9AknAn0Xt3KeOuAMyLL27Aw')]
    [TestCase('28',  '[$7F000000;0 $7FFFFFFF;1],claRed,Linear,False,1,1,0.98,xzuiefrDt56/86ljLwb1bej+fPaXm2A94taHMT0muSdPPOwRiP97/VSMfwfs3tP+uAQE1N38DQ')]
    [TestCase('29',  '[claBlack;0 claWhite;1],claRed,Linear,False,1,0.5,0.98,xzeiefrDt46/86ljLwb1bej+fPaXm2A94taHMT0muSdPfOwRiP97/VSMfwfs3tP+uAQE1N38DQ')]
    [TestCase('30',  '[claBlack;0 claWhite;1],claRed,Linear,True,1,1,0.98,uBuS+PzHF7/98bM/T8b0RCjv+/yZ24B93MyPAb0mx8uePPMNuO+9AknAn0Xt3KeOuAMyLL27Aw')]
    [TestCase('31',  '[$7F000000;0 $7FFFFFFF;1],claRed,Linear,True,1,1,0.98,xzuiefrDt56/86ljLwb1bej+fPaXm2A94taHMT0muSdPPOwRiP97/VSMfwfs3tP+uAQE1N38DQ')]
    [TestCase('32',  '[claBlack;0 claWhite;1],claRed,Linear,True,1,0.5,0.98,xzeiefrDt46/86ljLwb1bej+fPaXm2A94taHMT0muSdPfOwRiP97/VSMfwfs3tP+uAQE1N38DQ')]
    [TestCase('33',  '[claBlack;0 claWhite;1],claRed,Linear,False,2,1,0.98,1VeyOvjAV15B9aCDD4b5pYRe//O4P3B96dGjwfwnsYvve+geSK86+VmMfUTi0rdff/vjd24ADg')]
    [TestCase('34',  '[$7F000000;0 $7FFFFFFF;1],claRed,Linear,False,2,1,0.98,1VeyOvjAV15B9aCDD4b3nZi+/vGmS2D96NmTEb0mrof/++YaUF+6+Fxw7obm2V9ffADcfz4/Ew')]
    [TestCase('35',  '[claBlack;0 claWhite;1],claRed,Linear,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('36',  '[claBlack;0 claWhite;1],claRed,Linear,True,0.5,1,0.98,7ovivP2Ng27D+aBnb0P1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('37',  '[$7F000000;0 $7FFFFFFF;1],claRed,Linear,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('38',  '[claBlack;0 claWhite;1],claRed,Linear,True,3,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('39',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,False,1,1,0.98,ug+yePzJG8/98bNHT4b0RSgP/PuU33C93MybMX0myMOe/PMNyC8+AknEr0Xt25+uOAQ0KJ07Aw')]
    [TestCase('40',  '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Linear,False,1,1,0.98,yDOyOfrEu56/86pjLwb1bugOPfaVm2B94taPQf0luSNPvOwRkB88/VWMjwfs3c8O+QQE1M28DQ')]
    [TestCase('41',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,False,1,0.5,0.98,yDOyOfrEu56/86pjLwb1begOPfaVm2B94taPQf0luSNPvOwRkB88/VWMjwfs3c/++AQF1M28DQ')]
    [TestCase('42',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,True,1,1,0.98,ug+yePzJG8/98bNHT4b0RSgP/PuU33C93MybMX0myMOe/PMNyC8+AknEr0Xt25+uOAQ0KJ07Aw')]
    [TestCase('43',  '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Linear,True,1,1,0.98,yDOyOfrEu56/86pjLwb1bugOPfaVm2B94taPQf0luSNPvOwRkB88/VWMjwfs3c8O+QQE1M28DQ')]
    [TestCase('44',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,True,1,0.5,0.98,yDOyOfrEu56/86pjLwb1begOPfaVm2B94taPQf0luSNPvOwRkB88/VWMjwfs3c/++AQF1M28DQ')]
    [TestCase('45',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,False,2,1,0.98,1VeyOvjAV15B9aCDDwb6pYBO//O4R4B96dGnsbwnsYvve+geRK96+ViEbUTi1qPvfvvce07ADw')]
    [TestCase('46',  '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Linear,False,2,1,0.98,1VeyOvjAV15B9aCDD8b3nZSu/vGnT3C96NmTAb0mrof/++YaTF/6+Fxs7obm21MvfADYfy7/Ew')]
    [TestCase('47',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('48',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,True,0.5,1,0.98,7ovSPP6Mj05D+p5jP0P1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('49',  '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Linear,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('50',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,True,3,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('51',  '[claBlack;-1 claWhite;0.8],claRed,Linear,True,1,1,0.98,rTdCuP7CD1898bQvP4b2QiCP+/6oy1B928lTsb0nzbv+vPQMvL++A0jkz8Xt1o/e+AI5QH37Aw')]
    [TestCase('52',  '[claBlack;0 claWhite;2],claRed,Linear,True,1,1,0.98,vwui+PvIHw8+87JDXwb0RTAPPPyT46C93M2nQTwmwttuPPEMnP88AkmcX8Xs4b8euAMrIB18BA')]
    [TestCase('53',  '[claRed;0 claBlue;1],claWhite,Linear,False,1,1,0.98,sD+SuP3DD8999LVTn0b1ODhPu/6sw2D93cxfYfwnvisvPe4MVJ+9BD6gn8Tw3acueP8zuEy8BA')]
    [TestCase('54',  '[$7FFF0000;0 $7F0000FF;1],claWhite,Linear,False,1,1,0.98,w0ui+frBs57/9KtrT0b1Z/CufPegj2A949Zv4bwmtFef/OkRVN97/k98/8bt39fOeAIEnC19Dg')]
    [TestCase('55',  '[claRed;0 claBlue;1],claWhite,Linear,False,1,0.5,0.98,w0ui+frCs57/9KtrT0b1ZvSufPegj2D94tZv4bwmtFef/OkRVN97/k98/8bt39e+eAIEnC19Dg')]
    [TestCase('56',  '[claRed;0 claBlue;1],claWhite,Linear,True,1,1,0.98,sD+SuP3DD8999LVTn0b1ODhPu/6sw2D93cxfYfwnvisvPe4MVJ+9BD6gn8Tw3acueP8zuEy8BA')]
    [TestCase('57',  '[$7FFF0000;0 $7F0000FF;1],claWhite,Linear,True,1,1,0.98,w0ui+frBs57/9KtrT0b1Z/CufPegj2A949Zv4bwmtFef/OkRVN97/k98/8bt39fOeAIEnC19Dg')]
    [TestCase('58',  '[claRed;0 claBlue;1],claWhite,Linear,True,1,0.5,0.98,w0ui+frCs57/9KtrT0b1ZvSufPegj2D94tZv4bwmtFef/OkRVN97/k98/8bt39e+eAIEnC19Dg')]
    [TestCase('59',  '[claRed;0 claBlue;1],claWhite,Linear,False,2,1,0.98,1VeyOvjAV15B9aCDDwb3qqDev/KuIzD96tOD4TwosIPf++cdVM/6+FmQ7cTmy0fwfvv4036CCQ')]
    [TestCase('60',  '[$7FFF0000;0 $7F0000FF;1],claWhite,Linear,False,2,1,0.98,1VeyOvjAV15B9aCDD0b2oKT+fvGiP0C96dqDIf0mrYPvu+YZVG+6+F10Lsfo1qMvfADnq07AEA')]
    [TestCase('61',  '[claRed;0 claBlue;1],claWhite,Linear,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('62',  '[claRed;0 claBlue;1],claWhite,Linear,True,0.5,1,0.98,5kcSvfqLM44D96yTX0T1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('63',  '[$7FFF0000;0 $7F0000FF;1],claWhite,Linear,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('64',  '[claRed;0 claBlue;1],claWhite,Linear,True,3,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('65',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,False,1,1,0.98,sEtiuP7BD8999LRHn0b2OjBP+/+yv3C93MpPMbwouz8/fewMPG99BT2cb8Tw26sOOP4xqHw8BQ')]
    [TestCase('66',  '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Linear,False,1,1,0.98,wlOSefvAs57/9KpjT8b1aOyu/Peki2B94tVrwTwns2Of/OgRSL+7/k5478bt3te++AEDlE29Dg')]
    [TestCase('67',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,False,1,0.5,0.98,wlOCefvAs57/9KpjT8b1Z/CuPPiki2B94tVrwTwns1+v/OgQSL+7/k5478bt3te++AEElE29Dg')]
    [TestCase('68',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,True,1,1,0.98,sEtiuP7BD8999LRHn0b2OjBP+/+yv3C93MpPMbwouz8/fewMPG99BT2cb8Tw26sOOP4xqHw8BQ')]
    [TestCase('69',  '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Linear,True,1,1,0.98,wlOSefvAs57/9KpjT8b1aOyu/Peki2B94tVrwTwns2Of/OgRSL+7/k5478bt3te++AEDlE29Dg')]
    [TestCase('70',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,True,1,0.5,0.98,wlOCefvAs57/9KpjT8b1Z/CuPPiki2B94tVrwTwns1+v/OgQSL+7/k5478bt3te++AEElE29Dg')]
    [TestCase('71',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,False,2,1,0.98,1VeyOvjAV15B9aCDDwb3qqTev/KtGyD96tJ7AT0osoPPO+gdWL+6+FmYDQXmxEfAf/sDrG6CCA')]
    [TestCase('72',  '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Linear,False,2,1,0.98,1VeyOvjAV15B9aCDDwb2oKj+fvGhNzC96dl/Mf0mroPv++YZWG96+F14Pofo0qOffADslz5AEA')]
    [TestCase('73',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('74',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,True,0.5,1,0.98,5UMyvfmNF67D9rGPr0T1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('75',  '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Linear,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('76',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,True,3,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('77',  '[claRed;-1 claBlue;0.8],claWhite,Linear,True,1,1,0.98,ty8SOfvKBz++9biP34byL1hP+/ucz3B94tSXsTsnuF///OsOSA+9AzhsD4Ty57fet/8waHz8Aw')]
    [TestCase('78',  '[claRed;0 claBlue;2],claWhite,Linear,True,1,1,0.98,q0dSuP7BC3+98rU7b4b2PChPe/+vw1A93MlHMT0ox/M+ffEMhF9+BEPMP0Xv2ZuO+AA4BN17BA')]
    [TestCase('79',  '[claRed;0 $7F000000;0.5 claBlue;1],claWhite,Linear,True,1,1,0.98,xDNSOfq/115/9a5TLwb0YgRfvPecq6B9386H4XsmtjtPvOkNSJ/7/VB8j8br38teuAMKfF39DA')]
    [TestCase('80',  '[claRed;0 $7F000000;1 claBlue;0.5],claWhite,Linear,True,1,1,0.98,ujMSufvHE3/+97WDD0fzMFhPO/2kx5A94dKLEXwlt1effO0TfE/8AT1ErwTz9OfONwIivEx8BQ')]
    [TestCase('81',  '[claRed;0 $7F000000;-1 claBlue;0.5],claWhite,Linear,True,1,1,0.98,ujMSufvHE3/+97WDD0fzMFhPe/2kx6A94dKLgXsmtm/fvOsPTN/8AjZU34Pz7Lu+NwAvTHx8Aw')]
    [TestCase('82',  '[claRed;1 $7F000000;0.5 claBlue;0],claWhite,Linear,True,1,1,0.98,xCeC+fjFw14/87F3H0byXhRfPPWMt4C94tOrYXwlvB//u+wPkA88/FCE70bs5MeeOAYNmN28Cw')]
    [TestCase('83',  '[claRed;1 $7F000000;0 claBlue;0.5],claWhite,Linear,True,1,1,0.98,xyNy+frN/w5/97CTD4fzPkA/PPuXv6C949qzIfwkv0OP/O8SpI+9ADhgX0Tz8q8e+AMzfNz7AQ')]
    [TestCase('84',  '[claRed;1 $7F000000;0 claBlue;0.5 claWhite;0.49],claWhite,Linear,True,1,1,0.98,zzvS+P/F766+77pjD4fwSDSPffq33xB95farMfwkxTu/vOwRrC8+BD2UL4T08e9+9wM4sAw8Bg')]
    [TestCase('85',  '[claRed;1 $7F000000;0 claBlue;0.5 claWhite;0.49],claWhite,Linear,False,1,1,0.98,zzvS+P/F766+77pjD4fwSDSPffq33xB95farMfwkxTu/vOwRrC8+BD2UL4T08e9+9wM4sAw8Bg')]
    [TestCase('86',  '[claRed;1 $7F000000;0 claBlue;0.5 claWhite;0.49],claWhite,Linear,True,2,0.5,0.98,1VeyOvjAV15B9aCDD4b2mqB+/vCfU2D96NyPQf0lrXsf/OYXWJ/691uMTobn9meffQPM9+7/EA')]
    [TestCase('87',  '[],claWhite,Linear,True,1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('88',  '[claBlack;0 claWhite;1],claWhite,Radial,False,1,1,0.98,zTeiePzDH8/978A7z8bvQUAvPfq04xB95PCvob0lwv/efe/n778//EqIvwXt4vc+eANBdP06CA')]
    [TestCase('89',  '[$7F000000;0 $7FFFFFFF;1],claWhite,Radial,False,1,1,0.98,0UeiefrCu56/8rBfb4bya/SePfWlnzA95uiXgb0ltkP/fOr+o988+lVwj8fr4fvOeAQL/I08EA')]
    [TestCase('90',  '[claBlack;0 claWhite;1],claWhite,Radial,False,1,0.5,0.98,0UeiefrCu46/8rBfb4bya/SePfWlnzA95uiXgb0ltkP/fOr+o988+lVwj8fr4f/OeAQL+I08EA')]
    [TestCase('91',  '[claBlack;0 claWhite;1],claWhite,Radial,True,1,1,0.98,zTeiePzDH8/978A7z8bvQUAvPfq04xB95PCvob0lwv/efe/n778//EqIvwXt4vc+eANBdP06CA')]
    [TestCase('92',  '[$7F000000;0 $7FFFFFFF;1],claWhite,Radial,True,1,1,0.98,0UeiefrCu56/8rBfb4bya/SePfWlnzA95uiXgb0ltkP/fOr+o988+lVwj8fr4fvOeAQL/I08EA')]
    [TestCase('93',  '[claBlack;0 claWhite;1],claWhite,Radial,True,1,0.5,0.98,0UeiefrCu46/8rBfb4bya/SePfWlnzA95uiXgb0ltkP/fOr+o988+lVwj8fr4f/OeAQL+I08EA')]
    [TestCase('94',  '[claBlack;0 claWhite;1],claWhite,Radial,False,2,1,0.98,1VeyOvjAV15B9aCDD8b5poB+P/O3R2A96tKzoXwnsIf/e+geRL+6+VcQ/kPg8O8PQPjOg+8/Cg')]
    [TestCase('95',  '[$7F000000;0 $7FFFFFFF;1],claWhite,Radial,False,2,1,0.98,1VeyOvjAV15B9aCDD4b3npi+vvGmT2A96dmbAX0mrYcP/OYZTF/6+Fy0robl6He//P7SA/8+EQ')]
    [TestCase('96',  '[claBlack;0 claWhite;1],claWhite,Radial,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('97',  '[claBlack;0 claWhite;1],claWhite,Radial,True,0.5,1,0.98,7mOyPQCga04E9Li3z0P1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('98',  '[$7F000000;0 $7FFFFFFF;1],claWhite,Radial,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('99',  '[claBlack;0 claWhite;1],claWhite,Radial,True,3,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('100', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,False,1,1,0.98,zUuy+PvCJ6/97sA378bvQjh//fm20/C85PKzkT0mwg8Pvu7l78+/+0qE78Xr5O9O+AJIgK06CA')]
    [TestCase('101', '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Radial,False,1,1,0.98,0U+yOfrBv44/8rBbf4bybPDOPfWmlyB95umbcf0ltkcP/en9o+/8+VVsr0fr4vveOAQPAF48EA')]
    [TestCase('102', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,False,1,0.5,0.98,0U+yOfrBv34/8rBbf4bybPC+PfWmlyB95umbcf0ltkcPPer9o+/8+VVsn0fr4vveOAQPAF48EA')]
    [TestCase('103', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,True,1,1,0.98,zUuy+PvCJ6/97sA378bvQjh//fm20/C85PKzkT0mwg8Pvu7l78+/+0qE78Xr5O9O+AJIgK06CA')]
    [TestCase('104', '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Radial,True,1,1,0.98,0U+yOfrBv44/8rBbf4bybPDOPfWmlyB95umbcf0ltkcP/en9o+/8+VVsr0fr4vveOAQPAF48EA')]
    [TestCase('105', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,True,1,0.5,0.98,0U+yOfrBv34/8rBbf4bybPC+PfWmlyB95umbcf0ltkcPPer9o+/8+VVsn0fr4vveOAQPAF48EA')]
    [TestCase('106', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,False,2,1,0.98,1VeyOvjAV15B9aCDDwb6pYBO//O5R4B96dGnoXwnsIv/e+gfQK/6+VgkXoTe799/P/jFU2+/Cw')]
    [TestCase('107', '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Radial,False,2,1,0.98,1VeyOvjAV15B9aCDD8b3nZSu/vGnT2D96NmT8XwmrYcP/OYaTF/6+FzA3obk529v/P7N677+EQ')]
    [TestCase('108', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('109', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,True,0.5,1,0.98,71eifQCkb04E9LW330P1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('110', '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Radial,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('111', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,True,3,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('112', '[claBlack;-1 claWhite;0.8],claWhite,Radial,True,1,1,0.98,0juCuP7ED4/97sk7v8bsOlg/vfnI/8C85wKYUX8kxuu+fvHTJ9BB+EaovwXw2RufeARAiO36CQ')]
    [TestCase('113', '[claBlack;0 claWhite;2],claWhite,Radial,True,1,1,0.98,yhui+PvGIw8+8rk/n0bxQzyv/Pqh53C94N+3kbwlv/cOPe/5t989/0mEb4Xs5Of+dwMyRL27Bg')]
    [TestCase('114', '[claBlack;0 claWhite;1],claRed,Radial,False,1,1,0.98,tC+yeP3GE4+98bMrf0b2SRjvO/6gx0C93Mhngbwnx9cOPfEJjG+9BEasr4Xr16fOuAE5RN17BQ')]
    [TestCase('115', '[$7F000000;0 $7FFFFFFF;1],claRed,Radial,False,1,1,0.98,xUOyufrDt36/86lXP8b1b+D+PPebj0B94tRz8bwmuSuPfOsPcL97/lOAjwfr3NMeuQMH5P38Dg')]
    [TestCase('116', '[claBlack;0 claWhite;1],claRed,Radial,False,1,0.5,0.98,xEOyufrDt36/86lXP8b1b+D+PPebj0B94tRz8bwmuSuPfOsPcL97/lOAjwfr3NMeeQMH4O28Dg')]
    [TestCase('117', '[claBlack;0 claWhite;1],claRed,Radial,True,1,1,0.98,tC+yeP3GE4+98bMrf0b2SRjvO/6gx0C93Mhngbwnx9cOPfEJjG+9BEasr4Xr16fOuAE5RN17BQ')]
    [TestCase('118', '[$7F000000;0 $7FFFFFFF;1],claRed,Radial,True,1,1,0.98,xUOyufrDt36/86lXP8b1b+D+PPebj0B94tRz8bwmuSuPfOsPcL97/lOAjwfr3NMeuQMH5P38Dg')]
    [TestCase('119', '[claBlack;0 claWhite;1],claRed,Radial,True,1,0.5,0.98,xEOyufrDt36/86lXP8b1b+D+PPebj0B94tRz8bwmuSuPfOsPcL97/lOAjwfr3NMeeQMH4O28Dg')]
    [TestCase('120', '[claBlack;0 claWhite;1],claRed,Radial,False,2,1,0.98,1VeyOvjAV15B9aCDD0b5pohu//O3O2C96dGjobwnsIv/e+geQK+6+ViMncTh0btvv/nnf14ADg')]
    [TestCase('121', '[$7F000000;0 $7FFFFFFF;1],claRed,Radial,False,2,1,0.98,1VeyOvjAV15B9aCDD0b3nZi+/vGmS2D96NmTAb0mrYf/++YaTF/6+FxwDkfm2F9vvP/efz4/Ew')]
    [TestCase('122', '[claBlack;0 claWhite;1],claRed,Radial,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('123', '[claBlack;0 claWhite;1],claRed,Radial,True,0.5,1,0.98,7nMCff2Rb25D+J5br0P1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('124', '[$7F000000;0 $7FFFFFFF;1],claRed,Radial,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('125', '[claBlack;0 claWhite;1],claRed,Radial,True,3,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('126', '[claBlack;0.2 claWhite;0.8],claRed,Radial,False,1,1,0.98,sjfieP3HF3998bInj8b2Swz/e/6fwyD93MdfYTwoyNc+vfAIhE99BUWgz8Xq1p/+OAE8TN17BQ')]
    [TestCase('127', '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Radial,False,1,1,0.98,xEfSufrEt25/86lXTwb2cNwOPfeajzC94tNz4fwmuSufPOsPbK/7/lN8n8fq288ueQMJ6P38Dg')]
    [TestCase('128', '[claBlack;0.2 claWhite;0.8],claRed,Radial,False,1,0.5,0.98,xEfSufrEt25/86lXTwb2cNwOffeajzC94tNz4fwmuSufPOsPbK/7/lN8j8fq288ueQMJ5P28Dg')]
    [TestCase('129', '[claBlack;0.2 claWhite;0.8],claRed,Radial,True,1,1,0.98,sjfieP3HF3998bInj8b2Swz/e/6fwyD93MdfYTwoyNc+vfAIhE99BUWgz8Xq1p/+OAE8TN17BQ')]
    [TestCase('130', '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Radial,True,1,1,0.98,xEfSufrEt25/86lXTwb2cNwOPfeajzC94tNz4fwmuSufPOsPbK/7/lN8n8fq288ueQMJ6P38Dg')]
    [TestCase('131', '[claBlack;0.2 claWhite;0.8],claRed,Radial,True,1,0.5,0.98,xEfSufrEt25/86lXTwb2cNwOffeajzC94tNz4fwmuSufPOsPbK/7/lN8j8fq288ueQMJ5P28Dg')]
    [TestCase('132', '[claBlack;0.2 claWhite;0.8],claRed,Radial,False,2,1,0.98,1VeyOvjAV15B9aCDDwb6pYBO//O5Q4B96dGjoXwnsIv/e+gfQK/6+Vh8jcTh1Js/v/jjfx7ADw')]
    [TestCase('133', '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Radial,False,2,1,0.98,1VeyOvjAV15B9aCDD4b3nZSuPvKnT2C96NmT8XwmrYcP/OYaTF/6+Fxo/kbm2k9PPP/cfx4/FA')]
    [TestCase('134', '[claBlack;0.2 claWhite;0.8],claRed,Radial,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('135', '[claBlack;0.2 claWhite;0.8],claRed,Radial,True,0.5,1,0.98,72sCvf2Sa14D+JtTn0P1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('136', '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Radial,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('137', '[claBlack;0.2 claWhite;0.8],claRed,Radial,True,3,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('138', '[claBlack;-1 claWhite;0.8],claRed,Radial,True,1,1,0.98,qkdi+P7CCz898bQjT0b3RBSPu/+svzB928c/cT0ozcM+ffMKpH/+BEfY38Xs1Y/u+AE8TJ27BA')]
    [TestCase('139', '[claBlack;0 claWhite;2],claRed,Radial,True,1,1,0.98,vReyOPzHG+/98rI7b4b0RygP/PyX14C93MuTAbwmwuOePPAKiL98A0iUb8Xr378+uAIuLC08BQ')]
    [TestCase('140', '[claRed;0 claBlue;1],claWhite,Radial,False,1,1,0.98,tjNSuPzFF/999Lpvj0bzK1D/evynz3B94NKDQT0mxBf/PPEOnF/+AT+s7wT07rfeNwI3rAw8AQ')]
    [TestCase('141', '[$7FFF0000;0 $7F0000FF;1],claWhite,Radial,False,1,1,0.98,xkOCefrCt67/9K17T0b0YfyOPPaek2A95NmDUf0lt0t/POsSeC/8/FCAL0fv59+e+AMGlA29DA')]
    [TestCase('142', '[claRed;0 claBlue;1],claWhite,Radial,False,1,0.5,0.98,xUOCefrCt67/9K17T0b0YPx+fPaek2A95NmDUf0lt0t/fOsSeD88/U+AL0fv59ue+AMGlA29DA')]
    [TestCase('143', '[claRed;0 claBlue;1],claWhite,Radial,True,1,1,0.98,tjNSuPzFF/999Lpvj0bzK1D/evynz3B94NKDQT0mxBf/PPEOnF/+AT+s7wT07rfeNwI3rAw8AQ')]
    [TestCase('144', '[$7FFF0000;0 $7F0000FF;1],claWhite,Radial,True,1,1,0.98,xkOCefrCt67/9K17T0b0YfyOPPaek2A95NmDUf0lt0t/POsSeC/8/FCAL0fv59+e+AMGlA29DA')]
    [TestCase('145', '[claRed;0 claBlue;1],claWhite,Radial,True,1,0.5,0.98,xUOCefrCt67/9K17T0b0YPx+fPaek2A95NmDUf0lt0t/fOsSeD88/U+AL0fv59ue+AMGlA29DA')]
    [TestCase('146', '[claRed;0 claBlue;1],claWhite,Radial,False,2,1,0.98,1VeyOvjAV15B9aCDD0b3qpze//KuK0D96tSLIT0os4PPe+gcYL86+FmE/QTm1Gcg/vzw2x6DCQ')]
    [TestCase('147', '[$7FFF0000;0 $7F0000FF;1],claWhite,Radial,False,2,1,0.98,1VeyOvjAV15B9aCDD0b2oKTufvGiQ0C96dqHMf0mr4Pv++YZXF86+F1sLofo2rPPOwHjr47AEA')]
    [TestCase('148', '[claRed;0 claBlue;1],claWhite,Radial,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('149', '[claRed;0 claBlue;1],claWhite,Radial,True,0.5,1,0.98,6E/C/PqDb64D+KmrP0T1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('150', '[$7FFF0000;0 $7F0000FF;1],claWhite,Radial,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('151', '[claRed;0 claBlue;1],claWhite,Radial,True,3,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('152', '[claRed;0.2 claBlue;0.8],claWhite,Radial,False,1,1,0.98,uCsS+PzDHw++9LxzbwbzJ1Tf+vurz3A94NODgb0lxhfvvPEPrI/+AD+0/4T18ceudwI3oPw7AA')]
    [TestCase('153', '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Radial,False,1,1,0.98,x0NiefrCu74/9a57P0b0XgBvPPagk2A95NqDcX0luE9/vOsSgE+8/FCELwfw6OOO+AMGkA09DA')]
    [TestCase('154', '[claRed;0.2 claBlue;0.8],claWhite,Radial,False,1,0.5,0.98,x0NiefrCu74/9a57Pwb0XgBvPPagk2A95NqDcb0luE9/vOsSgE+8/FCEL0fw6OOO+AMGkA09DA')]
    [TestCase('155', '[claRed;0.2 claBlue;0.8],claWhite,Radial,True,1,1,0.98,uCsS+PzDHw++9LxzbwbzJ1Tf+vurz3A94NODgb0lxhfvvPEPrI/+AD+0/4T18ceudwI3oPw7AA')]
    [TestCase('156', '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Radial,True,1,1,0.98,x0NiefrCu74/9a57P0b0XgBvPPagk2A95NqDcX0luE9/vOsSgE+8/FCELwfw6OOO+AMGkA09DA')]
    [TestCase('157', '[claRed;0.2 claBlue;0.8],claWhite,Radial,True,1,0.5,0.98,x0NiefrCu74/9a57Pwb0XgBvPPagk2A95NqDcb0luE9/vOsSgE+8/FCEL0fw6OOO+AMGkA09DA')]
    [TestCase('158', '[claRed;0.2 claBlue;0.8],claWhite,Radial,False,2,1,0.98,1VeyOvjAV15B9aCDDwb3qqTev/KtGyD96tJ/MX0os4O/u+gcYK86+FqMPQXl0X9Q/v30w37DCA')]
    [TestCase('159', '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Radial,False,2,1,0.98,1VeyOvjAV15B9aCDD0b2oKj+fvGhO0C96dl/Qf0mr4PvO+cZXF86+F1wTgfo2MPfuwHlo76AEA')]
    [TestCase('160', '[claRed;0.2 claBlue;0.8],claWhite,Radial,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('161', '[claRed;0.2 claBlue;0.8],claWhite,Radial,True,0.5,1,0.98,6FPCfPqBd85D+Ky3X0T1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('162', '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Radial,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('163', '[claRed;0.2 claBlue;0.8],claWhite,Radial,True,3,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('164', '[claRed;-1 claBlue;0.8],claWhite,Radial,True,1,1,0.98,uifyuPrLC1++9bqfz4bxKWgvu/qZ03C949enMTwmu1PfvO0PcH89Ajl0P0T07r++NwEyaFx8Ag')]
    [TestCase('165', '[claRed;0 claBlue;2],claWhite,Radial,True,1,1,0.98,rj8yOP7CD4+98rdLX4b1NjQve/6tx1B93cxXoT0nyuce/fINqL8+A0PQbwXx4Z9uOAI5/Ly7Ag')]
    [TestCase('166', '[claRed;0 $7F000000;0.5 claBlue;1],claWhite,Radial,True,1,1,0.98,yScSefvG764/96p3/0X2VxAvPPiYq6C94tOXgXwlvR/v++oUiA+8/UyIv8bv7PNuuAMYTL08CA')]
    [TestCase('167', '[claRed;0 $7F000000;1 claBlue;0.5],claWhite,Radial,True,1,1,0.98,vC8yefnIM39+9bqHD0fvPWgPPPeVx0D94defkfwns1+vPO0LVA99/0aMzwTw3L9e+AEO3Lz8CA')]
    [TestCase('168', '[claRed;0 $7F000000;-1 claBlue;0.5],claWhite,Radial,True,1,1,0.98,vSvS+PnLO3++9rqf30bxJ3gvu/may0A95NabgTwnt2MP/ewOaJ99ATmAHwT16ssOeAAvXFz8AQ')]
    [TestCase('169', '[claRed;1 $7F000000;0.5 claBlue;0],claWhite,Radial,True,1,1,0.98,wSvCefvKy34/9advD4b3ZgC/PPmQp2D94c9/oTsnuyMP/OkSWG97AEuAn8bq09c+OQIWiJ18DA')]
    [TestCase('170', '[claRed;1 $7F000000;0 claBlue;0.5],claWhite,Radial,True,1,1,0.98,vj9yufjNJ19+9LJ/7wbzPVQPfPuUu9C849BrQbwpr1/ffesKQM/8BDp0DwTv0rseOf4wzEy8BQ')]
    [TestCase('171', '[claRed;1 $7F000000;0 claBlue;0.5 claWhite;0.49],claWhite,Radial,True,1,1,0.98,yRtSOfzNCz9+8rxnv4bwN1D/+/mr53C85NtDsX0ouRvfPvD2l49+Aj6kz8Txzv9eeQAnEH28BQ')]
    [TestCase('172', '[claRed;1 $7F000000;0 claBlue;0.5 claWhite;0.49],claWhite,Radial,False,1,1,0.98,yRtSOfzNCz9+8rxnv4bwN1D/+/mr53C85NtDsX0ouRvfPvD2l49+Aj6kz8Txzv9eeQAnEH28BQ')]
    [TestCase('173', '[claRed;1 $7F000000;0 claBlue;0.5 claWhite;0.49],claWhite,Radial,True,2,0.5,0.98,1VeyOvjAV15B9aCDD4b2m5ye/vCgU2A96duXIf0lrIMPfOYYUD+6+F1Ubgfp9ZtPvPveN58/Ew')]
    [TestCase('174', '[],claWhite,Radial,True,1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('175', '[claRed;0.3],claWhite,Radial,True,1,1,0.98,pksSuP+/By/98LUjL4b3QBhfOwCzvzC92scv8T0oz7s+/fQMtA9/BEj07wXu1IvuOAI8UH07BA')]
    [TestCase('176', '[claRed;0.3],claBlue,Radial,True,1,1,0.98,xv+xOPvJI08+9LJHbwbzRTQvvPuO59D93M6/gbslve8u/O4LgA/8AUl8L0Xs5tu+twMkFG38BA')]
    [TestCase('177', '[claRed;0.3],claWhite,Linear,True,1,1,0.98,pksSuP+/By/98LUjL4b3QBhfOwCzvzC92scv8T0oz7s+/fQMtA9/BEj07wXu1IvuOAI8UH07BA')]
    [TestCase('178', '[claRed;0.3],claBlue,Linear,True,1,1,0.98,xv+xOPvJI08+9LJHbwbzRTQvvPuO59D93M6/gbslve8u/O4LgA/8AUl8L0Xs5tu+twMkFG38BA')]
    [TestCase('179', '[claRed;0 claBlue;1 claBlue;1],claWhite,Radial,False,1,1,0.98,tjNSuPzFF/999Lpvj0bzK1D/evynz3B94NKDQT0mxBf/PPEOnF/+AT+s7wT07rfeNwI3rAw8AQ')]
    [TestCase('180', '[claRed;0 claRed;0 claBlue;1],claWhite,Radial,False,1,1,0.98,tjNSuPzFF/999Lpvj0bzK1D/evynz3B94NKDQT0mxBf/PPEOnF/+AT+s7wT07rfeNwI3rAw8AQ')]
    [TestCase('181', '[claRed;0 claBlue;1 claBlue;1],claWhite,Linear,False,1,1,0.98,sD+SuP3DD8999LVTn0b1ODhPu/6sw2D93cxfYfwnvisvPe4MVJ+9BD6gn8Tw3acueP8zuEy8BA')]
    [TestCase('182', '[claRed;0 claRed;0 claBlue;1],claWhite,Linear,False,1,1,0.98,sD+SuP3DD8999LVTn0b1ODhPu/6sw2D93cxfYfwnvisvPe4MVJ+9BD6gn8Tw3acueP8zuEy8BA')]
    [TestCase('183', '[$00FF0000;0 $FF0000FF;1],claBlack,Linear,True,1,1,0.98,1TfCufnFo07A9aVnT8b0deCuvfWTh5B95N6X8XslvCtP/OoNYH/7/FF430br59NeeAUJpD39DA')]
    [TestCase('184', '[$00FF0000;0 $FF0000FF;1],claBlack,Radial,True,1,1,0.98,1CtS+fjGrw7A9aFzzwX2ZATPvPmQr9A94tePcfwlrC9/vOkVdB+7/05wD8bs5PtOeAMKkA19Cg')]
    [TestCase('185', '[$00FF0000;0 $FF0000FF;1],claBlack,Linear,True,1,0.5,0.98,1Ucy+vjDf96A9aJzLwb1hcTe/fKUb3A95t+PoXwls1cvPOgRXM+6+lhoHwjr4+veeAXvF659Eg')]
    [TestCase('186', '[$00FF0000;0 $FF0000FF;1],claBlack,Radial,True,1,0.5,0.98,1UMCuvjDg76A9aB778X1fNhu/fSTg5A95duH4bwlq1dPvOcVZJ/6+1dkv8fr4v/OeATwC449EQ')]
    procedure TestFillPathGradient(const APoints, AModulateColor: string; AGradientStyle: TGradientStyle; ABlending: Boolean; const AScale, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,1KuS+v6to06A8LN3b0bwfMjOvPGIt8A94c2fAf0kwDt/e+gK2K87+188vwbt8buOtwXzI27+Dw')]
    procedure TestFillPathSolid(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,1KeS+v6to06A8LJ3b0bwfMjOvPGIs8A94c2fAf0kwDt/e+gK1K87+188vwbt8buetwXzI27+Dw')]
    procedure TestFillPathSolid2(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,1fdyegWZ4w4/68hf74b1lawOvvCUU1B95+CDQb0lqnsPvOUVUA86+F5UX8nq4ANfeQXVgw4+GA')]
    procedure TestFillPathSolid3(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,29/C+AiW456/7qibv8TpZ9j+Of6cS4H+1LzTQEAk648Ofe79j5D+/GxYb8T39Xe+9wITDH58EA')]
    procedure TestFillPathSolid4(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,0/ty+gWb7z6/68Vrv0brYuSe+/J6EzE+2rq/sTwk1vf+Ouv/W0A9/l0ET4P07j8utgAbuF29Dw')]
    procedure TestFillPathSolid5(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,29/C+AiW456/7qibv8TpZ9j+Of6cS4H+1LzTQEAk648Ofe79j5D+/GxYb8T39Xe+9wITDH58EA')]
    procedure TestFillPathSolid6(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  '300,300,1,2,-50,-100,60,-200,-50,660,343,1,true,0.98,gSPguujuK24GDiCDj0H1lawOPvCVV1A96OCDEf8D+b8Oe+FO9PDw16RXX8nq4ANfeQXVgw4+GA')]
    [TestCase('2',  '300,300,1,2,-50,-100,60,-200,-50,660,343,1,false,0.98,gSPguujuK24GDiCDj0H1lawOPvCVV1A96OCDEf8D+b8Oe+FO9PDw16RXX8nq4ANfeQXVgw4+GA')]
    [TestCase('3',  '300,300,1,2,-50,-100,60,-200,-50,660,343,0.5,true,0.98,qz/BevDXP+7DAWCDz0P1lawOPvCVV1A96OCDMb4U0iOPe+MyJHD15wJUX8nq4ANfeQXVgw4+GA')]
    [TestCase('4',  '300,300,1,2,-50,-100,60,-200,-50,660,343,0.5,false,0.98,qz/BevDXP+7DAWCDz0P1lawOPvCVV1A96OCDMb4U0iOPe+MyJHD15wJUX8nq4ANfeQXVgw4+GA')]
    [TestCase('5',  '300,300,1,2,-50,-100,60,-200,-50,660,343,1,true,0.98,gSPguujuK24GDiCDj0H1lawOPvCVV1A96OCDEf8D+b8Oe+FO9PDw16RXX8nq4ANfeQXVgw4+GA')]
    [TestCase('6',  '300,300,1,2,-50,-100,60,-200,-50,660,343,1,false,0.98,gSPguujuK24GDiCDj0H1lawOPvCVV1A96OCDEf8D+b8Oe+FO9PDw16RXX8nq4ANfeQXVgw4+GA')]
    [TestCase('7',  '300,300,1,2,-50,-100,60,-200,-50,660,343,0.5,true,0.98,qz/BevDXP+7DAWCDz0P1lawOPvCVV1A96OCDMb4U0iOPe+MyJHD15wJUX8nq4ANfeQXVgw4+GA')]
    [TestCase('8',  '300,300,1,2,-50,-100,60,-200,-50,660,343,0.5,false,0.98,qz/BevDXP+7DAWCDz0P1lawOPvCVV1A96OCDMb4U0iOPe+MyJHD15wJUX8nq4ANfeQXVgw4+GA')]
    [TestCase('9',  '300,300,1,2,-50,-100,60,0,0,200,200,1,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('10', '300,300,1,2,-50,-100,30,0,0,200,200,1,false,0.98,1GNReu3Tlx5CFT/P7nz1lawOPvCVV1A96OCDcb0lrHcvPOUTZO+5919UX8nq4ANfeQXVgw4+GA')]
    [TestCase('11', '300,300,1,2,-50,-100,30,0,0,200,200,0.5,true,0.98,1d+BuvLJd75BBW8rf0H1lawOPvCVV1A96OCDYb0lq38ffOUUXP/592BUX8nq4ANfeQXVgw4+GA')]
    [TestCase('12', '300,300,1,2,-50,-100,30,0,0,200,200,0.5,false,0.98,1d+BuvLJd75BBW8rf0H1lawOPvCVV1A96OCDYb0lq38ffOUUXP/592BUX8nq4ANfeQXVgw4+GA')]
    [TestCase('13', '300,300,1,2,-50,-100,30,0,0,200,200,1,true,0.98,1GNReu3Tlx5CFT/P7nz1lawOPvCVV1A96OCDcb0lrHcvPOUTZO+5919UX8nq4ANfeQXVgw4+GA')]
    [TestCase('14', '300,300,1,2,-50,-100,30,0,0,200,200,1,false,0.98,1GNReu3Tlx5CFT/P7nz1lawOPvCVV1A96OCDcb0lrHcvPOUTZO+5919UX8nq4ANfeQXVgw4+GA')]
    [TestCase('15', '300,300,1,2,-50,-100,30,0,0,200,200,0.5,true,0.98,1d+BuvLJd75BBW8rf0H1lawOPvCVV1A96OCDYb0lq38ffOUUXP/592BUX8nq4ANfeQXVgw4+GA')]
    [TestCase('16', '300,300,1,2,-50,-100,30,0,0,200,200,0.5,false,0.98,1d+BuvLJd75BBW8rf0H1lawOPvCVV1A96OCDYb0lq38ffOUUXP/592BUX8nq4ANfeQXVgw4+GA')]
    [TestCase('17', '300,300,1,-2,50,500,30,-200,-50,660,343,1,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GAEH8nm1NNu+QDRSy6+Fw')]
    [TestCase('18', '300,300,1,-2,50,500,30,-200,-50,660,343,1,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GAEH8nm1NNu+QDRSy6+Fw')]
    [TestCase('19', '300,300,1,-2,50,500,30,-200,-50,660,343,0.5,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GAsP8no2utuOQPTZx7+Fw')]
    [TestCase('20', '300,300,1,-2,50,500,30,-200,-50,660,343,0.5,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GAsP8no2utuOQPTZx7+Fw')]
    [TestCase('21', '300,300,-1,2,-50,-100,30,-200,-50,660,343,1,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('22', '300,300,-1,2,-50,-100,30,-200,-50,660,343,1,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('23', '300,300,-1,2,-50,-100,30,-200,-50,660,343,0.5,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('24', '300,300,-1,2,-50,-100,30,-200,-50,660,343,0.5,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('25', '300,300,-1,2,-50,-100,-60,0,0,200,200,1,true,0.98,2cuQvvnnc988BIbbnv7iOdjOdwKMHzGBz6/PsEAptYNuvurqO289B0d0/4bl3qeuOwQO7O28Bg')]
    [TestCase('26', '300,300,-1,2,-50,-100,-60,0,0,200,200,1,false,0.98,2cuQvvnnc988BIbbnv7iOdjOdwKMHzGBz6/PsEAptYNuvurqO289B0d0/4bl3qeuOwQO7O28Bg')]
    [TestCase('27', '300,300,-1,2,-50,-100,-60,0,0,200,200,0.5,true,0.98,15Oh/PjU5x7//JMvXwLsZ8DuevmRu0D/28crAX8nsAM/PegASL+7/1RkLwjo39eO+gTyN359Dw')]
    [TestCase('28', '300,300,-1,2,-50,-100,-60,0,0,200,200,0.5,false,0.98,15Oh/PjU5x7//JMvXwLsZ8DuevmRu0D/28crAX8nsAM/PegASL+7/1RkLwjo39eO+gTyN359Dw')]
    [TestCase('29', '300,300,1,-2,-50,500,30,0,0,200,200,1,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUXsmgsM//O7WF1969GNwjsTh6iNwPQ/fI309Eg')]
    [TestCase('30', '300,300,1,-2,-50,500,30,0,0,200,200,1,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUXsmgsM//O7WF1969GNwjsTh6iNwPQ/fI309Eg')]
    [TestCase('31', '300,300,1,-2,-50,500,30,0,0,200,200,0.5,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUfwllqMvPOr2Nz969mHg7kbm5ZNvewra0709FQ')]
    [TestCase('32', '300,300,1,-2,-50,500,30,0,0,200,200,0.5,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUfwllqMvPOr2Nz969mHg7kbm5ZNvewra0709FQ')]
    [TestCase('33', '80,80,1,1,0,0,0,0,0,60,60,0,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('34', '80,80,1,1,0,0,0,60,0,60,60,1,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('35', '80,80,1,1,0,0,0,0,60,60,60,1,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('36', '80,80,1,1,0,0,0,0,0,60,60,0,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('37', '80,80,1,1,0,0,0,60,0,60,60,1,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('38', '80,80,1,1,0,0,0,0,60,60,60,1,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('39', '80,80,1,1,0,0,10,0,0,60,60,0,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('40', '80,80,1,1,0,0,10,60,0,60,60,1,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('41', '80,80,1,1,0,0,10,0,60,60,60,1,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('42', '80,80,1,1,0,0,10,0,0,60,60,0,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('43', '80,80,1,1,0,0,10,60,0,60,60,1,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('44', '80,80,1,1,0,0,10,0,60,60,60,1,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    procedure TestFillPathSolidWithMatrix(ASurfaceWidth, ASurfaceHeight: Integer; ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg, ADestLeft, ADestTop, ADestRight, ADestBottom, AOpacity: Single; ABlending: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,1LeSegCqsx6A77Zzf0bvd9CO/PGFx+C938mn8bwkxC9v++gI8P+7+144P4bt9a8+twX5E44+Dg')]
    procedure TestFillPathSolidWithModulateColor(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,wsfSugKkY/6+7ceffwftYjiPO/N4y0C+5derETwjw3+/+ukIrL88AEUwD8Xx9JvutgTwf+1+CQ')]
    procedure TestFillPathSolidWithResource(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  '[0.5;0 1;1 0;1],horse.webp,claWhite,Tile,True,1,0.98,3C8SuQP0H3865PsbP4bn5z9fuv2zk4FC19HjgD8AFXCQgRQO1J5FCRbUHjcKjezuwCQPzB2/9g')]
    [TestCase('2',  '[0.5;0 1;1 0;1],horse.webp,claBlue,Tile,True,1,0.98,tVfyeQX170775et/70bs3GffuQWCH1ECzsDv8PcACPA/fPf6L28/DEho/vYPLLRueQhZuCz8Bw')]
    [TestCase('3',  '[0.5;0 1;1 0;1],horse.webp,claWhite,Tile,True,0.4,0.98,2EcCuvzVp55+7sRbH8bvT+SevPWh12B/4dpDMX4W1eNPfvgTJK8+/0IgD0L3JfhuPBLsO37+Cg')]
    [TestCase('4',  '[0.5;0 1;1 0;1],horse.webp,claBlue,Tile,True,0.4,0.98,yFdiev3Vk+4+775/b8bxS/Re/PiNp1D/3dNHMfsW0K8fvOwKRC88AFb4DoL5/+NuuQYKyE29EQ')]
    [TestCase('5',  '[0.5;0 1;1 0;1],horse.webp,claWhite,Tile,False,1,0.98,3C8SuQP0H3865PsbP4bn5z9fuv2zk4FC19HjgD8AFXCQgRQO1J5FCRbUHjcKjezuwCQPzB2/9g')]
    [TestCase('6',  '[0.5;0 1;1 0;1],horse.webp,claBlue,Tile,False,1,0.98,tVfyeQX170775et/70bs3GffuQWCH1ECzsDv8PcACPA/fPf6L28/DEho/vYPLLRueQhZuCz8Bw')]
    [TestCase('7',  '[0.5;0 1;1 0;1],horse.webp,claWhite,Tile,False,0.4,0.98,2EcCuvzVp55+7sRbH8bvT+SevPWh12B/4dpDMX4W1eNPfvgTJK8+/0IgD0L3JfhuPBLsO37+Cg')]
    [TestCase('8',  '[0.5;0 1;1 0;1],horse.webp,claBlue,Tile,False,0.4,0.98,yFdiev3Vk+4+775/b8bxS/Re/PiNp1D/3dNHMfsW0K8fvOwKRC88AFb4DoL5/+NuuQYKyE29EQ')]
    [TestCase('9',  '[0.5;0 1;1 0;1],horse.webp,claWhite,TileOriginal,True,1,0.98,3C8SuQP0H3865PsbP4bn5z9fuv2zk4FC19HjYEQoLLwuvu8OvE8G+frjn8EI7rvdNhtBkB077w')]
    [TestCase('10', '[0.5;0 1;1 0;1],horse.webp,claBlue,TileOriginal,True,1,0.98,tVfyeQX170775et/70bs3GffuQWCH1ECzsDvoLoq74sPN+U9KB8/CATM/gAF9S8dNA5QsBy88g')]
    [TestCase('11', '[0.5;0 1;1 0;1],horse.webp,claWhite,TileOriginal,True,0.4,0.98,2EcCuvzVp55+7sRbH8bvT+SevPWh12B/4dpDIYAm3zPvvOkSgO+++DeMT8b25n9eOA4BIN78Bw')]
    [TestCase('12', '[0.5;0 1;1 0;1],horse.webp,claBlue,TileOriginal,True,0.4,0.98,yFdiev3Vk+4+775/b8bxS/Re/PiNp1D/3dNHQbwnxocPeuUlRA+8/jsg/0X16UdO9wgHyD09CQ')]
    [TestCase('13', '[0.5;0 1;1 0;1],horse.webp,claWhite,TileOriginal,False,1,0.98,3C8SuQP0H3865PsbP4bn5z9fuv2zk4FC19HjYEQoLLwuvu8OvE8G+frjn8EI7rvdNhtBkB077w')]
    [TestCase('14', '[0.5;0 1;1 0;1],horse.webp,claBlue,TileOriginal,False,1,0.98,tVfyeQX170775et/70bs3GffuQWCH1ECzsDvoLoq74sPN+U9KB8/CATM/gAF9S8dNA5QsBy88g')]
    [TestCase('15', '[0.5;0 1;1 0;1],horse.webp,claWhite,TileOriginal,False,0.4,0.98,2EcCuvzVp55+7sRbH8bvT+SevPWh12B/4dpDIYAm3zPvvOkSgO+++DeMT8b25n9eOA4BIN78Bw')]
    [TestCase('16', '[0.5;0 1;1 0;1],horse.webp,claBlue,TileOriginal,False,0.4,0.98,yFdiev3Vk+4+775/b8bxS/Re/PiNp1D/3dNHQbwnxocPeuUlRA+8/jsg/0X16UdO9wgHyD09CQ')]
    [TestCase('17', '[0.5;0 1;1 0;1],horse.webp,claWhite,TileStretch,True,1,0.98,3j/yOAT2Mz+64/z7Pgbo6E9vuvy7j7GC19YPscAHAhjAwBINtC7DAi2Yb/YKhfAOvx0dYI09+A')]
    [TestCase('18', '[0.5;0 1;1 0;1],horse.webp,claBlue,TileStretch,True,1,0.98,t1fyOQb09y675ex/74bt2G+POQWHE2HCzsDvkPgG9Pdfe/YELO89Clek7jcLK6D++QpckIy7Bw')]
    [TestCase('19', '[0.5;0 1;1 0;1],horse.webp,claWhite,TileStretch,True,0.4,0.98,2U8C+vzWr45+7sVLHwbwUOyePPWk03C/4dxTsb4Zzr/vvfcSFK99/Exwz4H3IvyuOw/yD959Cw')]
    [TestCase('20', '[0.5;0 1;1 0;1],horse.webp,claBlue,TileStretch,True,0.4,0.98,yVdiuv3Vl+4+776DX0bySvg+vPiPo2D/3dNHcTsZyLPPe+wPRJ97/1wQb4L3/tueuQcLuA29EQ')]
    [TestCase('21', '[0.5;0 1;1 0;1],horse.webp,claWhite,TileStretch,False,1,0.98,3j/yOAT2Mz+64/z7Pgbo6E9vuvy7j7GC19YPscAHAhjAwBINtC7DAi2Yb/YKhfAOvx0dYI09+A')]
    [TestCase('22', '[0.5;0 1;1 0;1],horse.webp,claBlue,TileStretch,False,1,0.98,t1fyOQb09y675ex/74bt2G+POQWHE2HCzsDvkPgG9Pdfe/YELO89Clek7jcLK6D++QpckIy7Bw')]
    [TestCase('23', '[0.5;0 1;1 0;1],horse.webp,claWhite,TileStretch,False,0.4,0.98,2U8C+vzWr45+7sVLHwbwUOyePPWk03C/4dxTsb4Zzr/vvfcSFK99/Exwz4H3IvyuOw/yD959Cw')]
    [TestCase('24', '[0.5;0 1;1 0;1],horse.webp,claBlue,TileStretch,False,0.4,0.98,yVdiuv3Vl+4+776DX0bySvg+vPiPo2D/3dNHcTsZyLPPe+wPRJ97/1wQb4L3/tueuQcLuA29EQ')]
    [TestCase('25', '[0.5;0],horse.webp,claBlue,TileStretch,False,0.4,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('26', '[0.5;0 0.5;0],horse.webp,claBlue,TileStretch,False,0.4,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('27', '[0.5;0 0.5;1],horse.webp,claBlue,TileStretch,False,0.4,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('28', '[0.5;0 1;1],horse.webp,claBlue,TileStretch,False,0.4,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('29', '[],horse.webp,claBlue,TileStretch,False,0.4,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('30', '[0.5;0 1;1 0;1 $FFFFFF;$FFFFFF],horse.webp,claBlue,TileStretch,False,0.4,0.98,yVdiuv3Vl+4+776DX0bySvg+vPiPo2D/3dNHcTsZyLPPe+wPRJ97/1wQb4L3/tueuQcLuA29EQ')]
    [TestCase('31', '[0.5;0],horse.webp,claBlue,TileStretch,True,0.4,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('32', '[0.5;0 0.5;0],horse.webp,claBlue,TileStretch,True,0.4,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('33', '[0.5;0 0.5;1],horse.webp,claBlue,TileStretch,True,0.4,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('34', '[0.5;0 1;1],horse.webp,claBlue,TileStretch,True,0.4,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('35', '[],horse.webp,claBlue,TileStretch,True,0.4,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('36', '[0.5;0 1;1 0;1 $FFFFFF;$FFFFFF],horse.webp,claBlue,TileStretch,True,0.4,0.98,yVdiuv3Vl+4+776DX0bySvg+vPiPo2D/3dNHcTsZyLPPe+wPRJ97/1wQb4L3/tueuQcLuA29EQ')]
    [TestCase('37', '[0.5;0],horse.webp,claWhite,TileStretch,True,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('38', '[0.5;0 0.5;0],horse.webp,claWhite,TileStretch,True,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('39', '[0.5;0 0.5;1],horse.webp,claWhite,TileStretch,True,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('40', '[0.5;0 1;1],horse.webp,claWhite,TileStretch,True,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('41', '[],horse.webp,claWhite,TileStretch,True,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('42', '[0.5;0 1;1 0;1 $FFFFFF;$FFFFFF],horse.webp,claWhite,TileStretch,True,1,0.98,3j/yOAT2Mz+64/z7Pgbo6E9vuvy7j7GC19YPscAHAhjAwBINtC7DAi2Yb/YKhfAOvx0dYI09+A')]
    procedure TestFillPolygonBitmap(const APoints: string; const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode; ABlending: Boolean; const AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  'android.svg,claWhite,Tile,0,0,1,1,0.4,0.98,3leBO/nHM4+/+L5bL0T4YtQOP/C5A3B+6+F3Ub4UwLs/ffIAiL979kmgH8bu9iuvuwDuw15+GQ')]
    [TestCase('2',  'android.svg,claRed,Tile,0,0,1,1,0.4,0.98,03+Be/W5z67A+qRvn0P3X6QOPu+sD/A+4+NjQb0VvnuP/OsMxD969j6An8Xq6CffugD4Pz5+Fw')]
    [TestCase('3',  'android.svg,claWhite,Tile,0,0,1,1,1,0.98,7Ne/vPrTd9C8/eofX4H8FBB/QPDuhx8A8ONn0T/73wsAvwXh1z++8yYUMMH0GGAvv/kTJM8+Gw')]
    [TestCase('4',  'android.svg,claRed,Tile,0,0,1,1,1,0.98,0D/AfPGvf4//AqtL77/6DZz+/e3Om3/B2+cvEb393G9PvfX+a3D68wvA/z/r818fvfkr0I0+Fg')]
    [TestCase('5',  'android.svg,claWhite,Tile,0.15,0.15,0.85,0.85,0.4,0.98,0WeCe/6zz57C9Kxfz4T2hQSf/vWXw0D96PY7cb0muW//e+YZXJ889kNc34jv21vvuQboV25+EQ')]
    [TestCase('6',  'android.svg,claRed,Tile,0.15,0.15,0.85,0.85,0.4,0.98,xltye/6ut06C9pt/bwX3gNw+/fOMmzC+5uATobwnuXv/+uUZNP+6+UJ8D8nuzvOe+QbuM179Dg')]
    [TestCase('7',  'android.svg,claWhite,Tile,0.15,0.15,0.85,0.85,1,0.98,y3fS/Aefh38E9L0n74L4bIhvf/6ZYyE96hbMoH0o0FPvu+cdYG8A8xdkH4j20uPPuggEEO7+Bg')]
    [TestCase('8',  'android.svg,claRed,Tile,0.15,0.15,0.85,0.85,1,0.98,r1uiPAiUS69D+JR3b4T5YCT/+/l9/4B/5N9nkDsrzm9veeYeAG88/BS0fwj1s9v+uQgTuE08AQ')]
    [TestCase('9',  'android.svg,claWhite,TileOriginal,0,0,1,1,0.4,0.98,1yNCevm2Vw7B+Zp3r0T1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('10', 'android.svg,claRed,TileOriginal,0,0,1,1,0.4,0.98,2BeCOvqzV37B+ZSTb0T1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('11', 'android.svg,claWhite,TileOriginal,0,0,1,1,1,0.98,2NOhefuoV35AAJJjr0L1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('12', 'android.svg,claRed,TileOriginal,0,0,1,1,1,0.98,3LdBev2gV64BAIOv/0H1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('13', 'android.svg,claWhite,TileOriginal,0.15,0.15,0.85,0.85,0.4,0.98,12+Cu/u4g+5C9amLP0X1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('14', 'android.svg,claRed,TileOriginal,0.15,0.15,0.85,0.85,0.4,0.98,2mNSO/qwg05C9p57P0X1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('15', 'android.svg,claWhite,TileOriginal,0.15,0.15,0.85,0.85,1,0.98,2Y/CvACtx04F9bWX70P1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('16', 'android.svg,claRed,TileOriginal,0.15,0.15,0.85,0.85,1,0.98,4W9CfP2Xv67D95prD0T1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('17', 'android.svg,claWhite,TileStretch,0,0,1,1,0.4,0.98,3KchOwHiW7+/8s5330XyVOwOAPS44xD+5ehr8f8iz3Nf/erelx+99lGMTwXxyy+P+ggNPF49FA')]
    [TestCase('18', 'android.svg,claRed,TileStretch,0,0,1,1,0.4,0.98,w6NB+/vZ975/97WTDwb2UawePvajj0A/3ssX8f0lxIt/u+n5W497/U6A78Xty+NuOQkMtJ28DQ')]
    [TestCase('19', 'android.svg,claWhite,TileStretch,0,0,1,1,1,0.98,55fQew4V3FC97hNkj8Xt81P/QvrssyG/4vNH0UMfBVxf//KM97+B9DncL//6q2tfvA1h2D38DQ')]
    [TestCase('20', 'android.svg,claRed,TileStretch,0,0,1,1,1,0.98,qJMwfAH+5089+tavD0b36rMu/v645zDCzqtz4H4m6puv+u/PY989BTLAz0DyrLeO+Q5eiHy6/Q')]
    [TestCase('21', 'android.svg,claWhite,TileStretch,0.15,0.15,0.85,0.85,0.4,0.98,1UtyuQC38w4A7atLjwXtYuTO/vSzx7D93/dvkb4kr//Ovenvv289+VlMvwbq3/OuOAX6N648EA')]
    [TestCase('22', 'android.svg,claRed,TileStretch,0.15,0.15,0.85,0.85,0.4,0.98,vlei+f+5096/8KRzfwbxZsj+PPmbqwB+3ddHkX0mtBffPOkFgO+7/lIw30bs39tuOAT68x29DA')]
    [TestCase('23', 'android.svg,claWhite,TileStretch,0.15,0.15,0.85,0.85,1,0.98,1Duitw2q1/994Lvz3oTgFDjff/zfb0G+0xlMgYAjtjtewO+1V4DC+k48z8Lo3d+OtwQyxJ36Aw')]
    [TestCase('24', 'android.svg,claRed,TileStretch,0.15,0.15,0.85,0.85,1,0.98,nE8SeAuui4996atfL8fqIPBe+wakJwF/zcrn4P0nw3Mevu7tw7++CD38LoPu3Z8ONwIxHL07+w')]
    procedure TestFillRectBitmap(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode; const ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  'android.svg,claWhite,Tile,true,1,0.98,4jcewQIb8JB/Edzr/3z7ihuggAY6DPDD+PIX/33jHjTgwAkM7BB+/divr/gDCmzwAv89qH+/8w')]
    [TestCase('2',  'android.svg,claWhite,Tile,true,0.5,0.98,2C8+gQENyAB/ENf3bzz5iT/wPwQvDNDD9voDrz3iHCAwgAgD+ND9/s2Df7gFAizAwv0/cN//8g')]
    [TestCase('3',  'android.svg,claWhite,Tile,false,1,0.98,4jcewQIb8JB/Edzr/3z7ihuggAY6DPDD+PIX/33jHjTgwAkM7BB+/divr/gDCmzwAv89qH+/8w')]
    [TestCase('4',  'android.svg,claWhite,Tile,false,0.5,0.98,2C8+gQENyAB/ENf3bzz5iT/wPwQvDNDD9voDrz3iHCAwgAgD+ND9/s2Df7gFAizAwv0/cN//8g')]
    [TestCase('5',  'android.svg,claRed,Tile,true,1,0.98,3VfOP/j5QyCBFabXD/v5oNdv/fsMaECF6+CXzvzhAJDvvv8ZTJG6+axbH3n92ctPwAFbzJ696A')]
    [TestCase('6',  'android.svg,claRed,Tile,true,0.5,0.98,1j+eAPz8d9C/Erzvb3v4kxtQvv4YPIAE8PHDHn3hDdA/fwMJJBH8/LdXv3gC6t9vQf9OAO9+7Q')]
    [TestCase('7',  'android.svg,claRed,Tile,false,1,0.98,3VfOP/j5QyCBFabXD/v5oNdv/fsMaECF6+CXzvzhAJDvvv8ZTJG6+axbH3n92ctPwAFbzJ696A')]
    [TestCase('8',  'android.svg,claRed,Tile,false,0.5,0.98,1j+eAPz8d9C/Erzvb3v4kxtQvv4YPIAE8PHDHn3hDdA/fwMJJBH8/LdXv3gC6t9vQf9OAO9+7Q')]
    [TestCase('9',  'android.svg,claWhite,TileOriginal,true,1,0.98,zzdOwQADpHC+DuL/f7z2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('10', 'android.svg,claWhite,TileOriginal,true,0.5,0.98,zy9egQABpHA+D9oDILz2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('11', 'android.svg,claWhite,TileOriginal,false,1,0.98,zzdOwQADpHC+DuL/f7z2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('12', 'android.svg,claWhite,TileOriginal,false,0.5,0.98,zy9egQABpHA+D9oDILz2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('13', 'android.svg,claRed,TileOriginal,true,1,0.98,zgsewQP1h7D/DtdLoLv2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('14', 'android.svg,claRed,TileOriginal,true,0.5,0.98,zhtOAQL6lxA/D9QrwLv2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('15', 'android.svg,claRed,TileOriginal,false,1,0.98,zgsewQP1h7D/DtdLoLv2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('16', 'android.svg,claRed,TileOriginal,false,0.5,0.98,zhtOAQL6lxA/D9QrwLv2h19AvwEkEMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('17', 'android.svg,claWhite,TileStretch,true,1,0.98,8R/ugAEMHJF/ENNrb/vzoUOggf85oKBC+QP8DgDeLIwvwQjjm0G++ddzD7kD729QA/5FDJAA+Q')]
    [TestCase('18', 'android.svg,claWhite,TileStretch,true,0.5,0.98,4CMuwQAF4BD/D9O3nzv1lFNwgAAvWDAD9wL0vr7fI8xfAAjvT/H9/M1jv7gF9S/gQv1EpG+A9Q')]
    [TestCase('19', 'android.svg,claWhite,TileStretch,false,1,0.98,8R/ugAEMHJF/ENNrb/vzoUOggf85oKBC+QP8DgDeLIwvwQjjm0G++ddzD7kD729QA/5FDJAA+Q')]
    [TestCase('20', 'android.svg,claWhite,TileStretch,false,0.5,0.98,4CMuwQAF4BD/D9O3nzv1lFNwgAAvWDAD9wL0vr7fI8xfAAjvT/H9/M1jv7gF9S/gQv1EpG+A9Q')]
    [TestCase('21', 'android.svg,claRed,TileStretch,true,1,0.98,sxc+gfT0J5D/G5Wz7zv9mZ/fPAQG1L+F5borHnvlEshv/AUnBHE6CtBbvzr78LuPQP9CvM4+6Q')]
    [TestCase('22', 'android.svg,claRed,TileStretch,true,0.5,0.98,wR9egfr6ZwC/FbTf3/v5kP8f/gIV8L8E7d6PPjzjFuj/vQYQAAE8BclXj3kB9dePAf5C+I6/7Q')]
    [TestCase('23', 'android.svg,claRed,TileStretch,false,1,0.98,sxc+gfT0J5D/G5Wz7zv9mZ/fPAQG1L+F5borHnvlEshv/AUnBHE6CtBbvzr78LuPQP9CvM4+6Q')]
    [TestCase('24', 'android.svg,claRed,TileStretch,false,0.5,0.98,wR9egfr6ZwC/FbTf3/v5kP8f/gIV8L8E7d6PPjzjFuj/vQYQAAE8BclXj3kB9dePAf5C+I6/7Q')]
    procedure TestFillRectBitmapWithChessBackground(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode; ABlending: Boolean; const AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  'android.svg,claWhite,Tile,true,1,0.98,+xNiegPHtw5+6uVzvwb2NASP//rS+0B+7eh3wQIf3jPv/O7gQ6B9/134T8b315++eQMh4M69DQ')]
    [TestCase('2',  'android.svg,claWhite,Tile,true,0.5,0.98,6DeCuv3Dh67/78J7b8b1ZdjOvvWzq9C96uR/EUAixVt/POr7z9/7+1+o30fx28+OeQT7s+79Eg')]
    [TestCase('3',  'android.svg,claWhite,Tile,false,1,0.98,+xNiegPHtw5+6uVzvwb2NASP//rS+0B+7eh3wQIf3jPv/O7gQ6B9/134T8b315++eQMh4M69DQ')]
    [TestCase('4',  'android.svg,claWhite,Tile,false,0.5,0.98,6DeCuv3Dh67/78J7b8b1ZdjOvvWzq9C96uR/EUAixVt/POr7z9/7+1+o30fx28+OeQT7s+79Eg')]
    [TestCase('5',  'android.svg,claRed,Tile,true,1,0.98,4BeS+frFr/4888h3j8f5Psz+e/mlw4A/57kjcQAfzi8PO+4CQKB7AmbUj0bz1k/eOAopTI59CQ')]
    [TestCase('6',  'android.svg,claRed,Tile,true,0.5,0.98,2zcievnCgy4/9LR/z4b3arwO/fSdj3C+581T4T4ivFeP++kMzN96/WOU/wfv26ce+Qf/Z869EA')]
    [TestCase('7',  'android.svg,claRed,Tile,false,1,0.98,4BeS+frFr/4888h3j8f5Psz+e/mlw4A/57kjcQAfzi8PO+4CQKB7AmbUj0bz1k/eOAopTI59CQ')]
    [TestCase('8',  'android.svg,claRed,Tile,false,0.5,0.98,2zcievnCgy4/9LR/z4b3arwO/fSdj3C+581T4T4ivFeP++kMzN96/WOU/wfv26ce+Qf/Z869EA')]
    [TestCase('9',  'android.svg,claWhite,TileOriginal,true,1,0.98,21cCu/i/V15B9qKLv0X1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('10', 'android.svg,claWhite,TileOriginal,true,0.5,0.98,2FfSevjAV17B9aGH70X1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('11', 'android.svg,claWhite,TileOriginal,false,1,0.98,21cCu/i/V15B9qKLv0X1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('12', 'android.svg,claWhite,TileOriginal,false,0.5,0.98,2FfSevjAV17B9aGH70X1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('13', 'android.svg,claRed,TileOriginal,true,1,0.98,2kuyOvm/S44B956Pz0X1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('14', 'android.svg,claRed,TileOriginal,true,0.5,0.98,2E+yuvi/U35B9p+L70X1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('15', 'android.svg,claRed,TileOriginal,false,1,0.98,2kuyOvm/S44B956Pz0X1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('16', 'android.svg,claRed,TileOriginal,false,0.5,0.98,2E+yuvi/U35B9p+L70X1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('17', 'android.svg,claWhite,TileStretch,true,1,0.98,2pfRORbqP5C97Ahkn8Pn/I9vgP3Uy4E/3BBk8cIfDMgOP+2zVwCD9Uuw/4D8wwdP+AxPxP36Bw')]
    [TestCase('18', 'android.svg,claWhite,TileStretch,true,0.5,0.98,2PdBOgfVS38/8dR3z4TuSRw///a1E2E+4vhzIYAi2yePfenk14/+9lWEL4Xz0gPfOAkSJI78Dw')]
    [TestCase('19', 'android.svg,claWhite,TileStretch,false,1,0.98,2pfRORbqP5C97Ahkn8Pn/I9vgP3Uy4E/3BBk8cIfDMgOP+2zVwCD9Uuw/4D8wwdP+AxPxP36Bw')]
    [TestCase('20', 'android.svg,claWhite,TileStretch,false,0.5,0.98,2PdBOgfVS38/8dR3z4TuSRw///a1E2E+4vhzIYAi2yePfenk14/+9lWEL4Xz0gPfOAkSJI78Dw')]
    [TestCase('21', 'android.svg,claRed,TileStretch,true,1,0.98,lp/B+Qzbuz/998+nH4bx9A+fewGfG0HCzLjP0H0l9g9Pu+rvr9++A0Fov8H1xXduNgtamFy6+A')]
    [TestCase('22', 'android.svg,claRed,TileStretch,true,0.5,0.98,tvsxugLNC0+/9reXH0bzRdzevPiat9B/2swnkX0l0UevO+gCgH/8/VBgj0Xw0r/edwgYjC18CA')]
    [TestCase('23', 'android.svg,claRed,TileStretch,false,1,0.98,lp/B+Qzbuz/998+nH4bx9A+fewGfG0HCzLjP0H0l9g9Pu+rvr9++A0Fov8H1xXduNgtamFy6+A')]
    [TestCase('24', 'android.svg,claRed,TileStretch,false,0.5,0.98,tvsxugLNC0+/9reXH0bzRdzevPiat9B/2swnkX0l0UevO+gCgH/8/VBgj0Xw0r/edwgYjC18CA')]
    procedure TestFillRectBitmapWithClipping(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode; ABlending: Boolean; const AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  'android.svg,300,300,Tile,1,2,-50,-100,30,-200,-50,660,343,1,true,0.98,wtOAegAQXB88B7wvYAAGN4ivO+r6y+AC+fv774EM5i+gfvHQX99+3eEnTv7iNhgfABj5a95CAw')]
    [TestCase('2',  'android.svg,300,300,Tile,1,2,-50,-100,30,-200,-50,660,343,0.6,true,0.98,ym+ROv3w9z4+ALHrr0L/XTCfvOzSn7BA8vCXEIAWzuufvezsW+886BSgvsLlExBvvRDrc+6ACw')]
    [TestCase('3',  'android.svg,300,300,Tile,1,2,-50,-100,30,0,0,200,200,1,true,0.98,tlegu/ivSz7EDlsj33/1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('4',  'android.svg,300,300,Tile,1,2,-50,-100,30,0,0,200,200,0.6,true,0.98,wyNBe/i2Ux7DBHZLX0L1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('5',  'android.svg,300,300,Tile,1,-2,50,500,30,-200,-50,660,343,1,true,0.98,Imzie/71A787+0Zkn4DvDDhvwOr+DwJB6N+/QL0Jt4uQ/PsmDI957hMUL0HuAQQgPfU7pK7DDw')]
    [TestCase('6',  'android.svg,300,300,Tile,-1,2,-50,-100,30,-200,-50,660,343,0.6,true,0.98,4Mux+/rir47A+omLn0T1lawOPvCVV1A96OCD0X0jp18vvOQZpC/69FRUX8nq4ANfeQXVgw4+GA')]
    [TestCase('7',  'android.svg,300,300,Tile,-1,2,250,-50,30,0,0,200,200,1,true,0.98,xrcAugIlhE/8/7fXnwD2S2SuveyPc0DC3a6vIP8TLDBP/OJHdOD47h1UX8nq4ANfeQXVgw4+GA')]
    [TestCase('8',  'android.svg,300,300,Tile,1,-2,-50,200,30,0,0,200,200,0.6,true,0.98,s5PhOgHXs19F+JNbAMb/e1TvffelOyA/5+Zv0b8asWsf/urzpxA+9x1s/8fo1uf++Ajzf558Ew')]
    [TestCase('9',  'android.svg,300,300,TileOriginal,1,2,150,20,-90,-200,-50,660,343,1,true,0.98,4k9iefzFy37B9pp734X6gOyufveTk6C95e+D4b8hxGMuP/C/LxAA/mR4T8Ps84PueAY01J16CQ')]
    [TestCase('10', 'android.svg,300,300,TileOriginal,1,2,-50,100,-90,-200,50,660,343,0.6,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDwT0pld8/++z10/679THEPkbtylOfOw/jV67+Eg')]
    [TestCase('11', 'android.svg,300,300,TileOriginal,1,2,50,100,20,0,0,200,200,1,true,0.98,xbux+v/Dy25C74zjr0b1lawOPvCVV1A96OCDoT0URkCP/eZ75J87+ElUX8nq4ANfeQXVgw4+GA')]
    [TestCase('12', 'android.svg,300,300,TileOriginal,1,2,50,100,20,0,0,200,200,0.6,true,0.98,zPuxuvzCn/7B8ZS7b0b1lawOPvCVV1A96OCDgT0bCFzvfOZTrP86+FJUX8nq4ANfeQXVgw4+GA')]
    [TestCase('13', 'android.svg,300,300,TileOriginal,1,-2,50,500,30,-200,-50,660,343,1,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('14', 'android.svg,300,300,TileOriginal,-1,2,-50,-100,60,-200,-50,660,343,0.6,true,0.98,21sBe/e7C36BAob/DkP1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('15', 'android.svg,300,300,TileOriginal,-1,2,-50,-100,-60,0,0,200,200,1,true,0.98,1g9BPP3BP64ABH/HTkT1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('16', 'android.svg,300,300,TileOriginal,1,-2,-50,300,10,0,0,200,200,0.6,true,0.98,4BOCe/fUcy5C9Z6bf0b1lawOPvCVV1A96OCDYf0Vq1tPvOcbZDC49BtUX8nq4ANfeQXVgw4+GA')]
    [TestCase('17', 'android.svg,300,300,TileStretch,1,2,-50,-100,30,-200,-50,660,343,1,true,0.98,H5Aff/8PcDC+FRIE37v4rTtgxvwitDGB6/Wfnz/rCpjwfQ4xJFF++djn/7gFIWwAwglScA4BAg')]
    [TestCase('18', 'android.svg,300,300,TileStretch,1,2,-50,-100,30,-200,-50,660,343,0.6,true,0.98,AqxQffzvm3+/COQ373/3CpgPg/fqK6E/6uxfsL4C5Csw/f0mbJD8+A6sj//6B9yfPgggeN7/Cg')]
    [TestCase('19', 'android.svg,300,300,TileStretch,1,2,-50,-100,30,0,0,200,200,1,true,0.98,5s9QOfLPk/4BDYOvjkD1lawOPvCVV1A96OCDMX4msluP/OIVlA869lxUX8nq4ANfeQXVgw4+GA')]
    [TestCase('20', 'android.svg,300,300,TileStretch,1,2,-50,-100,30,0,0,200,200,0.6,true,0.98,32vhufTJe76BA44Dv0L1lawOPvCVV1A96OCD0f0lr2tf/OMVfA/69l5UX8nq4ANfeQXVgw4+GA')]
    [TestCase('21', 'android.svg,300,300,TileStretch,1,-2,50,500,30,-200,-50,660,343,1,true,0.98,30/C9/W1824+8qBnz4XnK4Tufv4JZIB93vbHQb4RyIMwPhLFRz9FBFCYz/rzEbi/xPVF9E9/+w')]
    [TestCase('22', 'android.svg,300,300,TileStretch,-1,2,-50,-100,-60,-200,-50,660,343,0.6,true,0.98,AWQAvvzxF4AA/+JPHwL3HOgev/77K9B/8vBzMH8B5d9/vwAIYNA9+AnMX0D5998vAAIYdA9+Ag')]
    [TestCase('23', 'android.svg,300,300,TileStretch,-1,2,-50,-100,-60,0,0,200,200,1,true,0.98,DhTvQAYWDJH/BvTnLr7pJfzePvjZC+F/2fdDET0ipwO/PeQOrN+6AGRQj8fs82OPuAH2/408Dw')]
    [TestCase('24', 'android.svg,300,300,TileStretch,1,-2,-50,500,30,0,0,200,200,0.6,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDMb4klZ+vfOsISP+592DkfgfwBlB/uxPiM56/FQ')]
    procedure TestFillRectBitmapWithMatrix(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer; AWrapMode: TWrapMode; ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg, ADestLeft, ADestTop, ADestRight, ADestBottom, AOpacity: Single; ABlending: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',   '[claBlack;0 claWhite;1],claWhite,Linear,False,1,1,0.98,ondgQBGUh18A991fgMTwvk+wQurId5FC4v8LYX8UI0yvPe/pa+/DAsnHfz0O45uuPRFNmE5/7g')]
    [TestCase('2',   '[$7F000000;0 $7FFFFFFF;1],claWhite,Linear,False,1,1,0.98,vGeBvQSq795A9r7vTwXzKnxvQO2u5/B/5fBHYf4c5mfffOr/Y/9+/RWMf0P84s9+ewsRjK5+Aw')]
    [TestCase('3',   '[claBlack;0 claWhite;1],claWhite,Linear,False,1,0.5,0.98,vGeB/QSq795A9r/zTwXzKXxvQO2v5/B/5fBHYf4c52fffOr/Y/++/RSMb0P84s+OewsRjK4+Aw')]
    [TestCase('4',   '[claBlack;0 claWhite;1],claWhite,Linear,True,1,1,0.98,ondgQBGUh18A991fgMTwvk+wQurId5FC4v8LYX8UI0yvPe/pa+/DAsnHfz0O45uuPRFNmE5/7g')]
    [TestCase('5',   '[$7F000000;0 $7FFFFFFF;1],claWhite,Linear,True,1,1,0.98,vGeBvQSq795A9r7vTwXzKnxvQO2u5/B/5fBHYf4c5mfffOr/Y/9+/RWMf0P84s9+ewsRjK5+Aw')]
    [TestCase('6',   '[claBlack;0 claWhite;1],claWhite,Linear,True,1,0.5,0.98,vGeB/QSq795A9r/zTwXzKXxvQO2v5/B/5fBHYf4c52fffOr/Y/++/RSMb0P84s+OewsRjK4+Aw')]
    [TestCase('7',   '[claBlack;0 claWhite;1],claWhite,Linear,False,2,1,0.98,pYfC+ReV919/6d2LT4blL/w+QPjek7A95cFXwfsbx9PAOgfLs57CAymkvjkCAXgfgQA2fH6A8A')]
    [TestCase('8',   '[$7F000000;0 $7FFFFFFF;1],claWhite,Linear,False,2,1,0.98,vW8y+gerJ19A776HL4btYtQuP/S5d4C95tFvgbwguStgO/bwB08+/kX8jkH28D8/PQMFfE5/BA')]
    [TestCase('9',   '[claBlack;0 claWhite;1],claWhite,Linear,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('10',  '[claBlack;0 claWhite;1],claWhite,Linear,True,0.5,1,0.98,rncQwQ2JKx8D++ZPEED1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('11',  '[$7F000000;0 $7FFFFFFF;1],claWhite,Linear,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('12',  '[claBlack;0 claWhite;1],claWhite,Linear,True,3,0.5,0.98,3E8C+frFw64+76xjv4bxUIxuvvbDI6D849TrIX4plm8fffLdH8+9/lbUzoH06h9vPAMMMM7+BQ')]
    [TestCase('13',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,False,1,1,0.98,mX8gwBGCe2/A+M9LIMXvxh9Qgue9WyHD3+0/scATKzgP//DjWw9FAsMDMP0R4MNuPhJFyM7/7A')]
    [TestCase('14',  '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Linear,False,1,1,0.98,uGth/QSh594A97fnn4XyLmQvwOup10DA4+dfAb8c61+PPev8V48//RKsTwP+4OPu+wsNpO6+Ag')]
    [TestCase('15',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,False,1,0.5,0.98,t2txPQWh694A97jnn4XyLWQ/wOup20DA4+dfAb8c61+PPev8V48//RGsT0P+4OPu+wsOpO5+Ag')]
    [TestCase('16',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,True,1,1,0.98,mX8gwBGCe2/A+M9LIMXvxh9Qgue9WyHD3+0/scATKzgP//DjWw9FAsMDMP0R4MNuPhJFyM7/7A')]
    [TestCase('17',  '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Linear,True,1,1,0.98,uGth/QSh594A97fnn4XyLmQvwOup10DA4+dfAb8c61+PPev8V48//RKsTwP+4OPu+wsNpO6+Ag')]
    [TestCase('18',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,True,1,0.5,0.98,t2txPQWh694A97jnn4XyLWQ/wOup20DA4+dfAb8c61+PPev8V48//RGsT0P+4OPu+wsOpO5+Ag')]
    [TestCase('19',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,False,2,1,0.98,pIfSuRWW9w//6913v4bmMPRe//jSe4D+4rBz8Tkfv89weQbUtw7BBCw0Tnr+ASBvwAIw8M3A8A')]
    [TestCase('20',  '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Linear,False,2,1,0.98,vW9C+garJz+A8L57bwbuY9C+fvS0a/B95ch7oXsitSfA+vX1B4+9/kbE3oH08RPfPAQCOG6/BA')]
    [TestCase('21',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('22',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,True,0.5,1,0.98,lEuQgBCHG7+D++OHkH/1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('23',  '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Linear,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('24',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,True,3,0.5,0.98,3EsCufrFx66+76tjr8bxT4w+/vbBG9B849HrQT0pl2u//PLnH++8/1qkDsLy5fueewMQ4G3+Bw')]
    [TestCase('25',  '[claBlack;-1 claWhite;0.8],claWhite,Linear,True,1,1,0.98,1nOwwRPJPwBA7BWUYML3p/9ABO8I6NH/7jxocIIQQki/wPDbX58GAspLULwV3gOv/hFNFJ8/8A')]
    [TestCase('26',  '[claBlack;0 claWhite;2],claWhite,Linear,True,1,1,0.98,pH8QQA6QU58//NQb8ATyyCtggem1N5HD3983EfwaCUgfeer5j9+/AeprH0AE0+vNug1UwA299g')]
    [TestCase('27',  '[claBlack;0 claWhite;1],claRed,Linear,False,1,1,0.98,o3sw/wmXQx9//7zjH0T1zcvP/+yvA+FD2cLDID4f9f8eePAiRDA7AdcbYML7qxNevA+AZDw67Q')]
    [TestCase('28',  '[$7F000000;0 $7FFFFFFF;1],claRed,Linear,False,1,1,0.98,vGvx/ACrzz5A+q6zH0X1MTjvvu6ir6DA4NEjwX0i0EMf+uoczJ+6/By470XzxoveugoqdC28Ag')]
    [TestCase('29',  '[claBlack;0 claWhite;1],claRed,Linear,False,1,0.5,0.98,vGfxPAGrzz5A+q6zD0X1MTjvvu6ir6CA4NEjwX0i0EMf+uobzJ+6/Bu470XzxYveugorcB28Ag')]
    [TestCase('30',  '[claBlack;0 claWhite;1],claRed,Linear,True,1,1,0.98,o3sw/wmXQx9//7zjH0T1zcvP/+yvA+FD2cLDID4f9f8eePAiRDA7AdcbYML7qxNevA+AZDw67Q')]
    [TestCase('31',  '[$7F000000;0 $7FFFFFFF;1],claRed,Linear,True,1,1,0.98,vGvx/ACrzz5A+q6zH0X1MTjvvu6ir6DA4NEjwX0i0EMf+uoczJ+6/By470XzxoveugoqdC28Ag')]
    [TestCase('32',  '[claBlack;0 claWhite;1],claRed,Linear,True,1,0.5,0.98,vGfxPAGrzz5A+q6zD0X1MTjvvu6ir6CA4NEjwX0i0EMf+uobzJ+6/Bu470XzxYveugorcB28Ag')]
    [TestCase('33',  '[claBlack;0 claWhite;1],claRed,Linear,False,2,1,0.98,oIei+RWV8/8+69l7f0bmM+Bev/nVc0D+4bBDcXkgvl9QOQbLJ39/Azlobvv40FsPfwBZBI097A')]
    [TestCase('34',  '[$7F000000;0 $7FFFFFFF;1],claRed,Linear,False,2,1,0.98,u28y+gaqJy9A8Lx/P8btZMi+/vS1Y8D95MhjYfsitO+v+vXwP7/8/UzgbsLx2C8//AIXxM09Ag')]
    [TestCase('35',  '[claBlack;0 claWhite;1],claRed,Linear,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('36',  '[claBlack;0 claWhite;1],claRed,Linear,True,0.5,1,0.98,1c/Qvf6Hy44CC44vb3/1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('37',  '[$7F000000;0 $7FFFFFFF;1],claRed,Linear,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('38',  '[claBlack;0 claWhite;1],claRed,Linear,True,3,0.5,0.98,20sC+frFw55+76tjr4bxUIg+/vbBG7B849HjQf0ol2O//PLjMw+9/1bEPgLy3Bvf+wIWyO29BA')]
    [TestCase('39',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,False,1,1,0.98,nItQPwuNL8++/cPrn0T00Mf/P+qp77AD3MT70D4e+ee++PEnYAC7ANI7UML8pi++fBGDRPz56g')]
    [TestCase('40',  '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Linear,False,1,1,0.98,uXMBvQGnwx6A+bK3X8X0MzgPP+2fo4AA4tI/Ef4h0jNvuuse2I96/BnE38Xzw5sOewssZA28AQ')]
    [TestCase('41',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,False,1,0.5,0.98,uW8BvQGmwx6A+bK3X8X0MjgPP+2fo4AA4tI/Eb4h0jNvuuse3I96/BnI38Xzw5sOewstYP17AQ')]
    [TestCase('42',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,True,1,1,0.98,nItQPwuNL8++/cPrn0T00Mf/P+qp77AD3MT70D4e+ee++PEnYAC7ANI7UML8pi++fBGDRPz56g')]
    [TestCase('43',  '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Linear,True,1,1,0.98,uXMBvQGnwx6A+bK3X8X0MzgPP+2fo4AA4tI/Ef4h0jNvuuse2I96/BnE38Xzw5sOewssZA28AQ')]
    [TestCase('44',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,True,1,0.5,0.98,uW8BvQGmwx6A+bK3X8X0MjgPP+2fo4AA4tI/Eb4h0jNvuuse3I96/BnI38Xzw5sOewstYP17AQ')]
    [TestCase('45',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,False,2,1,0.98,pIfSuRWW9w//6913v4bmMPRe//jSe4D+4rBzMXggum/AuAXMIz8/AzoYHvv32T9/vgBT7Jx97Q')]
    [TestCase('46',  '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Linear,False,2,1,0.98,vW9C+garJz+A8L57bwbuY9C+fvS0a/B95ch7wfoisvdvevXxP5+8/U24TkLx3SPv+wIUuN39Ag')]
    [TestCase('47',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('48',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,True,0.5,1,0.98,z8tgff+Iz37CC4c7v371lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('49',  '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Linear,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('50',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,True,3,0.5,0.98,3EsCufrFx66+76tjr8bxT4w+/vbBG9B849HrEf0ol2uv/PLlJ9+8/1ioHkLy4AdvuwIT1A0+Bw')]
    [TestCase('51',  '[claBlack;-1 claWhite;0.8],claRed,Linear,True,1,1,0.98,uEvgPgW3rz/AAqzTT8L7y5vPvvTJU+ED07znT0AfBuQuefEjQPC7AtKLgEL+nysePRCFgNz56w')]
    [TestCase('52',  '[claBlack;0 claWhite;2],claRed,Linear,True,1,1,0.98,pYOAvwqRM/8+AMPfz0T0z+f//+qp/0AE28ETcTsg8yNf9uoW+I/7APCXjwL7uKctOg1uqIz69Q')]
    [TestCase('53',  '[claRed;0 claBlue;1],claWhite,Linear,False,1,1,0.98,xB8wwAPJs49AB7wnAEP9wA8APvXMc+FE1skTYPkXDTBfNdnz1x5/DPgzX38JrM9MNwVTPG38+w')]
    [TestCase('54',  '[$7FFF0000;0 $7F0000FF;1],claWhite,Linear,False,1,1,0.98,zTtx/f3FA/9A/q7Tj0T5K1wPvvKx4xBB39TLYLse3Fu/eN8EFI98AixEbwT6xutNeAUU4D09Cg')]
    [TestCase('55',  '[claRed;0 claBlue;1],claWhite,Linear,False,1,0.5,0.98,zDtxPf7FB/9A/q7Xj0T5KlwPvvKx5yAB39TLYLse3FuveN8EFI98AixEXwT6xudNeAUU3D39CQ')]
    [TestCase('56',  '[claRed;0 claBlue;1],claWhite,Linear,True,1,1,0.98,xB8wwAPJs49AB7wnAEP9wA8APvXMc+FE1skTYPkXDTBfNdnz1x5/DPgzX38JrM9MNwVTPG38+w')]
    [TestCase('57',  '[$7FFF0000;0 $7F0000FF;1],claWhite,Linear,True,1,1,0.98,zTtx/f3FA/9A/q7Tj0T5K1wPvvKx4xBB39TLYLse3Fu/eN8EFI98AixEbwT6xutNeAUU4D09Cg')]
    [TestCase('58',  '[claRed;0 claBlue;1],claWhite,Linear,True,1,0.5,0.98,zDtxPf7FB/9A/q7Xj0T5KlwPvvKx5yAB39TLYLse3FuveN8EFI98AixEXwT6xudNeAUU3D39CQ')]
    [TestCase('59',  '[claRed;0 claBlue;1],claWhite,Linear,False,2,1,0.98,p0eyuBeh4//+761vj0TpK9Q+/gTyq0A/1aTPH/wf0XNAewPGC/+ACzkkv/oCt6sPQP1hsK2/5w')]
    [TestCase('60',  '[$7FFF0000;0 $7F0000FF;1],claWhite,Linear,False,2,1,0.98,vk+y+QexGy+A8qd3T0XvYcAufvrDf1C+3sKrsLwivvuve/TuM3/9AU08H8L2zFe/fAEbGN4+AA')]
    [TestCase('61',  '[claRed;0 claBlue;1],claWhite,Linear,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('62',  '[claRed;0 claBlue;1],claWhite,Linear,True,0.5,1,0.98,42egQvlps65ABs3D/0L1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('63',  '[$7FFF0000;0 $7F0000FF;1],claWhite,Linear,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('64',  '[claRed;0 claBlue;1],claWhite,Linear,True,3,0.5,0.98,z2+CeP25ky6+7qxn/4XwWzC+vvnSC5D839Q/YX4rn09vffLbS3/+AlJkjwL1x28/PQIfAE5+/Q')]
    [TestCase('65',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,False,1,1,0.98,zx8wAAPX5z+BCq/7b4L+xP8vvvnUmzEF0r/Tn3gYBVB/NNbrk17/DvsXHz8Jrpf8dgJNVA29/Q')]
    [TestCase('66',  '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Linear,False,1,1,0.98,0jtxvf3MH08BAKe/PwT6LVAf/vS1+0AB3c+r8Poe12tP+N0A9K58Ay00T8T5x88t+AMR7I39Cg')]
    [TestCase('67',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,False,1,0.5,0.98,0jtxvf3MI08BAKi/PwT6LFQf/vS1+0AB3c+r8Poe2Gs/uN0A8L68Ay00PwT6x8st+AMS6I39Cg')]
    [TestCase('68',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,True,1,1,0.98,zx8wAAPX5z+BCq/7b4L+xP8vvvnUmzEF0r/Tn3gYBVB/NNbrk17/DvsXHz8Jrpf8dgJNVA29/Q')]
    [TestCase('69',  '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Linear,True,1,1,0.98,0jtxvf3MH08BAKe/PwT6LVAf/vS1+0AB3c+r8Poe12tP+N0A9K58Ay00T8T5x88t+AMR7I39Cg')]
    [TestCase('70',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,True,1,0.5,0.98,0jtxvf3MI08BAKi/PwT6LFQf/vS1+0AB3c+r8Poe2Gs/uN0A8L68Ay00PwT6x8st+AMS6I39Cg')]
    [TestCase('71',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,False,2,1,0.98,pT9CuBee6+8+7qdb/8PpMqxefgX2o+A+0pyTr/0h1T/QuwTDMz8BCzyEj/sBpsvPgP1s1F0/6A')]
    [TestCase('72',  '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Linear,False,2,1,0.98,vUuC+QevIy/A8aRvD4XvZKw+vvrGfyA+3b6LgL0jwN//+/TtR5+9AU5sf0L2w2cfvQEgLK4+AA')]
    [TestCase('73',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('74',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,True,0.5,1,0.98,5V9gQ/dqi44AB9+/T0T1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('75',  '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Linear,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('76',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,True,3,0.5,0.98,zm+CeP24kw5+7qxj34XwXCi+vvnSC3C839Mzsf4qoT9/vfLcY09+Ak+M/4LzwovfPQMm5K39+g')]
    [TestCase('77',  '[claRed;-1 claBlue;0.8],claWhite,Linear,True,1,1,0.98,niOQgAqk644+/dbTsAT1pJsw/uiz32BE3e/b8PYUAIA/tNb1k16+DPOjfj4KwZOc9gRM7Aw9+w')]
    [TestCase('78',  '[claRed;0 claBlue;2],claWhite,Linear,True,1,1,0.98,xyNw/wLN19+ABq/3HwL/xcsPvvfUg3GE0r+zPz0cDAhvd+UKhL/9B+X3DwEEpHtNegps7Bw79A')]
    [TestCase('79',  '[claRed;0 $7F000000;0.5 claBlue;1],claWhite,Linear,True,1,1,0.98,4+sQAP6uN0/BBsOzP0P9ELgffumtV6FE2r+HcDge5Qt/NdnzDw9+AinY7gL9s0999gYm/N18CA')]
    [TestCase('80',  '[claRed;0 $7F000000;1 claBlue;0.5],claWhite,Linear,True,1,1,0.98,vBOgwQXCZ/+/CNiD8AT6tpvwPfC8T6HF3dvPEPkZ6uM/t+UbSJ/6AgDQ3oH+7ItteAhBiBz8/Q')]
    [TestCase('81',  '[claRed;0 $7F000000;-1 claBlue;0.5],claWhite,Linear,True,1,1,0.98,vROgAQbDZ/+/CNeH8AT6tpvwPfC9T7HF3dvPIDYT/psvtNb6k969C/BvHr4KypNc9gVLuAz9+Q')]
    [TestCase('82',  '[claRed;1 $7F000000;0.5 claBlue;0],claWhite,Linear,True,1,1,0.98,sAPRfgd7U84+9chXYEPz7/evftyakwDD2ujX8Lshzzu/9+si+E96+v1zn0T0w0NO+w9N4Fx7+A')]
    [TestCase('83',  '[claRed;1 $7F000000;0 claBlue;0.5],claWhite,Linear,True,1,1,0.98,l6sA/wq29+6+/Mpn0Mfw4StQP/Kpo/AB5+brYboW7a//duwvsP/5AcInH0AB16+N+xB2mCv76A')]
    [TestCase('84',  '[claRed;1 $7F000000;0 claBlue;0.5 claWhite;0.49],claWhite,Linear,True,1,1,0.98,mOPgPRXBs89B7NSPQAXr3u+vxPfjh3H+6SgkwXoW9J/Pt+gsrB96BcdT7z8D0t/dug93sIt76w')]
    [TestCase('85',  '[claRed;1 $7F000000;0 claBlue;0.5 claWhite;0.49],claWhite,Linear,False,1,1,0.98,mOPgPRXBs89B7NSPQAXr3u+vxPfjh3H+6SgkwXoW9J/Pt+gsrB96BcdT7z8D0t/dug93sIt76w')]
    [TestCase('86',  '[claRed;1 $7F000000;0 claBlue;0.5 claWhite;0.49],claWhite,Linear,True,2,0.5,0.98,x1dCugK1387A8LCPL4bwccAuf/OzbyC959578f0etXNwPPX1245+/T5ADwL3BYCvPgTqG19ABQ')]
    [TestCase('87',  '[],claWhite,Linear,True,1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('88',  '[claBlack;0 claWhite;1],claWhite,Radial,False,1,1,0.98,wK8w/xCPJ39++Obfz8PswleAQufmR0ED5wkIYT4bDECv+OXZ98+B+gaQj0IBt0+tuQxziJ15Aw')]
    [TestCase('89',  '[$7F000000;0 $7FFFFFFF;1],claWhite,Radial,False,1,1,0.98,y4fxfASov+7/9sOv7wTxLHxPwOu+z0CA5/RH0X0g22NfuuX3p+99+TNw/wX2zCt+OQkkBM67DQ')]
    [TestCase('90',  '[claBlack;0 claWhite;1],claWhite,Radial,False,1,0.5,0.98,y4PxvASov+7/9sOz7wTxK4BPwOu+01CA5/VH4X0g22NfuuX3p+99+TN07wX2zCeOOQklBM67DQ')]
    [TestCase('91',  '[claBlack;0 claWhite;1],claWhite,Radial,True,1,1,0.98,wK8w/xCPJ39++Obfz8PswleAQufmR0ED5wkIYT4bDECv+OXZ98+B+gaQj0IBt0+tuQxziJ15Aw')]
    [TestCase('92',  '[$7F000000;0 $7FFFFFFF;1],claWhite,Radial,True,1,1,0.98,y4fxfASov+7/9sOv7wTxLHxPwOu+z0CA5/RH0X0g22NfuuX3p+99+TNw/wX2zCt+OQkkBM67DQ')]
    [TestCase('93',  '[claBlack;0 claWhite;1],claWhite,Radial,True,1,0.5,0.98,y4PxvASov+7/9sOz7wTxK4BPwOu+01CA5/VH4X0g22NfuuX3p+99+TN07wX2zCeOOQklBM67DQ')]
    [TestCase('94',  '[claBlack;0 claWhite;1],claWhite,Radial,False,2,1,0.98,pIvCuRWW+w+/691zv0bmMfR+gPfcozC95MBXEboju2sguQW4H78B/z9YX7kECgSgAf0gMF8A7A')]
    [TestCase('95',  '[$7F000000;0 $7FFFFFFF;1],claWhite,Radial,False,2,1,0.98,vXMy+garKz+A8L57b8btZNA+//O4e0B95tBvsbsks/efevXnO9+9+1BYb4H39YN/fQH72z4/Ag')]
    [TestCase('96',  '[claBlack;0 claWhite;1],claWhite,Radial,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('97',  '[claBlack;0 claWhite;1],claWhite,Radial,True,0.5,1,0.98,4V8QwgORx/5C/b6/f0L1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('98',  '[$7F000000;0 $7FFFFFFF;1],claWhite,Radial,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('99',  '[claBlack;0 claWhite;1],claWhite,Radial,True,3,0.5,0.98,3EsCufrFx66+76tjr8bxUIw+/vbBG8B849DrMf0olme/fPPnK9+8/1nQDoLy5xu/OwINDD5+Bg')]
    [TestCase('100', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,False,1,1,0.98,v9+Q/w+A+y6+9uTPr8TrxEOgQuboD4ED6QgYgX0dBURf+OTQEyCC+QpsDwP7uDMN+QqBaF14Ag')]
    [TestCase('101', '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Radial,False,1,1,0.98,yp8h/QOgq84/9sKrX4XwLXRfAOu+s3CA6PRPYX0h12c/OuXzsx/++DVgP8byzBs+OQgr9D17DQ')]
    [TestCase('102', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,False,1,0.5,0.98,ypshPQSgq84/9sKrX4XwLHhfAOu/t3CA6PRPcX0h2Gc/OuXytx/++DVgPwbzzBs+OQgs9C07DQ')]
    [TestCase('103', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,True,1,1,0.98,v9+Q/w+A+y6+9uTPr8TrxEOgQuboD4ED6QgYgX0dBURf+OTQEyCC+QpsDwP7uDMN+QqBaF14Ag')]
    [TestCase('104', '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Radial,True,1,1,0.98,yp8h/QOgq84/9sKrX4XwLXRfAOu+s3CA6PRPYX0h12c/OuXzsx/++DVgP8byzBs+OQgr9D17DQ')]
    [TestCase('105', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,True,1,0.5,0.98,ypshPQSgq84/9sKrX4XwLHhfAOu/t3CA6PRPcX0h2Gc/OuXytx/++DVgPwbzzBs+OQgs9C07DQ')]
    [TestCase('106', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,False,2,1,0.98,pIfSuRWW9w//6913vwbnNACv//fVi+B947V/sXgjv1eAuAbFH39AAUdYb7kCEQhAwf0TGL9A6Q')]
    [TestCase('107', '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Radial,False,2,1,0.98,vW9C+garJz+A8L57b0buZdTe/vO1b6C95cqDAXsktetP+vXtOz/9/FRYb4H2+IdPvQH0z27/AA')]
    [TestCase('108', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('109', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,True,0.5,1,0.98,3lMQQgOUr05D/ba7H0P1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('110', '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Radial,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('111', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,True,3,0.5,0.98,3EsCufrFx66+76tjr8bxT4w+/vbBG9B849HrMb0olme/PPPnK9+8/1m0TsLy4fteewIQ5A0+CA')]
    [TestCase('112', '[claBlack;-1 claWhite;0.8],claWhite,Radial,True,1,1,0.98,548QQRPHB/B+7RpM8IH1qQchRO0Z1DEA8UFswIETMkQPPuzTq3/F/eYb0H4Oy1e+vA9jiL78+Q')]
    [TestCase('113', '[claBlack;0 claWhite;2],claWhite,Radial,True,1,1,0.98,s5uAPw6OI6++/NjbnwTwyi9QAejEH/ED4uQ7kTse/kOftuXx09++/QhQn8L9vkfNuAtnPD36AA')]
    [TestCase('114', '[claBlack;0 claWhite;1],claRed,Radial,False,1,1,0.98,mZ9gvwyIF18+AcDLf0T23Nevf+69ByFE2bebEHwjBABP9uYFwK99BAhkf4P6rhNtOQpuGN15/Q')]
    [TestCase('115', '[$7F000000;0 $7FFFFFFF;1],claRed,Radial,False,1,1,0.98,t3sBfQKkt94/+7CnP8X1OUDfPu+pr8DA4MsPsXwk10MvOeYNiN87/jRcb4byxwtu+QchzP37Cg')]
    [TestCase('116', '[claBlack;0 claWhite;1],claRed,Radial,False,1,0.5,0.98,t3sBfQKkt95/+7CnP8X1OEDfPu+pr8CA4MsPsXwk2EMvOeYNjN97/jRcb8byxwtu+QcizP27Cg')]
    [TestCase('117', '[claBlack;0 claWhite;1],claRed,Radial,True,1,1,0.98,mZ9gvwyIF18+AcDLf0T23Nevf+69ByFE2bebEHwjBABP9uYFwK99BAhkf4P6rhNtOQpuGN15/Q')]
    [TestCase('118', '[$7F000000;0 $7FFFFFFF;1],claRed,Radial,True,1,1,0.98,t3sBfQKkt94/+7CnP8X1OUDfPu+pr8DA4MsPsXwk10MvOeYNiN87/jRcb4byxwtu+QchzP37Cg')]
    [TestCase('119', '[claBlack;0 claWhite;1],claRed,Radial,True,1,0.5,0.98,t3sBfQKkt95/+7CnP8X1OEDfPu+pr8CA4MsPsXwk2EMvOeYNjN97/jRcb8byxwtu+QcizP27Cg')]
    [TestCase('120', '[claBlack;0 claWhite;1],claRed,Radial,False,2,1,0.98,o4viuRWW+//+69xzzwblM9x+v/jSawC+4bBDQfgiwmeQeAXJRx9AAkScfnv5y49PfwBc/Kz96A')]
    [TestCase('121', '[$7F000000;0 $7FFFFFFF;1],claRed,Radial,False,2,1,0.98,vHNC+garKy+A8L57b0btZMS+fvS0Y7D95Mhj0ToktvNPevXvTw99/VL4fgLy1ktf/AIZwN29AA')]
    [TestCase('122', '[claBlack;0 claWhite;1],claRed,Radial,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('123', '[claBlack;0 claWhite;1],claRed,Radial,True,0.5,1,0.98,438QAP+A2x5CB4sfT0L1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('124', '[$7F000000;0 $7FFFFFFF;1],claRed,Radial,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('125', '[claBlack;0 claWhite;1],claRed,Radial,True,3,0.5,0.98,3EsCufrFx66+76tjr8bxUIw+/vbBG9B849DrMb0ol2e/fPPnK9+8/1msDgLy3g+PuwIWzA1+Bg')]
    [TestCase('126', '[claBlack;0.2 claWhite;0.8],claRed,Radial,False,1,1,0.98,j8Ogfw5/7w6+/sfPnwT34NfPv+676yBE27erwPskBPheducCuA++BQlIj4P4rvNcuQhzIN25/g')]
    [TestCase('127', '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Radial,False,1,1,0.98,so8hPQOfo74/+rSnX0X2O0Dvfu+oo7CA4cwXgTwl1z8/eeYMiA/8/jVMf4bxx/td+QYk0O17Cw')]
    [TestCase('128', '[claBlack;0.2 claWhite;0.8],claRed,Radial,False,1,0.5,0.98,soshfQOfo64/+rSnX0X2OkDvfu+oo8CA4cwXgTwl2D8veeYMiA/8/jVMf4bxx/tdOQck0O17Cw')]
    [TestCase('129', '[claBlack;0.2 claWhite;0.8],claRed,Radial,True,1,1,0.98,j8Ogfw5/7w6+/sfPnwT34NfPv+676yBE27erwPskBPheducCuA++BQlIj4P4rvNcuQhzIN25/g')]
    [TestCase('130', '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Radial,True,1,1,0.98,so8hPQOfo74/+rSnX0X2O0Dvfu+oo7CA4cwXgTwl1z8/eeYMiA/8/jVMf4bxx/td+QYk0O17Cw')]
    [TestCase('131', '[claBlack;0.2 claWhite;0.8],claRed,Radial,True,1,0.5,0.98,soshfQOfo64/+rSnX0X2OkDvfu+oo8CA4cwXgTwl2D8veeYMiA/8/jVMf4bxx/tdOQck0O17Cw')]
    [TestCase('132', '[claBlack;0.2 claWhite;0.8],claRed,Radial,False,2,1,0.98,pIfSuRWW9w//6913v8bkMABfv/jRZyD+4rJvITgjxG+AOAbMLw/AAUt8Dvv3z4/vfgFc4Nz95w')]
    [TestCase('133', '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Radial,False,2,1,0.98,vW9C+garJz+A8L57bwbtY9S+fvSzX8C95cl3wXokt/dPuvXxQw/9/FbsPkLx2EsvfAMZsO09AA')]
    [TestCase('134', '[claBlack;0.2 claWhite;0.8],claRed,Radial,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('135', '[claBlack;0.2 claWhite;0.8],claRed,Radial,True,0.5,1,0.98,4GsggP+Ay84BBocTj0L1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('136', '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Radial,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('137', '[claBlack;0.2 claWhite;0.8],claRed,Radial,True,3,0.5,0.98,3EsCufrFx66+76tjr8bxT4w+/vbBG9B849HrMb0olme/PPPnK9+8/1mcLsLy4ftOewIU2P39Bw')]
    [TestCase('138', '[claBlack;-1 claWhite;0.8],claRed,Radial,True,1,1,0.98,s1/wvgavk9//A67Hf0L806e/fvXRVxHE0rbTL78hCugu+OwUBCA9BOorEAP9oqe9Ow182Lz58w')]
    [TestCase('139', '[claBlack;0 claWhite;2],claRed,Radial,True,1,1,0.98,oJOQPwyKH5/+AMXP/8T01+/vv+uw/2AE27sDYToi+idvNeYIuM98Agk8H4P6uSe9eAplBF36/Q')]
    [TestCase('140', '[claRed;0 claBlue;1],claWhite,Radial,False,1,1,0.98,zDOgvwHayx/BBcA/IAP+ogdAvfLEa1GE2c8bMP0a96cPeO4svE87AcBPEIEGuzfuew6D8Ov66g')]
    [TestCase('141', '[$7FFF0000;0 $7F0000FF;1],claWhite,Radial,False,1,1,0.98,0Uch/fzNEz+B/bDjn4T5HFivffGs39DA4NjPQD0g0ZcP+ukhiK+6/BDQP4X4zp+u+gksOH28AQ')]
    [TestCase('142', '[claRed;0 claBlue;1],claWhite,Radial,False,1,0.5,0.98,0UMh/fzNEz+B/bDjn8T5G1ivffGt49DA4NjPQD0g0ZcP+ukhiK+6/BDQP8X4zpuu+gksOH18AQ')]
    [TestCase('143', '[claRed;0 claBlue;1],claWhite,Radial,True,1,1,0.98,zDOgvwHayx/BBcA/IAP+ogdAvfLEa1GE2c8bMP0a96cPeO4svE87AcBPEIEGuzfuew6D8Ov66g')]
    [TestCase('144', '[$7FFF0000;0 $7F0000FF;1],claWhite,Radial,True,1,1,0.98,0Uch/fzNEz+B/bDjn4T5HFivffGs39DA4NjPQD0g0ZcP+ukhiK+6/BDQP4X4zp+u+gksOH28AQ')]
    [TestCase('145', '[claRed;0 claBlue;1],claWhite,Radial,True,1,0.5,0.98,0UMh/fzNEz+B/bDjn8T5G1ivffGt49DA4NjPQD0g0ZcP+ukhiK+6/BDQP8X4zpuu+gksOH18AQ')]
    [TestCase('146', '[claRed;0 claBlue;1],claWhite,Radial,False,2,1,0.98,pjtCuBee5/9+7qhf/8PpLuRO/gb0r6A/1ajbL34gxDfwuwbE+45ACCvAbjoDxoP/fvxR7O2A6w')]
    [TestCase('147', '[$7FFF0000;0 $7F0000FF;1],claWhite,Radial,False,2,1,0.98,vkty+QevHy/A8aRvD4XvYsgufvvFg4C+3sSvsP0it98P/PXtK089AEYM78H200Mv/AATNH7/AQ')]
    [TestCase('148', '[claRed;0 claBlue;1],claWhite,Radial,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('149', '[claRed;0 claBlue;1],claWhite,Radial,True,0.5,1,0.98,5btA/vlp117CCq+Lr3/1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('150', '[$7FFF0000;0 $7F0000FF;1],claWhite,Radial,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('151', '[claRed;0 claBlue;1],claWhite,Radial,True,3,0.5,0.98,zm+CeP24kw5+7qxj34XwXCi+vvnSC3C839Mzkb4qoD9vffLaW0/+AUx8D8PzxoN/vQIgAB4+/A')]
    [TestCase('152', '[claRed;0.2 claBlue;0.8],claWhite,Radial,False,1,1,0.98,2CNwf//jC5BBCbYnQMP8nu9vffLLi9ED2sr/330a9rtPuO0v2P/6/r1zUMEIvXcOPA+G5Nt66A')]
    [TestCase('153', '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Radial,False,1,1,0.98,1j8RvfvRL39B/6vXrwT5Gki/ffGw85AA4dXDkP0f0J8vuukimH96+w/kX8X5z7uuegotNH18AA')]
    [TestCase('154', '[claRed;0.2 claBlue;0.8],claWhite,Radial,False,1,0.5,0.98,1jsR/fvRM39B/6vXrwT5GUy/ffGw86AA4dW/kP0f0Z8vuukimI96+w7kX8X5zru+egouMG08AA')]
    [TestCase('155', '[claRed;0.2 claBlue;0.8],claWhite,Radial,True,1,1,0.98,2CNwf//jC5BBCbYnQMP8nu9vffLLi9ED2sr/330a9rtPuO0v2P/6/r1zUMEIvXcOPA+G5Nt66A')]
    [TestCase('156', '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Radial,True,1,1,0.98,1j8RvfvRL39B/6vXrwT5Gki/ffGw85AA4dXDkP0f0J8vuukimH96+w/kX8X5z7uuegotNH18AA')]
    [TestCase('157', '[claRed;0.2 claBlue;0.8],claWhite,Radial,True,1,0.5,0.98,1jsR/fvRM39B/6vXrwT5GUy/ffGw86AA4dW/kP0f0Z8vuukimI96+w7kX8X5zru+egouMG08AA')]
    [TestCase('158', '[claRed;0.2 claBlue;0.8],claWhite,Radial,False,2,1,0.98,pT9CuBee6+8+7qdb/0PrNayefgb4w4D/0aCnbz4fwhvguwfFHy8AByPQTrsDwYvv/vpNLD4B7g')]
    [TestCase('159', '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Radial,False,2,1,0.98,vUuC+QevIy/A8aRvD0XwZqxOPvvGi3A+3cCX0H0its//e/btOx+9/0EUXwL30EcfPAARVJ4/Aw')]
    [TestCase('160', '[claRed;0.2 claBlue;0.8],claWhite,Radial,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('161', '[claRed;0.2 claBlue;0.8],claWhite,Radial,True,0.5,1,0.98,68fgfflp995CDa+Pz3/1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('162', '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Radial,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('163', '[claRed;0.2 claBlue;0.8],claWhite,Radial,True,3,0.5,0.98,zm+CeP24kw5+7qxj34XwXCi+vvnSC3C839Mzkf4qoT9vffLaW0/+AUyYD8PywpP/fQMl4L29+g')]
    [TestCase('164', '[claRed;-1 claBlue;0.8],claWhite,Radial,True,1,1,0.98,oi9AQAmu+94+/NjfwET1lJfQfeev2wAE3/Pb0DgW+LfP9eASCI/8Btk7T78Jxk+9uAlkTDw88w')]
    [TestCase('165', '[claRed;0 claBlue;2],claWhite,Radial,True,1,1,0.98,yy8wvwHV5x/BBbADEEL/tsevffbQfzEE1MO3H78dAUS/uO8n+N97AsmD0MECqy+e/A6ERFy66w')]
    [TestCase('166', '[claRed;0 $7F000000;0.5 claBlue;1],claWhite,Radial,True,1,1,0.98,52tQwPnWe/9CCKnrz8IE85eff+uu96AC5Muf0Dsf1qN/N+s22K+4/dczsAT6wKOefAtX9Dy8+Q')]
    [TestCase('167', '[claRed;0 $7F000000;1 claBlue;0.5],claWhite,Radial,True,1,1,0.98,vluwAQHaV88AAdFPUMT7rWtA/uulF8EE3+GnAHob5SdAtugqGG85AMkLQIAErysuPA1cDAz88g')]
    [TestCase('168', '[claRed;0 $7F000000;-1 claBlue;0.5],claWhite,Radial,True,1,1,0.98,vmPwQALij1/AAtZ7sAT+j3swPe6sI2EE4t6T4Lka4zsAN+owIE+6A7QDoD8JvTfeuwt+kLt77A')]
    [TestCase('169', '[claRed;1 $7F000000;0.5 claBlue;0],claWhite,Radial,True,1,1,0.98,pUdAAAZ/by5/+qhrYEP3BuifQd+qQ3AB4er7AHkc8d8eNdb9Qx88CBbMjgP4sB+N+AQlYA69CA')]
    [TestCase('170', '[claRed;1 $7F000000;0 claBlue;0.5],claWhite,Radial,True,1,1,0.98,hKvBPQ2t50489N8LsIfy1SvgPPWfw0AC4tMnQfkk2MOvuOUGxF49CggIv4D9vavNeARcTEw7/A')]
    [TestCase('171', '[claRed;1 $7F000000;0 claBlue;0.5 claWhite;0.49],claWhite,Radial,True,1,1,0.98,ozuBOxLDl8+99NHXz8PqyifQvvHVo7E/4viPkH8g7jsvvOr6Q49ABA3wLwEJuvvdOgpL+Ly7/A')]
    [TestCase('172', '[claRed;1 $7F000000;0 claBlue;0.5 claWhite;0.49],claWhite,Radial,False,1,1,0.98,ozuBOxLDl8+99NHXz8PqyifQvvHVo7E/4viPkH8g7jsvvOr6Q49ABA3wLwEJuvvdOgpL+Ly7/A')]
    [TestCase('173', '[claRed;1 $7F000000;0 claBlue;0.5 claWhite;0.49],claWhite,Radial,True,2,0.5,0.98,yWdiuv+1w84A869/L8bwcbS+v/K4Z4D85+CLAT0mp8OvO/DpM0+9+E/4ngH/8GePvPz2Rw9BCQ')]
    [TestCase('174', '[],claWhite,Radial,True,1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('175', '[claRed;0.3],claWhite,Radial,True,1,1,0.98,yyfAvgHQ/x8BBqHHH4EAyoMf/vnbkxGEzrZX/wAgDORuOfEiNGC8A9K3oAL/mydOfQ+GnMy57A')]
    [TestCase('176', '[claRed;0.3],claBlue,Radial,True,1,1,0.98,p4vQvwuMI9/+AMrXf4Xz0gcgwOij95AE3b9nwTgh8EePdOUKsN+7AAoUr4L6xDv9dwpb8Ny6/g')]
    [TestCase('177', '[claRed;0.3],claWhite,Linear,True,1,1,0.98,yyfAvgHQ/x8BBqHHH4EAyoMf/vnbkxGEzrZX/wAgDORuOfEiNGC8A9K3oAL/mydOfQ+GnMy57A')]
    [TestCase('178', '[claRed;0.3],claBlue,Linear,True,1,1,0.98,p4vQvwuMI9/+AMrXf4Xz0gcgwOij95AE3b9nwTgh8EePdOUKsN+7AAoUr4L6xDv9dwpb8Ny6/g')]
    [TestCase('179', '[claRed;0 claBlue;1 claBlue;1],claWhite,Radial,False,1,1,0.98,zDOgvwHayx/BBcA/IAP+ogdAvfLEa1GE2c8bMP0a96cPeO4svE87AcBPEIEGuzfuew6D8Ov66g')]
    [TestCase('180', '[claRed;0 claRed;0 claBlue;1],claWhite,Radial,False,1,1,0.98,zDOgvwHayx/BBcA/IAP+ogdAvfLEa1GE2c8bMP0a96cPeO4svE87AcBPEIEGuzfuew6D8Ov66g')]
    [TestCase('181', '[claRed;0 claBlue;1 claBlue;1],claWhite,Linear,False,1,1,0.98,xB8wwAPJs49AB7wnAEP9wA8APvXMc+FE1skTYPkXDTBfNdnz1x5/DPgzX38JrM9MNwVTPG38+w')]
    [TestCase('182', '[claRed;0 claRed;0 claBlue;1],claWhite,Linear,False,1,1,0.98,xB8wwAPJs49AB7wnAEP9wA8APvXMc+FE1skTYPkXDTBfNdnz1x5/DPgzX38JrM9MNwVTPG38+w')]
    [TestCase('183', '[$00FF0000;0 $FF0000FF;1],claBlack,Linear,True,1,1,0.98,wbfB+v/Gz34A9qCHHwf0XPDuvvanbxD+5dLPAboj41ffdeMOaB+8+yggD4X2yJM9uAswlG37Bw')]
    [TestCase('184', '[$00FF0000;0 $FF0000FF;1],claBlack,Radial,True,1,1,0.98,wx8iOQLEL7/+8bKHnwbzQARvPPyVzzA/3c6DobsmtRvPO+sNfL/7AkEsjwTw4ceeNwUXUK28BQ')]
    [TestCase('185', '[$00FF0000;0 $FF0000FF;1],claBlack,Linear,True,1,0.5,0.98,ywfCOvzDk+7A9aCHn8b0eMx+fvOeY7A959mnobskx2/veOQSXA/7+UQ8L4fw1EvOeAgDCL78Dw')]
    [TestCase('186', '[$00FF0000;0 $FF0000FF;1],claBlack,Radial,True,1,0.5,0.98,zDviOf3Cww6A86mHT0b0atg+PfaVk0C+4teDgfwlsE/ve+gRaO+6/VBA70bt4eN+eAX26139Dg')]
    procedure TestFillRectGradient(const APoints, AModulateColor: string; AGradientStyle: TGradientStyle; ABlending: Boolean; const AScale, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  '[claBlack;0 claWhite;1],claWhite,Linear,False,1,1,0.98,k7vA+Qe6s18/55HnX8rvECwvQf7rv2B75uN3ogIPCEifPvbAMwD+ACeIgMAAy4O+PP5KGP67CA')]
    [TestCase('2',  '[$7F000000;0 $7FFFFFFF;1],claWhite,Linear,False,1,1,0.98,tIsx+v+9A19A7pmzL4jyU+yeP/fAi2B85+H7AUAa2WdP/e3rxw98/ETw/8T11sMO+wEPTP58EA')]
    [TestCase('3',  '[claBlack;0 claWhite;1],claWhite,Linear,False,1,0.5,0.98,tIcxOgC9B18A7pmzL4jyUuyeP/fAi2B85+H7AUAa2WdP/e3qxw98/ETw78T11sMOuwEQTP58EA')]
    [TestCase('4',  '[claBlack;0 claWhite;1],claWhite,Linear,True,1,1,0.98,k7vA+Qe6s18/55HnX8rvECwvQf7rv2B75uN3ogIPCEifPvbAMwD+ACeIgMAAy4O+PP5KGP67CA')]
    [TestCase('5',  '[$7F000000;0 $7FFFFFFF;1],claWhite,Linear,True,1,1,0.98,tIsx+v+9A19A7pmzL4jyU+yeP/fAi2B85+H7AUAa2WdP/e3rxw98/ETw/8T11sMO+wEPTP58EA')]
    [TestCase('6',  '[claBlack;0 claWhite;1],claWhite,Linear,True,1,0.5,0.98,tIcxOgC9B18A7pmzL4jyUuyeP/fAi2B85+H7AUAa2WdP/e3qxw98/ETw78T11sMOuwEQTP58EA')]
    [TestCase('7',  '[claBlack;0 claWhite;1],claWhite,Linear,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCD0Xob32uheAHWy55A/kaE7jsCIOyOfgcPwD4AAw')]
    [TestCase('8',  '[$7F000000;0 $7FFFFFFF;1],claWhite,Linear,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDEXwgxXdQevP2E089+1PsrkL2APTuewbyox6/DQ')]
    [TestCase('9',  '[claBlack;0 claWhite;1],claWhite,Linear,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('10', '[claBlack;0 claWhite;1],claWhite,Linear,True,0.5,1,0.98,6nMS//aRp12EAduv33/1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('11', '[$7F000000;0 $7FFFFFFF;1],claWhite,Linear,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('12', '[claBlack;0 claWhite;1],claWhite,Linear,True,3,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDEX4jlb8vvewCdI869mlkr8XpAcjPuwPjQz6+HA')]
    [TestCase('13', '[claBlack;0.2 claWhite;0.8],claWhite,Linear,False,1,1,0.98,nMcA+gW4ny+/6I3H30rzGThPAP3kr7D75NWjooMODVAPP/m3L6B+/iakgAABx4NuPf1LOI77CA')]
    [TestCase('14', '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Linear,False,1,1,0.98,uY9R+v68+04A75enb0j0V/Auv/a9g4C85toTcgAa3GuPPe/mw188+0P8/8T108NuewEQXM58EA')]
    [TestCase('15', '[claBlack;0.2 claWhite;0.8],claWhite,Linear,False,1,0.5,0.98,uI9ROv+8+z4A75enf0j0V/Auv/a9g4C85tsTggAa3GuPfe/mw198+0MA4AT208NuewEQXM58EA')]
    [TestCase('16', '[claBlack;0.2 claWhite;0.8],claWhite,Linear,True,1,1,0.98,nMcA+gW4ny+/6I3H30rzGThPAP3kr7D75NWjooMODVAPP/m3L6B+/iakgAABx4NuPf1LOI77CA')]
    [TestCase('17', '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Linear,True,1,1,0.98,uY9R+v68+04A75enb0j0V/Auv/a9g4C85toTcgAa3GuPPe/mw188+0P8/8T108NuewEQXM58EA')]
    [TestCase('18', '[claBlack;0.2 claWhite;0.8],claWhite,Linear,True,1,0.5,0.98,uI9ROv+8+z4A75enf0j0V/Auv/a9g4C85tsTggAa3GuPfe/mw198+0MA4AT208NuewEQXM58EA')]
    [TestCase('19', '[claBlack;0.2 claWhite;0.8],claWhite,Linear,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCD8Tkd42PBNwHZzw7A/lJQTjwAILi+fQcSdJ7/Bg')]
    [TestCase('20', '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Linear,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDoXshx3PgefP3Ew+9+1nU3kL1ANyOewb0e96+Dw')]
    [TestCase('21', '[claBlack;0 claWhite;1],claWhite,Radial,False,1,1,0.98,oAOxuQe4Z3/+5Jivb8ruFRAfAf76i4A76eZvkn0Q51fvO+jUV2B8BzJ0f0H15l+eeQBZXK28Cg')]
    [TestCase('22', '[$7F000000;0 $7FFFFFFF;1],claWhite,Radial,False,1,1,0.98,u68x+v+83+4/7ZyXPwjyVtyeP/fHc3C86OP3cf0ayW//u+b11z/7/0lkb8Xv47N++QIX8F19EQ')]
    [TestCase('23', '[claBlack;0 claWhite;1],claWhite,Radial,False,1,0.5,0.98,u6sx+v+83+4/7ZyXPwjyVeCeP/fIc3C86OP3cf0ayW//++b01z/7/0lkbwXw469++QIX7F19EQ')]
    [TestCase('24', '[claBlack;0 claWhite;1],claWhite,Radial,True,1,1,0.98,oAOxuQe4Z3/+5Jivb8ruFRAfAf76i4A76eZvkn0Q51fvO+jUV2B8BzJ0f0H15l+eeQBZXK28Cg')]
    [TestCase('25', '[$7F000000;0 $7FFFFFFF;1],claWhite,Radial,True,1,1,0.98,u68x+v+83+4/7ZyXPwjyVtyeP/fHc3C86OP3cf0ayW//u+b11z/7/0lkb8Xv47N++QIX8F19EQ')]
    [TestCase('26', '[claBlack;0 claWhite;1],claWhite,Radial,True,1,0.5,0.98,u6sx+v+83+4/7ZyXPwjyVeCeP/fIc3C86OP3cf0ayW//++b01z/7/0lkbwXw469++QIX7F19EQ')]
    [TestCase('27', '[claBlack;0 claWhite;1],claWhite,Radial,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDcfoe4RPRtwDGC6+A+l2Y/nsBLhSfPgj4/94AAA')]
    [TestCase('28', '[$7F000000;0 $7FFFFFFF;1],claWhite,Radial,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCD4TsixkvwOfPuM199+V/4vgL2Bwj/+wbnv24/DA')]
    [TestCase('29', '[claBlack;0 claWhite;1],claWhite,Radial,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('30', '[claBlack;0 claWhite;1],claWhite,Radial,True,0.5,1,0.98,6HtCP/aYk71E/Man30H1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('31', '[$7F000000;0 $7FFFFFFF;1],claWhite,Radial,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('32', '[claBlack;0 claWhite;1],claWhite,Radial,True,3,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDET4jlb8vvewCeH/69Whgn8XpAMy/ewPlQx5+HA')]
    [TestCase('33', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,False,1,1,0.98,pBux+QW6Yw9+5J2bLwvxHAj/AP3za5D76NqP0vwQ6k+/u+XWW6D8CDBYjwH07GseuQBgZL08Cg')]
    [TestCase('34', '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Radial,False,1,1,0.98,vbsx+v69374/7Z+Pn0jzWdh+v/bEY3C86N0LEj0bymfve+X22197AEhYf0Xv5rc+OQMa9G09EQ')]
    [TestCase('35', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,False,1,0.5,0.98,vbcxOv+9367/7J+PnwjzWNh+v/bEY3C86N0LEj0bymfvu+X22197AEhYf0Xv5rc+OQMb9G09EQ')]
    [TestCase('36', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,True,1,1,0.98,pBux+QW6Yw9+5J2bLwvxHAj/AP3za5D76NqP0vwQ6k+/u+XWW6D8CDBYjwH07GseuQBgZL08Cg')]
    [TestCase('37', '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Radial,True,1,1,0.98,vbsx+v69374/7Z+Pn0jzWdh+v/bEY3C86N0LEj0bymfve+X22197AEhYf0Xv5rc+OQMa9G09EQ')]
    [TestCase('38', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,True,1,0.5,0.98,vbcxOv+9367/7J+PnwjzWNh+v/bEY3C86N0LEj0bymfvu+X22197AEhYf0Xv5rc+OQMb9G09EQ')]
    [TestCase('39', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDEXoe4Qch+AHHIy+A+mBgPjz/NdAOvgnwt54AAQ')]
    [TestCase('40', '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Radial,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDsfshxkMQuvPuPx99+WDc3sL0Cuiuuwfjn06/DA')]
    [TestCase('41', '[claRed;0 claBlue;1],claBlack,Linear,False,1,1,0.98,nr9g+QHFX9+97o9/X0ryGRA/PwDUY9B85LqHQnsUyzuf+ersN8A4CClMz4H04yue+f8+CN19BQ')]
    [TestCase('42', '[$7FFF0000;0 $7F0000FF;1],claBlack,Linear,False,1,1,0.98,uo8BOv3D257/8Zh/L8jzV9yuPvi1XxB95s0DUvwcu1/fOugBxG85AEVQn4Xv4pd+uQIKxO39Dg')]
    [TestCase('43', '[claRed;0 claBlue;1],claBlack,Linear,False,1,0.5,0.98,uYsBOv3D257/8Zd/P8jzV+CuPvi1XxB95s0HQvwcu1/fOugBxG85AERQn4Xv4pd+uQIKxO29Dg')]
    [TestCase('44', '[claRed;0 claBlue;1],claBlack,Linear,True,1,1,0.98,nr9g+QHFX9+97o9/X0ryGRA/PwDUY9B85LqHQnsUyzuf+ersN8A4CClMz4H04yue+f8+CN19BQ')]
    [TestCase('45', '[$7FFF0000;0 $7F0000FF;1],claBlack,Linear,True,1,1,0.98,uo8BOv3D257/8Zh/L8jzV9yuPvi1XxB95s0DUvwcu1/fOugBxG85AEVQn4Xv4pd+uQIKxO39Dg')]
    [TestCase('46', '[claRed;0 claBlue;1],claBlack,Linear,True,1,0.5,0.98,uYsBOv3D257/8Zd/P8jzV+CuPvi1XxB95s0HQvwcu1/fOugBxG85AERQn4Xv4pd+uQIKxO29Dg')]
    [TestCase('47', '[claRed;0 claBlue;1],claBlack,Linear,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDAfoc4gshOAHLOx8A+1+ATjz//ecOvAM0LC69CQ')]
    [TestCase('48', '[$7FFF0000;0 $7F0000FF;1],claBlack,Linear,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDsTshxkcQOvPwSw+9+WDs3sL07vOuugQEVJ79EA')]
    [TestCase('49', '[claRed;0 claBlue;1],claBlack,Linear,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('50', '[claRed;0 claBlue;1],claBlack,Linear,True,0.5,1,0.98,2I+i/fZtn82DA8BDr0H1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('51', '[$7FFF0000;0 $7F0000FF;1],claBlack,Linear,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('52', '[claRed;0 claBlue;1],claBlack,Linear,True,3,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDET4jlb8vvewCeH/69Whkn8XpAMy/ewPlQx5+HA')]
    [TestCase('53', '[claRed;0.2 claBlue;0.8],claBlack,Linear,False,1,1,0.98,nr9g+QHFX9+97o9/X0ryGRA/PwDUY9B85LqHQnsUyzuf+ersN8A4CClMz4H04yue+f8+CN19BQ')]
    [TestCase('54', '[$7FFF0000;0.2 $7F0000FF;0.8],claBlack,Linear,False,1,1,0.98,uo8BOv3D257/8Zh/L8jzV9yuPvi1XxB95s0DUvwcu1/fOugBxG85AEVQn4Xv4pd+uQIKxO39Dg')]
    [TestCase('55', '[claRed;0.2 claBlue;0.8],claBlack,Linear,False,1,0.5,0.98,uYsBOv3D257/8Zd/P8jzV+CuPvi1XxB95s0HQvwcu1/fOugBxG85AERQn4Xv4pd+uQIKxO29Dg')]
    [TestCase('56', '[claRed;0.2 claBlue;0.8],claBlack,Linear,True,1,1,0.98,nr9g+QHFX9+97o9/X0ryGRA/PwDUY9B85LqHQnsUyzuf+ersN8A4CClMz4H04yue+f8+CN19BQ')]
    [TestCase('57', '[$7FFF0000;0.2 $7F0000FF;0.8],claBlack,Linear,True,1,1,0.98,uo8BOv3D257/8Zh/L8jzV9yuPvi1XxB95s0DUvwcu1/fOugBxG85AEVQn4Xv4pd+uQIKxO39Dg')]
    [TestCase('58', '[claRed;0.2 claBlue;0.8],claBlack,Linear,True,1,0.5,0.98,uYsBOv3D257/8Zd/P8jzV+CuPvi1XxB95s0HQvwcu1/fOugBxG85AERQn4Xv4pd+uQIKxO29Dg')]
    [TestCase('59', '[claRed;0.2 claBlue;0.8],claBlack,Linear,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDAfoc4gshOAHLOx8A+1+ATjz//ecOvAM0LC69CQ')]
    [TestCase('60', '[$7FFF0000;0.2 $7F0000FF;0.8],claBlack,Linear,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDsTshxkcQOvPwSw+9+WDs3sL07vOuugQEVJ79EA')]
    [TestCase('61', '[claRed;0 claBlue;1],claBlack,Radial,False,1,1,0.98,nr9g+QHFX9+97o9/X0ryGRA/PwDUY9B85LqHQnsUyzuf+ersN8A4CClMz4H04yue+f8+CN19BQ')]
    [TestCase('62', '[$7FFF0000;0 $7F0000FF;1],claBlack,Radial,False,1,1,0.98,uo8BOv3D257/8Zh/L8jzV9yuPvi1XxB95s0DUvwcu1/fOugBxG85AEVQn4Xv4pd+uQIKxO39Dg')]
    [TestCase('63', '[claRed;0 claBlue;1],claBlack,Radial,False,1,0.5,0.98,uYsBOv3D257/8Zd/P8jzV+CuPvi1XxB95s0HQvwcu1/fOugBxG85AERQn4Xv4pd+uQIKxO29Dg')]
    [TestCase('64', '[claRed;0 claBlue;1],claBlack,Radial,True,1,1,0.98,nr9g+QHFX9+97o9/X0ryGRA/PwDUY9B85LqHQnsUyzuf+ersN8A4CClMz4H04yue+f8+CN19BQ')]
    [TestCase('65', '[$7FFF0000;0 $7F0000FF;1],claBlack,Radial,True,1,1,0.98,uo8BOv3D257/8Zh/L8jzV9yuPvi1XxB95s0DUvwcu1/fOugBxG85AEVQn4Xv4pd+uQIKxO39Dg')]
    [TestCase('66', '[claRed;0 claBlue;1],claBlack,Radial,True,1,0.5,0.98,uYsBOv3D257/8Zd/P8jzV+CuPvi1XxB95s0HQvwcu1/fOugBxG85AERQn4Xv4pd+uQIKxO29Dg')]
    [TestCase('67', '[claRed;0 claBlue;1],claBlack,Radial,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDAfoc4gshOAHLOx8A+1+ATjz//ecOvAM0LC69CQ')]
    [TestCase('68', '[$7FFF0000;0 $7F0000FF;1],claBlack,Radial,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDsTshxkcQOvPwSw+9+WDs3sL07vOuugQEVJ79EA')]
    [TestCase('69', '[claRed;0 claBlue;1],claBlack,Radial,False,-2,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('70', '[claRed;0 claBlue;1],claBlack,Radial,True,0.5,1,0.98,2I+i/fZtn82DA8BDr0H1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('71', '[$7FFF0000;0 $7F0000FF;1],claBlack,Radial,True,-1,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('72', '[claRed;0 claBlue;1],claBlack,Radial,True,3,0.5,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDET4jlb8vvewCeH/69Whkn8XpAMy/ewPlQx5+HA')]
    [TestCase('73', '[claRed;0.2 claBlue;0.8],claBlack,Radial,False,1,1,0.98,nr9g+QHFX9+97o9/X0ryGRA/PwDUY9B85LqHQnsUyzuf+ersN8A4CClMz4H04yue+f8+CN19BQ')]
    [TestCase('74', '[$7FFF0000;0.2 $7F0000FF;0.8],claBlack,Radial,False,1,1,0.98,uo8BOv3D257/8Zh/L8jzV9yuPvi1XxB95s0DUvwcu1/fOugBxG85AEVQn4Xv4pd+uQIKxO39Dg')]
    [TestCase('75', '[claRed;0.2 claBlue;0.8],claBlack,Radial,False,1,0.5,0.98,uYsBOv3D257/8Zd/P8jzV+CuPvi1XxB95s0HQvwcu1/fOugBxG85AERQn4Xv4pd+uQIKxO29Dg')]
    [TestCase('76', '[claRed;0.2 claBlue;0.8],claBlack,Radial,True,1,1,0.98,nr9g+QHFX9+97o9/X0ryGRA/PwDUY9B85LqHQnsUyzuf+ersN8A4CClMz4H04yue+f8+CN19BQ')]
    [TestCase('77', '[$7FFF0000;0.2 $7F0000FF;0.8],claBlack,Radial,True,1,1,0.98,uo8BOv3D257/8Zh/L8jzV9yuPvi1XxB95s0DUvwcu1/fOugBxG85AEVQn4Xv4pd+uQIKxO39Dg')]
    [TestCase('78', '[claRed;0.2 claBlue;0.8],claBlack,Radial,True,1,0.5,0.98,uYsBOv3D257/8Zd/P8jzV+CuPvi1XxB95s0HQvwcu1/fOugBxG85AERQn4Xv4pd+uQIKxO29Dg')]
    [TestCase('79', '[claRed;0.2 claBlue;0.8],claBlack,Radial,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDAfoc4gshOAHLOx8A+1+ATjz//ecOvAM0LC69CQ')]
    [TestCase('80', '[$7FFF0000;0.2 $7F0000FF;0.8],claBlack,Radial,False,2,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDsTshxkcQOvPwSw+9+WDs3sL07vOuugQEVJ79EA')]
    procedure TestFillRectGradient2(const APoints, AModulateColor: string; AGradientStyle: TGradientStyle; ABlending: Boolean; const AScale, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,False,0.9,1,0.98,skPPOvy/G28DCjHDXn33k6D+/fSqO6C95dxDkf/vKzwevvViGJIt9pFfb8nq3QtfOQXYe+69Fw')]
    [TestCase('2',  '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,False,0.9,1,0.98,skPPOvy/G28DCjHDXn33k6D+/fSqO6C95dxDkf/vKzwevvViGJIt9pFfb8nq3QtfOQXYe+69Fw')]
    [TestCase('3',  '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,False,0.9,1,0.98,skPPOvy/G28DCjHDXn33k6D+/fSqO6C95dxDkf/vKzwevvViGJIt9pFfb8nq3QtfOQXYe+69Fw')]
    [TestCase('4',  '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,False,0.9,1,0.98,skPPOvy/G28DCjHDXn33k6D+/fSqO6C95dxDkf/vKzwevvViGJIt9pFfb8nq3QtfOQXYe+69Fw')]
    [TestCase('5',  '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,False,0.9,0.5,0.98,xMvAOvrAu27C/2gjr0H2lKQOfvKgS4D95t5jcX4K698eve08uMAz9/hbb8nq3wdfeQXXf/79Fw')]
    [TestCase('6',  '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,False,0.9,0.5,0.98,xMvAOvrAu27C/2gjr0H2lKQOfvKgS4D95t5jcX4K698eve08uMAz9/hbb8nq3wdfeQXXf/79Fw')]
    [TestCase('7',  '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,False,0.9,0.5,0.98,xMvAOvrAu27C/2gjr0H2lKQOfvKgS4D95t5jcX4K698eve08uMAz9/hbb8nq3wdfeQXXf/79Fw')]
    [TestCase('8',  '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,False,0.9,0.5,0.98,xMvAOvrAu27C/2gjr0H2lKQOfvKgS4D95t5jcX4K698eve08uMAz9/hbb8nq3wdfeQXXf/79Fw')]
    [TestCase('9',  '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,False,0.9,1,0.98,xG/fOf7fZ48CBe2O3nr3k6D+/fSqO6C95dxDEUL4DsRtPvxoLJIw8ZRfb8nq3QtfOQXYe+69Fw')]
    [TestCase('10', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,False,0.9,1,0.98,xG/fOf7fZ48CBe2O3nr3k6D+/fSqO6C95dxDEUL4DsRtPvxoLJIw8ZRfb8nq3QtfOQXYe+69Fw')]
    [TestCase('11', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,False,0.9,1,0.98,xG/fOf7fZ48CBe2O3nr3k6D+/fSqO6C95dxDEUL4DsRtPvxoLJIw8ZRfb8nq3QtfOQXYe+69Fw')]
    [TestCase('12', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,False,0.9,1,0.98,xG/fOf7fZ48CBe2O3nr3k6D+/fSqO6C95dxDEUL4DsRtPvxoLJIw8ZRfb8nq3QtfOQXYe+69Fw')]
    [TestCase('13', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,False,0.9,0.5,0.98,zeNAOvvQ3/5B/UYHb0D2lKQOfvKgS4D95t5jsf8O3KM+/fA/wEC19Ppbb8nq3wdfeQXXf/79Fw')]
    [TestCase('14', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,False,0.9,0.5,0.98,zeNAOvvQ3/5B/UYHb0D2lKQOfvKgS4D95t5jsf8O3KM+/fA/wEC19Ppbb8nq3wdfeQXXf/79Fw')]
    [TestCase('15', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,False,0.9,0.5,0.98,zeNAOvvQ3/5B/UYHb0D2lKQOfvKgS4D95t5jsf8O3KM+/fA/wEC19Ppbb8nq3wdfeQXXf/79Fw')]
    [TestCase('16', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,False,0.9,0.5,0.98,zeNAOvvQ3/5B/UYHb0D2lKQOfvKgS4D95t5jsf8O3KM+/fA/wEC19Ppbb8nq3wdfeQXXf/79Fw')]
    [TestCase('17', '[claRed;0 claBlue;1],claBlack,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,False,0.9,0.5,0.98,yM8Qu/y5j/6B/XUXj4T1kaTOvfGcV2C95t13AXwK0QffOuY5xDC19yNYb8nq3wdfeQXWf/79Fw')]
    [TestCase('18', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,0,False,False,False,0.9,1,0.98,wwuh/gTJ3/+99OIzkYPwhh/A+vKWo2GGtp8Pz78cH7T/uuw5CLC8/+0zL0H+60NetQ9c6Fv66Q')]
    [TestCase('19', '[claRed;0 claNull;0.5 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,0,False,False,False,0.9,1,0.98,3H8RffWx024A84s70ATxA5iPAeaTw1BByMgz8D0g/V9gOORT1K/49A5AD8X54aMu9REKBK37/A')]
    [TestCase('20', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,True,0.9,1,0.98,zDcu//MJ1A/+IIgD7/j4jmsA/wMqGOBD8f0HL0DvDYSvwAPuK1F0721PXzgH+fOfgvxDOD/A8Q')]
    [TestCase('21', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,True,0.9,1,0.98,zDcu//MJ1A/+IIgD7/j4jmsA/wMqGOBD8f0HL0DvDYSvwAPuK1F0721PXzgH+fOfgvxDOD/A8Q')]
    [TestCase('22', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,True,0.9,1,0.98,zDcu//MJ1A/+IIgD7/j4jmsA/wMqGOBD8f0HL0DvDYSvwAPuK1F0721PXzgH+fOfgvxDOD/A8Q')]
    [TestCase('23', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,True,0.9,1,0.98,zDcu//MJ1A/+IIgD7/j4jmsA/wMqGOBD8f0HL0DvDYSvwAPuK1F0721PXzgH+fOfgvxDOD/A8Q')]
    [TestCase('24', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,True,0.9,0.5,0.98,zS9OAPoEPEB+GK2Hb7r3i2cgvwInFNDD8v/7zj7oFMgfgAX0E/G495dPXzgH+vOPgvxCOD/A8Q')]
    [TestCase('25', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,True,0.9,0.5,0.98,zS9OAPoEPEB+GK2Hb7r3i2cgvwInFNDD8v/7zj7oFMgfgAX0E/G495dPXzgH+vOPgvxCOD/A8Q')]
    [TestCase('26', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,True,0.9,0.5,0.98,zS9OAPoEPEB+GK2Hb7r3i2cgvwInFNDD8v/7zj7oFMgfgAX0E/G495dPXzgH+vOPgvxCOD/A8Q')]
    [TestCase('27', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,True,0.9,0.5,0.98,zS9OAPoEPEB+GK2Hb7r3i2cgvwInFNDD8v/7zj7oFMgfgAX0E/G495dPXzgH+vOPgvxCOD/A8Q')]
    [TestCase('28', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,True,0.9,1,0.98,3mc+/vUpJBD9G0TPbvb4jmsA/wMqGOBD8f0HrwL48A//QArzP2E36nBPXzgH+fOfgvxDOD/A8Q')]
    [TestCase('29', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,True,0.9,1,0.98,3mc+/vUpJBD9G0TPbvb4jmsA/wMqGOBD8f0HrwL48A//QArzP2E36nBPXzgH+fOfgvxDOD/A8Q')]
    [TestCase('30', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,True,0.9,1,0.98,3mc+/vUpJBD9G0TPbvb4jmsA/wMqGOBD8f0HrwL48A//QArzP2E36nBPXzgH+fOfgvxDOD/A8Q')]
    [TestCase('31', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,True,0.9,1,0.98,3mc+/vUpJBD9G0TPbvb4jmsA/wMqGOBD8f0HrwL48A//QArzP2E36nBPXzgH+fOfgvxDOD/A8Q')]
    [TestCase('32', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,True,0.9,0.5,0.98,10feP/sUZNC9FYtrL7n3i2cgvwInFNDD8v/7DoDsBYxPwAj3H3E69ZlPXzgH+vOPgvxCOD/A8Q')]
    [TestCase('33', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,True,0.9,0.5,0.98,10feP/sUZNC9FYtrL7n3i2cgvwInFNDD8v/7DoDsBYxPwAj3H3E69ZlPXzgH+vOPgvxCOD/A8Q')]
    [TestCase('34', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,True,0.9,0.5,0.98,10feP/sUZNC9FYtrL7n3i2cgvwInFNDD8v/7DoDsBYxPwAj3H3E69ZlPXzgH+vOPgvxCOD/A8Q')]
    [TestCase('35', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,True,0.9,0.5,0.98,10feP/sUZNC9FYtrL7n3i2cgvwInFNDD8v/7DoDsBYxPwAj3H3E69ZlPXzgH+vOPgvxCOD/A8Q')]
    [TestCase('36', '[claRed;0 claBlue;1],claBlack,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,True,0.9,0.5,0.98,0jOugPz+E9A9Frp7P/32h2Pg/gEjJLCD8v4LT/zn+u/fPf7xH2E6+MNPX3gH+vOPgvxCOD/A8Q')]
    [TestCase('37', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,0,False,False,True,0.9,1,0.98,2ZNegPP7W9B+EqWHIDwCjddPewIQlJ8E58lbPvzeExRwfQccDEF4/6Yj//gFBdzPwflQZH4/4w')]
    [TestCase('38', '[claRed;0 claNull;0.5 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,0,False,False,True,0.9,1,0.98,+IcOAPoAjHB+DrwH8Pr8jP8fPv4arG/D7e8DnzzdDPRvfgf8R6F5+7Ifv/gB/hNgwftItO7+5w')]
    procedure TestFillRectGradientRadial(const APoints, AModulateColor: string; AGradientScaleX, AGradientScaleY, AGradientPosX, AGradientPosY, AGradientRotationDegree, AGradientRotationCenterX, AGradientRotationCenterY, AGradientSkewX, AGradientSkewY, ADestLeftPercent, ADestTopPercent, ADestRightPercent, ADestBottomPercent, ACanvasScaleX, ACanvasScaleY, ACanvasOffsetX, ACanvasOffsetY, ACanvasRotationDeg: Single; AApplyClip, ABlending, ADrawBackgroundChess: Boolean; const ABitmapScale, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  '[claRed;0 claBlue;1],claWhite,0,0,1,1,0,0,1,1,1,1,0,0,0,False,True,1,1,False,0.98,0uuef+3yL7B/GGa37zjrnzew/Pfx1z+F2wWUzvrnDSz/Pfz0T+G2AaUbPjgI4TNfvftE2O0B7w')]
    [TestCase('2',  '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0,1,1,1,1,0,0,0,False,True,1,1,False,0.98,dc+dwvvGS+/+EFRbsPrmjluAffbt0y8F0/5TvrnjFWSfffv1S8F0/5QHr7oAwrePwP5gEE0+3w')]
    [TestCase('3',  '[claRed;0 claBlue;1],claWhite,1,0,0,1,0,0,1,1,1,1,0,0,0,False,True,1,1,False,0.98,sU/+AfHbuw8/F2sr8Pr1rjPg+vsJ2K/G2PFfvjjeM2iP/PTqN0H3CMbHPvkHxG/fPvxcsL2A6w')]
    [TestCase('4',  '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0,0,1,1,1,1,0,0,0,False,True,1,1,False,0.98,5CNP/+v+U/B/GmSXX/jroy+APPjx1z9F3QekDrvoCyAPPvz0T1E3Aqnb3fcJ6Q+fPPo7AH7C8Q')]
    [TestCase('5',  '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0,0,1,1,1,1,0,0,0,False,True,1,1,False,0.98,Y5ftAv26J78+D1Z7QDvmimOwPfbs0x9F0fxDfvniF3CPffv1S1H0/pBLD/v+u9dPQQBp7Mx93A')]
    [TestCase('6',  '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0,1,1,1,1,0,0,0,False,True,1,1,False,0.98,tktewu/bu+8+GHI7YHv6tSdAev0T4F8H2eZTLjjcQIQv/PHkL6E3DNPbHrkJvm/ffvtgyP2A7Q')]
    [TestCase('7',  '[claRed;0 claBlue;1],claWhite,0,0,1,1,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,False,0.98,pXNhuw2/z49/+bHX30TwzBvQu/aie4FD09ZvAL0gC4wPeOUjDB//BwAEzz8JtevctQ5khKx78g')]
    [TestCase('8',  '[claRed;0 claBlue;1],claWhite,1,1,0,0,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,False,0.98,eoPh+xSUK1898tR/oEbyvM8/u/WrU6GC0d1rYHwcDJjP9+kvZM+9Bf/fPwEJt/M9+BFzNGx56A')]
    [TestCase('9',  '[claRed;0 claBlue;1],claWhite,1,0,0,1,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,False,0.98,kWdRfBCsg68++sMfMMbyzLO/u/2yv8GCzcULEPocALx/NuIm3A7/Cg28n0AKsHu99g1rmJy67w')]
    [TestCase('10', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,False,0.98,rWcxewzF7+8/+6e3j8Tvzyvw+/agh7HD09RzIL0hC4gfeOQh+G5/CADgfv8Itr989Q1ihAz88w')]
    [TestCase('11', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,False,0.98,cosRPBaNBw+98N6f8MbyubsPe/WtS4EC0d9nMHwbDZy/N+sydH89Bf4DkEEJth+euBJ0OAy55g')]
    [TestCase('12', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,False,0.98,kluRPBCth7/++8MbYAbz0JPv+/+526GCy73fHzkc/M8PNuEltC5/DBDgn8AKrYOd9gxrsKy68A')]
    [TestCase('13', '[claRed;0 claBlue;1],claWhite,0,0,1,1,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,False,0.98,pv9PffYEdG+CCXDnzv7m8pe/vfrGM1BB5uFjkLwMFliu9+QlYCF4/nZj7gEFtoct9wY2+J2A3w')]
    [TestCase('14', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,False,0.98,lmffvvTK//7+AqvXP4Po53MPffnWJ9CB4M0fsHsGLLB+9+IjiFG2AHav30ILjUPOeguEqB2/2w')]
    [TestCase('15', '[claRed;0 claBlue;1],claWhite,1,0,0,1,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,False,0.98,nm/vfvPeM48ACZx/f0Lq6XuvPP7ki1DD3Lfvz/oFKbx+9twUJKG3BIh3v8IKjwNOuQdq+B0A4A')]
    [TestCase('16', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,False,0.98,rBfwPPYTlO8CC2O//n3m9Jvf/frCNzCB5+VzwPwNEUS+N+UlWJH4/XYnngEEv1eddgYsCN5A4A')]
    [TestCase('17', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,False,0.98,kE8v//S8336+Abj/D8To5Gvf/PjaI/BB38kTgDsFMcRud+IjkPE1AXbvL0MMhHdO+wuOmO3+2g')]
    [TestCase('18', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,False,0.98,nVNPf/LaL28ACqKPH0Pr62uvfADnq5DD2ay/D3oELNj+NdoQAIF3Boyn38ILhw9++QZwCC5A4Q')]
    [TestCase('19', '[claRed;0 claBlue;1],claWhite,0,0,1,1,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,xZ9Q/Pm98+6BBWlDPwH3iaTufPWja6D95MxDcfwB8wsv++kwsFD2//prT0nr3BNP+QTfY879Fg')]
    [TestCase('20', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,t2dgPf2xqx4BA2d/j0H3iqTuvPWja6A95cw/oX0G5Qef/O07yLD0/O1rTwnr2hNfOQXgY879Fg')]
    [TestCase('21', '[claRed;0 claBlue;1],claWhite,1,0,0,1,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,w4fAfPu6264BBWFDH8H2jqAuvfamZ9B948snob0E7u9O/Os1xID1/vdvT8nq2RNvOQXgZ779Fg')]
    [TestCase('22', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,yKcQPPm/Aw8CBmozLwH3iaTefPWja6D95MxDMfwA9gvvOukuqLC2AP1rT0nr3BNP+QTfY879Fg')]
    [TestCase('23', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,tV+Q/f2vm+6AAmePn0H3iqTuvPWia5A95cs/4T0H4gfvvO490GA0/OprT8nq2hNfOQXgY879Fg')]
    [TestCase('24', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,xoewPPu7375BBV0z/4D2kJxOPfenY+C94ssb8f0E7+du/Os0yIA1//lvT4nq2BN/OQXgZ779Fg')]
    [TestCase('25', '[claRed;0 claBlue;1],claWhite,0,0,1,1,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,ydeQ+vjF106C/2gz/8H1kKDuffGdV1B9591nsTwB7dOuuuo4oPB1+w1Yb8nq3wdfeQXXf/79Fw')]
    [TestCase('26', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,w4fg+/e3o+6A/G5rf8P1kKDuffGdV0C9591nEX4J5ON+vO4/vHB09vpbb8nq3wdfeQXXf/79Fw')]
    [TestCase('27', '[claRed;0 claBlue;1],claWhite,1,0,0,1,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,x7fgevnAv94B/mI3H0L2lKT+ffKfS4D95t5jIX4G7sP+e+w6qGB1+QhYb8nq3wdfeQXXf/79Fw')]
    [TestCase('28', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,yedwuvnJ354CAGYnv8H1kKDuffGdV1B9591nYbz/788++uk3nDB2/BFYb8nq3wdfeQXXf/79Fw')]
    [TestCase('29', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,wnsAPPezn54A/HB3z8P1kaDuffGcV0C9591nYT4L4uPufO9AwCB09fVbb8nq3wdfeQXXf/79Fw')]
    [TestCase('30', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,ybewuvnBv+7B/V8rv0H2lKQOfvKgS4D95t5jgf4G8LsuPOw5qIC1+QpYb8nq3wdfeQXXf/79Fw')]
    [TestCase('31', '[claRed;0 claBlue;1],claWhite,0,0,1,1,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,ysugOvjF1y7CAGc//8H3iqieffOgZ0D959VbITwB9RffOuoxpHB2APxrT0nr3BNP+QTfY879Fg')]
    [TestCase('32', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,xYuwO/i7px4B/mdf/wL4jKS+vfOgY0C959RPsX0G5gefvO49zKA0/e9rT8nq2hNfOQXgZ879Fg')]
    [TestCase('33', '[claRed;0 claBlue;1],claWhite,1,0,0,1,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,yKvw+vjBv74B/2A7D4L4kKTevfSiW2A959RHwf0E7vNO/Ow3wGA1//lvT4nq2BNvOQXgZ779Fg')]
    [TestCase('34', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,y9dgevjI325CAWY3v8H3iaiePfOgZ0D959VbwTsA+Bt/OukvnNA2Af5rP0nr3RNP+QTfX979Fg')]
    [TestCase('35', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,xH/gO/i5n96A/WdnL0P4jaTO/fOgY0C959RPEb4H4gP/fO8/1DB0/O1rT8nq2RNvOQXgZ879Fg')]
    [TestCase('36', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,yK/gevnBu77B/lwz38H4kqTu/fSjV2D95tRDEX4F7ueOPO02xFA1//pvT4nq1xN/OQXha779Fg')]
    [TestCase('37', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,0.5,False,0.98,yK/gevnBu77B/lwz38H4kqTu/fSjV2D95tRDEX4F7ueOPO02xFA1//pvT4nq1xN/OQXha779Fg')]
    [TestCase('38', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,1,False,0.98,uwcfe/rDHy8CCBnnvj38jqDO/fmwU3B95cgH0f7lMkwOvfRXNKIwBpWDP0nqziOf+QTsT279FQ')]
    [TestCase('39', '[claRed;0 claBlue;1],claGreen,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,1,False,0.98,wTtPOwG2v64CCEKzbgL6gqw+/fani5C95MtfgfnsBOz+uOdWQLJx/9mLP4nr1ycvuQTkS559FQ')]
    [TestCase('40', '[claRed;0 claBlue;1],claGreen,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,0.5,False,0.98,y8fwuvy7iw7C/nEbP8T3i6yeffOec3B95tVvYTsJ1zd/uuY2zNC1+xxwTwnr2xdPOQXdZ879Fg')]
    [TestCase('41', '[claRed;0 claBlue;1],claWhite,1.15,-0.15,-0.15,1.15,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,1,False,0.98,uwNfu/nBIw+CCCP/Tr77iKCevfiuX1A95skXwT3kMHBO/PNYKNKwBZGDP8nq0iN/uQTrS369FQ')]
    [TestCase('42', '[claRed;0 claBlue;1],claWhite,1.15,-0.15,-0.15,1.15,0,-0.15,1.15,0.85,1,1.3,0,-20,30,True,True,0.9,1,False,0.98,qJtuP/+sN3+BEy07/7z4gpgOPPyzdyB+4LfnoPzgN5Bu+/FVFCIxB5ODPwnr1CNfuQTqR469FQ')]
    [TestCase('43', '[claRed;0.3 claNull;0.5 claBlue;0.7],claWhite,1.15,-0.15,-0.15,1.15,0,-0.15,1.15,0.85,1,1.3,0,-20,30,True,True,0.9,1,False,0.98,zU8BO/LMj17AAHcX30L3ipC+PPuuhzC+377/YL0Mxaee/eYljDA2/Rp0T8nq1htf+QTiY669Fg')]
    [TestCase('44', '[claRed;0 claBlue;1],claWhite,0,0,1,1,0,0,1,1,1,1,0,0,0,False,True,1,1,True,0.98,0uuef+3yL7B/GGa37zjrnzew/Pfx1z+F2wWUzvrnDSz/Pfz0T+G2AaUbPjgI4TNfvftE2O0B7w')]
    [TestCase('45', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0,1,1,1,1,0,0,0,False,True,1,1,True,0.98,dc+dwvvGS+/+EFRbsPrmjluAffbt0y8F0/5TvrnjFWSfffv1S8F0/5QHr7oAwrePwP5gEE0+3w')]
    [TestCase('46', '[claRed;0 claBlue;1],claWhite,1,0,0,1,0,0,1,1,1,1,0,0,0,False,True,1,1,True,0.98,sU/+AfHbuw8/F2sr8Pr1rjPg+vsJ2K/G2PFfvjjeM2iP/PTqN0H3CMbHPvkHxG/fPvxcsL2A6w')]
    [TestCase('47', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0,0,1,1,1,1,0,0,0,False,True,1,1,True,0.98,5CNP/+v+U/B/GmSXX/jroy+APPjx1z9F3QekDrvoCyAPPvz0T1E3Aqnb3fcJ6Q+fPPo7AH7C8Q')]
    [TestCase('48', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0,0,1,1,1,1,0,0,0,False,True,1,1,True,0.98,Y5ftAv26J78+D1Z7QDvmimOwPfbs0x9F0fxDfvniF3CPffv1S1H0/pBLD/v+u9dPQQBp7Mx93A')]
    [TestCase('49', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0,1,1,1,1,0,0,0,False,True,1,1,True,0.98,tktewu/bu+8+GHI7YHv6tSdAev0T4F8H2eZTLjjcQIQv/PHkL6E3DNPbHrkJvm/ffvtgyP2A7Q')]
    [TestCase('50', '[claRed;0 claBlue;1],claWhite,0,0,1,1,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,True,0.98,uvfNf/cHkHB+HZLfj7z6iSMQOwDye48G57mrLjrkAyDwewEMcIC5CMGr3jcD7cfe//ZPPP7/7Q')]
    [TestCase('51', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,True,0.98,jwtOgP7c7088FraHUL78etNvOv/7T69F5cGnfvnfBCywOwYYxCC4BsCDTzkD8M8/Avpe8L394w')]
    [TestCase('52', '[claRed;0 claBlue;1],claWhite,1,0,0,1,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,True,0.98,puu9QPr0R4A9HqQn4D39ibv/OgcDuM9F4ahHLnfg+E9gOv4PQHC5C85jn3gE6Ve/APZWUO4+6w')]
    [TestCase('53', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,True,0.98,wu+dP/YOsMA+H4i/Lzz6jTMwewDwg7+G57ivTjrlAxwAfAAJXMA5CcGHfjcD75t+P/ZNPF5A7w')]
    [TestCase('54', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,True,0.98,hhN+wP/Wy++7FMCnoD79d8NP+v79R4/F5MKjXvneBSygOwca2OA3BsCnn3kD7/ufwvpf8F194g')]
    [TestCase('55', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,True,0.98,p+P9wPn2S6D9H6QjEH79jZsvewkK2K9F36EbLvbf9GPgOf0NGIA5DdGDr/gE5l+fAPVWbP4+7A')]
    [TestCase('56', '[claRed;0 claBlue;1],claWhite,0,0,1,1,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,True,0.98,t1vuvvEXCDB/Hm7zrvnthSdQvP/umz/G4+cTL/rnBchvPAMTuMD3BKPfrXgADrgO/wEVuA1D7A')]
    [TestCase('57', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,True,0.98,psN9wO/dk7+7F6rnH77vegeQe/7/j8/G3dPPTrnhHCAwfAES4PB1B6Irn3kG5Xe/AgZiaH1B6A')]
    [TestCase('58', '[claRed;0 claBlue;1],claWhite,1,0,0,1,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,True,0.98,rsuNgO7xx0+9HZqPT33xfAtAewMM8E8I2r2fbvjgGSgge/sDeDA3C7XzbvkF5zM/QQJJuH3C7A')]
    [TestCase('59', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,True,0.98,vXOefvElKLC/H2HP3njthy9w/P/qny/G5OsjXzrpAbR/vAMTrCB4BKOjXTj/F4SOfgEKyD0D7Q')]
    [TestCase('60', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,True,0.98,oavNAPDObz87FrYL4P7veP9/O/4CjO+G3M+/HnngITQg/AAR6JD1B6Jn77kH3Kc/gwZtWE2B5w')]
    [TestCase('61', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,True,0.98,rq/tgO3twz/9HqGf733yf/9PewUPEIAI17JvnrffG0Swuvj+WxD3DLgjn/kG4D9fwQFPyI0C7g')]
    [TestCase('62', '[claRed;0 claBlue;1],claWhite,0,0,1,1,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,xUcOQPgBYBB+Fqm/T7v3i1+g/gEdGJAD8/b3vjzhHZQfvgHsDzF7/bFPP7gH+/OPAvxDKE+A8Q')]
    [TestCase('63', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,uBMegfv1G0D9E6f/j/v3i1+gPgIdGIBD8/b37r3lDpCPfwX2K5E5+qRPT3gH+vOfAvxELE+A8Q')]
    [TestCase('64', '[claRed;0 claBlue;1],claWhite,1,0,0,1,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,xC9+wPn9S9D9FaC/H3v3j1fwfgMgFMCD8fbb7j3kF3w/vwPwI2F6/K5PTzgH+POvAvxELE+A8Q')]
    [TestCase('65', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,yFPev/cDcDA+F6mzP7v3i1+g/gEdGJAD8/b3fjzgIJTfvQDpB4E7/rRPP7gH+/OPAvxDKE+A8Q')]
    [TestCase('66', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,tQdOQfzyDxB9E6YLkPv3jF+wPgIcGIBD8/b3Lr7mC5Dffwb4LzF5+aFPT3gH+fOfAvxELE+A8Q')]
    [TestCase('67', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,xjNuwPn/T+B9Fp2z/zr3kVcQvwMhENDD8PXTLn7kGHBfvwPvJ2G6/LBPT/gG9/OvAvxEMD/A8Q')]
    [TestCase('68', '[claRed;0 claBlue;1],claWhite,0,0,1,1,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,0j8uwPgJXCA+GK2Xvzr3h2MAvwEkIKBD8////vzeFriv/QLw/xD7+6xPX3gH+u9/gvxCOD/A8Q')]
    [TestCase('69', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,zOt9gff7J8A8FbPPLzz3h2MQvwEkIJCD8/7/bj7nDciPvwb3F6H59plPX3gH+u+PgvxCOD/A8Q')]
    [TestCase('70', '[claRed;0 claBlue;1],claWhite,1,0,0,1,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,0RtuAPkEQLC9Fqebz7r3imMgvwInFNDD8v/7fj7kFqz/fgTyA5H6+adPXzgH+vOPgvxCOD/A8Q')]
    [TestCase('71', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,00v+f/kNYHC+GKuLbzr3h2MAvwEkIKBD8///vnzdGLQ//QHv92D7/LBPX3gH+u9/gvxCOD/A8Q')]
    [TestCase('72', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,zN+dAff3I3C8FLXbfzz3h2MQvwEkIJCD8/77rr7oC8zvfwf5G1H59ZVPX3gH+u+PgvxCOD/A8Q')]
    [TestCase('73', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,0xtOQPkFQMB9FqSPf7r3i2cgvwInFNDD8v/7zr7kGaQvPwTxA7E6+qlPXzgH+vOPgvxCOD/A8Q')]
    [TestCase('74', '[claRed;0 claBlue;1],claWhite,0,0,1,1,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,0TNOAPgIWPA9GK+jD3v3hVvQfgEiHIDD8/v7frzgHpz/PQHsCzG7/bFPP7gH/POPAvxDKF+A8Q')]
    [TestCase('75', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,zPddAfj+L+C8FbDH/7v3iFvw/gEjGICD8/rzHv7lDoyvvwX4L2F5+qVPTzgH+fOfAvxELE+A8Q')]
    [TestCase('76', '[claRed;0 claBlue;1],claWhite,1,0,0,1,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,zxeuwPgEQIB9FqijHzv4i1sQvwIkEKAD8/rrHn7kF3hv/wPyJyF6/K5PTzgH+POvAvxEMD+A8Q')]
    [TestCase('77', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,0j8eQPgKYDD+GK+bz3r3hVvQfgEiHIDD8/v/LnzfIaCPfQDq/6B7/rRPP7gH/PN/AvxDKF+A8Q')]
    [TestCase('78', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,y+uNAfj7I6D8FLDPP/z3iFsA/wEjFICD8/rvbv7mC4gPgAb6NwG5+aJPTzgH+fOfAvxELE+A8Q')]
    [TestCase('79', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,zxeeQPkEQIB9FqWb73r4jVsg/wIlDLCD8vrnfv7kF2yfPwTxKxF6/K9PT/gG9/O/QvxEMD/A8Q')]
    [TestCase('80', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,0.5,True,0.98,zxeeQPkEQIB9FqWb73r4jVsg/wIlDLCD8vrnfv7kF2yfPwTxKxF6/K9PT/gG9/O/QvxEMD/A8Q')]
    [TestCase('81', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,1,True,0.98,0Ae+f/IJ3J/8HHgv/zn6k1MAfwQlCKAD8fLbjv/oFNC+/wDpV6H2+J1PP3gG8/PvwvtHKD+A8Q')]
    [TestCase('82', '[claRed;0 claBlue;1],claGreen,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,1,True,0.98,1Tvu//j9fx/9HKD3rj74h2NgfgEdQMBD8PUzPzrw5nOvO/TnY7H38eFXL7gH+/t/wvs/JG9A8Q')]
    [TestCase('83', '[claRed;0 claBlue;1],claGreen,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,0.5,True,0.98,0jOugPz+E9B9Frl/T333h2PQfgEgKMBD8vsTz7voALyfvf3xM6H6+NJTT3gH+/N/AvxBMF+A8Q')]
    [TestCase('84', '[claRed;0 claBlue;1],claWhite,1.15,-0.15,-0.15,1.15,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,1,True,0.98,zwP+f/EI4H98HYFHj3r5jVPQPgMjFIDD8fPrfn7nE/D+fgDpS9F2+JlPL/gG9vPPgvtGIE+A8Q')]
    [TestCase('85', '[claRed;0 claBlue;1],claWhite,1.15,-0.15,-0.15,1.15,0,-0.15,1.15,0.85,1,1.3,0,-20,30,True,True,0.9,1,True,0.98,sBsuAPT0y698G3qzL3v4k1NQ/gMYHJBD8OrjXj3kGhAfPv7mNyG3+ZtPLzgH+POvgvtFHE9A8Q')]
    [TestCase('86', '[claRed;0.3 claNull;0.5 claBlue;0.7],claWhite,1.15,-0.15,-0.15,1.15,0,-0.15,1.15,0.85,1,1.3,0,-20,30,True,True,0.9,1,True,0.98,ux/ugf/9TyB/Eqrrz7z3lUfQ/gQbGNDD7vDP3rzhH8xf/wL78yA7AMNTTzgH9/OvAvxDLD+A8Q')]
    procedure TestFillRectGradientStartEndPoint(const APoints, AModulateColor: string; AStartX, AStartY, AStopX, AStopY, ADestLeftPercent, ADestTopPercent, ADestRightPercent, ADestBottomPercent, ACanvasScaleX, ACanvasScaleY, ACanvasOffsetX, ACanvasOffsetY, ACanvasRotationDeg: Single; AApplyClip, ABlending: Boolean; const ABitmapScale, AOpacity: Single; ADrawBackgroundChess: Boolean; AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,y19yef+1A3//7r1zf8bxWwQ/PPiT01B+3dGPIX0ovBc//OoHrA88/04Y38Tt+6OOdwYLjH39CQ')]
    procedure TestFillRectSolid(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,y19yef+1A3//7r1zf8bxXAQ/PPiT01C+3dGPIX0ovBs/vOoHqA88/04Y38Tt+qOOdwYKjH09Cg')]
    procedure TestFillRectSolid2(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,x4/SOAWng0+9594/P8f0k6je/fCVR2C95eJvYb0no2+/++caPL95+VtYb8nq3wdfeQXWf/79Fw')]
    procedure TestFillRectSolid3(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,tXtiNg2aa3885r9nf4TxLUQ/eQ3RZ7G/y7x7wAEp6/OtP/fzNxDBCkZEn7/7AUguuwRNSG67Ag')]
    procedure TestFillRectSolid4(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,wWcyuAarq4896Npj/8btH1S/OgGXU2F/08KzoXwqzJ8efPD39/89BzfQ3j/47PN9uQNNFL07AQ')]
    procedure TestFillRectSolid5(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,tXtiNg2aa3885r9nf4TxLUQ/eQ3RZ7G/y7x7wAEp6/OtP/fzNxDBCkZEn7/7AUguuwRNSG67Ag')]
    procedure TestFillRectSolid6(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  '300,300,1,2,-50,-100,60,-200,-50,660,343,1,true,0.98,++t+vuz1w8A/HTy3Tnj7RvxOewO8l2D/17KrAL/z6pc+QAkBvNE09G5vXzr0HZTf/hJGnPv44w')]
    [TestCase('2',  '300,300,1,2,-50,-100,60,-200,-50,660,343,1,false,0.98,++t+vuz1w8A/HTy3Tnj7RvxOewO8l2D/17KrAL/z6pc+QAkBvNE09G5vXzr0HZTf/hJGnPv44w')]
    [TestCase('3',  '300,300,1,2,-50,-100,60,-200,-50,660,343,0.5,true,0.98,6J+QfPLbj49ACW4bL3/4bdSu/Pmpd2D+38kXMX4MywsfvvcLjGA39uZj34Hv/08fPAwODH37/Q')]
    [TestCase('4',  '300,300,1,2,-50,-100,60,-200,-50,660,343,0.5,false,0.98,6J+QfPLbj49ACW4bL3/4bdSu/Pmpd2D+38kXMX4MywsfvvcLjGA39uZj34Hv/08fPAwODH37/Q')]
    [TestCase('5',  '300,300,1,2,-50,-100,60,-200,-50,660,343,1,true,0.98,++t+vuz1w8A/HTy3Tnj7RvxOewO8l2D/17KrAL/z6pc+QAkBvNE09G5vXzr0HZTf/hJGnPv44w')]
    [TestCase('6',  '300,300,1,2,-50,-100,60,-200,-50,660,343,1,false,0.98,++t+vuz1w8A/HTy3Tnj7RvxOewO8l2D/17KrAL/z6pc+QAkBvNE09G5vXzr0HZTf/hJGnPv44w')]
    [TestCase('7',  '300,300,1,2,-50,-100,60,-200,-50,660,343,0.5,true,0.98,6J+QfPLbj49ACW4bL3/4bdSu/Pmpd2D+38kXMX4MywsfvvcLjGA39uZj34Hv/08fPAwODH37/Q')]
    [TestCase('8',  '300,300,1,2,-50,-100,60,-200,-50,660,343,0.5,false,0.98,6J+QfPLbj49ACW4bL3/4bdSu/Pmpd2D+38kXMX4MywsfvvcLjGA39uZj34Hv/08fPAwODH37/Q')]
    [TestCase('9',  '300,300,1,2,-50,-100,60,0,0,200,200,1,true,0.98,v3MB+uanG66CElnn3n/1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('10', '300,300,1,2,-50,-100,30,0,0,200,200,1,false,0.98,L+CfgNrWp/6EFQxv/Xr1lawOPvCVV1A96OCDUb4cot8e/OoPbOC38ThUX8nq4ANfeQXVgw4+GA')]
    [TestCase('11', '300,300,1,2,-50,-100,30,0,0,200,200,0.5,true,0.98,AhihfenLfz6DBVZ3fkD1lawOPvCVV1A96OCD0T0hpi8PPOgS4P/49ExUX8nq4ANfeQXVgw4+GA')]
    [TestCase('12', '300,300,1,2,-50,-100,30,0,0,200,200,0.5,false,0.98,AhihfenLfz6DBVZ3fkD1lawOPvCVV1A96OCD0T0hpi8PPOgS4P/49ExUX8nq4ANfeQXVgw4+GA')]
    [TestCase('13', '300,300,1,2,-50,-100,30,0,0,200,200,1,true,0.98,L+CfgNrWp/6EFQxv/Xr1lawOPvCVV1A96OCDUb4cot8e/OoPbOC38ThUX8nq4ANfeQXVgw4+GA')]
    [TestCase('14', '300,300,1,2,-50,-100,30,0,0,200,200,1,false,0.98,L+CfgNrWp/6EFQxv/Xr1lawOPvCVV1A96OCDUb4cot8e/OoPbOC38ThUX8nq4ANfeQXVgw4+GA')]
    [TestCase('15', '300,300,1,2,-50,-100,30,0,0,200,200,0.5,true,0.98,AhihfenLfz6DBVZ3fkD1lawOPvCVV1A96OCD0T0hpi8PPOgS4P/49ExUX8nq4ANfeQXVgw4+GA')]
    [TestCase('16', '300,300,1,2,-50,-100,30,0,0,200,200,0.5,false,0.98,AhihfenLfz6DBVZ3fkD1lawOPvCVV1A96OCD0T0hpi8PPOgS4P/49ExUX8nq4ANfeQXVgw4+GA')]
    [TestCase('17', '300,300,1,-2,50,500,30,-200,-50,660,343,1,true,0.98,vztRe/vr52D9++YjoQP8zqtf+gAlBPBG09C7Hb716gdvfwgJMOF193vD77z6pQ9QQgBvNA292w')]
    [TestCase('18', '300,300,1,-2,50,500,30,-200,-50,660,343,1,false,0.98,vztRe/vr52D9++YjoQP8zqtf+gAlBPBG09C7Hb716gdvfwgJMOF193vD77z6pQ9QQgBvNA292w')]
    [TestCase('19', '300,300,1,-2,50,500,30,-200,-50,660,343,0.5,true,0.98,yscB+/nWo2+/+MNT0MT4MSwvfPjeLzCC3dibv30Ny0e//fYPRPD39+2PH8Pywofv/QIj2I29+Q')]
    [TestCase('20', '300,300,1,-2,50,500,30,-200,-50,660,343,0.5,false,0.98,yscB+/nWo2+/+MNT0MT4MSwvfPjeLzCC3dibv30Ny0e//fYPRPD39+2PH8Pywofv/QIj2I29+Q')]
    [TestCase('21', '300,300,-1,2,-50,-100,30,-200,-50,660,343,1,true,0.98,L+CfgNrWp/6EFQxv/Xr1lawOPvCVV1A96OCDUb4cot8e/OoPbOC38ThUX8nq4ANfeQXVgw4+GA')]
    [TestCase('22', '300,300,-1,2,-50,-100,30,-200,-50,660,343,1,false,0.98,L+CfgNrWp/6EFQxv/Xr1lawOPvCVV1A96OCDUb4cot8e/OoPbOC38ThUX8nq4ANfeQXVgw4+GA')]
    [TestCase('23', '300,300,-1,2,-50,-100,30,-200,-50,660,343,0.5,true,0.98,AhihfenLfz6DBVZ3fkD1lawOPvCVV1A96OCD0T0hpi8PPOgS4P/49ExUX8nq4ANfeQXVgw4+GA')]
    [TestCase('24', '300,300,-1,2,-50,-100,30,-200,-50,660,343,0.5,false,0.98,AhihfenLfz6DBVZ3fkD1lawOPvCVV1A96OCD0T0hpi8PPOgS4P/49ExUX8nq4ANfeQXVgw4+GA')]
    [TestCase('25', '300,300,-1,2,-50,-100,-60,0,0,200,200,1,true,0.98,+AePPusBiHC/G07zbnf18DPePfnTV4GByJ73PgAdGmROvvXXg9A+BSzcXwL7jnP+txBWPJy38g')]
    [TestCase('26', '300,300,-1,2,-50,-100,-60,0,0,200,200,1,false,0.98,+AePPusBiHC/G07zbnf18DPePfnTV4GByJ73PgAdGmROvvXXg9A+BSzcXwL7jnP+txBWPJy38g')]
    [TestCase('27', '300,300,-1,2,-50,-100,-60,0,0,200,200,0.5,true,0.98,56+gvPHhc2+ACHc7v371QmzuvfS013B/2L87wH4h4/cuve3273+8/kaY3wXzt7uuOAsWXM16BQ')]
    [TestCase('28', '300,300,-1,2,-50,-100,-60,0,0,200,200,0.5,false,0.98,56+gvPHhc2+ACHc7v371QmzuvfS013B/2L87wH4h4/cuve3273+8/kaY3wXzt7uuOAsWXM16BQ')]
    [TestCase('29', '300,300,1,-2,-50,500,30,0,0,200,200,1,true,0.98,1VeievjBV25B9aCD/4X1lqj+ffCVV2A96N9/cXogjUsw/fGrx55+9k4cr37h9mdvPhAl+Hs55g')]
    [TestCase('30', '300,300,1,-2,-50,500,30,0,0,200,200,1,false,0.98,1VeievjBV25B9aCD/4X1lqj+ffCVV2A96N9/cXogjUsw/fGrx55+9k4cr37h9mdvPhAl+Hs55g')]
    [TestCase('31', '300,300,1,-2,-50,500,30,0,0,200,200,0.5,true,0.98,1VeiOvjAV25B9aCDD4b1lqgOfvCVV2A96OB/4fsinOefvOvgD19891c4/wPm6zPv+wr9P807/w')]
    [TestCase('32', '300,300,1,-2,-50,500,30,0,0,200,200,0.5,false,0.98,1VeiOvjAV25B9aCDD4b1lqgOfvCVV2A96OB/4fsinOefvOvgD19891c4/wPm6zPv+wr9P807/w')]
    [TestCase('33', '80,80,1,1,0,0,0,0,0,60,60,0,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('34', '80,80,1,1,0,0,0,60,0,60,60,1,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('35', '80,80,1,1,0,0,0,0,60,60,60,1,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('36', '80,80,1,1,0,0,0,0,0,60,60,0,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('37', '80,80,1,1,0,0,0,60,0,60,60,1,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('38', '80,80,1,1,0,0,0,0,60,60,60,1,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('39', '80,80,1,1,0,0,10,0,0,60,60,0,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('40', '80,80,1,1,0,0,10,60,0,60,60,1,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('41', '80,80,1,1,0,0,10,0,60,60,60,1,false,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('42', '80,80,1,1,0,0,10,0,0,60,60,0,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('43', '80,80,1,1,0,0,10,60,0,60,60,1,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('44', '80,80,1,1,0,0,10,0,60,60,60,1,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    procedure TestFillRectSolidWithMatrix(ASurfaceWidth, ASurfaceHeight: Integer; ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg, ADestLeft, ADestTop, ADestRight, ADestBottom, AOpacity: Single; ABlending: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,yWMy+QCzIx9/7cNvn0bxUBjfu/mS64B+286TEf0ovwM//OsEvG98AEoM70PuAIwutwYVXF09Bw')]
    procedure TestFillRectSolidWithModulateColor(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,t4PSeQCt8w6+7d3Xf4jvIXhPOvqNA0H+5NqPgbwnuZc//OwHUD/9BC8Ez4H2CoAuNwQraJz9/w')]
    procedure TestFillRectSolidWithResource(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',   'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,False,0,0,1,1,0.4,0.98,x6sQPv/aT2/B+8Xfv4L3KmzPvvjSAxBA8NqzoDwL3a9PvfgF8G88+TJ4H4P331+fvf8ANI89Cw')]
    [TestCase('2',   'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,False,0,0,1,1,0.4,0.98,xx8hfPfL/95A/oqnL8H0PxQf/fK/E8DA4dZjkDwRw09P/PIGILD49xo8P8TxyxcffAMLOJ69Bg')]
    [TestCase('3',   'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,False,0,0,1,1,1,0.98,sSsugwkBvGBBBf1rsD37iovgvwUsgD+E/NJ7j3vjKfAvfxXt3/D/+uyzvzkL3evvw/ZAQMB89w')]
    [TestCase('4',   'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,False,0,0,1,1,1,0.98,skdvPvbb9w/AC2rb7zn0vbe/O/f+q++F2MazXnvy5/+evAbwU8E297Abj7z8qzcvQABbzP187A')]
    [TestCase('5',   'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0.4,0.98,1AfCewXG9x4B7sjTP4XzSXC//vSsJ7H94/cvob8h3Xtf/egNbD+++j2E78X5zMseuQwFUG59DA')]
    [TestCase('6',   'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0.4,0.98,wefxuv+9757A9KePr0XzTgCP/fSZ9yB/3dALcT4k12cvu+kalI97/Dl8X8bzxqfeeAoStO17Bg')]
    [TestCase('7',   'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,0YthvRnQ479A4wRM4MPw15+w//vOX0I+3RqwIIMbKGxPf+4BjH9E/gfMv8AQr3u+eBdNCH68+g')]
    [TestCase('8',   'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,okNx+wq5z2//87CfH8Xv4oe/fPye5+EBzblbEEAiGjTPOfAi8N+9Av+734EBnh8e+BFugMz46w')]
    [TestCase('9',   'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,False,0,0,1,1,0.4,0.98,x68QPv/aT2/B+8Xfv4LvIbDP/vGvF6HA3t//cP0jsEtPPeQQjK95+ltYn4jt1VvPuALfU049Fw')]
    [TestCase('10',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,False,0,0,1,1,0.4,0.98,xx8hfPfL/95A/ounLwH0Mkwv/e+cuzBB2MKXoP0mrDevPOYLZF/5+VVY74jt0TPfOAHbUz79FQ')]
    [TestCase('11',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,False,0,0,1,1,1,0.98,sSsugwkBvGBBBf1rsD3ncz/hv/TXN6IF0d0zoL0hufM+P+IJ3C+5/VNgf4fxxdvft/7sCx58FQ')]
    [TestCase('12',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,False,0,0,1,1,1,0.98,skdvPvbb9w/AC2rb7znynUPAu++lTwGHwJU3Hz4prceufef7f0+4/ERgPwjyu3sv+PrkD/47Eg')]
    [TestCase('13',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0.4,0.98,1AfCewXG9x4B7sjTP4XzSXC//vSsJ7H94/cvIX8cz1cfPen7p198+1mMH8by279uuQMBUH68Eg')]
    [TestCase('14',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0.4,0.98,wevxuv+9757A9KePr0XzTgCP/fSZ9yB/3dALAX4gxEMPfOcIkC97/lKIzwbw26NOuQMBHM68EA')]
    [TestCase('15',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,0YthvRnQ479A4wRM4MPw15+w//vOX0I+3Rqw4MEOBhSf/u7TG9C/AE7YP8H+01tu+QBDBD46Cg')]
    [TestCase('16',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,okNx+wq5z2//87CfH8Xv4oe/fPye5+EBzblb8H4Y6+P+u+r069/8BzzY7wL40xM+uQBBjP16BQ')]
    [TestCase('17',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,False,0,0,1,1,0.4,0.98,2JcwvvziZ08B+7rHz8L5Iqz//vrXI6D/7uGv4D0M07NvffkM8F+7+B9wr8H48lPv/AT1z04/CA')]
    [TestCase('18',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,False,0,0,1,1,0.4,0.98,0xMxPPfKB0+B/omPz4H2PSBPffS/L4CA4dZ7gHwPwEsPvPIQWLA39hg8n4Py1itvuwMDAG49Bg')]
    [TestCase('19',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,False,0,0,1,1,1,0.98,3feNQwMU/BCBA+Iv8H0AdTNxwAo5zA8D+eRvz/7lDvhvfxf920C9+b2fH/YNDsgvQgQjPB/B7w')]
    [TestCase('20',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,False,0,0,1,1,1,0.98,0Ct/fvXaCxBBDGafbzv4udcfvPr981+F18bvXjvu4fP+ewYI3DH08qsX/7r+yGeP/gBIRH186w')]
    [TestCase('21',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0.4,0.98,1Pux+wTOA//A78fHPwXzSZC//vK2KzE+5fo/sX8h3G+vfOsMhO99+DqEfwX61qseeQ4IEP68CQ')]
    [TestCase('22',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0.4,0.98,xOMBu/++B58A9qR/zwXzUgx/PfWZA3E/3c8DQX4k02eveuoboM97+ztkP0b00I9O+AoPsA08Bg')]
    [TestCase('23',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,0mshvRfjA1CA5wIsAITv1e/A//bnY4L/4CLcMEMbJlCvPfT9y7+D+ALMr38RyCu++BtUZG079A')]
    [TestCase('24',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,qzeBOwu6C1A/96p/X8Xv7KOffPyfA5KCzLRDoL8iEECfOPIlFFA+AAWAfwEDueOdNhNleAx56w')]
    [TestCase('25',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,True,0,0,1,1,0.4,0.98,+JcwvvvuWw+A/NCXb0L9IFBf/vjKwz/B8MizMH4L4bu//fYI4D98+h/0TsP3918/fAfm1/7/Bg')]
    [TestCase('26',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,True,0,0,1,1,0.4,0.98,1xeRPPbOA/8AAYqDv0H2PCgfvfHCB1BB49CfoLwQxk+vvPEPONC39xMMn0Pw0U+fewQC4O39Aw')]
    [TestCase('27',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,True,0,0,1,1,1,0.98,LPSNgwAz3AC+Bxi077wJcUfQ/gUY5P4G/qV7b3/kMgwwABH0s5D//b1jProLGux/gAr/U9+C7A')]
    [TestCase('28',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,True,0,0,1,1,1,0.98,2TN///Lk/2+AEmh/T/v3tuOvu/MEiF/H27lDr7vx7/+evQMGjJE096Cj/vr4u7/vvgJE7My95Q')]
    [TestCase('29',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,0.4,0.98,2AtSuwPL914A8NajnwTzSajPvvK4/1D+5wE80f4i1nM/vOgNmJ99+TZY/wX41Zf++AsRFO78CQ')]
    [TestCase('30',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,0.4,0.98,xgfSOgC8A68A+KNvj4XzVxSPffWZ84A/3s3zoD0l0XOfeukboI87/DpIv4b00YN+OAsXvB28Bg')]
    [TestCase('31',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,1,0.98,3JdRPBXb4+8+6CjQf8Lv1ivhf/bs98F/5zLU8EAfFlyPPO0A+P8C+/df7wAMxfN9uBVscC279A')]
    [TestCase('32',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,1,0.98,rosR+wu2A4B//KdPz8Tw+bO/fP2f27FCz7AfEL4kC1iPOO8iENA9AgE0z0IDu789dxN6nCx57A')]
    [TestCase('33',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,True,0,0,1,1,0.4,0.98,+JcwvvvuWw+A/NCXb0L9IFBf/vjKxz/B8MizMH4L4bu//fYI4D98+h/0TsP3918/fAfm1/7/Bg')]
    [TestCase('34',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,True,0,0,1,1,0.4,0.98,1xeRPPbOA/8AAYqDv0H2PCgfvfHCB1BB49CfoLwQxk+vvPEPONC39xMMn0Pw0U+fewQC4O39Aw')]
    [TestCase('35',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,True,0,0,1,1,1,0.98,LPiNgwAz3AC+Bxi0/7wJcUfQ/gUY5P4G/qV7b3/kMhAwABH0s5A//r1jProLGux/gAr/U9+C7A')]
    [TestCase('36',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,True,0,0,1,1,1,0.98,2TN///Lk/29AEmh/X/v3tuOvu/MEiF/H27lHr7vx7/+evQMGjJE096Cj/vr4u7/vvgJE7My95Q')]
    [TestCase('37',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,0.4,0.98,2AtSuwPL924A8NajnwTzSajPvvK4/1D+5wE80f4i1nM/vOgNmJ99+TZY/wX41Zf++AsRFO78CQ')]
    [TestCase('38',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,0.4,0.98,xgfSOgC8A68A+KNvj4XzVxR/ffWZ84A/3s3zoD0l0XOfeukboI87/DpIv4b00YN+OAsXvB28Bg')]
    [TestCase('39',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,1,0.98,25dRPBXb4+8+6CjQf8Lv1ivhf/bs98F/5zLU8EAfFlyPPO0A+P8C+/df7wAMxfN9uBVscC279A')]
    [TestCase('40',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,1,0.98,rosR+wu2A4B//KdPz8Tw+bO/fP2f27FCz7AfEL4kC1iPOO8iENA9AgE0z0IDu789dxN6nCx57A')]
    [TestCase('41',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,True,0,0,1,1,0.4,0.98,2JcwvvziZ08B+7rHz8L5Iqz//vrXI6D/7uGv4D0M07NvffkM8F+7+B9wr8H48lPvPAX1z07/Bw')]
    [TestCase('42',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,True,0,0,1,1,0.4,0.98,0xMxPPfKB0+B/omPz4H2PSBPffS/L4CA4dZ7gHwPwEsPvPIQWLA39hg8n8Py1itvuwMDAG49Bg')]
    [TestCase('43',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,True,0,0,1,1,1,0.98,3feNQwMU/BCBA+Iv8H0AdTNxwAo5zA8D+eNvzz7mDvhvfxf920C9+b2fH/YNDsgvQgQjPB/B7w')]
    [TestCase('44',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,True,0,0,1,1,1,0.98,0Ct/fvXaCyBBDGafbzv4udcfvPr981+F18bvTnvu4fP+ewYI3DH08qsX/7r+yGeP/gBIRH186w')]
    [TestCase('45',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,0.4,0.98,1Puh+wTOA//A78fHPwXzSZC//vK2KzE+5fo/sX8h3G+vfOsMhO99+DqEfwX61qseeQ4IEP68CQ')]
    [TestCase('46',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,0.4,0.98,xOMBu/++B58A9qR/zwXzUgx/PfWZA3E/3c8DQX4k02uveuoboM97+ztkP0b00I9O+AoPsA08Bg')]
    [TestCase('47',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,1,0.98,0mshvRfjA1CA5wIsAITv1e/A//bnY4K/4CLcMEMbJlCvPfT9y7+D+ALMr38RyCu++BtUZG079A')]
    [TestCase('48',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,1,0.98,qzeBOwu6C1A/96p/X4Xv7KOffPyfA5KCzLVDoL8iEECvOPIkFFA+AAV8fwEDueOdNhNleAx56w')]
    [TestCase('49',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,0.4,0.98,yNdQvv/aQ3/B+Mf3L8P2K3jvvvfTEyDA7NXLQDwN2KfPvPcC/F/8+DN8/wL54mcfPQIBKK99Cg')]
    [TestCase('50',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,0.4,0.98,xz8xPPjN4+5A/I+rr4H0OihP/fHBGwAB4Ml7APwSvlfPu/EEKMB4+B5UPwTzyxu/uwUKOL69Bg')]
    [TestCase('51',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,1,0.98,tJfewwoCpKBB/gKk0H75i6sgwAIxqG+E88Wzr/roHdjfvRLl++A/+u63X/kO5f+/wvxDJAC99Q')]
    [TestCase('52',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,1,0.98,sJOPPvjhs0/ABnXnHzvzsOMvvPQBvH8G1KbvDvr22w9vOwTpY+H2+LpTf7z/rEM/vwVZ0E097A')]
    [TestCase('53',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0.4,0.98,0QeCewXG++5A7sjXP4XyS1ivPvWsH6G94vYvsX8h2Yt/vekJbC+++j2Az0X6yssOOQwHRD69DQ')]
    [TestCase('54',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0.4,0.98,v+vxev+9826A9KiTv8XyTvSOPfWZ8xC/3NILUX4k1Hc/u+kYkJ+7/Dt0P8bzxa+++AkSrM17Bw')]
    [TestCase('55',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,yo/BfBnP708A5AVQEITu3V+Qf/zOTyJ+2he0MIMbIJSvP/D3j19E/gjAb4ARqXt+OBZR8P07/g')]
    [TestCase('56',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,nUdRewq51w8/87SrT4Xu42fPfPyf18GBy71XwP8iEmAP+u8d6A9+AwOon0EBmy/OdxBsbGw47g')]
    [TestCase('57',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,0.4,0.98,yNdQvv/aQ3/B+Mf3L4PvIbDP/vGvF6HA3t//cP0jsEtPPeQQjK95+ltYn4jt1VvPuALfU049Fw')]
    [TestCase('58',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,0.4,0.98,xz8xPPjN4+5A/I+rrwH0Mkwv/e+cuzBB2MKXoP0mrDevPOYLZF/5+VVY74jt0TPfOAHbUz79FQ')]
    [TestCase('59',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,1,0.98,tJfewwoCpKBB/gKk0D7ncz/hv/TXN6IF0d0zoL0hufM+P+IJ3C+5/VNgf4fxxdvft/7sCx58FQ')]
    [TestCase('60',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,1,0.98,sJOPPvjhs0/ABnXnHzvynUPAu++lTwGHwJU3Hz4prceufef7f0+4/ERgPwjyu3sv+PrkD/47Eg')]
    [TestCase('61',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0.4,0.98,0QeCewXG++5A7sjXP4XyTFivPvWsH6G94vYvIX8cz1cfPen7p198+1mMH8by279uuQMBUH68Eg')]
    [TestCase('62',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0.4,0.98,v+vxev+9826A9KiTv8XyTvSOPfWZ8xC/3NILAX4gxEMPfOcIkC97/lKIzwbw26NOuQMBHM68EA')]
    [TestCase('63',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,yo/BfBnP708A5AVQEITu3V+Qf/zOTyJ+2he04MEOBhSf/u7TG9C/AE7YP8H+01tu+QBDBD46Cg')]
    [TestCase('64',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,nUdRewq51w8/87SrT4Xu42fPfPyf18GBy71X8H4Y6+P+u+r069/8BzzY7wL40xM+uQBBjP16BQ')]
    [TestCase('65',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,0.4,0.98,2sOAfv3iW19B+LzjT0P5Irgf//naL7B/69vHkH0OzqvvPPgI/E97+B90j0H69VtvPAf1w15/Bw')]
    [TestCase('66',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,0.4,0.98,0jNB/PfN626B/I2TTwL2ODR/ffPAN8DA38mTADwRvE+Pu/ENYMC39htQnwP01y8PuwUCAI49Bg')]
    [TestCase('67',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,1,0.98,4WNeBAUV4FBB/OdzEP/+dU/BQAhA7E9D8NOv773rAugvfhT1+zD9+L6n37URFNwPgQkkJF9B7g')]
    [TestCase('68',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,1,0.98,znevvvfgw29BB3Grr3z3rAeQfPgBAPBF06YnD/ry1gff+gMB7GG09LVH//oByXOP/QVGQL186w')]
    [TestCase('69',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0.4,0.98,0ftxuwTOB88A8MfLTwXyS3iv/vK1IyE+5PlDsX8h2X/f/OsIgN99+DuAb0X61K/+uA0JBM48Cw')]
    [TestCase('70',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0.4,0.98,wufxev++C2/A9aWH34XyUgB/PfWa/2C/3NADIb4k0XfPeuoaoN+7+z1gH0b0z5cueAoOqO07Bw')]
    [TestCase('71',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,zG+BvBfiD+A/6AIwIETt26+Qf/fmU2L/3R/cQEMbH3T/vfX1x5/D+APAbz8Swi9uOBpXTP169w')]
    [TestCase('72',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,pjthewq6E/A+9q2Hj4Xu7YOvvPyg93ECy7g/UD8jCWjv+PEgDHD+AAhwL8ECtvddthFkYKy47Q')]
    [TestCase('73',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,0.4,0.98,yudw/v7cN58B+MoDkIP1LYT/fvXONzCA6tPfIHwP06uPfPX//1+8+DB4/8L54mff/AMHCM89CQ')]
    [TestCase('74',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,0.4,0.98,yEtBPPfRzw4B/JGvL8LzOjCP/fC8JyAB38GPsLsUu1+fO/ABKMD4+B5YPwT0yhePewcPJO69BQ')]
    [TestCase('75',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,1,0.98,usMuBAkGhODB+wrE8D/2kctQQP0iAIBE7sDrX3ruD+RfPQ3e+8A/+eizX7kQ5P8PggFS2G+98g')]
    [TestCase('76',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,1,0.98,tbuvvvXrg39ABnrzX3zxsv+//PH3399G0ZMfXzn70yvfegDjY9H2+bpfj3wCqTe/PgplnL096g')]
    [TestCase('77',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0.4,0.98,zwdSewXH+77A7snXPwXyTkB//vSrG8F94vYzoX8h1pOPPeoEZC+++j2Az4X6x8feOAsGPA79Dg')]
    [TestCase('78',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0.4,0.98,vevhev+/706A9KqXz4XyT+x+PfWb6yC/3NQLIX4k0XtfO+kXjK/7/Dx0P8bzwq+uOAkQpL17CA')]
    [TestCase('79',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,xY9BPBnQ778/5QdUAMTs4ycQf/zMO1J+2Ra4IEMbGKjP//Drg0+E/gfEbwASonMe+BNQ2I07AQ')]
    [TestCase('80',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,mEcx+wm9069+87q3b0Xu5k+vvPyjx+GBy8FbUL8iCmxPuu4Y3C8+BAakf0EBli+Otw5pYEy48A')]
    [TestCase('81',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,0.4,0.98,yutw/v7cN58B+MoDkIPvIbDP/vGvF6HA3t//cP0jsEtPPeQQjK95+ltYn4jt1VvPuALfU049Fw')]
    [TestCase('82',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,0.4,0.98,yEtBPPfRzw4B/JGvLwL0Mkwv/e+cuzBB2MKXoP0mrDevPOYLZF/5+VVY74jt0TPfOAHbUz79FQ')]
    [TestCase('83',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,1,0.98,usMuBAkFhODB+wrE8D/ncz/hv/TXN6IF0d0zoL0hufM+P+IJ3C+5/VNgf4fxxdvft/7sCx58FQ')]
    [TestCase('84',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,1,0.98,tbuvvvXrg39ABnrzXzzynUPAu++lTwGHwJU3Hz4prceufef7f0+4/ERgPwjyu3sv+PrkD/47Eg')]
    [TestCase('85',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0.4,0.98,zwdSewXH+77A7snXPwXyTkB//vSrG8F94vYzIX8cz1cfPen7p198+1mMH8by279uuQMBUH68Eg')]
    [TestCase('86',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0.4,0.98,vevhev+/706A9KqXz4XyT+x+PfWb6yC/3NQLAX4gxEMPfOcIkC97/lKIzwbw26NOuQMBHM68EA')]
    [TestCase('87',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,xY9BPBnQ778/5QdUAMTs4ycQf/zMO1J+2Ra44MEOBhSf/u7TG9C/AE7YP8H+01tu+QBDBD46Cg')]
    [TestCase('88',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,mEcx+wm9069+87q3b0Xu5k+vvPyjx9GBy8Fb8H4Y6+P+u+r069/8BzzY7wL40xM+uQBBjP16BQ')]
    [TestCase('89',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,0.4,0.98,3NOwvvzkT38B97/zvwP4JMQ///fUT8B/6djfcL0Qya+/PPYF/D87+B10nwH79Fcv/Aj7p34/Bg')]
    [TestCase('90',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,0.4,0.98,1D9RPPfR135B/I+Xz4L1OECvffK9Q/CA3sGjsPsSuFtfO/ALXLA39xtUrwP11ivfegcH7K19BQ')]
    [TestCase('91',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,1,0.98,54vOhAMawKDB+e6XMMD7e2fhQAMzOHAD683nnz3x9fO/PQ/t+xA9+Lij73UTE9hfAA4z3L5B6w')]
    [TestCase('92',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,1,0.98,0pvffvXqk59BBna37731rB8gvfX4I2CG0JNXTzn3zR9f+v/770G09bRTL3sExmMPvQpSEC196Q')]
    [TestCase('93',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0.4,0.98,z/sxuwTOB4+A8MjLP4XxTWR//vK1GzG+4/lDoX8h1oPv/OsEfN+9+DuAX4X60a/e+AwI/K08DA')]
    [TestCase('94',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0.4,0.98,wOfhev+/B0/A9aiL34XyU/huPfWb93B/3NIDAX4kznvfOuoYmN/7+z5gD0b0zZsOuAkNoM07CA')]
    [TestCase('95',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,xm8BfBfjD0B/6QU0EMTr4XcQf/flP5I/3R/gIAMbGIQv/vXqu58D+QTET38Suy8e+BdVNK06+g')]
    [TestCase('96',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,oDtBOwq+E6C+9rSXrwXu8GuP/Pyk54ECy7xD8P4iAnAv+fAbAJC+AQtw/4ACsf8dtg9hTHz47w')]
    [TestCase('97',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,0.4,0.98,y+twfv7cM5+B980HwMP0L4QPv/TKRyBA6tPjMHwQ0bNvfPT9/098+C50/8L54WPfvAQKAO+9CA')]
    [TestCase('98',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,0.4,0.98,yVNRvPbTyw4B/JOzX0LzPDCfffC5KxDB3r+TsHsVumdvu+8AJMA4+R1UP4T0yROPOwgSHA5+BQ')]
    [TestCase('99',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,1,0.98,vMcuhAcHePAB+xDMYAD0ls9wAPsYKFCE7b7zj/rwC/T//Ara97D/+OSnX/kQ4vcfQgNavL998Q')]
    [TestCase('100', 'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,1,0.98,tsfPPvTwd3/ABX/3zzzwtvsPPfHu67/G0I4nTzn90T9vOv/gW9H2+rhbj3wDpyu/PgxshA0+6Q')]
    [TestCase('101', 'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0.4,0.98,zQcyewXH+54A78rbP8XxTzhf/vSrF9E94vYzoX8h1ZePPeoCZC/++j2Az4X6xsfO+AoGOP58Dw')]
    [TestCase('102', 'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0.4,0.98,vOvROv/A7z7A9Kybz4XyUOhuPfWc5zD/3NQPEX4k0Htf++gViL87/T10P4bzwa+e+AgQpL37CA')]
    [TestCase('103', 'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,wosB/BjR62//5QhY4EPs5RPQfvzLM4J+2Ra8EAMbFbDP//Dle0+E/gfEbwASnmv+9xJPzG17Ag')]
    [TestCase('104', 'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,lUch+wm/04/+876/b0Xu50N//Pylv/HBy8JjIL8iCGhv+u0W1D9+BAikfwEBky9+9w1nXEz48Q')]
    [TestCase('105', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,0.4,0.98,y+twfv7cM5+B980HwIPvIbDP/vGvF6HA3t//cP0jsEtPPeQQjK95+ltYn4jt1VvPuALfU049Fw')]
    [TestCase('106', 'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,0.4,0.98,yVNRvPbTyw4B/JOzXwL0Mkwv/e+cuzBB2MKXoP0mrDevPOYLZF/5+VVY74jt0TPfOAHbUz79FQ')]
    [TestCase('107', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,1,0.98,vMcuxAcHePAB+xDMYADncz/hv/TXN6IF0d0zoL0hufM+P+IJ3C+5/VNgf4fxxdvft/7sCx58FQ')]
    [TestCase('108', 'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,1,0.98,tsfPPvTwd3/ABX/3zzzynUPAu++lTwGHwJU3Hz4prceufef7f0+4/ERgPwjyu3sv+PrkD/47Eg')]
    [TestCase('109', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0.4,0.98,zQcyewXH+54A78rbP8XxTzhf/vSrF9E94vY3IX8cz1cfPen7p198+1mMH8by279uuQMBUH68Eg')]
    [TestCase('110', 'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0.4,0.98,vOvROv/A7z7A9Kybz4XyUOhuPfWc5zD/3NQPAX4gxEMPfOcIkC97/lKIzwbw26NOuQMBHM68EA')]
    [TestCase('111', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,wo8B/BjR62//5QhY4EPs5RPQfvzLM4J+2Ra84MEOBhSf/u7TG9C/AE7YP8H+01tu+QBDBD46Cg')]
    [TestCase('112', 'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,lUcR+wm/04++876/b0Xu50N//Pylv/HBy8Jj8H4Y6+P+u+r069/8BzzY7wL40xM+uQBBjP16BQ')]
    [TestCase('113', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,0.4,0.98,3dPAfvzlS3/B9sL330P3JsRP//bRW7D/6NjjgL0Rx7efPPUE+D87+BtwnwH79FMvvAn+n56/BQ')]
    [TestCase('114', 'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,0.4,0.98,1UNhfPbT034B/JGb/wL1OjzPPfK5R/BA3r+nsLsTt2M/u+8JXMB39xpQr0P11SfPOggK5M39BA')]
    [TestCase('115', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,1,0.98,6Y/eRAIcsLDB+PWjkAD6gGsRAQEqXEAD6svvv73z8QNg/Qzp8wD997OX77UTEcxPwA87wP4B6g')]
    [TestCase('116', 'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,1,0.98,1KMP//Pwh58BBnu7X370sBtg/fTwL1DGz45fPzn5yi/v+f7351G09rFLL3sFxVf/fAxZ+Gx96A')]
    [TestCase('117', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0.4,0.98,zvshuwTOB2/A8MnLP0XxTlxfPvO1F0G+4/lHoT8h1Yfv/OsCeN+9+DyEX4X60K/eeAwI+K28DA')]
    [TestCase('118', 'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0.4,0.98,v+fhev/ABz8A9qmP74XyU/ReffWc83C/3NIH8X0kzXvv+ukWmN87/D9g/0X0zJsOOAkMnM17CA')]
    [TestCase('119', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,w2vBOxfkD/A+6gY48APr42PQfvfkN7I/3R7kEMMaFYgvvvXls59D+QXITz8SuCsO+BZUKI06+w')]
    [TestCase('120', 'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,njsx+wnBD3D+9rebr0Xu8WNfPP2m36FCy75LwL4iAHA/OfAY+K/+AQ1w/0ACrv8Ntg5fSHz48A')]
    [TestCase('121', 'horse.webp,claWhite,Tile,0,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,14PB/RnP37/A4wNYkIPy1b9wv/rRV7J+4CC4oIIbKlAP/+wFgI9E/gjg/0AQtHOuuBZGEL68+A')]
    [TestCase('122', 'horse.webp,claWhite,TileOriginal,0,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,14PB/RnP37/A4wNYkIPy1b9wv/rRV7J+4CC44MEOBhSf/u7TG9C/AE7YP8H+01tu+QBDBD46Cg')]
    [TestCase('123', 'horse.webp,claWhite,TileStretch,0,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,2GOB/RfiA0AA6AA4oIPx0xOBv/XpW/L/4yjkwAIbKDBv/fICvM+D+ALg//8QzSOuOBtObK078g')]
    [TestCase('124', 'horse.webp,claWhite,Tile,0,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,14PB/RnP37/A4wNYkIPy1b9wv/rRV7J+4CC4oIIbKlAP/+wFgI9E/gjg/0AQtHOuuBZGEL68+A')]
    [TestCase('125', 'horse.webp,claWhite,TileOriginal,0,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,14PB/RnP37/A4wNYkIPy1b9wv/rRV7J+4CC44MEOBhSf/u7TG9C/AE7YP8H+01tu+QBDBD46Cg')]
    [TestCase('126', 'horse.webp,claWhite,TileStretch,0,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,2GOB/RfiA0AA6AA4oIPx0xOBv/XpW/L/4yjkwAIbKDBv/fICvM+D+ALg//8QzSOuOBtObK078g')]
    [TestCase('127', 'horse.webp,claWhite,Tile,0,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,14PB/RnP37/A4wNYkIPy1b9wv/rRV7J+4CC4oIIbKlAP/+wFgI9E/gjg/0AQtHOuuBZGEL68+A')]
    [TestCase('128', 'horse.webp,claWhite,TileOriginal,0,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,14PB/RnP37/A4wNYkIPy1b9wv/rRV7J+4CC44MEOBhSf/u7TG9C/AE7YP8H+01tu+QBDBD46Cg')]
    [TestCase('129', 'horse.webp,claWhite,TileStretch,0,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,2GOB/RfiA0AA6AA4oIPx0xOBv/XpW/L/4yjkwAIbKDBv/fICvM+D+ALg//8QzSOuOBtObK078g')]
    [TestCase('130', 'horse.webp,claWhite,Tile,0,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,14PB/RnP37/A4wNYkIPy1b9wv/rRV7J+4CC4oIIbKlAP/+wFgI9E/gjg/0AQtHOuuBZGEL68+A')]
    [TestCase('131', 'horse.webp,claWhite,TileOriginal,0,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,14PB/RnP37/A4wNYkIPy1b9wv/rRV7J+4CC44MEOBhSf/u7TG9C/AE7YP8H+01tu+QBDBD46Cg')]
    [TestCase('132', 'horse.webp,claWhite,TileStretch,0,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,2GOB/RfiA0AA6AA4oIPx0xOBv/XpW/L/4yjkwAIbKDBv/fICvM+D+ALg//8QzSOuOBtObK078g')]
    [TestCase('133', 'horse.webp,claWhite,Tile,0.06,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,1H+x/RnP399A4wRMsAPy1begP/vQW4I+3xy44EIbKVgfv+0DhI+E/gjY30AQsXfOOBdKEK48+Q')]
    [TestCase('134', 'horse.webp,claWhite,TileOriginal,0.06,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,1H+x/RnP399A4wRMsAPy1begP/vQW4I+3xy44MEOBhSf/u7TG9C/AE7YP8H+01tu+QBDBD46Cg')]
    [TestCase('135', 'horse.webp,claWhite,TileStretch,0.06,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,1mNxPRji/29A5wIw0MPw1Auh//XoX8L/4iTgAAMbJzx/vfP/w8/D+ALUz/8QyifOuBtSbJ278g')]
    [TestCase('136', 'horse.webp,claWhite,Tile,0.06,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,0nuBPRrP588A4wRI0APx1qegP/vQW1K+3Rq4AAMbJmw//+4AjH9E/gjMn4AQr3fOeBdPDI58+g')]
    [TestCase('137', 'horse.webp,claWhite,TileOriginal,0.06,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,0nuBPRrP588A4wNI0APx1qegP/vQW1K+3Rq44MEOBhSf/u7TG9C/AE7YP8H+01tu+QBDBD46Cg')]
    [TestCase('138', 'horse.webp,claWhite,TileStretch,0.06,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,01tBPRjhB2AA5wEs4MPv1fugP/boX5I/4SHgEMMaJUyfvfT8y8+D+APMnz8RyCfO+BtVaH278w')]
    [TestCase('139', 'horse.webp,claWhite,Tile,0.06,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,0HNhPRrO768A4wNI0EPw15eAP/vRV0K+3Bi88IIaI3xP/+/8k38E/gjIf8AQrXPOOBdRBF58+w')]
    [TestCase('140', 'horse.webp,claWhite,TileOriginal,0.06,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,z3NhPRrO768A4wNI0EPw15eAP/vRV0K+3Bi84MEOBhSf/u7TG9C/AE7YP8H+01tu+QBDBD46Cg')]
    [TestCase('141', 'horse.webp,claWhite,TileStretch,0.06,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,0VMhfRjhD0AA5wEo4APv1ueAP/bpW4I/4CDkAIMaImCvffX5y7+D+ATEfz8RxifOeBtXYF279A')]
    [TestCase('142', 'horse.webp,claWhite,Tile,0.06,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,zm9RfRrO758A4wNI0APw2I9w//rRU0I+3BjA4EIaIoRPP/D6k38E/gjEb8AQrG/OOBdSBE48/A')]
    [TestCase('143', 'horse.webp,claWhite,TileOriginal,0.06,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,zm9BfRrO758A4wNI0MPv2I9w//rRU0I+3BjA4MEOBhSf/u7TG9C/AE7YP8H+01tu+QBDBD46Cg')]
    [TestCase('144', 'horse.webp,claWhite,TileStretch,0.06,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,0E8RvRjgEyAA5wEo4MPu1t9w//XpV4L/3yDo8EIaIWivvfX3y8+D+ATEbz8RxSfOOBtYYE179Q')]
    [TestCase('145', 'horse.webp,claWhite,Tile,0.15,0.15,,Round,False,0.15,0.15,0.85,0.85,1,0.98,14PB/RnP37/A4wNYkIPy1b9wv/rRV7J+4CC4oIIbKlAP/+wFgI9E/gjg/0AQtHOuuBZGEL68+A')]
    [TestCase('146', 'horse.webp,claWhite,TileOriginal,0.15,0.15,,Round,False,0.15,0.15,0.85,0.85,1,0.98,14PB/RnP37/A4wNYkIPy1b9wv/rRV7J+4CC44MEOBhSf/u7TG9C/AE7YP8H+01tu+QBDBD46Cg')]
    [TestCase('147', 'horse.webp,claWhite,TileStretch,0.15,0.15,,Round,False,0.15,0.15,0.85,0.85,1,0.98,2GOB/RfiA0AA6AA4oIPx0xOBv/XpW/L/4yjkwAIbKDBv/fICvM+D+ALg//8QzSOuOBtObK078g')]
    [TestCase('148', 'horse.webp,claWhite,Tile,0.15,0.15,,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,14PB/RnP37/A4wNYkIPy1b9wv/rRV7J+4CC4oIIbKlAP/+wFgI9E/gjg/0AQtHOuuBZGEL68+A')]
    [TestCase('149', 'horse.webp,claWhite,TileOriginal,0.15,0.15,,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,14PB/RnP37/A4wNYkIPy1b9wv/rRV7J+4CC44MEOBhSf/u7TG9C/AE7YP8H+01tu+QBDBD46Cg')]
    [TestCase('150', 'horse.webp,claWhite,TileStretch,0.15,0.15,,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,2GOB/RfiA0AA6AA4oIPx0xOBv/XpW/L/4yjkwAIbKDBv/fICvM+D+ALg//8QzSOuOBtObK078g')]
    [TestCase('151', 'horse.webp,claWhite,Tile,0.15,0.15,,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,14PB/RnP37/A4wNYkIPy1b9wv/rRV7J+4CC4oIIbKlAP/+wFgI9E/gjg/0AQtHOuuBZGEL68+A')]
    [TestCase('152', 'horse.webp,claWhite,TileOriginal,0.15,0.15,,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,14PB/RnP37/A4wNYkIPy1b9wv/rRV7J+4CC44MEOBhSf/u7TG9C/AE7YP8H+01tu+QBDBD46Cg')]
    [TestCase('153', 'horse.webp,claWhite,TileStretch,0.15,0.15,,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,2GOB/RfiA0AA6AA4oIPx0xOBv/XpW/L/4yjkwAIbKDBv/fICvM+D+ALg//8QzSOuOBtObK078g')]
    [TestCase('154', 'horse.webp,claWhite,Tile,0.15,0.15,,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,14PB/RnP37/A4wNYkIPy1b9wv/rRV7J+4CC4oIIbKlAP/+wFgI9E/gjg/0AQtHOuuBZGEL68+A')]
    [TestCase('155', 'horse.webp,claWhite,TileOriginal,0.15,0.15,,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,14PB/RnP37/A4wNYkIPy1b9wv/rRV7J+4CC44MEOBhSf/u7TG9C/AE7YP8H+01tu+QBDBD46Cg')]
    [TestCase('156', 'horse.webp,claWhite,TileStretch,0.15,0.15,,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,2GOB/RfiA0AA6AA4oIPx0xOBv/XpW/L/4yjkwAIbKDBv/fICvM+D+ALg//8QzSOuOBtObK078g')]
    [TestCase('157', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|TopRight|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,0YthvRnQ479A4wRM4MPw15+w//vOX0I+3RqwoIIbKlAP/+wFgI9E/gjMv8AQr3u+eBdNCH68+g')]
    [TestCase('158', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|TopRight|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,0YthvRnQ479A4wRM4MPw15+w//vOX0I+3Rqw4MEOBhSf/u7TG9C/AE7YP8H+01tu+QBDBD46Cg')]
    [TestCase('159', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|TopRight|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,0mshvRfjA1CA5wIsAITv1e/A//bnY4L/4CLcwAIbKDBv/fICvM+D+ALMr38RyCu++BtUZG079A')]
    [TestCase('160', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|TopRight|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,yo/BfBnP708A5AVQEITu3V+Qf/zOTyJ+2he0oIIbKlAP/+wFgI9E/gjAb4ARqXt+OBZR8P07/g')]
    [TestCase('161', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|TopRight|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,yo/BfBnP708A5AVQEITu3V+Qf/zOTyJ+2he04MEOBhSf/u7TG9C/AE7YP8H+01tu+QBDBD46Cg')]
    [TestCase('162', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|TopRight|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,zG+BvBfiD+A/6AIwIETt26+Qf/fmU2L/3R/cwAIbKDBv/fICvM+D+ALAbz8Swi9uOBpXTP169w')]
    [TestCase('163', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|TopRight|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,xY9BPBnQ778/5QdUAMTs4ycQf/zMO1J+2Ra4oIIbKlAP/+wFgI9E/gjEbwASonMe+BNQ2I07AQ')]
    [TestCase('164', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|TopRight|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,xY9BPBnQ778/5QdUAMTs4ycQf/zMO1J+2Ra44MEOBhSf/u7TG9C/AE7YP8H+01tu+QBDBD46Cg')]
    [TestCase('165', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|TopRight|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,xm8BfBfjD0B/6QU0EMTr4XcQf/flP5I/3R/gwAIbKDBv/fICvM+D+ALET38Suy8e+BdVNK06+g')]
    [TestCase('166', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|TopRight|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,wosB/BjR62//5QhY4EPs5RPQfvzLM4J+2Ra8oIIbKlAP/+wFgI9E/gjEbwASnmv+9xJPzG17Ag')]
    [TestCase('167', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|TopRight|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,wo8B/BjR62//5QhY4EPs5RPQfvzLM4J+2Ra84MEOBhSf/u7TG9C/AE7YP8H+01tu+QBDBD46Cg')]
    [TestCase('168', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|TopRight|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,w2vBOxfkD/A+6gY48APr42PQfvfkN7I/3R7kwAIbKDBv/fICvM+D+ALITz8SuCsO+BZUKI06+w')]
    [TestCase('169', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,0YthvRnQ479A4wRM4IPy1b9wv/rRV7J+4CC4oIIbKlAP/+wFgI9E/gjMv8AQr3u+eBdNCH68+g')]
    [TestCase('170', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,0YthvRnQ479A4wRM4IPy1b9wv/rRV7J+4CC44MEOBhSf/u7TG9C/AE7YP8H+01tu+QBDBD46Cg')]
    [TestCase('171', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,0mshvRfjA1CA5wIsAITx0xOBv/XpW/L/4yjkwAIbKDBv/fICvM+D+ALMr38RyCu++BtUZG079A')]
    [TestCase('172', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,yo/BfBnP708A5AVQEITy1b9wv/rRV7J+4CC4oIIbKlAP/+wFgI9E/gjAb4ARqXt+OBZR8P07/g')]
    [TestCase('173', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,yo/BfBnP708A5AVQEITy1b9wv/rRV7J+4CC44MEOBhSf/u7TG9C/AE7YP8H+01tu+QBDBD46Cg')]
    [TestCase('174', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,zG+BvBfiD+A/6AIwIITx0xOBv/XpW/L/4yjkwAIbKDBv/fICvM+D+ALAbz8Swi9uOBpXTP169w')]
    [TestCase('175', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,xY9BPBnQ778/5QdUAITy1b9wv/rRV7J+4CC4oIIbKlAP/+wFgI9E/gjEbwASonMe+BNQ2I07AQ')]
    [TestCase('176', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,xY9BPBnQ778/5QdUAITy1b9wv/rRV7J+4CC44MEOBhSf/u7TG9C/AE7YP8H+01tu+QBDBD46Cg')]
    [TestCase('177', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,xm8BfBfjD0B/6QU0EITx0xOBv/XpW/L/4yjkwAIbKDBv/fICvM+D+ALET38Suy8e+BdVNK06+g')]
    [TestCase('178', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,wosB/BjR62//5QhY4IPy1b9wv/rRV7J+4CC4oIIbKlAP/+wFgI9E/gjEbwASnmv+9xJPzG17Ag')]
    [TestCase('179', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,wo8B/BjR62//5QhY4IPy1b9wv/rRV7J+4CC44MEOBhSf/u7TG9C/AE7YP8H+01tu+QBDBD46Cg')]
    [TestCase('180', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,w2vBOxfkD/A+6gY48IPx0xOBv/XpW/L/4yjkwAIbKDBv/fICvM+D+ALITz8SuCsO+BZUKI06+w')]
    [TestCase('181', 'horse.webp,claWhite,Tile,0,0,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,14PB/RnP37/A4wNYkIPy1b9wv/rRV7J+4CC4oIIbKlAP/+wFgI9E/gjg/0AQtHOuuBZGEL68+A')]
    [TestCase('182', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,14PB/RnP37/A4wNYkIPy1b9wv/rRV7J+4CC44MEOBhSf/u7TG9C/AE7YP8H+01tu+QBDBD46Cg')]
    [TestCase('183', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,2GOB/RfiA0AA6AA4oIPx0xOBv/XpW/L/4yjkwAIbKDBv/fICvM+D+ALg//8QzSOuOBtObK078g')]
    [TestCase('184', 'horse.webp,claWhite,Tile,0,0,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,14PB/RnP37/A4wNYkIPy1b9wv/rRV7J+4CC4oIIbKlAP/+wFgI9E/gjg/0AQtHOuuBZGEL68+A')]
    [TestCase('185', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,14PB/RnP37/A4wNYkIPy1b9wv/rRV7J+4CC44MEOBhSf/u7TG9C/AE7YP8H+01tu+QBDBD46Cg')]
    [TestCase('186', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,2GOB/RfiA0AA6AA4oIPx0xOBv/XpW/L/4yjkwAIbKDBv/fICvM+D+ALg//8QzSOuOBtObK078g')]
    [TestCase('187', 'horse.webp,claWhite,Tile,0,0,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,14PB/RnP37/A4wNYkIPy1b9wv/rRV7J+4CC4oIIbKlAP/+wFgI9E/gjg/0AQtHOuuBZGEL68+A')]
    [TestCase('188', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,14PB/RnP37/A4wNYkIPy1b9wv/rRV7J+4CC44MEOBhSf/u7TG9C/AE7YP8H+01tu+QBDBD46Cg')]
    [TestCase('189', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,2GOB/RfiA0AA6AA4oIPx0xOBv/XpW/L/4yjkwAIbKDBv/fICvM+D+ALg//8QzSOuOBtObK078g')]
    [TestCase('190', 'horse.webp,claWhite,Tile,0,0,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,14PB/RnP37/A4wNYkIPy1b9wv/rRV7J+4CC4oIIbKlAP/+wFgI9E/gjg/0AQtHOuuBZGEL68+A')]
    [TestCase('191', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,14PB/RnP37/A4wNYkIPy1b9wv/rRV7J+4CC44MEOBhSf/u7TG9C/AE7YP8H+01tu+QBDBD46Cg')]
    [TestCase('192', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,2GOB/RfiA0AA6AA4oIPx0xOBv/XpW/L/4yjkwAIbKDBv/fICvM+D+ALg//8QzSOuOBtObK078g')]
    [TestCase('193', 'horse.webp,claWhite,Tile,0.5,0.5,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,rbPRuB3j+9+95vpHUMPk379fvP3F/zHA2ApYscEWFjT/f+/Mj19E+ivMXwEJpIuetws/cC07Ag')]
    [TestCase('194', 'horse.webp,claWhite,TileOriginal,0.5,0.5,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,rbPRuB3j+9+95vpHUMPk379fvP3F/zHA2ApYYQEPA+zOfu7S/19A/1Wsj8H73W/ueAI3yL25Bw')]
    [TestCase('195', 'horse.webp,claWhite,TileStretch,0.5,0.5,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,sJNxOBz2H1D96vYjUEPk3AuAvPne/0EB3BR0wcEVEhDP/vLNr2+D9SjQ378KvlfedxBD5Kx6+w')]
    [TestCase('196', 'horse.webp,claWhite,Tile,0.5,0.5,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,sgsiuBPEky/93sZzP8bh5VtPPQLUb2H+0gCcUQAixFNeAvS5Q7BD/UE0T8HuyJ8eOAFCKC07/g')]
    [TestCase('197', 'horse.webp,claWhite,TileOriginal,0.5,0.5,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,sgsiuBPEky/93sZzP8bh5VtPPQLUb2H+0gCcsYAewWv+AO+2G6DAAFY8L0Ps58fONwMyeH06Ag')]
    [TestCase('198', 'horse.webp,claWhite,TileStretch,0.5,0.5,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,uf/hNxHWu2/84sdbb8bg3Z/fPf/sd0H/1A64IUAgv0POQfm2R9CC+zxMD8Dz2Y/+twU3vHy7+w')]
    [TestCase('199', 'horse.webp,claWhite,Tile,0.5,0.5,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,0d9RuP3Sc//+6binT8f3LER/ff/EtyD+3/Vf8T4ooMtuQe/ga28//zcILwTo2dseuQMwrP05CA')]
    [TestCase('200', 'horse.webp,claWhite,TileOriginal,0.5,0.5,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,0d9RuP3Sc//+6binT8f3LER/ff/EtyD+3/VfUT8mn9PeQOzcY58+AUMM/4Tn2/fOOAQt2N15CA')]
    [TestCase('201', 'horse.webp,claWhite,TileStretch,0.5,0.5,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,1tdRePvch69+7LGnTwf1KGgPPv7Tx3D+3/9z4f4nmL9OAfPdY4++/TIwX8Pt4+/OOAcnUI16Bw')]
    [TestCase('202', 'horse.webp,claWhite,Tile,0.5,0.5,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,1zMS+PjC/94+9K+Hf0b+YBCP/fyqm6A96ORDET4nmjevvef+Z8+9AkpQb4bn0eu+OQQfIG47DA')]
    [TestCase('203', 'horse.webp,claWhite,TileOriginal,0.5,0.5,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,1zMS+PjC/94+9K+Hf0b+YBCP/fyqm6A96ORDET4nmjevvef+Z8+9AkpQb4bn0eu+OQQfIG47DA')]
    [TestCase('204', 'horse.webp,claWhite,TileStretch,0.5,0.5,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,2jMSuPfIC98+9amLX8b8XBzfvfywn7B95+pfAf4mlCuf/er9W/98AUZwz0Xs1+++eQYX0N17Cw')]
    [TestCase('205', 'horse.webp,claWhite,Tile,0.5,0.5,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,1,0.98,14PBvRnP37/A4wNYkMPk379fvP3F/zHA2AlYscEWFjT/f+/Mj19E+ivg/0AQtHOuuBZGEL68+A')]
    [TestCase('206', 'horse.webp,claWhite,TileOriginal,0.5,0.5,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,1,0.98,14PB/RnP37/A4wNYkMPk379fvP3F/zHA2ApYYQEPA+zOfu7S/19A/1XYP8H+01tu+QBDBD46Cg')]
    [TestCase('207', 'horse.webp,claWhite,TileStretch,0.5,0.5,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,1,0.98,2GeB/RfhA0AA6AA4oEPk3AuAvPne/0EB3BN0wcEVEgzP/vLNr2+D9Sjg//8QzSOuOBtObK078g')]
    [TestCase('208', 'horse.webp,claWhite,Tile,0.5,0.5,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,14uxfRnN29/A4wJYgMPh4ldP/QHWb4G+0v+nUYAhxU9ewvO5Q6CD/UHc/0AQtnOeuBZHEJ68+A')]
    [TestCase('209', 'horse.webp,claWhite,TileOriginal,0.5,0.5,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,14uxfRnN29/A4wFYgMPh4ldP/QHWb4G+0v+nsUAewmsOwe62H5DAAFbUL8H+1Ftu+QBEBC46Cg')]
    [TestCase('210', 'horse.webp,claWhite,TileStretch,0.5,0.5,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,2GuBvRfg/3/A5/8/oMPg2pvf/f7ue1G/1Ay8IcAfwEPOQfm2R8DC+z3c//8QziOeOBtObJ178g')]
    [TestCase('211', 'horse.webp,claWhite,Tile,0.5,0.5,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,3Jeh/RfLw9+A4wBUsIP3HUiPvf/Lw0D+3fd7sT4npMPuQe/hZ29//zPY7wAQvWOuuBdMJJ58+Q')]
    [TestCase('212', 'horse.webp,claWhite,TileOriginal,0.5,0.5,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,3Jeh/RfLw9+A4/9XsIP3HUiPvf/Lw0D+3fd7Mb8lotNOAevcY39+AEPI/8D+2UN+OQFJDD56Cg')]
    [TestCase('213', 'horse.webp,claWhite,TileStretch,0.5,0.5,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,3ndxfRbd43/A5/w70MP0GWwv/v3a06A+3gCIob4mm7euQfPeX4/+/S/c7/8Q1BuOOBxTfK178w')]
    [TestCase('214', 'horse.webp,claWhite,Tile,0.5,0.5,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,37eBfRfHk3/A5AAwsMP8SCDPffy1n9A95O1j0b0nnRPvPuj+Rx9+AUDkbwEPwFveuBdKQP58+Q')]
    [TestCase('215', 'horse.webp,claWhite,TileOriginal,0.5,0.5,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,37eBfRfHk3/A5AAwsMP8SCDPffy1n9A95O1jEX4nnR+/vuX7T7/9AEjUTwH+3C/O+QBHHI56Cg')]
    [TestCase('216', 'horse.webp,claWhite,TileStretch,0.5,0.5,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,4JdR/RXZry/A6P0X0EP7QzBPvvu7o+C94/FzoT0nlgu/Puz8P089AD3sb8AP1QveOBxRlP278w')]
    [TestCase('217', 'horse.webp,claWhite,Tile,0.75,0.75,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,r++h9xvb3x/94+b/P8Th1Kc/fP7Cx4G/1gScAUEY/6e+wO7DBxBF+T+Q/8D/rruedwVCWF17/g')]
    [TestCase('218', 'horse.webp,claWhite,TileOriginal,0.75,0.75,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,r++h9xvb3x/94+b/P8Th1Kc/fP7Cx4G/1gScIcET87dO/+3IF+AA/1mI7wH345duOAMwlN15BA')]
    [TestCase('219', 'horse.webp,claWhite,TileStretch,0.75,0.75,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,s9NB9xrtB3D85+PfT0Thz/OPvPray3GA2Q64McEW+ovOf/PBExCE9Tugbz8Cxo/OdwpB4Cw7+Q')]
    [TestCase('220', 'horse.webp,claWhite,Tile,0.75,0.75,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,sgsiuBPEky/93sZzP8bh5VtPPQLUb2H+0gCcUQAixFNeAvS5Q7BD/UE0T8HuyJ8eOAFCKC07/g')]
    [TestCase('221', 'horse.webp,claWhite,TileOriginal,0.75,0.75,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,sgsiuBPEky/93sZzP8bh5VtPPQLUb2H+0gCcsYAewWv+AO+2G6DAAFY8L0Ps58fONwMyeH06Ag')]
    [TestCase('222', 'horse.webp,claWhite,TileStretch,0.75,0.75,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,uf/hNxHWu2/84sdbb8bg3Z/fPf/sd0H/1A64IUAgv0POQfm2R9CC+zxMD8Dz2Y/+twU3vHy7+w')]
    [TestCase('223', 'horse.webp,claWhite,Tile,0.75,0.75,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,0gdCOPnGI48/86m3X8b7Wgjfffyqg8A95e4z4T0pnxtffuf/Tw9+AElAn8XozPc+OQUkBA67DQ')]
    [TestCase('224', 'horse.webp,claWhite,TileOriginal,0.75,0.75,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,0gdCOPnGI48/86m3X8b7Wgjfffyqg8A95e4zAX4onxsvfub+S689AU5A74XozQMfOQUjFP76DQ')]
    [TestCase('225', 'horse.webp,claWhite,TileStretch,0.75,0.75,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,1QdSOPjLL38/9KS3Pwb6Vhwvfvuvi9B95PNPwX0omQ9P/ur+Rz99/0ZcH8Xs0v8u+QYcuG37DA')]
    [TestCase('226', 'horse.webp,claWhite,Tile,0.75,0.75,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,22sSefvAm15B+Y6Pz4T7kazevfeLb4B9595DwTwoqGOvO+MikD/7/VeIrwjszzevOAT/b948FA')]
    [TestCase('227', 'horse.webp,claWhite,TileOriginal,0.75,0.75,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,22sSefvAm15B+Y6Pz4T7kazevfeLb4B9595DwTwoqGOvO+MikD/7/VeIrwjszzevOAT/b948FA')]
    [TestCase('228', 'horse.webp,claWhite,TileStretch,0.75,0.75,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,3WsyefrCl04B+o2Xz4T6jrDO/feKa4B9595HkTwnpFOf++UjiJ/6/VKUfwjt0Dvf+AP4Oz49Ew')]
    [TestCase('229', 'horse.webp,claWhite,Tile,0.75,0.75,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,1,0.98,14PBvRnO37/A4wNYkMPh1Kc/PP7Cx5G/1gScAUEY/6e+wO7DAxBF+T/g/0AQtHOuuBZGEL68+A')]
    [TestCase('230', 'horse.webp,claWhite,TileOriginal,0.75,0.75,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,1,0.98,14fBvRnO37/A4wNYkMPh1Kc/PP7Cx5G/1gScMYET87dO/+3IF+AA/1rYP8H+01tu+QBDBC46Cg')]
    [TestCase('231', 'horse.webp,claWhite,TileStretch,0.75,0.75,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,1,0.98,2GeB/Rfh/18A6AA4oEPhzu+PvPray4FA2Q68McEW+ovOf/PBEwCE9Tvg//8QzSOuOBtObK078g')]
    [TestCase('232', 'horse.webp,claWhite,Tile,0.75,0.75,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,14uxfRnN29/A4wJYgMPh4ldP/QHWb4G+0v+nUYAhxU9ewvO5Q6CD/UHc/0AQtnOeuBZHEJ68+A')]
    [TestCase('233', 'horse.webp,claWhite,TileOriginal,0.75,0.75,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,14uxfRnN29/A4wFYgMPh4ldP/QHWb4G+0v+nsUAewmsOwe62H5DAAFbUL8H+1Ftu+QBEBC46Cg')]
    [TestCase('234', 'horse.webp,claWhite,TileStretch,0.75,0.75,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,2GuBvRfg/3/A5/8/oMPg2pvf/f7ue1G/1Ay8IcAfwEPOQfm2R8DC+z3c//8QziOeOBtObJ178g')]
    [TestCase('235', 'horse.webp,claWhite,Tile,0.75,0.75,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,3qeRvRfJp59A5ABAsMP6RRQP/vuzj/D94fRTkf0oogNPv+f/Oz++/0LgP4EPv1/OuBdLNM58+Q')]
    [TestCase('236', 'horse.webp,claWhite,TileOriginal,0.75,0.75,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,3qeRvRfJp59A5ABAsMP6RRQP/vuzj/D94fRT8T0ooQ/vvuT8P5/9/03MH0H+2zeu+QBIFG56Cg')]
    [TestCase('237', 'horse.webp,claWhite,TileStretch,0.75,0.75,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,34dhPRbbw09A6Pwn0AP5QSiPvvq5kxB+4fhjcX0onPcev+v+M2+9/kDkP0AQ1Q++OBxSiN278w')]
    [TestCase('238', 'horse.webp,claWhite,Tile,0.75,0.75,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,4suRvRjIg7+A5fsjcMP5ccw+/veYc9B94+tb0bwoqTfvvOMcXP97/VPk3wEQxGeOOBZBZB59+Q')]
    [TestCase('239', 'horse.webp,claWhite,TileOriginal,0.75,0.75,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,4c+RvRjIg7+A5fsjcMP5ccw+/veYc9B94+tbIX0oqUO/POEaYJ/7/FvUvwH/4Dt+ef8+QL56Cg')]
    [TestCase('240', 'horse.webp,claWhite,TileStretch,0.75,0.75,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,4q9hPRfYo1+A6fgHkIP4bdBuPveZb+D94ulXkbwnpCu/POcdVE/7/E7oD8EQ2ROOuBpHvE188w')]
    [TestCase('241', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('242', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,Round,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('243', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('244', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,Round,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('245', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('246', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,Round,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('247', 'horse.webp,claWhite,Tile,15,15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('248', 'horse.webp,claWhite,Tile,15,15,AllCorners,Round,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('249', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('250', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,Round,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('251', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('252', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,Round,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('253', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('254', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('255', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('256', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('257', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('258', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('259', 'horse.webp,claWhite,Tile,15,15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('260', 'horse.webp,claWhite,Tile,15,15,AllCorners,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('261', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('262', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('263', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('264', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('265', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('266', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('267', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('268', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('269', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('270', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('271', 'horse.webp,claWhite,Tile,15,15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('272', 'horse.webp,claWhite,Tile,15,15,AllCorners,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('273', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('274', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('275', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('276', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('277', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('278', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('279', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('280', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('281', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('282', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('283', 'horse.webp,claWhite,Tile,15,15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('284', 'horse.webp,claWhite,Tile,15,15,AllCorners,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('285', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('286', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('287', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('288', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('289', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('290', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,Round,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('291', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('292', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,Round,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('293', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('294', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,Round,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('295', 'horse.webp,claWhite,Tile,0,0,AllCorners,Round,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('296', 'horse.webp,claWhite,Tile,0,0,AllCorners,Round,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('297', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,Round,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('298', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,Round,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('299', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,Round,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('300', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,Round,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('301', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('302', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('303', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('304', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('305', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('306', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('307', 'horse.webp,claWhite,Tile,0,0,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('308', 'horse.webp,claWhite,Tile,0,0,AllCorners,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('309', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('310', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('311', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('312', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('313', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('314', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('315', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('316', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('317', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('318', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('319', 'horse.webp,claWhite,Tile,0,0,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('320', 'horse.webp,claWhite,Tile,0,0,AllCorners,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('321', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('322', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('323', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('324', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('325', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('326', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('327', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('328', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('329', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('330', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('331', 'horse.webp,claWhite,Tile,0,0,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('332', 'horse.webp,claWhite,Tile,0,0,AllCorners,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('333', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('334', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('335', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('336', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    procedure TestFillRoundRectBitmap(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode; AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType; ASmallSize: Boolean; ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',   'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,False,0,0,1,1,0.85,0.98,wQcugwQNpIBADOgvkHz4jHswfwMtxK8E/Oc/n7zhJ+SPgBDy60A/+9y/nzkH5vsvxPdQ4L+98w')]
    [TestCase('2',   'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,False,0,0,1,1,0.85,0.98,w/cu//Pt+1//EWu3b7nyuMOvO/cG5C9G3d2Tbnzu7xtvPgT1T3E3+Ko///v6vF/vwP9myN196g')]
    [TestCase('3',   'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,False,0,0,1,1,1,0.98,vwOOQwUQpODAC+w3wLz4jX8w/wMvtM+E/eJLb7zhKeC/QBLx55B/+uHTzzkH4/9vBPdS/D/98w')]
    [TestCase('4',   'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,False,0,0,1,1,1,0.98,wRvPvvHq249/Elmr//jxwKcPe/UB4I9G2deDTrzw5+8uvgP0X2G29qU/n7z4sUevQABtiG396A')]
    [TestCase('5',   'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0.85,0.98,5gPegQMSmHC/CeFbcHv7kaug/gMiSBDC8v/7jn/fH+SPAgju74C+/sl33zgK7Tuwgv41uO/A9A')]
    [TestCase('6',   'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0.85,0.98,v8ctAPf/i1D+F5rDf7z6m78PPAT44y8F5a2v/vzkFLTPvQkKQOF4AsJn33n93+svwvlQbM496A')]
    [TestCase('7',   'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,6/vtAQQWmKC/CORnYDv8k7uAfgQhVMCB8v//7v/eIOAPQwjs67B+/sp//7gK60ewwv4z0A8B9Q')]
    [TestCase('8',   'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,vLPtv/X/g1B+GZC3nzz7nqN/uwTx219F4p6j3rzlEqiPPQoNTAH4AsJrL3r72usfQvlTSF595g')]
    [TestCase('9',   'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,False,0,0,1,1,0.85,0.98,wQcugwQNpIBADOgvkPzzkVvgfgImKDDE9/dbrz3gGfgPgAb1CwG+AMZrX/gG+AeAwvxCPE9A8g')]
    [TestCase('10',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,False,0,0,1,1,0.85,0.98,w/cu//Pt+1//EWu3b3n9tYNvO/78Y2+F6bmDDr7mD9S/Pgvpt1D9/7pr/3gH77fPgvk7QC+A7w')]
    [TestCase('11',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,False,0,0,1,1,1,0.98,vwOOQwUQpODAC+w3wLzzklvQfgImLFBE+PVvr/3fGfQvQAb0CyH+AMdvX/gG9wuQwvxDPE+A8g')]
    [TestCase('12',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,False,0,0,1,1,1,0.98,wRvPvvHq249/Elmr/7j+vVu/uv31R7/F561zLn7nDcif/gvmp0D9/7hvH3kH7avfAvk6QB9A7w')]
    [TestCase('13',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0.85,0.98,5gPegQMSmHC/CeFbcHv7kaug/gMiSBDC8v/7/r7gJ/yPgAjsG5E+/tJjr/gE7gOwQvtKnN+/8w')]
    [TestCase('14',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0.85,0.98,v8ctAPf/i1D+F5rDf7z6m78PPAT44y8F5a2vfvzoEMxP/gQH8AC8BMNfH3r/7sefQvtJOI+A7w')]
    [TestCase('15',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,6/vtAQQWmKC/CORnYDv8k7uAfgQhVMCB8v//Tn/gKfi/wAjpH8H+/dVjv7gE6wfAAvtMsM//8w')]
    [TestCase('16',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,vLPtv/X/g1B+GZC3nzz7nqN/uwTx219F4p6jXjzqD8AfvgQK7MB7BcNfb/r967+fAvtKOJ8A7w')]
    [TestCase('17',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,False,0,0,1,1,0.85,0.98,59uNQ/8d2EAAC9H/3/z8ewex/wc5BKAD+fYzX//jEOzPQBIA6PA8+rSvf3YJENyvQgM3BF8B7Q')]
    [TestCase('18',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,False,0,0,1,1,0.85,0.98,3N8+f/PsD0BAEmeDr/r1tN8PPPoFJKDF3N3Hbvzq6g/f/QMKxEF19KU7r7r81IuPfwBXVG196Q')]
    [TestCase('19',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,False,0,0,1,1,1,0.98,68/tA/8j4JBACtH/D/39eCfBPwk8AKAD+vQ/rz/kD+j/QBQB5NA8+bK/L/YJE9ivQgQ1/I5B7A')]
    [TestCase('20',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,False,0,0,1,1,1,0.98,3v/ePvHo868AE1Vvf/r1vMd/+/gAJPCF2Ne/Przs4eOefQMM6NFz8qA7D7v6zXsP/wBaAO385w')]
    [TestCase('21',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0.85,0.98,5+edAQIitCA/Dd8/gHv6kPOgfv82TBDD9QYcrz/fHswfAQ3rH+H9+cV3D7gKAvSvQgI7LA8A7w')]
    [TestCase('22',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0.85,0.98,xrs9QPcAvFB+GpSnr3z6o9f/OwT5/7/F5KmbnjzlDMDffAsMYEF5AMczj7n+9rvvwPpJZP695w')]
    [TestCase('23',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,7NutQQIpuED/DOJLcPv6kguBPv85WPAC9gYkD4DeH8RvAQ7pJ/H9+MV//3cLBPS/QgM5LP+/7g')]
    [TestCase('24',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,xKsNgPUBwEB+HImX3/z6qL9fuwTy+w8G4pmLbvzlCbBfPAwQcIF4AMgvvzn99a+fgPpKQJ795Q')]
    [TestCase('25',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,True,0,0,1,1,0.85,0.98,KtiNw/w4vKB9Dv+b/7sEdz9QvgMcQP9G/cE/73/iLwBgAQ34y+D+/bWj/rkHGfgvgQgYFN+C6g')]
    [TestCase('26',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,True,0,0,1,1,0.85,0.98,5OceAPH0A7C/F2lrn7r1suufe/QMyF9H4NIPr7zt9Rs/vwEIgIH195zXrrr3ydPv/wFTDI2+5A')]
    [TestCase('27',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,True,0,0,1,1,1,0.98,OsztA/xCwIA9DgeE/zsHdDsgPgQbHJ/H/rVLX4DiMvzPAQ74vyB//bKDTroHH/z/gAoREF8D6Q')]
    [TestCase('28',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,True,0,0,1,1,1,0.98,6Affv+7z5+8/GVdPX3r1udP/+vEHvP/H3MoTj/zv7+8uvwAKmCF09pXDDvv0wM9v/wJWqDx+4g')]
    [TestCase('29',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,0.85,0.98,7w/uwP8cmOC9DQDsP3r6kSPBPv878F+D+xMUv33iEdgvAAftSzH9+7sXD3kGAMR/Av1POM+/7w')]
    [TestCase('30',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,0.85,0.98,yAPeP/j8s3D+HpGDPzz7ruMPPAX5298F56V7TvvmCNS//AgLYOH4AcT3rvr+95tvQftbhB6+6A')]
    [TestCase('31',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,1,0.98,9QvewP8hlNB9DQjo/zn7k0ehvv4/7E/D/BYcz73iD8xPAAfrVyE9+7oLLzkGAbxvAv1RPL8/7w')]
    [TestCase('32',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,1,0.98,x/+dv/b8t3C+IYZrTzz8tc9/+wXy0y/G5JVn7vrnBMw/PAkNcAF4AsTnDnv994svAftgZK795g')]
    [TestCase('33',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,True,0,0,1,1,0.85,0.98,KtiNw/w4vKB9Dv+b/7sEdz9QvgMcQP9G/cE773/iLwBgAQ34y+D+/bWj/rkHGfgvgQgYFN+C6g')]
    [TestCase('34',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,True,0,0,1,1,0.85,0.98,5OceAPH0A7C/F2prn7r1seuve/QMyF9H4NIPr7zt9Rs/vwEIgJH195zXrrr3ydPv/wFTDI2+5A')]
    [TestCase('35',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,True,0,0,1,1,1,0.98,OsztA/xCwHA9DgeE/zsHdDsgPgQbHJ/H/rVLX4DiMvzPAQ74vyB//bKDTroHH/z/gAoREF8D6Q')]
    [TestCase('36',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,True,0,0,1,1,1,0.98,6Affv+7z5+8/GVdPX3r1udP/+vEHvP/H3MoTj7zv7+8uvwAKmCF09pXDDvv0wM9v/wJWqDx+4g')]
    [TestCase('37',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,0.85,0.98,7w/uwP8cmOC9DQDsP3r6kSPBPv878F+D+xMUv33iEdgvAAftSzH9+7sXD3kGAMR/Av1POM+/7w')]
    [TestCase('38',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,0.85,0.98,yAPeP/j8s3D+HpGDPzz7ruMPPAX6298F56V/TvvmCNS//AgLYOH4AcT3rvr+95tvQftbhB6+6A')]
    [TestCase('39',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,1,0.98,9QvewP8hlNB9DQjo/zn7k0ehvv4/7E/D/BYcz73iD8xPAAfrVyE9+7oLLzkGAbxvAv1RPL8/7w')]
    [TestCase('40',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,1,0.98,x/+dv/b8t3C+IYZrTzz8tc9/+wXy0y/G5JVn7vrnBMw/PAkNcAF4AsTnDnv994svAftgZK795g')]
    [TestCase('41',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,True,0,0,1,1,0.85,0.98,59uNQ/8d2EAAC9H/3/z8ewex/wc5BKAD+fYzX//jEOzPQBIA6PA8+rSvf3YJENyvQgM3BF8B7Q')]
    [TestCase('42',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,True,0,0,1,1,0.85,0.98,3N8+f/PsD0BAEmeDr/r1tN8PPPoFJKDF3N3Hbvzq6g/f/QMKxEF19KU/r7r81IuPfwBXVG196Q')]
    [TestCase('43',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,True,0,0,1,1,1,0.98,68/tA/8j4JBACtH/D/39eCfBPwk8AKAD+vQ/rz/kD+j/QBQB5NA8+bK/L/YJE9ivQgQ1/I5B7A')]
    [TestCase('44',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,True,0,0,1,1,1,0.98,3v/ePvHo868AE1Vvf/r1vMd/+/gAJPCF2Ne/Przs4eOefQMM6NFz8qA7D7v6zXsP/wBaAO385w')]
    [TestCase('45',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,0.85,0.98,5+edAQIitCA/Dd8/gHv6kPOgfv82TBDD9QYcrz/fHswfAQ3rH+H9+cR3D7gKAvSvQgI7MA8A7w')]
    [TestCase('46',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,0.85,0.98,xrs9QPcAvFB+GpSnr3z6o9f/OwT5/7/F5KmbnjzlDMDffAsMYEF5AMczj7n+9rvvwPpJZP695w')]
    [TestCase('47',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,1,0.98,7NutQQIpuED/DOJLcPv6kguBPv85WPAC9gYkD4DeH8RvAQ7pJ+H9+MV7/3cLBPS/QgM5LP+/7g')]
    [TestCase('48',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,1,0.98,xKsNgPUBwEB+HImX3/z6qL9vuwTy+w8G4pmLbvzlCbBfPAwQcIF4AMgvv/n89bOfgPpKQJ795Q')]
    [TestCase('49',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,0.85,0.98,wgsuAwQMqJAADOgzoPz3jHMwfwMvyK9E++k/n7zgJfBvwBDz31C/+93Dn3kH5P8vxPdQ5N999A')]
    [TestCase('50',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,0.85,0.98,v+O+PvTw23+/E2+Pf/nyq8vPe/cG3G+G4M6bDnzs7kNfvgT2O7F3+rFvP3z6tFs/Qf9j6A0+7A')]
    [TestCase('51',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,1,0.98,vwd+gwQPrPCAC+s30Dz4jXcwvwMxvN9E/ORPf7zgJ+ifgBLy26D/+uLXzzkH4P9/BPdTBHC99A')]
    [TestCase('52',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,1,0.98,vAM//vHtu58/FF53HznysrM/e/UB0O8G3cWL3nvu5h8vPgT2Q7G2+a5z7/z3qEP/wP9prK096w')]
    [TestCase('53',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0.85,0.98,5//NgQMSmHD/CeBbgDv7kqeA/gMhRACC8v77jj/fH+ifAgju73C+/slzzzgK7Tuwwv41uO+A9A')]
    [TestCase('54',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0.85,0.98,wMONwPb/h2D+FpvLn3z7l9M/PAT64x8F5rGrnjzlFLzP/QcOPBH5AsVfz3n84vsvwvlNbN495w')]
    [TestCase('55',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,6/fdAQQVmKA/CeJncDv8lLNgfgQhUMBB8v7/7r/eIOAfQwjr66B+/sp737gK60vAAv8zzA8B9Q')]
    [TestCase('56',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,vq9tAPX/g2A+GJG/vzz8mruvewTz21+F46OffvzlEqx/PQgSSFF4A8VjD3r63f8fQvlPTG495Q')]
    [TestCase('57',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,0.85,0.98,wgsuAwQMqJAADOgzoPzzkVvgfgImKDDE9/dbrz3gGfgPgAb1CwG+AMZrX/gG+AeAwvxCPE9A8g')]
    [TestCase('58',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,0.85,0.98,v+O+PvTw23+/E3CPf3n9tYNvO/78Y2+F6bmDDr7mD9S/Pgvpt1D9/7pr/3gH77fPgvk7QC+A7w')]
    [TestCase('59',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,1,0.98,vwd+gwQPrPCAC+s30LzzklvQfgImLFBE+PVvr/3fGfQvQAb0CyH+AMdvX/gG9wuQwvxDPE+A8g')]
    [TestCase('60',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,1,0.98,vAM//vHtu58/FF53H7n+vVu/uv31R7/F561zLn7nDcif/gvmp0D9/7hvH3kH7avfAvk6QB9A7w')]
    [TestCase('61',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0.85,0.98,5//NgQMSmHD/CeBbgDv7kqeA/gMhRACC8v77/r7gJ/yPgAjsG5E+/tJjr/gE7gOwQvtKnN+/8w')]
    [TestCase('62',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0.85,0.98,wMONwPb/h2D+FpvLn3z7l9M/PAT64x8F5rGrfvzoEMxP/gQH8AC8BMNfH3r/7sefQvtJOI+A7w')]
    [TestCase('63',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,6/fdAQQVmKA/CeJncDv8lLNgfgQhUMBB8v7/Tn/gKfi/wAjpH8H+/dVjv7gE6wfAAvtMsM//8w')]
    [TestCase('64',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,vq9tAPX/g2A+GJG/vzz8mruvewTz21+F46OfXjzqD8AfvgQK7MB7BcNfb/r967+fAvtKOJ8A7w')]
    [TestCase('65',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,0.85,0.98,6N+dA/8d3FCACtAD0Lz8ef+w/wc8ALBD+PU7X//iD/i/QBIA3BB9+rS3n7YJDeC/wgI2CI8B7g')]
    [TestCase('66',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,0.85,0.98,2MvO/vPv62/AE2xbz3r2qOcvfPoGFPDF38/HDjzp6TvffQQKrJH19qxj/zr8zYfPv/9TcJ196w')]
    [TestCase('67',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,1,0.98,7NMNxP4i6KDACdADAP39dxvBPwlAAKAD+fNLr3/jDfjvQBQB2PC8+bHHT/YJENzPwgM0AL9B7Q')]
    [TestCase('68',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,1,0.98,2udevvHsy7+AFFo7nzr2rdOfO/kBFFAG3Ma/znvq4BeP/QMNzCF09ahnbzv6xXNfPwBWIB196g')]
    [TestCase('69',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0.85,0.98,5+edAQIitCB/Dd4/kDv6keuAvv82SBCD9QUcn//eHswvwQzsH+E9+sVz73cKAvivQgI6LA/A7g')]
    [TestCase('70',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0.85,0.98,x7udwPYAuGB+GZavz3z7oOsfPAT7/6+F5a2XTnzlDMDfvAkRWIH5AMkvb7n9+MvvwPpFZB6+5g')]
    [TestCase('71',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,7NudQQIouEA/DeBLgPv6kgNhfv85VPDC9QYk/7/eH8R/wQ3pI/H9+MV73zcLBPy/QgM5LA9A7g')]
    [TestCase('72',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,xqd9QPUBvFA+G4uf7zz8pNePuwT0+w8G456HDjzmCrRfPAoVaMH4AMorn/n7+MefgPpGQL695A')]
    [TestCase('73',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,0.85,0.98,wgteAwQJqMCAC+gzoPz2jnMgPwIs5I/E+e5Hr7zgJOxfQBD032D/+t3Dj/kG4wdAxPdS2O999A')]
    [TestCase('74',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,0.85,0.98,vt+uvvPzz49/FG5/n/nyqscfPPgHyH8G4cefzrvr8VNPPgX4M9G3+7V/P/z6sVt/Qf9i8C0+7Q')]
    [TestCase('75',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,1,0.98,wAOuwwQLrCDBCuw70Pz2j3cQPwIt3K+E+upXj7zgJuh/wBHy17A/+uHXv/kG3wugBPdV9I/99A')]
    [TestCase('76',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,1,0.98,vPsuvvHxq79/FVxnLznysKuPu/YCuP+G3b2Pjnvt6i8P/gT4O8H2+rOD77z4pENPgf9ouM097A')]
    [TestCase('77',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0.85,0.98,5gPOgQMRmGA/Ct9bcDv7k59w/gMgRBCC8v37jv/eH+yPQgjt74C+/shzz3gK7Tegwv42uO/A9A')]
    [TestCase('78',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0.85,0.98,wMPdgPYBhJB+Fp7Pr3z8ledfPAT94x+F5rWrXnzlE7TffQYTPEF5A8dXr/n74v8/QvpLeB6+5g')]
    [TestCase('79',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,6vvdAQQVmJB/CeJnYPv7latQPgQfTMBB8vz77r/eIOQfgwjr66B+/sl37/gK60ewAv800P8A9Q')]
    [TestCase('80',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,vrPNAPUBgJC+F5XH3zz9mNPfuwT2219F5KebLvzlEqiffQYYRIH4A8hX7/n53gMgwvlMWL695A')]
    [TestCase('81',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,0.85,0.98,wgteAwQJrMCAC+gzoPzzkVvgfgImKDDE9/dbrz3gGfgPgAb1CwG+AMZrX/gG+AeAwvxCPE9A8g')]
    [TestCase('82',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,0.85,0.98,vtuuvvPzz49/FG5/n3n9tYNvO/78Y2+F6bmDDr7mD9S/Pgvpt1D9/7pr/3gH77fPgvk7QC+A7w')]
    [TestCase('83',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,1,0.98,wAOuwwQLrCDBCuw70LzzklvQfgImLFBE+PVvr/3fGfQvQAb0CyH+AMdvX/gG9wuQwvxDPE+A8g')]
    [TestCase('84',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,1,0.98,vPsuvvHwq79/FVxnL7n+vVu/uv31R7/F561zLn7nDcif/gvmp0D9/7hvH3kH7avfAvk6QB9A7w')]
    [TestCase('85',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0.85,0.98,5gPOgQMRmGA/Ct9bcDv7k59w/gMgRBCC8v37/r7gJ/yPgAjsG5E+/tJjr/gE7gOwQvtKnN+/8w')]
    [TestCase('86',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0.85,0.98,wMPdgPYBhJB+Fp7Pr3z8ledfPAT94x+F5rWrfvzoEMxP/gQH8AC8BMNfH3r/7sefQvtJOI+A7w')]
    [TestCase('87',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,6vvdAQQVmJB/CeJnYPv7latQPgQfTMAB8vz7Tn/gKfi/wAjpH8H+/dVjv7gE6wfAAvtMsM//8w')]
    [TestCase('88',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,vrPNAPUBgJB+F5XH3zz9mNPfuwT2219F5KebXjzqD8AfvgQK7MB7BcNfb/r967+fAvtKOJ8A7w')]
    [TestCase('89',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,0.85,0.98,6dvdg/8b3IDACdAH0Pz7fPug/wY6GJDD9vlHbz/jDvivwBEA2BA9+rO3n3YJC+TfggI3AI8B7g')]
    [TestCase('90',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,0.85,0.98,18PevvPy34+AFGpL73r2puNvfPsHABBG4MfLzjvo7EfP/QQMqKE1+K9zD7v8yoMPgP9SeK197A')]
    [TestCase('91',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,1,0.98,7c9NRP8g5ODACNAHAL38ehex/wc+GJBD9/dXz3/jDPTfgBMC1AA9+bHLT/YJDuTvggM1+M6B7Q')]
    [TestCase('92',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,1,0.98,2dtufvHwu99AFVgrv3r2q8/ve/oC/I+G3L3Dfnvp5CN/vQQPxEG09qx3j7v6wm+fPwBVLD196w')]
    [TestCase('93',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0.85,0.98,5+eNAQIhtBD/Dd0/gDv6kedwvv81SCCD9QQcj//eH8wvgQzsH+E9+sVz33cKAvy/QgI6LB/A7g')]
    [TestCase('94',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0.85,0.98,x7vtwPYCuID+GJm33zz8nv9PfAT+/69F5rGX/nvlDLzvfAgWWKE5AcsrTzn9+df/APtEbE4+5g')]
    [TestCase('95',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,69udQQInuCB/Dd9HcLv6k/9QPv84VADD9QUk73/eH8B/QQ3qI/E9+cZ7z/cKBACwAgM5LB8A7g')]
    [TestCase('96',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,xqfdAPUDvIC+Go6nDz39ou+/+wT3+w/G46KHzjvmCqx/vAgaaPF4Acwjf3n7+dO/wPpESO794w')]
    [TestCase('97',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,0.85,0.98,xP9dQwQIpNAAC+kzoDz2j3MQPwEp+G8E+fFL3/zgJPA/AA/z31A/+tzDf7kG4g9wxPdTzP+99A')]
    [TestCase('98',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,0.85,0.98,v9vOvvP0y5+/FG17n7nyq8MvfPgFwG+G4Mefzjvr81MfPgX4M9H3+7eDL3z7sGOfAf9j6D2+7Q')]
    [TestCase('99',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,1,0.98,wvu9AwUJpDBBCuw7wDz2kXcQ/wAq8H/E+e5bv/zgJuhfgBDy16A/+eDXr7kG3RPQBPdW6I899Q')]
    [TestCase('100', 'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,1,0.98,vPtOfvHyp89/FVtfP/nxsaef+/YAtN8G3b2Pfjvt7DPfvQT4O8E2+7SL3zz5o0t/gf9osN397A')]
    [TestCase('101', 'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0.85,0.98,5gO+gQMQmFB/Ct9XcPv6lJtgvgMfRCCC8vz7jj/fHvCPQgjs74D+/sdv33gK7TOgwv42vN/A9A')]
    [TestCase('102', 'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0.85,0.98,wMf9gPYBhKB+Fp/Tv7z8lutvPAT/4x8F57WnPnzlE7TvPQYVOGG5A8hTr/n74v8/gvpLgD7+5g')]
    [TestCase('103', 'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,6vvNAQQUmIC/CeFnULv7lqdA/gMeTNBB8vv7/r/eH+wPgwjp67C+/sh37/gK6j+gAv801P9A9Q')]
    [TestCase('104', 'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,vbPtAPUCgKC+F5bL37z9mNfvuwT421+F5KibDjzmEaSv/QUaRKF4BMlT7/n53gMgAvpMYN795A')]
    [TestCase('105', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,0.85,0.98,xP9dQwQIpNAAC+kzoPzzkVvgfgImKDDE9/dbrz3gGfgPgAb1CwG+AMZrX/gG+AeAwvxCPE9A8g')]
    [TestCase('106', 'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,0.85,0.98,v9vOvvP0y5+/FG17n3n9tYNvO/78Y2+F6bmDDr7mD9S/Pgvpt1D9/7pr/3gH77fPgvk7QC+A7w')]
    [TestCase('107', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,1,0.98,wvu9AwUJpDBBCu07wLzzklvQfgImLFBE+PVvr/3fGfQvQAb0CyH+AMdvX/gG9wuQwvxDPE+A8g')]
    [TestCase('108', 'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,1,0.98,vPtOfvHyp89/FVtfP7n+vVu/uv31R7/F561zLn7nDcif/gvmp0D9/7hvH3kH7avfAvk6QB9A7w')]
    [TestCase('109', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0.85,0.98,5gO+gQMQmGB/Ct9XcPv6lJtgvgMfRCCC8vz7/r7gJ/yPgAjsG5E+/tJjr/gE7gOwQvtKnN+/8w')]
    [TestCase('110', 'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0.85,0.98,wMf9gPYBhKB+Fp/Tv7z8lutvPAT/4x8F57WnfvzoEMxP/gQH8AC8BMNfH3r/7sefQvtJOI+A7w')]
    [TestCase('111', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,6vvNAQQUmIC/CeFnULv7lqdA/gMeTNBB8vv7Tn/gKfi/wAjpH8H+/dVjv7gE6wfAAvtMsM//8w')]
    [TestCase('112', 'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,vbPtAPUCgKC+F5bL37z9mNfvuwT421+F5KibXjzqD8AfvgQK7MB7BcNfb/r967+fAvtKOJ8A7w')]
    [TestCase('113', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,0.85,0.98,6tPtw/8a1JBACdEL0Dz7ffeQPwY4JHAD9vxLjz/jDvifwBAA2BB9+bO3jzYJCuz/ggI5+J5B7g')]
    [TestCase('114', 'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,0.85,0.98,2L/+vvP024+AFGlH7zr2ptt/vPsG+B/G38fLvvvn7Uev/QQMqKE1+LF3D/v8yYcfgP9TdL397A')]
    [TestCase('115', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,1,0.98,78NdhP8f3PAACNELAD38exOh/wY7KGBD9vpb77/jDPTPgBIB0PA8+LDLP3YJDOwPgwM37N6B7Q')]
    [TestCase('116', 'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,1,0.98,2teOfvHyt++AFVcnzzr2rMf/u/oB+H8G3L3Hfjvp5SdffQQPxEH09q57fzv7wXevPwBWJE396w')]
    [TestCase('117', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0.85,0.98,5+eNAQIguAA/Dt0/cPv5kuNgfv80SCCD9QQYj//eHswvgQzsG+E9+sVz33cKAvyvQgI6MB/A7g')]
    [TestCase('118', 'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0.85,0.98,x78NwfYCuJD+GJq373z8ngdAfAT//6+F5rGT7nvlDLj//AcXWLF5AcwrTzn9+tf/QPtEcF4+5g')]
    [TestCase('119', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,69uNQQImvBD/Dd9HYHv6lPdAPv83UBDD9QQg73/eH8R/QQ3qI/E9+cZ7z/cKBACwQgM5MB8A7g')]
    [TestCase('120', 'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,xqv9APUDvJC+GpCnH339ovfP+wT5+w8G5KODrjvmCaiPPAgcaAG5Ac0jb3n7+tO/APtETP794w')]
    [TestCase('121', 'horse.webp,claWhite,Tile,0,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,6wsuwgMXoJD/B+VjQDv9ksegfgQhULAB9AP4vj/fIuDvwgju5/A+/8qH/zgK7U+ggv4xyA9B9A')]
    [TestCase('122', 'horse.webp,claWhite,TileOriginal,0,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,6wsuwgMXoJD/B+VjQDv9ksegfgQhULAB9AP4Tn/gKfi/wAjpH8H+/dVjv7gE6wfAAvtMsM//8w')]
    [TestCase('123', 'horse.webp,claWhite,TileStretch,0,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,7OvtwQEqwCD/C+NDUPv7kRehPv86WPCC9wsg3//eH8BfwQ7qJyF++cSD7/cKBvyvQgM5JP//7Q')]
    [TestCase('124', 'horse.webp,claWhite,Tile,0,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,6wsuwgMXoJD/B+VjQDv9ksegfgQhULAB9AP4vj/fIuDvwgju5/A+/8qH/zgK7U+ggv4xyA9B9A')]
    [TestCase('125', 'horse.webp,claWhite,TileOriginal,0,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,6wsuwgMXoJD/B+VjQDv9ksegfgQhULAB9AP4Tn/gKfi/wAjpH8H+/dVjv7gE6wfAAvtMsM//8w')]
    [TestCase('126', 'horse.webp,claWhite,TileStretch,0,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,7OvtwQEqwCD/C+NDUPv7kRehPv86WPCC9wsg3//eH8BfwQ7qJyF++cSD7/cKBvyvQgM5JP//7Q')]
    [TestCase('127', 'horse.webp,claWhite,Tile,0,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,6wsuwgMXoJD/B+VjQDv9ksegfgQhULAB9AP4vj/fIuDvwgju5/A+/8qH/zgK7U+ggv4xyA9B9A')]
    [TestCase('128', 'horse.webp,claWhite,TileOriginal,0,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,6wsuwgMXoJD/B+VjQDv9ksegfgQhULAB9AP4Tn/gKfi/wAjpH8H+/dVjv7gE6wfAAvtMsM//8w')]
    [TestCase('129', 'horse.webp,claWhite,TileStretch,0,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,7OvtwQEqwCD/C+NDUPv7kRehPv86WPCC9wsg3//eH8BfwQ7qJyF++cSD7/cKBvyvQgM5JP//7Q')]
    [TestCase('130', 'horse.webp,claWhite,Tile,0,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,6wsuwgMXoJD/B+VjQDv9ksegfgQhULAB9AP4vj/fIuDvwgju5/A+/8qH/zgK7U+ggv4xyA9B9A')]
    [TestCase('131', 'horse.webp,claWhite,TileOriginal,0,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,6wsuwgMXoJD/B+VjQDv9ksegfgQhULAB9AP4Tn/gKfi/wAjpH8H+/dVjv7gE6wfAAvtMsM//8w')]
    [TestCase('132', 'horse.webp,claWhite,TileStretch,0,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,7OvtwQEqwCD/C+NDUPv7kRehPv86WPCC9wsg3//eH8BfwQ7qJyF++cSD7/cKBvyvQgM5JP//7Q')]
    [TestCase('133', 'horse.webp,claWhite,Tile,0.06,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,6/8NwgMWmKA/COVjULv8ksOQfgQiVMBB8wH43j/fIdz/Qgjt69C+/sqD/3gK7Euwwv4yzA/B9A')]
    [TestCase('134', 'horse.webp,claWhite,TileOriginal,0.06,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,6/8NwgMWmKA/COVjULv8ksOQfgQiVMBB8wH4Tn/gKfi/wAjpH8H+/dVjv7gE6wfAAvtMsM//8w')]
    [TestCase('135', 'horse.webp,claWhite,TileStretch,0.06,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,7OPNAQIpuEB/DONHYHv7kBOhPv86WPDC9gkk///eH8BfQQ7pJwH++MR//zcLBfi/QgM5KP9/7g')]
    [TestCase('136', 'horse.webp,claWhite,Tile,0.06,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,6//9AQQWmKC/CORnYHv8k7uAfgQhUMDB8v/77j/fIdwPQwjs67C+/sp//7gK60ewwv4y0A8B9Q')]
    [TestCase('137', 'horse.webp,claWhite,TileOriginal,0.06,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,6//9AQQWmKC/CORnYHv8k7uAfgQhUMDB8v/7Tn/gKfi/wAjpH8H+/dVjv7gE6wfAAvtMsM//8w')]
    [TestCase('138', 'horse.webp,claWhite,TileStretch,0.06,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,7N+9QQIpuEC/DOJHcDv7kQuBPv85WABD9gckD8DeH8BvAQ7pI/H9+MV//zcLBPi/AgM5LP+/7g')]
    [TestCase('139', 'horse.webp,claWhite,Tile,0.06,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,6/vtAQQWmKC/CONnYDv8k7dwfgQhUMCB8v/7/j/fIeAPQwjs56C+/st///gK6kewwv4z0A9B9Q')]
    [TestCase('140', 'horse.webp,claWhite,TileOriginal,0.06,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,6/vtAQQWmKC/CONnYDv8k7dwfgQhUMCB8v/7Tn/gKfi/wAjpH8H+/dVjv7gE6wfAAvtMsM//8w')]
    [TestCase('141', 'horse.webp,claWhite,TileStretch,0.06,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,7N+tQQIpuED/DOFLgPv6kgdxfv85VPAC9gckD8DeIMBvwQ3pH/E9+cZ//zcLA/i/AgM5LA+A7g')]
    [TestCase('142', 'horse.webp,claWhite,Tile,0.06,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,6/vtAQQWmKD/CONnYDv8k7dwfgQhUMCB8v/7/v/eIeAPQwjs56C+/st///gK6kewwv4z0A9B9Q')]
    [TestCase('143', 'horse.webp,claWhite,TileOriginal,0.06,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,6/vtAQQWmKD/CONnYDv8k7dwfgQhUMCB8v/7Tn/gKfi/wAjpH8H+/dVjv7gE6wfAAvtMsM//8w')]
    [TestCase('144', 'horse.webp,claWhite,TileStretch,0.06,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,7NutQQIpuED/DOFLgPv6kgdxfv85VPAC9gcgD8DeIMBvwQ3pH/E9+cZ//zcLA/i/wgI4MA+A7g')]
    [TestCase('145', 'horse.webp,claWhite,Tile,0.15,0.15,,Round,False,0.15,0.15,0.85,0.85,1,0.98,6wsuwgMXoJD/B+VjQDv9ksegfgQhULAB9AP4vj/fIuDvwgju5/A+/8qH/zgK7U+ggv4xyA9B9A')]
    [TestCase('146', 'horse.webp,claWhite,TileOriginal,0.15,0.15,,Round,False,0.15,0.15,0.85,0.85,1,0.98,6wsuwgMXoJD/B+VjQDv9ksegfgQhULAB9AP4Tn/gKfi/wAjpH8H+/dVjv7gE6wfAAvtMsM//8w')]
    [TestCase('147', 'horse.webp,claWhite,TileStretch,0.15,0.15,,Round,False,0.15,0.15,0.85,0.85,1,0.98,7OvtwQEqwCD/C+NDUPv7kRehPv86WPCC9wsg3//eH8BfwQ7qJyF++cSD7/cKBvyvQgM5JP//7Q')]
    [TestCase('148', 'horse.webp,claWhite,Tile,0.15,0.15,,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,6wsuwgMXoJD/B+VjQDv9ksegfgQhULAB9AP4vj/fIuDvwgju5/A+/8qH/zgK7U+ggv4xyA9B9A')]
    [TestCase('149', 'horse.webp,claWhite,TileOriginal,0.15,0.15,,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,6wsuwgMXoJD/B+VjQDv9ksegfgQhULAB9AP4Tn/gKfi/wAjpH8H+/dVjv7gE6wfAAvtMsM//8w')]
    [TestCase('150', 'horse.webp,claWhite,TileStretch,0.15,0.15,,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,7OvtwQEqwCD/C+NDUPv7kRehPv86WPCC9wsg3//eH8BfwQ7qJyF++cSD7/cKBvyvQgM5JP//7Q')]
    [TestCase('151', 'horse.webp,claWhite,Tile,0.15,0.15,,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,6wsuwgMXoJD/B+VjQDv9ksegfgQhULAB9AP4vj/fIuDvwgju5/A+/8qH/zgK7U+ggv4xyA9B9A')]
    [TestCase('152', 'horse.webp,claWhite,TileOriginal,0.15,0.15,,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,6wsuwgMXoJD/B+VjQDv9ksegfgQhULAB9AP4Tn/gKfi/wAjpH8H+/dVjv7gE6wfAAvtMsM//8w')]
    [TestCase('153', 'horse.webp,claWhite,TileStretch,0.15,0.15,,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,7OvtwQEqwCD/C+NDUPv7kRehPv86WPCC9wsg3//eH8BfwQ7qJyF++cSD7/cKBvyvQgM5JP//7Q')]
    [TestCase('154', 'horse.webp,claWhite,Tile,0.15,0.15,,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,6wsuwgMXoJD/B+VjQDv9ksegfgQhULAB9AP4vj/fIuDvwgju5/A+/8qH/zgK7U+ggv4xyA9B9A')]
    [TestCase('155', 'horse.webp,claWhite,TileOriginal,0.15,0.15,,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,6wsuwgMXoJD/B+VjQDv9ksegfgQhULAB9AP4Tn/gKfi/wAjpH8H+/dVjv7gE6wfAAvtMsM//8w')]
    [TestCase('156', 'horse.webp,claWhite,TileStretch,0.15,0.15,,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,7OvtwQEqwCD/C+NDUPv7kRehPv86WPCC9wsg3//eH8BfwQ7qJyF++cSD7/cKBvyvQgM5JP//7Q')]
    [TestCase('157', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|TopRight|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,6/vtAQQWmKC/CORnYDv8k7uAfgQhVMCB8v//vj/fIuDvwgju5/A+/8p//7gK60ewwv4z0A8B9Q')]
    [TestCase('158', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|TopRight|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,6/vtAQQWmKC/CORnYDv8k7uAfgQhVMCB8v//Tn/gKfi/wAjpH8H+/dVjv7gE6wfAAvtMsM//8w')]
    [TestCase('159', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|TopRight|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,7NutQQIpuED/DOJLcPv6kguBPv85WPAC9gYk3//eH8BfwQ7qJyF++cR//3cLBPS/QgM5LP+/7g')]
    [TestCase('160', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|TopRight|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,6/fdAQQVmKA/CeJncDv8lLNgfgQhUMBB8v7/vj/fIuDvwgju5/A+/8p737gK60vAAv8zzA8B9Q')]
    [TestCase('161', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|TopRight|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,6/fdAQQVmKA/CeJncDv8lLNgfgQhUMBB8v7/Tn/gKfi/wAjpH8H+/dVjv7gE6wfAAvtMsM//8w')]
    [TestCase('162', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|TopRight|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,7NudQQIouEA/DeBLgPv6kgNhfv85VPDC9QYk3//eH8BfwQ7qJyF++cR73zcLBPy/QgM5LA9A7g')]
    [TestCase('163', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|TopRight|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,6vvdAQQVmJB/CeJnYPv7latQPgQfTMBB8vz7vj/fIuDvwgju5/A+/8p37/gK60ewAv800P8A9Q')]
    [TestCase('164', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|TopRight|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,6vvdAQQVmJB/CeJnYPv7latQPgQfTMAB8vz7Tn/gKfi/wAjpH8H+/dVjv7gE6wfAAvtMsM//8w')]
    [TestCase('165', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|TopRight|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,69udQQInuCB/Dd9HcLv6k/9QPv84VADD9QUk3//eH8BfwQ7qJyF++cR7z/cKBACwAgM5LB8A7g')]
    [TestCase('166', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|TopRight|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,6vvNAQQUmIC/CeFnULv7lqdA/gMeTNBB8vv7vj/fIuDvwgju5/A+/8p37/gK6j+gAv801P9A9Q')]
    [TestCase('167', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|TopRight|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,6vvNAQQUmIC/CeFnULv7lqdA/gMeTNBB8vv7Tn/gKfi/wAjpH8H+/dVjv7gE6wfAAvtMsM//8w')]
    [TestCase('168', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|TopRight|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,69uNQQImvBD/Dd9HYHv6lPdAPv83UBDD9QQg3//eH8BfwQ7qJyF++cR7z/cKBACwQgM5MB8A7g')]
    [TestCase('169', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,6/vtAQQWmKC/CORnYDv9ksegfgQhULAB9AP4vj/fIuDvwgju5/A+/8p//7gK60ewwv4z0A8B9Q')]
    [TestCase('170', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,6/vtAQQWmKC/CORnYDv9ksegfgQhULAB9AP4Tn/gKfi/wAjpH8H+/dVjv7gE6wfAAvtMsM//8w')]
    [TestCase('171', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,7NutQQIpuED/DOJLcPv7kRehPv86WPCC9wsg3//eH8BfwQ7qJyF++cR//3cLBPS/QgM5LP+/7g')]
    [TestCase('172', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,6/fdAQQVmKA/CeJncDv9ksegfgQhULAB9AP4vj/fIuDvwgju5/A+/8p737gK60vAAv8zzA8B9Q')]
    [TestCase('173', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,6/fdAQQVmKA/CeJncDv9ksegfgQhULAB9AP4Tn/gKfi/wAjpH8H+/dVjv7gE6wfAAvtMsM//8w')]
    [TestCase('174', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,7NudQQIouEA/DeBLgPv7kRehPv86WPCC9wsg3//eH8BfwQ7qJyF++cR73zcLBPy/QgM5LA9A7g')]
    [TestCase('175', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,6vvdAQQVmJB/CeJnYDv9ksegfgQhULAB9AP4vj/fIuDvwgju5/A+/8p37/gK60ewAv800P8A9Q')]
    [TestCase('176', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,6vvdAQQVmJB/CeJnYDv9ksegfgQhULAB9AP4Tn/gKfi/wAjpH8H+/dVjv7gE6wfAAvtMsM//8w')]
    [TestCase('177', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,69udQQInuCB/Dd9HcPv7kRehPv86WPCC9wsg3//eH8BfwQ7qJyF++cR7z/cKBACwAgM5LB8A7g')]
    [TestCase('178', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,6vvNAQQUmIC/CeFnUDv9ksegfgQhULAB9AP4vj/fIuDvwgju5/A+/8p37/gK6j+gAv801P9A9Q')]
    [TestCase('179', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,6vvNAQQUmIC/CeFnUDv9ksegfgQhULAB9AP4Tn/gKfi/wAjpH8H+/dVjv7gE6wfAAvtMsM//8w')]
    [TestCase('180', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,69uNQQImvBD/Dd9HYPv7kRehPv86WPCC9wsg3//eH8BfwQ7qJyF++cR7z/cKBACwQgM5MB8A7g')]
    [TestCase('181', 'horse.webp,claWhite,Tile,0,0,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,6wsuwgMXoJD/B+VjQDv9ksegfgQhULAB9AP4vj/fIuDvwgju5/A+/8qH/zgK7U+ggv4xyA9B9A')]
    [TestCase('182', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,6wsuwgMXoJD/B+VjQDv9ksegfgQhULAB9AP4Tn/gKfi/wAjpH8H+/dVjv7gE6wfAAvtMsM//8w')]
    [TestCase('183', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,7OvtwQEqwCD/C+NDUPv7kRehPv86WPCC9wsg3//eH8BfwQ7qJyF++cSD7/cKBvyvQgM5JP//7Q')]
    [TestCase('184', 'horse.webp,claWhite,Tile,0,0,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,6wsuwgMXoJD/B+VjQDv9ksegfgQhULAB9AP4vj/fIuDvwgju5/A+/8qH/zgK7U+ggv4xyA9B9A')]
    [TestCase('185', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,6wsuwgMXoJD/B+VjQDv9ksegfgQhULAB9AP4Tn/gKfi/wAjpH8H+/dVjv7gE6wfAAvtMsM//8w')]
    [TestCase('186', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,7OvtwQEqwCD/C+NDUPv7kRehPv86WPCC9wsg3//eH8BfwQ7qJyF++cSD7/cKBvyvQgM5JP//7Q')]
    [TestCase('187', 'horse.webp,claWhite,Tile,0,0,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,6wsuwgMXoJD/B+VjQDv9ksegfgQhULAB9AP4vj/fIuDvwgju5/A+/8qH/zgK7U+ggv4xyA9B9A')]
    [TestCase('188', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,6wsuwgMXoJD/B+VjQDv9ksegfgQhULAB9AP4Tn/gKfi/wAjpH8H+/dVjv7gE6wfAAvtMsM//8w')]
    [TestCase('189', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,7OvtwQEqwCD/C+NDUPv7kRehPv86WPCC9wsg3//eH8BfwQ7qJyF++cSD7/cKBvyvQgM5JP//7Q')]
    [TestCase('190', 'horse.webp,claWhite,Tile,0,0,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,6wsuwgMXoJD/B+VjQDv9ksegfgQhULAB9AP4vj/fIuDvwgju5/A+/8qH/zgK7U+ggv4xyA9B9A')]
    [TestCase('191', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,6wsuwgMXoJD/B+VjQDv9ksegfgQhULAB9AP4Tn/gKfi/wAjpH8H+/dVjv7gE6wfAAvtMsM//8w')]
    [TestCase('192', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,7OvtwQEqwCD/C+NDUPv7kRehPv86WPCC9wsg3//eH8BfwQ7qJyF++cSD7/cKBvyvQgM5JP//7Q')]
    [TestCase('193', 'horse.webp,claWhite,Tile,0.5,0.5,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,3RPOwAIMkPC+C9xDELv4lYMg/gEXPNBC8foH37/fIuTfAQriExH//cp3f7kI6BeQAv00xJ8A9g')]
    [TestCase('194', 'horse.webp,claWhite,TileOriginal,0.5,0.5,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,3RPOwAIMkPC+C9xDELv4lYMg/gEXPNBC8foHTz/gJ/yfAAnpH6G+/dRj73gD6vvfQvpMqL//8w')]
    [TestCase('195', 'horse.webp,claWhite,TileStretch,0.5,0.5,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,4O9tQAEftGD+D9gfEDv4ks9Qvv0xPNCD9AQg/7/eH8CvgA3jMyE++cd//zcKAeDfggE4NC9A7w')]
    [TestCase('196', 'horse.webp,claWhite,Tile,0.5,0.5,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,yg+OQQLzi8B/DNg78Lv3g1OAvgAdHJDD8ffvzj7jFuT/wAjgLwH/+85j73gC8tP/gvtJfM//8Q')]
    [TestCase('197', 'horse.webp,claWhite,TileOriginal,0.5,0.5,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,yg+OQQLzi8B/DNg78Lv3g1OAvgAdHJDD8ffvnj7iG/ifQAjhH5F+/dBDL/kB8uO/gvpOfM9/8g')]
    [TestCase('198', 'horse.webp,claWhite,TileStretch,0.5,0.5,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,0ANOwf8GrAB/ENgfILz2e5MQv/02KGDE8wUIr37hEdhvQA7dLyE++sl7r3cHBMTfAgA/EA9A7w')]
    [TestCase('199', 'horse.webp,claWhite,Tile,0.5,0.5,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,yBN+gQH9n0C/DtYjEHz5hk+wvgEVFMBD8vfv/j3gGvwPQAjsF4E+/spbb/gC78PPwvtFbM9/8g')]
    [TestCase('200', 'horse.webp,claWhite,TileOriginal,0.5,0.5,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,yBN+gQH9n0C/DtYjEHz5hk+wvgEVFMBD8vfv7j3gHQgAwAftD1H+/sxPn7gC7s+PQvtGbL8/8w')]
    [TestCase('201', 'horse.webp,claWhite,TileStretch,0.5,0.5,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,zAt+Qf8HsPD+EM8nAPz2gnNAfwAjJBBE8gIA773fEvTvPwzpD6F9/MWDj7cI+dOPQv88EF+A8Q')]
    [TestCase('202', 'horse.webp,claWhite,Tile,0.5,0.5,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,yxeuwQD8j8A+ENoXEDz3h2fwfgEiGADE8/7rzr3fHBDg/wfsD0G+/spLn3gD8Nt/wvtIYL//8g')]
    [TestCase('203', 'horse.webp,claWhite,TileOriginal,0.5,0.5,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,yxeuwQD8j8A+ENoXEDz3h2fwfgEiGADE8/7rzr3fHBDg/wfsD0G+/spLn3gD8Nt/wvtIYL//8g')]
    [TestCase('204', 'horse.webp,claWhite,TileStretch,0.5,0.5,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,zxeugf8CmMA+EdQf4Lv1gnNAfwEnHBAE8wQEr33fFQTAfwvrA3F9/cdr/zcI9t9/wv1AEC9A8g')]
    [TestCase('205', 'horse.webp,claWhite,Tile,0.5,0.5,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,1,0.98,6wsuggMXoJD/B+VjQLv4lYMg/gEXPNBC8foH37/fIuTfAQriExH//cqH/zgK7U+ggv4xyA9B9A')]
    [TestCase('206', 'horse.webp,claWhite,TileOriginal,0.5,0.5,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,1,0.98,6wsuggMXoJD/B+VjQLv4lYMg/gEXPNBC8foHTz/gJ/yfAAnpH6G+/dRjv7gE6wfAAvtMsM//8w')]
    [TestCase('207', 'horse.webp,claWhite,TileStretch,0.5,0.5,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,1,0.98,7OvtwQEqwCD/C+NDUDv4ks9Qvv0xPNCD9AQg/7/eH8CvgA3jMyE++ceD7/cKBvyvQgM5JP//7Q')]
    [TestCase('208', 'horse.webp,claWhite,Tile,0.5,0.5,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,6wsuwgMXnJD/B+VjQLv3g1OAvgAeHJDD8ffvzj7jFuj/wAjgKwH/+86H/3gK7k+ggv4xzA8B9A')]
    [TestCase('209', 'horse.webp,claWhite,TileOriginal,0.5,0.5,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,6wcuwgMXnJD/B+VjQLv3g1OAvgAeHJDD8ffvnj7iG/yfQAjhH5F+/dBjv7gE7AfAAvtMsM8/9A')]
    [TestCase('210', 'horse.webp,claWhite,TileStretch,0.5,0.5,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,7OvtAQIqwDD/C+JDULv2e5MQv/02KGCE8wQEr37hEdhvQA7dLyE++smH7/cKBvyvQgM4JA/A7Q')]
    [TestCase('211', 'horse.webp,claWhite,Tile,0.5,0.5,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,6ws+wgMYmID/B+RjQLv5hE+gvgEVFLAD8vfr/n3gGfwPQAjrF5H+/cmD73gK7kuggv4y0A/B8w')]
    [TestCase('212', 'horse.webp,claWhite,TileOriginal,0.5,0.5,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,6ws+wgMYmID/B+RjQLv5hE+gvgEVFLAD8vfr7j3gHAgQgAfsD0H+/sxfr3gE7APAQvtLtM8/9A')]
    [TestCase('213', 'horse.webp,claWhite,TileStretch,0.5,0.5,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,7OsNQgIquCD/C+FHYLv2gHNQ//8kJBAE8gD47j3gEfDPvwzoD7F9/MSH7zcLBfyPAgM5KA8A7g')]
    [TestCase('214', 'horse.webp,claWhite,Tile,0.5,0.5,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,6w9OAgQalHA/CORnUHv3hV/QvgEhHPCD8/7vzj3gGQjgfwjrD1E+/sd/73gK70+Agv4z0P8A9A')]
    [TestCase('215', 'horse.webp,claWhite,TileOriginal,0.5,0.5,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,6w9OAgQalHA/CORnUHv3hV/QvgEhHPCD8/7v3j3gGwzwvwfsDzG+/spfn7gE7gOwQvtMtM//8w')]
    [TestCase('216', 'horse.webp,claWhite,TileStretch,0.5,0.5,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,7e8dggIrtBA/DOBHcPv1gG9AvwAnIADE8gIAr73fEvy/PwzqB5H9/MWH7zcLBQCAAgM5KA9A7g')]
    [TestCase('217', 'horse.webp,claWhite,Tile,0.75,0.75,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,1QfeQAILjDB/DNs/IDv4jnNgfgIXRODC8Pv7zn/fI+TPwQnlFzF//ct7XzkI6hOwwvw3uI9A9Q')]
    [TestCase('218', 'horse.webp,claWhite,TileOriginal,0.75,0.75,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,1QfeQAILjDB/DNs/IDv4jnNgfgIXRODC8Pv7Xr/fKACgQAnpG7G+/dNj77gD7fv/wvpLqM+/8w')]
    [TestCase('219', 'horse.webp,claWhite,TileStretch,0.75,0.75,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,2euNAAEdtJB+ENgjILv3ib+g/v4wSNDD8wUY///dHszPQA7jJzG++ciH33cKAeTvwgE2PF/A7w')]
    [TestCase('220', 'horse.webp,claWhite,Tile,0.75,0.75,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,yg+OQQLzi8B/DNg78Lv3g1OAvgAdHJDD8ffvzj7jFuT/wAjgLwH/+85j73gC8tP/gvtJfM//8Q')]
    [TestCase('221', 'horse.webp,claWhite,TileOriginal,0.75,0.75,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,yg+OQQLzi8B/DNg78Lv3g1OAvgAdHJDD8ffvnj7iG/ifQAjhH5F+/dBDL/kB8uO/gvpOfM9/8g')]
    [TestCase('222', 'horse.webp,claWhite,TileStretch,0.75,0.75,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,0ANOwf8GrAB/ENgfILz2e5MQv/02KGDE8wUIr37hEdhvQA7dLyE++sl7r3cHBMTfAgA/EA9A7w')]
    [TestCase('223', 'horse.webp,claWhite,Tile,0.75,0.75,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,yh+ewQD6j+D+D9kTAHz3hVPQvgIfFNCD8//7zr3fGxDgPwjtD1F+/shHrzgD7dOvgvtIYL//8g')]
    [TestCase('224', 'horse.webp,claWhite,TileOriginal,0.75,0.75,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,yh+ewQD6j+D+D9kTAHz3hVPQvgIfFNCD8//7zr3fHBTg/wftC0G+/shDvzgD7dePQvtIYK8/8w')]
    [TestCase('225', 'horse.webp,claWhite,TileStretch,0.75,0.75,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,zSOuwf//l9C+ENUX4Pv1gWsg/wEkHODD8gQQrz3fFQTQvwvrB4E9/cVnHzgH9NufQv1AFB8A8g')]
    [TestCase('226', 'horse.webp,claWhite,Tile,0.75,0.75,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,yxdOQQH7m9A+ENQDkLv2h2sAvwEnHACE8wHwXv3hGRSAPwf3B4G9/8hTjzgH+u9/Qv1DSE8A8g')]
    [TestCase('227', 'horse.webp,claWhite,TileOriginal,0.75,0.75,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,yxdOQQH7m9A+ENQDkLv2h2sAvwEnHACE8wHwXv3hGRSAPwf3B4G9/8hTjzgH+u9/Qv1DSE8A8g')]
    [TestCase('228', 'horse.webp,claWhite,TileStretch,0.75,0.75,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,zRd+QQD9m8D+ENMLkPv1hGvw/gEmGABE8wH0Lv3gFQRw/wn4/+C8/8NfTzgI+++fwvw8FK8A8Q')]
    [TestCase('229', 'horse.webp,claWhite,Tile,0.75,0.75,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,1,0.98,6wsuggMXoJD/B+VjQHv4jnNgfgIXRODC8Pv7zn/fI+S/wQnlFzF//cuH/zgK7k+ggv4xyA9B9A')]
    [TestCase('230', 'horse.webp,claWhite,TileOriginal,0.75,0.75,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,1,0.98,6wsuggMXoJD/B+VjQHv4jnNgfgIXRODC8Pv7Xr/fKACgQAnpG7G+/dNjv7gE6wfAAvtMsM//8w')]
    [TestCase('231', 'horse.webp,claWhite,TileStretch,0.75,0.75,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,1,0.98,7OvtwQEqwCD/C+NDULv3ib+g/v4wSNDD8wUY///dHszPQA7jJzG++ciD7/cKBvyvQgM5JP//7Q')]
    [TestCase('232', 'horse.webp,claWhite,Tile,0.75,0.75,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,6wsuwgMXnJD/B+VjQLv3g1OAvgAeHJDD8ffvzj7jFuj/wAjgKwH/+86H/3gK7k+ggv4xzA8B9A')]
    [TestCase('233', 'horse.webp,claWhite,TileOriginal,0.75,0.75,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,6wcuwgMXnJD/B+VjQLv3g1OAvgAeHJDD8ffvnj7iG/yfQAjhH5F+/dBjv7gE7AfAAvtMsM8/9A')]
    [TestCase('234', 'horse.webp,claWhite,TileStretch,0.75,0.75,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,7OvtAQIqwDD/C+JDULv2e5MQv/02KGCE8wQEr37hEdhvQA7dLyE++smH7/cKBvyvQgM4JA/A7Q')]
    [TestCase('235', 'horse.webp,claWhite,Tile,0.75,0.75,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,6w8+AgQZlID/B+RjULv3hFOwfgIfGMBD8//73j3gGQjwfwjrD2H+/cZ/7zgK70+Qgv4y0P8A9A')]
    [TestCase('236', 'horse.webp,claWhite,TileOriginal,0.75,0.75,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,6w8+AgQZlID/B+RjULv3hFOwfgIfGMBD8//73j3gGxAAgAfsC0G+/shfr3gE7QOwQvtLtM//8w')]
    [TestCase('237', 'horse.webp,claWhite,TileStretch,0.75,0.75,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,7O8NggIqsCD/C+BHcLv1f2dAPwElINDD8gMIr33fE/zPfwzqB6H9/MSD7zcLBQCAAgM5KA9A7g')]
    [TestCase('238', 'horse.webp,claWhite,Tile,0.75,0.75,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,6g8uggQZmHD/B+JbMPv2hWPgvgEmIOBD8wH0fr3hFwigvwf0B7F9/8h/3/gK8ldwgv4x0B/B8w')]
    [TestCase('239', 'horse.webp,claWhite,TileOriginal,0.75,0.75,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,6g8uggQZmHD/B+JbMPv2hWPgvgEmIOBD8wH0fn3hGhCg/wb1B5H9/8pfjzgF8AugQvtKsO//8w')]
    [TestCase('240', 'horse.webp,claWhite,TileStretch,0.75,0.75,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,6+/9AQMqtCD/C98/UPv1gmsA/wAnIPDD8gD0Lr3gE/x//wr1/wA9/8OD/7cLBwSAwgI4KE8A7g')]
    [TestCase('241', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('242', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,Round,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('243', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('244', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,Round,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('245', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('246', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,Round,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('247', 'horse.webp,claWhite,Tile,15,15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('248', 'horse.webp,claWhite,Tile,15,15,AllCorners,Round,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('249', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('250', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,Round,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('251', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('252', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,Round,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('253', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('254', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('255', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('256', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('257', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('258', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('259', 'horse.webp,claWhite,Tile,15,15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('260', 'horse.webp,claWhite,Tile,15,15,AllCorners,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('261', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('262', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('263', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('264', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('265', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('266', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('267', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('268', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('269', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('270', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('271', 'horse.webp,claWhite,Tile,15,15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('272', 'horse.webp,claWhite,Tile,15,15,AllCorners,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('273', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('274', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('275', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('276', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('277', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('278', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('279', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('280', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('281', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('282', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('283', 'horse.webp,claWhite,Tile,15,15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('284', 'horse.webp,claWhite,Tile,15,15,AllCorners,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('285', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('286', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('287', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('288', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('289', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('290', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,Round,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('291', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('292', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,Round,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('293', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('294', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,Round,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('295', 'horse.webp,claWhite,Tile,0,0,AllCorners,Round,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('296', 'horse.webp,claWhite,Tile,0,0,AllCorners,Round,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('297', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,Round,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('298', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,Round,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('299', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,Round,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('300', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,Round,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('301', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('302', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('303', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('304', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('305', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('306', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('307', 'horse.webp,claWhite,Tile,0,0,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('308', 'horse.webp,claWhite,Tile,0,0,AllCorners,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('309', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('310', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('311', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('312', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('313', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('314', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('315', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('316', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('317', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('318', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('319', 'horse.webp,claWhite,Tile,0,0,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('320', 'horse.webp,claWhite,Tile,0,0,AllCorners,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('321', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('322', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('323', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('324', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('325', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('326', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('327', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('328', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('329', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('330', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('331', 'horse.webp,claWhite,Tile,0,0,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('332', 'horse.webp,claWhite,Tile,0,0,AllCorners,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('333', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('334', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('335', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    [TestCase('336', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,ziduQQD+o4B+D9IH0Lv2h19AvwEkDMCD9ALsXj3hGgyAfwf6A5E9AMJTX3gH+u+PwvxCOD8A8g')]
    procedure TestFillRoundRectBitmapWithChessBackground(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode; AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType; ASmallSize: Boolean; ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPD0Xz3suQf/vSbyI+5CLc8HwlrXOP/OYTWC87917IbwESuJ9OOBU8RB79/A')]
    [TestCase('2',  'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPD0Xz3suQf/vSbyI+5CLcQH0lrX9ffOQQYN+69mbIj4EA2mseuf0yKO76Cw')]
    [TestCase('3',  'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,zlMievbEew6B9KeTD8Xx3xeRf/bqb3K/5yoI8bwlrW+PfOcTWB9791zEn4AS0FM+eBpCrB389g')]
    [TestCase('4',  'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPD0Xz3suQf/vSbyI+5CLc8HwlrXOP/OYTWC879168L8ESs5/+9xNALJ58AA')]
    [TestCase('5',  'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPD0Xz3suQf/vSbyI+5CLcQH0lrX9ffOQQYN+69mbIj4EA2mseuf0yKO76Cw')]
    [TestCase('6',  'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,zlMievbEew6B9KeTD8Xx3xeRf/bqb3K/5yoI8bwlrW+PfOcTWB9791y4XwATylfutxhFlJ07+g')]
    [TestCase('7',  'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPD0Xz3suQf/vSbyI+5CLc8HwlrXOP/OYTWC879168H0ETq5OutxE/FD58Aw')]
    [TestCase('8',  'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPD0Xz3suQf/vSbyI+5CLcQH0lrX9ffOQQYN+69mbIj4EA2mseuf0yKO76Cw')]
    [TestCase('9',  'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,zlMievbEew6B9KeTD8Xx3xeRf/bqb3K/5yoI8bwlrW+PfOcTWB9791y8P0ATxFOedxZDfE37/A')]
    [TestCase('10', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPD0Xz3suQf/vSbyI+5CLc8HwlrXOP/OYTWC87917AL0ETqI9+txA+CA68BA')]
    [TestCase('11', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPD0Xz3suQf/vSbyI+5CLcQH0lrX9ffOQQYN+69mbIj4EA2mseuf0yKO76Cw')]
    [TestCase('12', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,zlMievbEew6B9KeTD8Xx3xeRf/bqb3K/5yoI8bwlrW+PfOcTWB9791zAP0ATwVN+dxVCcD37/Q')]
    [TestCase('13', 'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPD0Xx4KvQv/zPd8L94BvU8HwlrXOP/OYTWC87917IbwESuJ9OOBU8RB79/A')]
    [TestCase('14', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPD0Xx4KvQv/zPd8L94BvUQH0lrX9ffOQQYN+69mbIj4EA2mseuf0yKO76Cw')]
    [TestCase('15', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,zlMievbEew6B9KeTDwXw4ffQv/fndxK/5CQA8bwlrW+PfOcTWB9791zEn4AS0FM+eBpCrB389g')]
    [TestCase('16', 'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPDwXv5megP/3PZ6L93RnU8HwlrXOP/OYTWC879168L8ESs5/+9xNALJ58AA')]
    [TestCase('17', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPDwXv5megP/3PZ6L93RnUQH0lrX9ffOQQYN+69mbIj4EA2mseuf0yKO76Cw')]
    [TestCase('18', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,zlMievbEew6B9KeTD8Xt5rOgP/jmZ/K+4SIA8bwlrW+PfOcTWB9791y4XwATylfutxhFlJ07+g')]
    [TestCase('19', 'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPD0Xt7DMw//zNU9I93RjY8HwlrXOP/OYTWC879168H0ETq5OutxE/FD58Aw')]
    [TestCase('20', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPD0Xt7DMw//zNU9I93RjYQH0lrX9ffOQQYN+69mbIj4EA2mseuf0yKO76Cw')]
    [TestCase('21', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,zlMievbEew6B9KeTD0Xs7H8wP/jlUxL/4CEE8bwlrW+PfOcTWB9791y8P0ATxFOedxZDfE37/A')]
    [TestCase('22', 'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPD8Xs7hvg/vzMS/L93Bfg8HwlrXOP/OYTWC87917AL0ETqI9+txA+CA68BA')]
    [TestCase('23', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPD8Xs7hvg/vzMS/L93BfgQH0lrX9ffOQQYN+69mbIj4EA2mseuf0yKO76Cw')]
    [TestCase('24', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,zlMievbEew6B9KeTD4Xr7mvwPvjkS0K/4CEI8bwlrW+PfOcTWB9791zAP0ATwVN+dxVCcD37/Q')]
    procedure TestFillRoundRectBitmapWithClipping(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode; AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType; ASmallSize: Boolean; ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPD0Xz3suQf/vSbyI+5CLc8HwlrXOP/OYTWC87917IbwESuJ9OOBU8RB79/A')]
    [TestCase('2',  'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPD0Xz3suQf/vSbyI+5CLcQH0lrX9ffOQQYN+69mbIj4EA2mseuf0yKO76Cw')]
    [TestCase('3',  'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,zlMievbEew6B9KeTD8Xx3xeRf/bqb3K/5yoI8bwlrW+PfOcTWB9791zEn4AS0FM+eBpCrB389g')]
    [TestCase('4',  'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPD0Xz3suQf/vSbyI+5CLc8HwlrXOP/OYTWC879168L8ESs5/+9xNALJ58AA')]
    [TestCase('5',  'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPD0Xz3suQf/vSbyI+5CLcQH0lrX9ffOQQYN+69mbIj4EA2mseuf0yKO76Cw')]
    [TestCase('6',  'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,zlMievbEew6B9KeTD8Xx3xeRf/bqb3K/5yoI8bwlrW+PfOcTWB9791y4XwATylfutxhFlJ07+g')]
    [TestCase('7',  'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPD0Xz3suQf/vSbyI+5CLc8HwlrXOP/OYTWC879168H0ETq5OutxE/FD58Aw')]
    [TestCase('8',  'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPD0Xz3suQf/vSbyI+5CLcQH0lrX9ffOQQYN+69mbIj4EA2mseuf0yKO76Cw')]
    [TestCase('9',  'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,zlMievbEew6B9KeTD8Xx3xeRf/bqb3K/5yoI8bwlrW+PfOcTWB9791y8P0ATxFOedxZDfE37/A')]
    [TestCase('10', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPD0Xz3suQf/vSbyI+5CLc8HwlrXOP/OYTWC87917AL0ETqI9+txA+CA68BA')]
    [TestCase('11', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPD0Xz3suQf/vSbyI+5CLcQH0lrX9ffOQQYN+69mbIj4EA2mseuf0yKO76Cw')]
    [TestCase('12', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,zlMievbEew6B9KeTD8Xx3xeRf/bqb3K/5yoI8bwlrW+PfOcTWB9791zAP0ATwVN+dxVCcD37/Q')]
    [TestCase('13', 'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPD0Xx4KvQv/zPd8L94BvU8HwlrXOP/OYTWC87917IbwESuJ9OOBU8RB79/A')]
    [TestCase('14', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPD0Xx4KvQv/zPd8L94BvUQH0lrX9ffOQQYN+69mbIj4EA2mseuf0yKO76Cw')]
    [TestCase('15', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,zlMievbEew6B9KeTDwXw4ffQv/fndxK/5CQA8bwlrW+PfOcTWB9791zEn4AS0FM+eBpCrB389g')]
    [TestCase('16', 'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPDwXv5megP/3PZ6L93RnU8HwlrXOP/OYTWC879168L8ESs5/+9xNALJ58AA')]
    [TestCase('17', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPDwXv5megP/3PZ6L93RnUQH0lrX9ffOQQYN+69mbIj4EA2mseuf0yKO76Cw')]
    [TestCase('18', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,zlMievbEew6B9KeTD8Xt5rOgP/jmZ/K+4SIA8bwlrW+PfOcTWB9791y4XwATylfutxhFlJ07+g')]
    [TestCase('19', 'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPD0Xt7DMw//zNU9I93RjY8HwlrXOP/OYTWC879168H0ETq5OutxE/FD58Aw')]
    [TestCase('20', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPD0Xt7DMw//zNU9I93RjYQH0lrX9ffOQQYN+69mbIj4EA2mseuf0yKO76Cw')]
    [TestCase('21', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,zlMievbEew6B9KeTD0Xs7H8wP/jlUxL/4CEE8bwlrW+PfOcTWB9791y8P0ATxFOedxZDfE37/A')]
    [TestCase('22', 'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPD8Xs7hvg/vzMS/L93Bfg8HwlrXOP/OYTWC87917AL0ETqI9+txA+CA68BA')]
    [TestCase('23', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,zVciuvbDdx5B9KaPD8Xs7hvg/vzMS/L93BfgQH0lrX9ffOQQYN+69mbIj4EA2mseuf0yKO76Cw')]
    [TestCase('24', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,zlMievbEew6B9KeTD4Xr7mvwPvjkS0K/4CEI8bwlrW+PfOcTWB9791zAP0ATwVN+dxVCcD37/Q')]
    procedure TestFillRoundRectBitmapWithClipping2(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode; AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType; ASmallSize: Boolean; ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  'horse.webp,claWhite,Tile,1.5,1,0,-0.25,30,0.15,0.15,TopLeft|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,z9OOghbXyxBE/MJ7QH7e1k/PAQQflIF93iIcgsYR/RdfOvWm/zAC7IlzLzYR/MuP/hp8GG7E7A')]
    [TestCase('2',  'horse.webp,claWhite,TileOriginal,1.5,1,0,-0.25,30,0.15,0.15,TopLeft|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,z9OOghbXyxBE/MJ7QH7e1k/PAQQflIF93iIgIgINFSS/APW37xAB9AUUD3oJ2RvPfBiEGF4C7w')]
    [TestCase('3',  'horse.webp,claWhite,TileStretch,1.5,1,0,-0.25,30,0.15,0.15,TopLeft|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,1RvfAhHRt0DD+skbIADa01tPAgY8jBH93zgc0sEOCrQvuuzpZxCA+ZYvj7kW1p+vvRiLZL4D9A')]
    [TestCase('4',  'horse.webp,claWhite,Tile,1.5,1,0,-0.25,30,0.15,0.15,TopLeft|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,2QdvwhHatwDE+tGLkH7e1k/PAQQflIE93iIcgsYR/RdfOvWm/zAC7IlzLzYR/MuP/hp8GG7E7A')]
    [TestCase('5',  'horse.webp,claWhite,TileOriginal,1.5,1,0,-0.25,30,0.15,0.15,TopLeft|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,2QdvwhHatwCE+tGLoH7e1k/PAQQflIE93iIgIgINFSS/APW37xAB9AUUD3oJ2RvPfBiEGF4C7w')]
    [TestCase('6',  'horse.webp,claWhite,TileStretch,1.5,1,0,-0.25,30,0.15,0.15,TopLeft|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,30+/AgzUozBD+dgrcADa01tPAgY8jBH93zgc0sEOCrQvuuzpZxCA+ZYvj7kW1p+vvRiLZL4D9A')]
    [TestCase('7',  'horse.webp,claWhite,Tile,1.5,1,0,-0.25,30,0.15,0.15,TopLeft|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,2ysfQg3gr6BD+92H4H7e1k/PAQQflIE93iIcgsYR/RdfOvWm/zAC7IlzLzYR/MuP/hp8GG7E7A')]
    [TestCase('8',  'horse.webp,claWhite,TileOriginal,1.5,1,0,-0.25,30,0.15,0.15,TopLeft|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,2ysfQg3gr6BD+92H4H7e1k/PAQQflIE93iIgIgINFSS/APW37xAB9AUUD3oJ2RvPfBiEGF4C7w')]
    [TestCase('9',  'horse.webp,claWhite,TileStretch,1.5,1,0,-0.25,30,0.15,0.15,TopLeft|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,4W9vggfal9DC+eQrwADa01tPAgY8jBH93zgc0sEOCrQvuuzpZxCA+ZYvj7kW1p+vvRiLZL4D9A')]
    [TestCase('10', 'horse.webp,claWhite,Tile,1.5,1,0,-0.25,30,0.15,0.15,TopLeft|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,2jvfQQvlq2DD++OHAH/e1k/PAQQflIE93iIcgsYR/RdfOvWm/zAC7IlzPzYR+8uP/hp8GG7E7A')]
    [TestCase('11', 'horse.webp,claWhite,TileOriginal,1.5,1,0,-0.25,30,0.15,0.15,TopLeft|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,2jvfQQvlq2DD++KHAH/e1k/PAQQflIE93iIgIgINFSS/APW37xAB9AUUD3oJ2RvPfBiEGF4C7w')]
    [TestCase('12', 'horse.webp,claWhite,TileStretch,1.5,1,0,-0.25,30,0.15,0.15,TopLeft|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,4H8vwgXek5CC+uon4ADa01tPAgY8jBH93zgc0sEOCrQvuuzpZxCA+ZYvn7kW1p+v/RiKZL4D9A')]
    [TestCase('13', 'horse.webp,claWhite,Tile,1.5,1,0,-0.25,30,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,z9OOghbXyxBE/MJ7QP7d1WNfwQMdiHE93yI0gsYR/RdfOvWm/zAC7Il/73UQ/tP/vhl0/B0E7Q')]
    [TestCase('14', 'horse.webp,claWhite,TileOriginal,1.5,1,0,-0.25,30,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,z9OOghbXyxBE/MJ7QP7d1WNfwQMeiHE93yI0IgINFSS/APW37xAB9AUgz3kI2x8vPRd8/P0B7w')]
    [TestCase('15', 'horse.webp,claWhite,TileStretch,1.5,1,0,-0.25,30,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,1RvfAhHRt0DD+skbIIDZ0nPfgQU7fPG84Dgw0sEOCrQvuuzpZxCA+ZY7T/kV2KcffheDSF5D9A')]
    [TestCase('16', 'horse.webp,claWhite,Tile,1.5,1,0,-0.25,30,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,2QdvwhHatwDE+tGLkD7d0m+/gAMZeDF93yQwgsYR/RdfOvWm/zAC7ImDv3UQAtSfPxhuvH2D7g')]
    [TestCase('17', 'horse.webp,claWhite,TileOriginal,1.5,1,0,-0.25,30,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,2QdvwhHatwCE+tGLoD7d0m+/gAMZeDE93yQwIgINFSS/APW37xAB9AUkn3kI3yfPvRV1vF2B8A')]
    [TestCase('18', 'horse.webp,claWhite,TileStretch,1.5,1,0,-0.25,30,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,30+/AgzUozBD+dgrcMDYz38/QQU2cMH84Dow0sEOCrQvuuzpZxCA+ZZDD/kV3Ku//hV8CK4C9g')]
    [TestCase('19', 'horse.webp,claWhite,Tile,1.5,1,0,-0.25,30,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,2ysfQg3gr6BD+92H4D7d0G+fgAMUdAH93ScUgsYR/RdfOvWm/zAC7ImHj7URBMi//xdugK1C8Q')]
    [TestCase('20', 'horse.webp,claWhite,TileOriginal,1.5,1,0,-0.25,30,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,2ysfQg3gr6BD+92H4D7d0G+fgAMUdAH93ScUIgINFSS/APW37xAB9AUob/kJ4RvvfRV1gJ1A8w')]
    [TestCase('21', 'horse.webp,claWhite,TileStretch,1.5,1,0,-0.25,30,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,4W9vggfal9DC+eQrwADZzX8fgQUxaJG83z0U0sEOCrQvuuzpZxCA+ZZH3zgX36PfvhV9zO3B+A')]
    [TestCase('22', 'horse.webp,claWhite,Tile,1.5,1,0,-0.25,30,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,2jvfQQvlq2DD++OHAH/dz2+fgAMRdOH83CkEgsYR/RdfOvWm/zAC7ImDj7USBMCfPxhuaE1C8g')]
    [TestCase('23', 'horse.webp,claWhite,TileOriginal,1.5,1,0,-0.25,30,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,2jvfQQvlq2DD++KHAH/dz2+fgAMRdOH83CkEIgINFSS/APW37xAB9AUkX/kK4RPfvRV1aD2A9A')]
    [TestCase('24', 'horse.webp,claWhite,TileStretch,1.5,1,0,-0.25,30,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,4H8vwgXek5CC+uon4ADZzHsfgQUuaHG83j8E0sEOCrQvuuzpZxCA+ZZHzzgY35vP/hV8uH2B+Q')]
    procedure TestFillRoundRectBitmapWithMatrix(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode; AScaleX, AScaleY, ADeltaX, ADeltaY, ARotationDegree, AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType; ASmallSize: Boolean; ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',   '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,4nvhPfGms84ABKH737/9SIxf++uu57+C5Nr//zkF1/sPffIDwI/6/m6cvkD56HPvOwDkz+6+HA')]
    [TestCase('2',   '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,4nvhPfGms84ABKH737/9SIxf++uu57+C5Nr/zzoJu/vPfPQIyO849WTI/oHy8XsvfALmU848Gg')]
    [TestCase('3',   '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Round,0,0,1,1,1,0.98,5Fdx/u+hw66ABqEPwP7+O7TfOuuy06/D49m7b3n/3xMw/fQA1K/6/3F8Pv/76odfPP/n2x5/HQ')]
    [TestCase('4',   '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Round,0,0,1,1,1,0.98,5Fdx/u+hw66ABqEPwP7+O7TfOuuy06/D49m7XzoEvhPgPPcG3M+49GWsrgD09I+f/AHpS558Gg')]
    [TestCase('5',   '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,10Myffmxw97B+rxbn4T0aSjPPOt/L+FA3L6XgPsn0av/t+EaBK/9/UVIL0b0twf+Ng8UQL79Cw')]
    [TestCase('6',   '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,10Myffmxw97B+rxbn4T0aSjPPOt/L+FA3L6XQLwq3cufeOckTN98+j4sLwfxykte9w8QDA79CQ')]
    [TestCase('7',   '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,2D+ivfmu1+7B+8JTX4T0YUCffOp6V4FB2rhvMHso17NP9+Ab+F7+/kBEn8X1sNuN9hAfOK69CQ')]
    [TestCase('8',   '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,2D+ivfmu1+7B+8JTX4T0YUCffOp6V4FB2rhvELwr5tf/9+cnSF/9+jgkzwbyxisOtxEa+N18Bw')]
    [TestCase('9',   '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,7mMhfuuxuz7ACpTPX7/7WIjfeuyuD7AC5fPXz7oC18M/vvAF2J86/miwrsD18I8vfAH2p44/Gg')]
    [TestCase('10',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,7mMhfuuxuz7ACpTPX7/7WIjfeuyuD7AC5fPXf7sHvuMfPvIN3H949Frc7oHv+IePfAP3Hx69Fg')]
    [TestCase('11',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Round,0,0,1,1,1,0.98,8zfBPumvy/6/DpLfL/78TbBP+uuy/6+D5PeLX7r838+fvvIC8L86/2qULv/386evvAD8r86/Gg')]
    [TestCase('12',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Round,0,0,1,1,1,0.98,8zfBPumvy/6/DpLfL/78TbBP+uuy/6+D5PeLL3sCwfN/fvQL9D+481rInkDw/J8f/QL9D+68Fg')]
    [TestCase('13',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,2zdCPfqxuw4C/Ltjb4T1aDx/vOqBL0GB3cCjQHsn04vPt98cBK99/UZMf4b0uPfN9g0PWO69Cw')]
    [TestCase('14',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,2zdCPfqxuw4C/Ltjb4T1aDx/vOqBL0GB3cCjEHwq3q9v+OUmRK/8+T84f0fxzDtudw8NID49CQ')]
    [TestCase('15',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,3DPCvfquyy4C/cBfH4T1YFgvvOl9U/HB27p/8Lon2o8Pt94e9F5+/kJI/0X2scdddg8ZVN59CQ')]
    [TestCase('16',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,3DPCvfquyy4C/cBfH4T1YFgvvOl9U/HB27p/4Dsr57e/9+UpQB89+jowH4fyyBcONxEWEB69Bg')]
    [TestCase('17',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,+aOQfOrT+35BEFRv33wCRJwfe/bgNw8C8uYvQHr6me++//0n4KAy9NzvHgPt9N+/vvn3e66/AQ')]
    [TestCase('18',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,+5uQ/OnR+24BEE9Tj3wCTKR/e/fcQ0+C7+UvEDr7ldNe//0m3EBy9OMDDwPr6+PvPvrtn66/Ag')]
    [TestCase('19',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Round,0,0,1,1,1,0.98,/1fw/OfXF38BFUZrP7sENsiPevftA9/C8+fzv7nyldNuAAIqJGFx88Xf/kHt+AOgv/f9e+6//Q')]
    [TestCase('20',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Round,0,0,1,1,1,0.98,AUjgfOfUE2/BFEBL37oEP9APu/jpEx/D8Obzj7nzkbP+PwIpIOGw883z7kHr7QfQP/jxo+7//g')]
    [TestCase('21',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,0Z8RPfzKH48DA42rP8T7SwBPP+6QA+F/4c2bgL0i4Rugt+RDeE95+BD8f8f9tOsuug4atH27AQ')]
    [TestCase('22',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,0p8RvfvIH38DA4qbD0T7Twifv+6OC+H/38yXYD0j3Q+At+RDcA95+BUIcMf8rO9e+g4UyG07Ag')]
    [TestCase('23',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,0X+BvfzMP++DBYmz38P8PxCPv+2PH1FA4MlzgD0i6jfgduRLgC95+AIYIEcBrOdOOhAmkP26/Q')]
    [TestCase('24',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,0X+BPfzKP9+DBYafr4P8QxjvP+6NK1GA3slrYP0i5SuwduRLdN94+AgoIAcAouuOuhAfrP06/g')]
    [TestCase('25',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,4bPhvfKpgx6BAKkDoID8Prive+qv8y9D4cEnULkK0BNQvO/62186/3O0zgD86HPPOgLiw94+HA')]
    [TestCase('26',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,4bPhvfKpgx6BAKkDoID8Prive+qv8y9D4cEnMLoMsgvge/L/zw+59WfUHsL184M/+wTjS/48Gg')]
    [TestCase('27',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,45eBvvGlhw6BAqobsP/9L+Q/O+m03y9E4LvrnzgG1ytgfPH18286AHeYTj//6osPewHkz/7+HA')]
    [TestCase('28',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,45eBvvGlhw6BAqobsP/9L+Q/O+m03y9E4Lvrr3kIsyPg+/T749949Wi8zoD39peP+wTlQ868Gg')]
    [TestCase('29',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,1Eci/fixy55B+r9jv8TzahTffOt/J8FA28GTcPsozcdPeOMW/J59/kNI78XzuR/+9g0WHF49DQ')]
    [TestCase('30',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,1Eci/fixy55B+r9jv8TzahTffOt/J8FA28GTEPwq2t/fOOggSO+8+j4o74bwymM+dw4R8K08Cw')]
    [TestCase('31',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,1EOS/fiu264B+8Vbf4TzYiSvvOp7S2EB2btrELsp09OvN+MW7E5+/z9ET0X1svuNdg8hDD49Cw')]
    [TestCase('32',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,1EOS/fiu264B+8Vbf4TzYiSvvOp7S2EB2btr4Psr4u9PuOgiSG89+zggf4bxxkfeNhAb2H38CA')]
    [TestCase('33',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,7mMhfuuxuz7ACpTPX7/7WIjfeuyuD7AC5fPXz7oC18M/vvAF2J86/miwrsD18I8vfAH2p44/Gg')]
    [TestCase('34',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,7mMhfuuxuz7ACpTPX7/7WIjfeuyuD7AC5fPXf7sHvuMfPvIN3H949Frc7oHv+IePfAP3Hx69Fg')]
    [TestCase('35',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,8zfBPumvy/6/DpLfL/78TbBP+uuy/6+D5PeLX7r838+fvvIC8L86/2qULv/386evvAD8r86/Gg')]
    [TestCase('36',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,8zfBPumvy/6/DpLfL/78TbBP+uuy/6+D5PeLL3sCwfN/fvQL9D+481rInkDw/J8f/QL9D+68Fg')]
    [TestCase('37',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,2zdCPfqxuw4C/Ltjb4T1aDx/vOqBL0GB3cCjQHsn04vPt98cBK99/UZMf4b0uPfN9g0PWO69Cw')]
    [TestCase('38',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,2zdCPfqxuw4C/Ltjb4T1aDx/vOqBL0GB3cCjEHwq3q9v+OUmRK/8+T84f0fxzDtudw8NID49CQ')]
    [TestCase('39',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,3DPCvfquyy4C/cBfH4T1YFgvvOl9U/HB27p/8Lon2o8Pt94e9F5+/kJI/0X2scdddg8ZVN59CQ')]
    [TestCase('40',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,3DPCvfquyy4C/cBfH4T1YFgvvOl9U/HB27p/4Dsr57e/9+UpQB89+jowH4fyyBcONxEWEB69Bg')]
    [TestCase('41',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,88PgPO3T337BC1yDb/0CPaBfO/XmNy9C8NhPoLn6l+v+/v0n7NAy9eUP/0Lu9uNfvvvzp56/Ag')]
    [TestCase('42',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,9bvg/OzQ236BC1dnH70CRaTPO/bjP1/C7ddPgHn7k8+e/v0m6GCy9e0f74Ls7eOPPvzox56/Aw')]
    [TestCase('43',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,+H9APevW947BD1B/73sFLsjfOvb0A//C8dcbADnzlM9+PwIqNIHx9NAD3wHv+gcw//n4q97//g')]
    [TestCase('44',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,+nNA/erT835BD0pjjzsFONBfO/fwD08D79YX0Dj0j68OPwIpMBEx9dkXz8Hs7wtwv/rr194/AA')]
    [TestCase('45',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,z58R/fvKI19DAo2rTwT7TPRu/+2Q/6C/4M2XcP0i3jugN+VBfG+5+BPwb8f9su8eug4csE17Ag')]
    [TestCase('46',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,z58RffvII08DAoqbL8T6T/y+f+6PB7E/38yTUL0j2itw9+RBcC+5+Bj8X8f8qvNO+g4VyE07Aw')]
    [TestCase('47',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,zX9xffzMR7+DBIqz/wP8PwCvv+2PGxFA38lvcL0i51vg9uRJgE+5+AUIAEcBqus+OhAojM26/g')]
    [TestCase('48',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,zn+B/fvJR59DBIejz4P7QwgPAO6NJxGA3clnUD0j4kug9uRJdP/4+AsYAAcAofN+ehAgqM16/w')]
    [TestCase('49',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,5M8BPvGxXz4BAKwLcEH7PswPfOioA3CD37NH8DgPxyMg/Oz16z/6/3O03kD96F/vOQXnn+6+GQ')]
    [TestCase('50',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,5M8BPvGxXz4BAKwLcEH7PswPfOioA3CD37NHwPkPqRuge/D6y/949mXQTkL38muP+gfqLz59GA')]
    [TestCase('51',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,57eR/u+uYz7BAa8jsED8Lvyv++ar848E3qsPMDgLzD8wPO7vAzB6AXaYXr8A6W8POgXrow7/GQ')]
    [TestCase('52',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,57eR/u+uYz7BAa8jsED8Lvyv++ar848E3qsPIDkMqTOQO/L138849ma4HoH59X+/egjtHw69GA')]
    [TestCase('53',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,0EcCvfi0x16B+sNrz4TzbADPvOuCG+EA28SXUHspys+fOOQQ8I69/kNM30XzuDfe9gsUAP48Dg')]
    [TestCase('54',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,0EcCvfi0x16B+sNrz4TzbADPvOuCG+EA28SX8Lsq1uMP+eccQN/8+j8wv0bwx3Me9wwP2H28DA')]
    [TestCase('55',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,0Edyvfix216B+8pnn0TzZBCP/Op+P4EB2b9v8Doq0Nv/9+MQ4C6+/z1MT8X0sRNuNg0f6M28DA')]
    [TestCase('56',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,z0dyvfix216B+8pnn0TzZBCP/Op+P4EB2b9vsLsr3vOPeOgdPF99+zkoT0bxw1e+Ng4ZvC28Cg')]
    [TestCase('57',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,7mMhfuuxuz7ACpTPX7/7WIjfeuyuD7AC5fPXz7oC18M/vvAF2J86/miwrsD18I8vfAH2p44/Gg')]
    [TestCase('58',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,7mMhfuuxuz7ACpTPX7/7WIjfeuyuD7AC5fPXf7sHvuMfPvIN3H949Frc7oHv+IePfAP3Hx69Fg')]
    [TestCase('59',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,8zfBPumvy/6/DpLfL/78TbBP+uuy/6+D5PeLX7r838+fvvIC8L86/2qULv/386evvAD8r86/Gg')]
    [TestCase('60',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,8zfBPumvy/6/DpLfL/78TbBP+uuy/6+D5PeLL3sCwfN/fvQL9D+481rInkDw/J8f/QL9D+68Fg')]
    [TestCase('61',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,2zdCPfqxuw4C/Ltjb4T1aDx/vOqBL0GB3cCjQHsn04vPt98cBK99/UZMf4b0uPfN9g0PWO69Cw')]
    [TestCase('62',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,2zdCPfqxuw4C/Ltjb4T1aDx/vOqBL0GB3cCjEHwq3q9v+OUmRK/8+T84f0fxzDtudw8NID49CQ')]
    [TestCase('63',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,3DPCvfquyy4C/cBfH4T1YFgvvOl9U/HB27p/8Lon2o8Pt94e9F5+/kJI/0X2scdddg8ZVN59CQ')]
    [TestCase('64',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,3DPCvfquyy4C/cBfH4T1YFgvvOl9U/HB27p/4Dsr57e/9+UpQB89+jowH4fyyBcONxEWEB69Bg')]
    [TestCase('65',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,8uMw/e3Xx35BCWCPDz4DPJyvu/PrR1+C7s53QHn7lu+efvwm8NAy9ukn/8Lv+Os/vv3zu56/Ag')]
    [TestCase('66',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,89sgfe3Uw24BCVt3vz0DRKQPvPTnU58C7M1zIHn8ktM+Pvwl7HBy9vE37wLu7+9vfv7p257/Aw')]
    [TestCase('67',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,95+gPezb237BDFWPr7wFLcg/e/T6Fz/D78tHgDj0ktcOfwAoOJGx9dQf74Hw/RcQf/z4w+7//g')]
    [TestCase('68',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,+ZeQvevY136BDE9zT3wFNtC/e/X2J3/D7MpHYDj1jbeePgAnNBEx9t0z34Hu8RtQP/3s6+4/AA')]
    [TestCase('69',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,y5sBvfvKIx+DAY+vX4T6TOhuv+2S85B/4M6bMD0j20+wN+U/eJ/5+BXkT8f9se8OOg4csD27Aw')]
    [TestCase('70',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,zJsRffvIJw9DAYyfP0T6UPC+P+6Q/5D/3s2XIL0j10OA9+Q/cE85+Rr0T8f8qfM+eg4VxC07BA')]
    [TestCase('71',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,yXtxffzMR2/DA4y3H4T7QPSuP+2RD/E/38pzML0i43Pw9uRGfH85+QgA8EYBqOsuug8ojL06AA')]
    [TestCase('72',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,yneB/fvKS1+DA4mn3wP7RPwOgO2PG/E/3cprEH0j32ew9uRGdC85+Q4Q4AYAn/Nu+g8hpK26AA')]
    [TestCase('73',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,5dcR/u+0V07B/7ALwEH6QMw//OeiC2AD369H8DgRwysAfOzz6z96AHCszoD96FO/uQbrjw4/GA')]
    [TestCase('74',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,5dcR/u+0V07B/7ALwEH6QMw//OeiC2AD369HwLkRpiNw++/3x/8492LIXsL38ltfOgnuI159Fw')]
    [TestCase('75',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,6L+hfu6yVz6BAbMnEEH7MQDQe+ak+3+E3acTMLgNx0sAvO3tAzD6AXOQTv8A6V/P+Qbvkz4/GA')]
    [TestCase('76',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,6L+hfu6yVz6BAbMnEEH7MQDQe+ak+3+E3acTEDkOpj9Qu/Hy19/49mKwLgH69WuP+gnyE059Fw')]
    [TestCase('77',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,z0fyfPi2x07B+sZv34TzbPiu/OuDF/FA28WbQHspyc+/OOQO7H59/kNQ70Xztz/eNgsT9N28Dg')]
    [TestCase('78',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,z0fyfPi2x07B+sZv34TzbPiu/OuDF/FA28Wb4Hsq1d8vuecaPM/8+kA0rwbwxncONwwOzF08DQ')]
    [TestCase('79',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,zkNiffi0207B+8xrn0TzZQhvPOuAO5EB2cBz4Doqz9svOOQN2B6+/z5QT4X0sB9eNgwd3K08DQ')]
    [TestCase('80',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,zkNiffi0207B+8xrn0TzZQhvPOuAO5EB2cBzoHsr3O+vOOgaOD99+zosPwbxwV+udg0YsB18Cw')]
    [TestCase('81',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,7mMhfuuxuz7ACpTPX7/7WIjfeuyuD7AC5fPXz7oC18M/vvAF2J86/miwrsD18I8vfAH2p44/Gg')]
    [TestCase('82',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,7mMhfuuxuz7ACpTPX7/7WIjfeuyuD7AC5fPXf7sHvuMfPvIN3H949Frc7oHv+IePfAP3Hx69Fg')]
    [TestCase('83',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,8zfBPumvy/6/DpLfL/78TbBP+uuy/6+D5PeLX7r838+fvvIC8L86/2qULv/386evvAD8r86/Gg')]
    [TestCase('84',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,8zfBPumvy/6/DpLfL/78TbBP+uuy/6+D5PeLL3sCwfN/fvQL9D+481rInkDw/J8f/QL9D+68Fg')]
    [TestCase('85',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,2zdCPfqxuw4C/Ltjb4T1aDx/vOqBL0GB3cCjQHsn04vPt98cBK99/UZMf4b0uPfN9g0PWO69Cw')]
    [TestCase('86',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,2zdCPfqxuw4C/Ltjb4T1aDx/vOqBL0GB3cCjEHwq3q9v+OUmRK/8+T84f0fxzDtudw8NID49CQ')]
    [TestCase('87',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,3DPCvfquyy4C/cBfH4T1YFgvvOl9U/HB27p/8Lon2o8Pt94e9F5+/kJI/0X2scdddg8ZVN59CQ')]
    [TestCase('88',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,3DPCvfquyy4C/cBfH4T1YFgvvOl9U/HB27p/4Dsr57e/9+UpQB89+jowH4fyyBcONxEWEB69Bg')]
    [TestCase('89',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,8u9APe7Zv35BCGGXb34DPpzP+/LsU2/C7cuLEPn7lfd+Pvsk7NBy9uszD4Pw+fM/vv71v55/Ag')]
    [TestCase('90',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,8+dAve3Wv24BCFx/Dz4DRqA//PPoX69C68qH8Pj8kdsePvsk6HCy9vJD/8Lu7/dvPv/q456/Aw')]
    [TestCase('91',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,96/APezd036BC1abD70FL8Rve/P7J0/D7sdfULj0kdvu/v4nNJEx9tYr74Hx/RsQf/36y+6//g')]
    [TestCase('92',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,+aew/evaz25BC1F7r7wFOMzve/T3M4/D68ZbMLj1jbt+/v4mMBGx9t8/70Hv8h9QP/7t8+4/AA')]
    [TestCase('93',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,yZcB/fvLIw9DAZCzb0T6TeRuf+2S74B/4M6fED0j2VvA9+Q+dK85+RbgT8f9sOsOug0csC07BA')]
    [TestCase('94',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,ypcRffvIJ/8CAY2jPwT6UOy+/+2R+5C/3s6XAL0j1U+Q9+Q+bF95+RvwT8f8qPM+Og4VxC37BA')]
    [TestCase('95',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,x3dxffzMR09DA467H0T7QOyu/+ySC/H/3st3AL0i4X/w9uRFfJ95+Qn870YBp+cuOg8ojK36AA')]
    [TestCase('96',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,yHdx/fvKSz9DA4qn78P6RPQOQO2QE/E/3ctv4Hwj3XPAtuRFcD95+Q8M4Mb/nu9uug8gpK16AQ')]
    [TestCase('97',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,,Round,0.15,0.15,0.85,0.85,1,0.98,3DPCvfquyy4C/cBfH4T1YFgvvOl9U/HB27p/8Lon2o8Pt94e9F5+/kJI/0X2scdddg8ZVN59CQ')]
    [TestCase('98',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,,Round,0.15,0.15,0.85,0.85,1,0.98,3DPCvfquyy4C/cBfH4T1YFgvvOl9U/HB27p/8Lon2o8Pt94e9F5+/kJI/0X2scdddg8ZVN59CQ')]
    [TestCase('99',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,,Round,0.15,0.15,0.85,0.85,1,0.98,1HuBPf3LNw/EBom/v0P9Phg/f+2QG5EA4cx7YL0h6hvw9uNOfP/49/8rQAcBrttOug8kkC37/A')]
    [TestCase('100', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,,Bevel,0.15,0.15,0.85,0.85,1,0.98,3DPCvfquyy4C/cBfH4T1YFgvvOl9U/HB27p/8Lon2o8Pt94e9F5+/kJI/0X2scdddg8ZVN59CQ')]
    [TestCase('101', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,,Bevel,0.15,0.15,0.85,0.85,1,0.98,3DPCvfquyy4C/cBfH4T1YFgvvOl9U/HB27p/8Lon2o8Pt94e9F5+/kJI/0X2scdddg8ZVN59CQ')]
    [TestCase('102', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,,Bevel,0.15,0.15,0.85,0.85,1,0.98,1HuBPf3LNw/EBom/v0P9Phg/f+2QG5EA4cx7YL0h6hvw9uNOfP/49/8rQAcBrttOug8kkC37/A')]
    [TestCase('103', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,,InnerRound,0.15,0.15,0.85,0.85,1,0.98,3DPCvfquyy4C/cBfH4T1YFgvvOl9U/HB27p/8Lon2o8Pt94e9F5+/kJI/0X2scdddg8ZVN59CQ')]
    [TestCase('104', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,,InnerRound,0.15,0.15,0.85,0.85,1,0.98,3DPCvfquyy4C/cBfH4T1YFgvvOl9U/HB27p/8Lon2o8Pt94e9F5+/kJI/0X2scdddg8ZVN59CQ')]
    [TestCase('105', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,,InnerRound,0.15,0.15,0.85,0.85,1,0.98,1HuBPf3LNw/EBom/v0P9Phg/f+2QG5EA4cx7YL0h6hvw9uNOfP/49/8rQAcBrttOug8kkC37/A')]
    [TestCase('106', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,,InnerLine,0.15,0.15,0.85,0.85,1,0.98,3DPCvfquyy4C/cBfH4T1YFgvvOl9U/HB27p/8Lon2o8Pt94e9F5+/kJI/0X2scdddg8ZVN59CQ')]
    [TestCase('107', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,,InnerLine,0.15,0.15,0.85,0.85,1,0.98,3DPCvfquyy4C/cBfH4T1YFgvvOl9U/HB27p/8Lon2o8Pt94e9F5+/kJI/0X2scdddg8ZVN59CQ')]
    [TestCase('108', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,,InnerLine,0.15,0.15,0.85,0.85,1,0.98,1HuBPf3LNw/EBom/v0P9Phg/f+2QG5EA4cx7YL0h6hvw9uNOfP/49/8rQAcBrttOug8kkC37/A')]
    [TestCase('109', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|TopRight|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,2D+ivfmu1+7B+8JTX4T0YUCffOp6V4FB2rhv8Lon2o8Pt94e9F5+/kJEn8X1sNuN9hAfOK69CQ')]
    [TestCase('110', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|TopRight|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,3DPCvfquyy4C/cBfH4T1YFgvvOl9U/HB27p/8Lon2o8Pt94e9F5+/kJI/0X2scdddg8ZVN59CQ')]
    [TestCase('111', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|TopRight|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,0X+BvfzMP++DBYmz38P8PxCPv+2PH1FA4MlzYL0h6hvw9uNOfP/49/8bIEcBrOdOOhAmkP26/Q')]
    [TestCase('112', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|TopRight|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,1EOS/fiu264B+8Vbf4TzYiSvvOp7S2EB2btr8Lon2o8Pt94e9F5+/kJET0X1svuNdg8hDD49Cw')]
    [TestCase('113', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|TopRight|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,3DPCvfquyy4C/cBfH4T1YFgvvOl9U/HB27p/8Lon2o8Pt94e9F5+/kJI/0X2scdddg8ZVN59CQ')]
    [TestCase('114', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|TopRight|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,zX9xffzMR7+DBIqz/wP8PwCvv+2PGxFA38lvYL0h6hvw9uNOfP/49/8LAEcBqus+OhAojM26/g')]
    [TestCase('115', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|TopRight|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,0Edyvfix216B+8pnn0TzZBCP/Op+P4EB2b9v8Lon2o8Pt94e9F5+/kJMT8X0sRNuNg0f6M28DA')]
    [TestCase('116', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|TopRight|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,3DPCvfquyy4C/cBfH4T1YFgvvOl9U/HB27p/8Lon2o8Pt94e9F5+/kJI/0X2scdddg8ZVN59CQ')]
    [TestCase('117', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|TopRight|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,yXtxffzMR2/DA4y3H4T7QPSuP+2RD/E/38pzYL0h6hvw9uNOfP/49/8D8EYBqOsuug8ojL06AA')]
    [TestCase('118', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|TopRight|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,zkNiffi0207B+8xrn0TzZQhvPOuAO5EB2cBz8Lon2o8Pt94e9F5+/kJQT4X0sB9eNgwd3K08DQ')]
    [TestCase('119', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|TopRight|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,3DPCvfquyy4C/cBfH4T1YFgvvOl9U/HB27p/8Lon2o8Pt94e9F5+/kJI/0X2scdddg8ZVN59CQ')]
    [TestCase('120', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|TopRight|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,x3dxffzMR09DA467H0T7QOyu/+ySC/H/3st3YL0h6hvw9uNOfP/49///70YBp+cuOg8ojK36AA')]
    [TestCase('121', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,2D+ivfmu1+7B+8JTX4T1YFgvvOl9U/HB27p/8Lon2o8Pt94e9F5+/kJEn8X1sNuN9hAfOK69CQ')]
    [TestCase('122', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,3DPCvfquyy4C/cBfH4T1YFgvvOl9U/HB27p/8Lon2o8Pt94e9F5+/kJI/0X2scdddg8ZVN59CQ')]
    [TestCase('123', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,0X+BvfzMP++DBYmz30P9Phg/f+2QG5EA4cx7YL0h6hvw9uNOfP/49/8bIEcBrOdOOhAmkP26/Q')]
    [TestCase('124', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,1EOS/fiu264B+8Vbf4T1YFgvvOl9U/HB27p/8Lon2o8Pt94e9F5+/kJET0X1svuNdg8hDD49Cw')]
    [TestCase('125', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,3DPCvfquyy4C/cBfH4T1YFgvvOl9U/HB27p/8Lon2o8Pt94e9F5+/kJI/0X2scdddg8ZVN59CQ')]
    [TestCase('126', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,zX9xffzMR7+DBIqz/0P9Phg/f+2QG5EA4cx7YL0h6hvw9uNOfP/49/8LAEcBqus+OhAojM26/g')]
    [TestCase('127', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,0Edyvfix216B+8pnn4T1YFgvvOl9U/HB27p/8Lon2o8Pt94e9F5+/kJMT8X0sRNuNg0f6M28DA')]
    [TestCase('128', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,3DPCvfquyy4C/cBfH4T1YFgvvOl9U/HB27p/8Lon2o8Pt94e9F5+/kJI/0X2scdddg8ZVN59CQ')]
    [TestCase('129', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,yXtxffzMR2/DA4y3H0T9Phg/f+2QG5EA4cx7YL0h6hvw9uNOfP/49/8D8EYBqOsuug8ojL06AA')]
    [TestCase('130', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,zkNiffi0207B+8xrn4T1YFgvvOl9U/HB27p/8Lon2o8Pt94e9F5+/kJQT4X0sB9eNgwd3K08DQ')]
    [TestCase('131', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,3DPCvfquyy4C/cBfH4T1YFgvvOl9U/HB27p/8Lon2o8Pt94e9F5+/kJI/0X2scdddg8ZVN59CQ')]
    [TestCase('132', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,x3dxffzMR09DA467H0T9Phg/f+2QG5EA4cx7YL0h6hvw9uNOfP/49///70YBp+cuOg8ojK36AA')]
    [TestCase('133', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('134', '[$00FF0000;0 claNull;0.5 $000000FF;1],claWhite,Radial,0.08,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    procedure TestFillRoundRectGradient(const APoints, AModulateColor: string; AGradientStyle: TGradientStyle; AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType; ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',   '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,279ewfvlb7A/DqxLkDv3ozsQ/vcgFEDE7AX8DvzfKaRvv/rqc1G8Aef3HrgL2Ndff/daGO+/+g')]
    [TestCase('2',   '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,279ewfvlb7A/DqxLkDv3ozsQ/vcgFEDE7AX83vzjDaQvv/zve8H699wfX/kE4d+fv/lcnM799w')]
    [TestCase('3',   '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Round,0,0,1,1,1,0.98,3dteAfvgZ/C/DaVXkHv3qDfgPfYgFFCE6wX8zrvfLJRfv/jnhyH8Ae3nHngM0tPffvZeFN8//A')]
    [TestCase('4',   '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Round,0,0,1,1,1,0.98,3dteAfvgZ+C/DaVXkHv3qDfgPfYgFFCE6wX8znzkC5AP//rtk0F69uEXj7kE3dsfP/lhgF49+Q')]
    [TestCase('5',   '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,xvMdAfz5y1C/Fqznr/z1lBOg/gEPJLDE69fHHnrhCgDw/AD5t5B8Bdw3H3gD6nsvQPhMLM+/9Q')]
    [TestCase('6',   '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,xvMdAfz5y1C/Fqznr/z1lBOg/gEPJLDE69fH3jrkFhyAvQYD/MA7AtUfLzkA/cOPAPlI+B5/8w')]
    [TestCase('7',   '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,xOsNgfv4z3D/F6bj37z1lwOA/gELKOBE6tDDjrnhB/yP/P/5q3B8BuAzH7gC52e/v/dOLL8/9g')]
    [TestCase('8',   '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,xOsNgfv4z3D/F6bj37z1lwOA/gELKOBE6tDDbrrkFiAwvQYE/HB7AtgTT/n+/bs/gPhJ7O7+8w')]
    [TestCase('9',   '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,3MdOgfnkj5C/D6xH0Hv3sEMgPvodEMDD7BH4HjzfKHxPf/vog8F8AejvDjgL2+M/f/hdJC+A+g')]
    [TestCase('10',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,3MdOgfnkj5C/D6xH0Hv3sEMgPvodEMDD7BH4zjzkD5gvv/zvh6G699sbP/kE4tuff/penL499w')]
    [TestCase('11',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Round,0,0,1,1,1,0.98,3uNOgfjfi8B/D6ZT0Hv3tz/w/fgcEMBD6xP83vveKmQ/P/nkm6F8Ae/f/vcL1uOvvvdiIB8A/A')]
    [TestCase('12',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Round,0,0,1,1,1,0.98,3uNOgfjfi8B/D6ZT0Hv3tz/w/fgcEMBD6xP8rrzkDYQf//rtnyE69t8TX3kE3tsf//ljgE49+A')]
    [TestCase('13',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,xfsNgfz6y3D/Fq3jn/z1lBOQfgIPJLDE69jLHjrhC/j/fAD5v7B8BdwzP/gD6Hvvv/dLOM8/9g')]
    [TestCase('14',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,xfsNgfz6y3D/Fq3jn/z1lBOQfgIPJLDE69jL7jrkFhiAvQYD/LD7AdYfP3kA/L+PQPlJ/B7/8w')]
    [TestCase('15',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,w+/9wPv6z6A/GKffz/z1lwdwfgIMKOBE6tHHjjnhCPR/PP/5s5B8BuEvPzgD5Wd///ZMOK8/9w')]
    [TestCase('16',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,w+/9wPv5z6A/GKffz/z1lwdwfgIMKOBE6tHHfrrkFRwgvQYE/GA7AtkXX3n//LMvgPhK9O4+9A')]
    [TestCase('17',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,5BdvQPr1W7A/EZ7Lj/n0nZtQffYmLHAF7Aw4z7zn7yfvwAbwVzH4834jj/kD27e/gP9Y4D2A6A')]
    [TestCase('18',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,5g9vwPnyW6C/EJmzL7n0pJ+wffcjOLCF6Qo0r3zo6w+PwAbvU9E39IYzjzkC0bvvQABNBD7A6Q')]
    [TestCase('19',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Round,0,0,1,1,1,0.98,6D8/APnzT+B/EZS/H7n0oKfwfPQnNMCF6g1Ev7zo6P8uwQbuZ0G38XIXz3kD1a9vQABbpD0A5w')]
    [TestCase('20',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Round,0,0,1,1,1,0.98,6jc/wPjwS9D/EI+jr3j0qqtwffUjQACG5wxEj7zp49++wAbtX9H28XsrvzkByrOvwABPzD1A6A')]
    [TestCase('21',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,2t+NAPgRyOB+F6K7Dzz1g+fvPQAY7O8E7NXLDj3hChBQPgr4R/F5/bKP7/gE9i+ggvpURB7+6A')]
    [TestCase('22',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,29+NgPcPyNB+F5+r7/v0hus/vgAW9O+E6tXD7rzhBgQg/gn4P6G5/beb77gD7jPQAvtOXA6+6Q')]
    [TestCase('23',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,3M9dgPYUzOD+GJmvH/z0gs+v/f8W5B+F6s7D7jzhBxAgvgr3U1H5/K+bD3kE9TewQvpXGK595w')]
    [TestCase('24',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,3c9tAPYSzNC+GJab73v0htcPvgAU8C/F6M2/zvzhAgDwfQr3R/E4/bWrDzkD7D/ggvpQNK496A')]
    [TestCase('25',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,2Kv+APzoV8B/D64noDv3mEcwPvggDJBE7/n7DvzgKtCPP/rna1G8AusLf/gL0ss/P/VWJL8//A')]
    [TestCase('26',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,2Kv+APzoV8B/D64noDv3mEcwPvggDJBE7/n77rziDMQf//ztX/F6+d8rz3kF3duvP/hYsN49+g')]
    [TestCase('27',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,2sPuQPvjRwBAD6gvkDv3mz8AfvYfDLCE7vf/3rvgLcSPv/fkfxE8A/MDf7gMy8ef/vNaIK///Q')]
    [TestCase('28',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,2sPuQPvjRwBAD6gvkDv3mz8AfvYfDLCE7vf/3vziCbgPP/vqb4E6+OQn/zkF2Ncvf/dcmG69+w')]
    [TestCase('29',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,x+9twfv5y2D/Fa3vv7z2kSPAvgEQJLCE7NvH/jniCwAQvQH7r5D8Bdo//zcC75dfQPhLGM8/9A')]
    [TestCase('30',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,x+9twfv5y2D/Fa3vv7z2kSPAvgEQJLCE7NvHrjrkGBigfQYG/OA7AtQj/zj/ANifAPlH7B5/8g')]
    [TestCase('31',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,xudtAfv4z4D/Fqfr73z2kxugvgENJOBE69S/XnniCfyvvAD7o3D8Bt4773cB7Yf/v/dNFK+/9A')]
    [TestCase('32',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,xudtAfv4z4D/Fqfr73z2kxugvgENJOBE69S/LrrkGBxAPQYI/JC7AtcbD7n9AdRPQPhH3O5+8g')]
    [TestCase('33',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,3MdOgfnkj5C/D6xH0Hv3sEMgPvodEMDD7BH4HjzfKHxPf/vog8F8AejvDjgL2+M/f/hdJC+A+g')]
    [TestCase('34',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,3MdOgfnkj5C/D6xH0Hv3sEMgPvodEMDD7BH4zjzkD5gvv/zvh6G699sbP/kE4tuff/penL499w')]
    [TestCase('35',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,3uNOgfjfi8B/D6ZT0Hv3tz/w/fgcEMBD6xP83vveKmQ/P/nkm6F8Ae/f/vcL1uOvvvdiIB8A/A')]
    [TestCase('36',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,3uNOgfjfi8B/D6ZT0Hv3tz/w/fgcEMBD6xP8rrzkDYQf//rtnyE69t8TX3kE3tsf//ljgE49+A')]
    [TestCase('37',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,xfsNgfz6y3D/Fq3jn/z1lBOQfgIPJLDE69jLHjrhC/j/fAD5v7B8BdwzP/gD6Hvvv/dLOM8/9g')]
    [TestCase('38',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,xfsNgfz6y3D/Fq3jn/z1lBOQfgIPJLDE69jL7jrkFhiAvQYD/LD7AdYfP3kA/L+PQPlJ/B7/8w')]
    [TestCase('39',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,w+/9wPv6z6A/GKffz/z1lwdwfgIMKOBE6tHHjjnhCPR/PP/5s5B8BuEvPzgD5Wd///ZMOK8/9w')]
    [TestCase('40',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,w+/9wPv5z6A/GKffz/z1lwdwfgIMKOBE6tHHfrrkFRwgvQYE/GA7AtkXX3n//LMvgPhK9O4+9A')]
    [TestCase('41',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,4hN/QPv1R+A/EKG/jzn1lJdQffUqKKCF7AFMf7zm7jfPQAbvW2H49IBHj/kC17fPQP9V7F0A6A')]
    [TestCase('42',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,4wt/APvyQ9C/D5ynL/n0nJuwffYnNOAF6gBIX3zn6htvQAbvVwE49YdXjzkBzbv/wP9KEF4A6Q')]
    [TestCase('43',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,5T9PgPrzMyAAEJizH7n0l5/wPPMrMAAG6wFcT7zn5g8PAQbta4H38nRDzzkC0K9/wP9ZtG1A5g')]
    [TestCase('44',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,5zNPAPrwMxCAD5OXv7j0oKdwffQnPEAG6P9bL7zo4e+eAAbsZwE3831Xv/n/xbO/gABM3G2A5w')]
    [TestCase('45',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,2t+dAPgRxLA+F6HDHzz1g+fv/f8Y6O+E7NbH7vzhChhAPgn4PwG6/bWLD7kE9i+QgvpSTB7+6A')]
    [TestCase('46',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,2t+dgPcPyLD+Fp+z//v0hu8/fgAW8O/E6tbDznziBgwQPgn4N8H5/bqXD7kD7jPAwvpLZB5+6Q')]
    [TestCase('47',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,3M9tgPYUzMB+GJm3L/z0gs+vvf8V4B8F686/3vzhBxwAvgn4S2F5/bOTL3kE9TugAvpUIL595w')]
    [TestCase('48',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,3M99APYRzLB+GJWj/3v0htcPPgAT7B9F6c67vrziAgzQfQn4PxF5/bmjL/kC7D/QgvpNPL496A')]
    [TestCase('49',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,2KfuwPvrS9A/EK0fsDv3l0Ng/vgg/K/E7/P//jvhKti/f/roc0H8A+4Tf7gL0sP/PvVTLI+/+w')]
    [TestCase('50',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,2KfuwPvrS9A/EK0fsDv3l0Ng/vgg/K/E7/P/zjziDNAv//3sV/F6+uAv//kF3MuP//dWwN69+g')]
    [TestCase('51',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,2bveAPvnOxBAEKYjoDv3mT9Afvcg+N8E7/H/vnvhLdC/P/jkiwG8BPYHj3gMy7tfvvNXLG+//Q')]
    [TestCase('52',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,2bveAPvnOxBAEKYjoDv3mT9Afvcg+N8E7/H/vnziCsQfP/zqZ4F6+eYrP7oF18cP//ZZqG49/A')]
    [TestCase('53',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,yO+twfv6y4B/FbDz3zz3kDfg/gETJLAE7d7H3rniDfxP/QH+r5D8Bdk//3cB869/APhLDL9/8w')]
    [TestCase('54',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,yO+twfv6y4B/FbDz3zz3kDfg/gETJLAE7d7HfvrjGRCwvQUJ/NA7AtUjz3j+A+i/wPhG5C6/8Q')]
    [TestCase('55',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,xue9Afv5z7B/Fqrv/3z3kS/Q/gEQKODE69e/TvniC/jf/AD+n2D8Bt0/33cA8qcvQPdNCJ+/8w')]
    [TestCase('56',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,xue9Afv5z7B/Fqrv/3z3kS/Q/gEQKODE69e/DnrkGRBgfQUL/JC7Atgb3/j8BOhvQPhH2P6+8Q')]
    [TestCase('57',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,3MdOgfnkj5C/D6xH0Hv3sEMgPvodEMDD7BH4HjzfKHxPf/vog8F8AejvDjgL2+M/f/hdJC+A+g')]
    [TestCase('58',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,3MdOgfnkj5C/D6xH0Hv3sEMgPvodEMDD7BH4zjzkD5gvv/zvh6G699sbP/kE4tuff/penL499w')]
    [TestCase('59',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,3uNOgfjfi8B/D6ZT0Hv3tz/w/fgcEMBD6xP83vveKmQ/P/nkm6F8Ae/f/vcL1uOvvvdiIB8A/A')]
    [TestCase('60',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,3uNOgfjfi8B/D6ZT0Hv3tz/w/fgcEMBD6xP8rrzkDYQf//rtnyE69t8TX3kE3tsf//ljgE49+A')]
    [TestCase('61',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,xfsNgfz6y3D/Fq3jn/z1lBOQfgIPJLDE69jLHjrhC/j/fAD5v7B8BdwzP/gD6Hvvv/dLOM8/9g')]
    [TestCase('62',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,xfsNgfz6y3D/Fq3jn/z1lBOQfgIPJLDE69jL7jrkFhiAvQYD/LD7AdYfP3kA/L+PQPlJ/B7/8w')]
    [TestCase('63',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,w+/9wPv6z6A/GKffz/z1lwdwfgIMKOBE6tHHjjnhCPR/PP/5s5B8BuEvPzgD5Wd///ZMOK8/9w')]
    [TestCase('64',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,w+/9wPv5z6A/GKffz/z1lwdwfgIMKOBE6tHHfrrkFRwgvQYE/GA7AtkXX3n//LMvgPhK9O4+9A')]
    [TestCase('65',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,3gtvwPv0MwAAEKWvr7n1kZNwffUsIKDF7fdXP/zl7kPPAAbuU6G49X9fn3kC07v/gP5S/H3A5w')]
    [TestCase('66',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,4ANvgPvxL/C/D6CXX7n1mZvgffYoKNBF6/ZTD7zm6idvAAbtT0H49Ydvn7kAybsvAf9HIH4A6Q')]
    [TestCase('67',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,4TM/APvyH0AAEJ2fT3n1k58gPfMtIPCF7PZrD7zm5h8PwQXsY8H383Rj37kBzK+/wP5VxI0A5g')]
    [TestCase('68',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,4ic/gPrvGzCAD5eD33j1naOgffQpMDCG6fRn37vn4f+ewAXrX1E39Hx3z3n/wbP/gP9I8I2A5w')]
    [TestCase('69',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,2eO9wPcSwLC+FqPHP3z1g+sPvv8Y5N8E7djDznziCxwwPgj6NyE6/riHL7kE9jOgQvpPUD6+6A')]
    [TestCase('70',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,2t+9QPcQwKB+FqC3Dzz1hvNfPgAW7N+E69i/vjzjBgwQPgj6L+F5/r2XH3kD7zfQwvpJaD4+6Q')]
    [TestCase('71',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,29edQPYVxLD+F5q7Tzz1gtfPff8W2A/F69G7vrziCBwAfgj6Q5H5/baTTzkE9jugAvpRKN495w')]
    [TestCase('72',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,29OdwPUSyKC+F5erH/z0ht8v/v8U5A/F6dG3nnzjAxDAfQj6NzH5/byjT/kC7UPgQvpKRN695w')]
    [TestCase('73',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,2KMOwfvsR+B/EKwbsPv2lztwPvkf9K9E7/P/DrzhKdjPv/rpezE8BO8Tb7gL08PfPvVSLI9/+w')]
    [TestCase('74',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,2KMOwfvsR+B/EKwbsPv2lztwPvkf9K9E7/P/zjziDNAvP/7tVwG7+uEv//kF3cuP//dVwN69+g')]
    [TestCase('75',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,2rv+APvoNyBAEKUfsDv3mjdQvvcf8M+E7vEDz7vhK9DPf/jmjwG8BPYLb3gMzLs//vNVLG8//Q')]
    [TestCase('76',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,2rv+APvoNyBAEKUfsDv3mjdQvvcf8M+E7vEDrzziCsQff/zqZ5G6+eYrT7oF18P//vZZrG49/A')]
    [TestCase('77',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,yPPNwfv7y5B/FbHz37z3kDvg/gEUJLBE7d7D3vniDvxvPQL+r5D8Bdg//3cB9LuPAPhLDK//8g')]
    [TestCase('78',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,yPPNwfv7y5B/FbHz37z3kDvg/gEUJLBE7d7DfvrjGQzAvQUK/NA7AtUjv3j+A+y/APlG5C6/8Q')]
    [TestCase('79',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,xufdAfv608B/FqvvD733kTPQ/gERKOAE7Ni/PjnjDPgPPQH/n2D8Btw773cA868vQPdNBI8/8w')]
    [TestCase('80',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,xufdAfv608B/FqvvD733kTPQ/gERKOAE7Ni//nnkGQxwfQUN/IC7Atgbz/j8Bex/QPhH1P5+8Q')]
    [TestCase('81',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,3MdOgfnkj5C/D6xH0Hv3sEMgPvodEMDD7BH4HjzfKHxPf/vog8F8AejvDjgL2+M/f/hdJC+A+g')]
    [TestCase('82',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,3MdOgfnkj5C/D6xH0Hv3sEMgPvodEMDD7BH4zjzkD5gvv/zvh6G699sbP/kE4tuff/penL499w')]
    [TestCase('83',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,3uNOgfjfi8B/D6ZT0Hv3tz/w/fgcEMBD6xP83vveKmQ/P/nkm6F8Ae/f/vcL1uOvvvdiIB8A/A')]
    [TestCase('84',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,3uNOgfjfi8B/D6ZT0Hv3tz/w/fgcEMBD6xP8rrzkDYQf//rtnyE69t8TX3kE3tsf//ljgE49+A')]
    [TestCase('85',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,xfsNgfz6y3D/Fq3jn/z1lBOQfgIPJLDE69jLHjrhC/j/fAD5v7B8BdwzP/gD6Hvvv/dLOM8/9g')]
    [TestCase('86',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,xfsNgfz6y3D/Fq3jn/z1lBOQfgIPJLDE69jL7jrkFhiAvQYD/LD7AdYfP3kA/L+PQPlJ/B7/8w')]
    [TestCase('87',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,w+/9wPv6z6A/GKffz/z1lwdwfgIMKOBE6tHHjjnhCPR/PP/5s5B8BuEvPzgD5Wd///ZMOK8/9w')]
    [TestCase('88',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,w+/9wPv5z6A/GKffz/z1lwdwfgIMKOBE6tHHfrrkFRwgvQYE/GA7AtkXX3n//LMvgPhK9O4+9A')]
    [TestCase('89',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,3QNvQPz0KwDAD6arvzn2kpOAffUsGJBF7vRXH7zl70fPAAbuT7E49oBnn3kC0rcfAf5RBI7A5w')]
    [TestCase('90',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,3vtuwPvxK/C/D6KTX/n1mpvwffYoJNDF6/NT/3vm6ytvAAbuS0F49od3n7kAyLtPwf5GKI7A6A')]
    [TestCase('91',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,3ys/gPvyF0DAD5+bT/n1lJ9APfMtHOBF7fJn33vm5yP/wAXsW9F39HRn37kByqvfQP5T0I0A5g')]
    [TestCase('92',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,4R8/APvvEzCAD5l/77j1naOwffQpKCBG6vBnv3vn4wOfwAXrV2G39H17z3n/v68PAf9G+I1A5w')]
    [TestCase('93',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,2OPNgPcSvLA+FqTLT7z1gvMfvv8Z4N9E7drDvvziCxgwvgf8MzF6/rmHL3kE9zOgQvpOVE5+6A')]
    [TestCase('94',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,2ePNQPcQvKA+FqG7H3z1hvdvPgAX6N/E69m7nnzjBwwQvgf8K/F5/r6XL3kD8DfQgvpIbE4+6Q')]
    [TestCase('95',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,2tutAPYWwMB+F5u/X3z1gt/fff8X2A8F7NO7njzjCBwAvgf8P6E5/reTT/kD9z+gwvlQLP795g')]
    [TestCase('96',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,29e9wPUTxKA+F5ivLzz1huc//v8V5A9F6tKzfvzjAwzAvQf8M0E5/r2jT7kC7kfgQvpJSO595w')]
    [TestCase('97',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,,Round,0.15,0.15,0.85,0.85,1,0.98,w+/9wPv6z6A/GKffz/z1lwdwfgIMKOBE6tHHjjnhCPR/PP/5s5B8BuEvPzgD5Wd///ZMOK8/9w')]
    [TestCase('98',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,,Round,0.15,0.15,0.85,0.85,1,0.98,w+/9wPv6z6A/GKffz/z1lwdwfgIMKOBE6tHHjjnhCPR/PP/5s5B8BuEvPzgD5Wd///ZMOK8/9w')]
    [TestCase('99',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,,Round,0.15,0.15,0.85,0.85,1,0.98,3NNtwPYV0BD/GJqrDzz1gtOfPQAW6B+F6s7H/rzgBwRAfgv3V0E5/ayf7/gD9TewQvpZEM495w')]
    [TestCase('100', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,,Bevel,0.15,0.15,0.85,0.85,1,0.98,w+/9wPv6z6A/GKffz/z1lwdwfgIMKOBE6tHHjjnhCPR/PP/5s5B8BuEvPzgD5Wd///ZMOK8/9w')]
    [TestCase('101', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,,Bevel,0.15,0.15,0.85,0.85,1,0.98,w+/9wPv6z6A/GKffz/z1lwdwfgIMKOBE6tHHjjnhCPR/PP/5s5B8BuEvPzgD5Wd///ZMOK8/9w')]
    [TestCase('102', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,,Bevel,0.15,0.15,0.85,0.85,1,0.98,3NNtwPYV0BD/GJqrDzz1gtOfPQAW6B+F6s7H/rzgBwRAfgv3V0E5/ayf7/gD9TewQvpZEM495w')]
    [TestCase('103', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,,InnerRound,0.15,0.15,0.85,0.85,1,0.98,w+/9wPv6z6A/GKffz/z1lwdwfgIMKOBE6tHHjjnhCPR/PP/5s5B8BuEvPzgD5Wd///ZMOK8/9w')]
    [TestCase('104', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,,InnerRound,0.15,0.15,0.85,0.85,1,0.98,w+/9wPv6z6A/GKffz/z1lwdwfgIMKOBE6tHHjjnhCPR/PP/5s5B8BuEvPzgD5Wd///ZMOK8/9w')]
    [TestCase('105', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,,InnerRound,0.15,0.15,0.85,0.85,1,0.98,3NNtwPYV0BD/GJqrDzz1gtOfPQAW6B+F6s7H/rzgBwRAfgv3V0E5/ayf7/gD9TewQvpZEM495w')]
    [TestCase('106', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,,InnerLine,0.15,0.15,0.85,0.85,1,0.98,w+/9wPv6z6A/GKffz/z1lwdwfgIMKOBE6tHHjjnhCPR/PP/5s5B8BuEvPzgD5Wd///ZMOK8/9w')]
    [TestCase('107', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,,InnerLine,0.15,0.15,0.85,0.85,1,0.98,w+/9wPv6z6A/GKffz/z1lwdwfgIMKOBE6tHHjjnhCPR/PP/5s5B8BuEvPzgD5Wd///ZMOK8/9w')]
    [TestCase('108', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,,InnerLine,0.15,0.15,0.85,0.85,1,0.98,3NNtwPYV0BD/GJqrDzz1gtOfPQAW6B+F6s7H/rzgBwRAfgv3V0E5/ayf7/gD9TewQvpZEM495w')]
    [TestCase('109', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|TopRight|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,xOsNgfv4z3D/F6bj37z1lwOA/gELKOBE6tDDjjnhCPR/PP/5s5B8BuEzH7gC52e/v/dOLL8/9g')]
    [TestCase('110', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|TopRight|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,w+/9wPv6z6A/GKffz/z1lwdwfgIMKOBE6tHHjjnhCPR/PP/5s5B8BuEvPzgD5Wd///ZMOK8/9w')]
    [TestCase('111', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|TopRight|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,3M9dgPYUzOD+GJmvH/z0gs+v/f8W5B+F6s7D/rzgBwRAfgv3V0E5/aybD3kE9TewQvpXGK595w')]
    [TestCase('112', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|TopRight|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,xudtAfv4z4D/Fqfr73z2kxugvgENJOBE69S/jjnhCPR/PP/5s5B8BuE773cB7Yf/v/dNFK+/9A')]
    [TestCase('113', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|TopRight|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,w+/9wPv6z6A/GKffz/z1lwdwfgIMKOBE6tHHjjnhCPR/PP/5s5B8BuEvPzgD5Wd///ZMOK8/9w')]
    [TestCase('114', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|TopRight|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,3M9tgPYUzMB+GJm3L/z0gs+vvf8V4B8F686//rzgBwRAfgv3V0E5/ayTL3kE9TugAvpUIL595w')]
    [TestCase('115', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|TopRight|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,xue9Afv5z7B/Fqrv/3z3kS/Q/gEQKODE69e/jjnhCPR/PP/5s5B8BuE/33cA8qcvQPdNCJ+/8w')]
    [TestCase('116', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|TopRight|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,w+/9wPv6z6A/GKffz/z1lwdwfgIMKOBE6tHHjjnhCPR/PP/5s5B8BuEvPzgD5Wd///ZMOK8/9w')]
    [TestCase('117', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|TopRight|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,29edQPYVxLD+F5q7Tzz1gtfPff8W2A/F69G7/rzgBwRAfgv3V0E5/ayTTzkE9jugAvpRKN495w')]
    [TestCase('118', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|TopRight|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,xufdAfv608B/FqvvD733kTPQ/gERKOAE7Ni/jjnhCPR/PP/5s5B8BuE773cA868vQPdNBI8/8w')]
    [TestCase('119', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|TopRight|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,w+/9wPv6z6A/GKffz/z1lwdwfgIMKOBE6tHHjjnhCPR/PP/5s5B8BuEvPzgD5Wd///ZMOK8/9w')]
    [TestCase('120', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|TopRight|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,2tutAPYWwMB+F5u/X3z1gt/fff8X2A8F7NO7/rzgBwRAfgv3V0E5/ayTT/kD9z+gwvlQLP795g')]
    [TestCase('121', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,xOsNgfv4z3D/F6bj3/z1lwdwfgIMKOBE6tHHjjnhCPR/PP/5s5B8BuEzH7gC52e/v/dOLL8/9g')]
    [TestCase('122', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,w+/9wPv6z6A/GKffz/z1lwdwfgIMKOBE6tHHjjnhCPR/PP/5s5B8BuEvPzgD5Wd///ZMOK8/9w')]
    [TestCase('123', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,3M9dgPYUzOD+GJmvHzz1gtOfPQAW6B+F6s7H/rzgBwRAfgv3V0E5/aybD3kE9TewQvpXGK595w')]
    [TestCase('124', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,xudtAfv4z4D/Fqfr7/z1lwdwfgIMKOBE6tHHjjnhCPR/PP/5s5B8BuE773cB7Yf/v/dNFK+/9A')]
    [TestCase('125', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,w+/9wPv6z6A/GKffz/z1lwdwfgIMKOBE6tHHjjnhCPR/PP/5s5B8BuEvPzgD5Wd///ZMOK8/9w')]
    [TestCase('126', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,3M9tgPYUzMB+GJm3Lzz1gtOfPQAW6B+F6s7H/rzgBwRAfgv3V0E5/ayTL3kE9TugAvpUIL595w')]
    [TestCase('127', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,xue9Afv5z7B/Fqrv//z1lwdwfgIMKOBE6tHHjjnhCPR/PP/5s5B8BuE/33cA8qcvQPdNCJ+/8w')]
    [TestCase('128', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,w+/9wPv6z6A/GKffz/z1lwdwfgIMKOBE6tHHjjnhCPR/PP/5s5B8BuEvPzgD5Wd///ZMOK8/9w')]
    [TestCase('129', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,29edQPYVxLD+F5q7Tzz1gtOfPQAW6B+F6s7H/rzgBwRAfgv3V0E5/ayTTzkE9jugAvpRKN495w')]
    [TestCase('130', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,xufdAfv608B/FqvvD/31lwdwfgIMKOBE6tHHjjnhCPR/PP/5s5B8BuE773cA868vQPdNBI8/8w')]
    [TestCase('131', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,w+/9wPv6z6A/GKffz/z1lwdwfgIMKOBE6tHHjjnhCPR/PP/5s5B8BuEvPzgD5Wd///ZMOK8/9w')]
    [TestCase('132', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,2tutAPYWwMB+F5u/Xzz1gtOfPQAW6B+F6s7H/rzgBwRAfgv3V0E5/ayTT/kD9z+gwvlQLP795g')]
    [TestCase('133', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,0,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALsXv3gGgyAfwf6/5A9AMJPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('134', '[$00FF0000;0 claNull;0.5 $000000FF;1],claWhite,Radial,0.08,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALsXv3gGgyAfwf6/5A9AMJPb3gH+u9/gvxCPD/A8Q')]
    procedure TestFillRoundRectGradientWithChessBackground(const APoints, AModulateColor: string; AGradientStyle: TGradientStyle; AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType; ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,z7fSOfuYl36/7LNrnwXud9xeuvidv4D+2skP4fwnva8PPeb8f489AEM8f8Tt5Y+etwQTgL1+Cw')]
    procedure TestFillRoundRectGradientWithResource(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',   'claPurple,claWhite,True,0.15,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,oRevf/jNly/ACHsbcPzsswewPfbgv6/D2Ocjv3nwA3Q/fPr6+4D2AMizrnsDy0ePff4w1H0/8Q')]
    [TestCase('2',   'claPurple,claRed,True,0.15,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,sJfPvfbRv79ADWiL/3zx0pMPvffs55/E1cQ37zr44DNvvP0CMOE1+NL3vv36wF9PfgE+oF398w')]
    [TestCase('3',   'claPurple,claWhite,True,0.15,0.15,AllCorners,Round,0,0,1,1,1,0.98,l4eOQPjPy/9/DHUzwHrrjEegfffto78E1um3HjnnEnA/PP71R/F1Aq2TTrkHyFNPfv1AtK1/6g')]
    [TestCase('4',   'claPurple,claRed,True,0.15,0.15,AllCorners,Round,0,0,1,1,1,0.98,qRtPfvbT+69AEV+PX/vwsLffPPn70+/F0r/Tfjrw6SN//AH/hzE1+Lrnrrv9um8fvwBReE297Q')]
    [TestCase('5',   'claPurple,claWhite,True,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,lq9x+wyvW89+9MED8EXx47MvfPahR9EB1NiPAD0g+7evuOgkPO89BQ5gj4EEurtttw9ZoMx69A')]
    [TestCase('6',   'claPurple,claRed,True,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,p8Nh+wqrr19/9rN7j8XwA0xfvfihc6HB0b6D8D0mAGz/uOwhwK+9ARR4L0P+uw9utw9W7Ay69A')]
    [TestCase('7',   'claPurple,claWhite,True,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,i4+RexCri19+9Mcb8IXwxN/Pu/ejb6GC0Ndj4HwfCcAPOOkmOJ9+BwBkLwAJtIMddxFwTDw67g')]
    [TestCase('8',   'claPurple,claRed,True,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,nqeBOw6n5w+/9rd7bwXw6ms/Pfqko2HCzbhXAH4mD2hv+O0j1F9+AwaAD8IBtecNdxFtqFy57g')]
    [TestCase('9',   'claPurple,claWhite,True,0,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,q/MuAPXYh58/EGf3n/vqvAtAPfbh5w/E2f3nvjrvAlCPffj5A3F2/7mvzrsA1GMffv5AnN1/7g')]
    [TestCase('10',  'claPurple,claRed,True,0,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,vXcf/vHdu09AE1hnL3zv3Yd/fPfuE9AE1tkL33v34R/fvfsENIF19sL33n34x3fvPgFNYJ298A')]
    [TestCase('11',  'claPurple,claWhite,True,0,0.15,AllCorners,Round,0,0,1,1,1,0.98,pFsegfTcv1+/FF0LwDnplkcgPffv1y9F1wJwTrrlEUjP/fv1S9G1AJyTbnkE0nP/fv1SdC0A5w')]
    [TestCase('12',  'claPurple,claRed,True,0,0.15,AllCorners,Round,0,0,1,1,1,0.98,uPeuvvDh9w+AGExjfzruvasvfPj9AyAG09ifjnvv6gsffv8AiME09qfj3rv6woffPwBiMI396Q')]
    [TestCase('13',  'claPurple,claWhite,True,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,mpuBew2tT+++9b0TwMXx48PvO/WkPzGC1duX0Hwf/ZOPeOcmPM+9BA5s34EEvKtttw5VrAy78w')]
    [TestCase('14',  'claPurple,claRed,True,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,qrNxuwuqo3//97GHT4XxA2AP/fejaxFC07+PwL0lAUjvuOsjvG89AROIb0P+vvt9Nw9T+Ez68w')]
    [TestCase('15',  'claPurple,claWhite,True,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,j3uhOxGpe3/+9cIrsEXxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9zf0AJtm8NdxBrXIx67Q')]
    [TestCase('16',  'claPurple,claRed,True,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,o5eROw+m2y9/+LSHL8Xw6oPfPPmmm+GCz7ln4L0lEEBfuOwmzA++AgWQX8IBuM8d9xBqtKx57Q')]
    [TestCase('17',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,pAPvP/fSk++/C3QTIHzst/uvPfbiz69D2fIPL/ruBWC/PPr9+4B2AMOrznsCz0/vff43xH2/8A')]
    [TestCase('18',  'claPurple,claRed,False,0.08,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,tIP/vfTVv39AD2OHv/zw13/vfPfu868E1s4rX/v24yP/PP0GMMG1983z3j36w2ev/gBFkE098w')]
    [TestCase('19',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Round,0,0,1,1,1,0.98,m2/OwPbVy69/D2wrcLrqkDeQfffvt8/E1vWfrjnlFVjfvP35Q/H1AaePbrkGzF+/Pv1IpL2/6Q')]
    [TestCase('20',  'claPurple,claRed,False,0.08,0.15,AllCorners,Round,0,0,1,1,1,0.98,rgOPPvTZ/1/AE1iHHzvwtqOvvPj94+8F08u//rru7RMvfQEDhBF197Tj3vv8vnefPwBYZC297A')]
    [TestCase('21',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,l6OBOw2uV9/+9L8H4IXx478P/PWiR/HB1NiT8Pwf/aePeOgkPO89BQ5kr4EEu7N9dw9YqPz68w')]
    [TestCase('22',  'claPurple,claRed,False,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,qLtxOwurp2//9rJ/b0XxBFg/Pfiib9GB0r6L4P0lAVzveOwhwJ+9ARSAP0P+vAd+tw9W9Dw69A')]
    [TestCase('23',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,jIOh+xCrg2++9MUf4MXwxOu/+/akb8FC0ddr4PweC6z/9+gnOJ9+BwBoTwAJtHsdNxFvVGy67Q')]
    [TestCase('24',  'claPurple,claRed,False,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,oJ+Ruw6n4x8/97Z/X4Xw6ncfvfmko5HCzrdfAD4mEFRfuO0j0D8+AwaEL8IBttstdxFtsIz57Q')]
    [TestCase('25',  'claPurple,claWhite,True,0.15,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,oVefv/jQb0/AA4kjoP3srTPg/fPfvx8E1dBTz3j1+4dfu/fyF3H2AdDbrjsGzEuPfAIvzH3/8A')]
    [TestCase('26',  'claPurple,claRed,True,0.15,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,r9vPvffUk9+ACXOTH77xyrtvffbt7//E0qxj7/n71j9ve/v7PwE2+dkbv739wWdvfQU8oJ398w')]
    [TestCase('27',  'claPurple,claWhite,True,0.15,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,mM9ugPjTnx9ABoU/ILzrhHfQvfTsp0/F0c3z/jftCYRPu/rsZ9G1A7fDTvkKyVsf/QE/rL0/6g')]
    [TestCase('28',  'claPurple,claRed,True,0.15,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,qGtffvfYx78ADWyXv/zwputPvff8309Gz6MDX3n03jNPe//3k1F1+cIPvzsBvHsvfgVOeH297Q')]
    [TestCase('29',  'claPurple,claWhite,True,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,krth+wuxY5++88gHIIbw5JsvfPegQ7GB0tqPwPwg9tvvuOkfNP/9BQ9YT0EEudNNdw5ZhGw69g')]
    [TestCase('30',  'claPurple,claRed,True,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,o8tBOwqtry+/9beDv8XvAzRvffmja4GB0MGDsL0m+4s/+ewdvM89AhZs70L+uR8udw5W2Ly59g')]
    [TestCase('31',  'claPurple,claWhite,True,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,h5+Bew+ujx9+888fIIbvxcPfu/iia4HCztljsDwgA+hfOOohMK8+CAFY7/8Is5vt9g9wLMw58A')]
    [TestCase('32',  'claPurple,claRed,True,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,mq9hew2p67++9buDrwXv6k9fPfulm0GCzLxTwP0mCYy/OO4ezH/+Awlwv4EBsvvN9g9sjPy48A')]
    [TestCase('33',  'claPurple,claWhite,True,0,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,q/MuAPXYh58/EGf3n/vqvAtAPfbh5w/E2f3nvjrvAlCPffj5A3F2/7mvzrsA1GMffv5AnN1/7g')]
    [TestCase('34',  'claPurple,claRed,True,0,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,vXcf/vHdu09AE1hnL3zv3Yd/fPfuE9AE1tkL33v34R/fvfsENIF19sL33n34x3fvPgFNYJ298A')]
    [TestCase('35',  'claPurple,claWhite,True,0,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,pFsegfTcv1+/FF0LwDnplkcgPffv1y9F1wJwTrrlEUjP/fv1S9G1AJyTbnkE0nP/fv1SdC0A5w')]
    [TestCase('36',  'claPurple,claRed,True,0,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,uPeuvvDh9w+AGExjfzruvasvfPj9AyAG09ifjnvv6gsffv8AiME09qfj3rv6woffPwBiMI396Q')]
    [TestCase('37',  'claPurple,claWhite,True,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,mpuBew2tT+++9b0TwMXx48PvO/WkPzGC1duX0Hwf/ZOPeOcmPM+9BA5s34EEvKtttw5VrAy78w')]
    [TestCase('38',  'claPurple,claRed,True,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,qrNxuwuqo3//97GHT4XxA2AP/fejaxFC07+PwL0lAUjvuOsjvG89AROIb0P+vvt9Nw9T+Ez68w')]
    [TestCase('39',  'claPurple,claWhite,True,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,j3uhOxGpe3/+9cIrsEXxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9zf0AJtm8NdxBrXIx67Q')]
    [TestCase('40',  'claPurple,claRed,True,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,o5eROw+m2y9/+LSHL8Xw6oPfPPmmm+GCz7ln4L0lEEBfuOwmzA++AgWQX8IBuM8d9xBqtKx57Q')]
    [TestCase('41',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,nyvvP/jQh+9/B4Aj0DzttvvvffXnx6+D1+YzzznwBWQf/Pn7B5E2Acq/7nsE0VefPQAy4E0/8g')]
    [TestCase('42',  'claPurple,claRed,False,0.08,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,rqsffvbTr3+AC2uXb/3x1IMv/fbz76+E1MNH77r34iM/PP0ENAF2+NQH3/37xW9vvgI/sC299A')]
    [TestCase('43',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,lZveAPjSv69/Cno/MLvrjzfQPfb1q8+E1OfLLrnmFVwfvP33UwH2Aq+njvkIz2dfPv9CxI2/6w')]
    [TestCase('44',  'claPurple,claRed,False,0.08,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,pzOvPvbX70+AD2KX7zvxsqsPPfgD2O9F0b7jfrrv6xNPfAEBiEG1+Lv73vv+wH9PPwJSjA197g')]
    [TestCase('45',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,lKeB+wyvX8/+88IH8AXx47MvPPahR9HB09iT4Dwg+7+feOkiPP99BRBgf4EEur99dw9bnMy69A')]
    [TestCase('46',  'claPurple,claRed,False,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,pb9x+wqrr1//9bN/j4XwA0xfffiib6HB0b6H0D0m/3fvOO0fwL/9ARV0HwP+uw9+dw9Y7Az69A')]
    [TestCase('47',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,iYuhuxCsi1++88gb8EXwxOPPe/ekb5FC0NZn0DwfCcgP+OklOK++BwJgHwAJs4cdNxFySDx67g')]
    [TestCase('48',  'claPurple,claRed,False,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,nKOROw6n5//+9bd/b8Xv6m8//fmkn2HCzbhb8H0mDnBfuO4h1F++Awh8/4EBtOMdNxFvqFy57g')]
    [TestCase('49',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,pW9/f/bZT19AApErwP7rrksQPvHY048E08J7T/j58psPO/TtH1H2AtDjvvsHyz/vewY4nL2/7g')]
    [TestCase('50',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,s/vfffXdb98ACXmfL3/wytPfffTlB0AF0Z2Hb3n/z1MPe/j2O/E1+tgf7/3/v1vvPAlGdO098g')]
    [TestCase('51',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,ne9eAPbdey+ABI5LgD3qhZMgfvHju8+Fz70fX3fy/5/v+vbmb7H1BLfPXvkMyEtffAZKeA1A5w')]
    [TestCase('52',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,rZNv/vTin89ADHKj773vpwfAPfXz968GzZErr/j41UvfuvvwkzG1+sAX37sDumePvQlaSO296w')]
    [TestCase('53',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,j7tRewu1X1/+888TMEbw5of/+/ehN8GB0t2TgDwh8ec/eekaKA9+BhBYPwEEtt8dtwxXcCz69w')]
    [TestCase('54',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,nschuwmxq+9+9byPz4XvBCBPPfqmX5GB0MaHYH0m9ZNvOewYsN+9AhhsvwL+tSf+tgxTxIy5+A')]
    [TestCase('55',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,g59huw60i99989crQEbvx6ufe/mkX5HCztxrYHwg/fe/OOobIL/+CAJY378Ir6u9tg1tEIx58g')]
    [TestCase('56',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,la8xuwyu539+9cGTz4Xu6zcv/fupj1FCzMFbYL0mApT/eO0ZwI9+BAt0n0EBrQOO9g1qeMw48w')]
    [TestCase('57',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,q/MuAPXYh58/EGf3n/vqvAtAPfbh5w/E2f3nvjrvAlCPffj5A3F2/7mvzrsA1GMffv5AnN1/7g')]
    [TestCase('58',  'claPurple,claRed,True,0,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,vXcf/vHdu09AE1hnL3zv3Yd/fPfuE9AE1tkL33v34R/fvfsENIF19sL33n34x3fvPgFNYJ298A')]
    [TestCase('59',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,pFsegfTcv1+/FF0LwDnplkcgPffv1y9F1wJwTrrlEUjP/fv1S9G1AJyTbnkE0nP/fv1SdC0A5w')]
    [TestCase('60',  'claPurple,claRed,True,0,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,uPeuvvDh9w+AGExjfzruvasvfPj9AyAG09ifjnvv6gsffv8AiME09qfj3rv6woffPwBiMI396Q')]
    [TestCase('61',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,mpuBew2tT+++9b0TwMXx48PvO/WkPzGC1duX0Hwf/ZOPeOcmPM+9BA5s34EEvKtttw5VrAy78w')]
    [TestCase('62',  'claPurple,claRed,True,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,qrNxuwuqo3//97GHT4XxA2AP/fejaxFC07+PwL0lAUjvuOsjvG89AROIb0P+vvt9Nw9T+Ez68w')]
    [TestCase('63',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,j3uhOxGpe3/+9cIrsEXxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9zf0AJtm8NdxBrXIx67Q')]
    [TestCase('64',  'claPurple,claRed,True,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,o5eROw+m2y9/+LSHL8Xw6oPfPPmmm+GCz7ln4L0lEEBfuOwmzA++AgWQX8IBuM8d9xBqtKx57Q')]
    [TestCase('65',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,n0////fTf9//BIYvcP3tt/sfvvPr0++D1d1Xf7nxBGjfe/j4D5H2Ac3TDjwG1GNvPQIz6C2/8g')]
    [TestCase('66',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,rcs/fvbWo1+ACW+fD77y1IN//fX3+9/E0rtnn7r44Cvvu/sBOAE2+dYbD379x3c/vgRAvB399A')]
    [TestCase('67',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,lcfuwPfWr49/B4FL8HvskDMQfvT6uw9F0t3z3rjoFGTfu/vzX/G1A7O7vvkK0nMfvgFDzF3/6w')]
    [TestCase('68',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,plvPPvba3y9ADWejr/zxsqdf/fYI6C8Gz7QLH7rw6Rvve//9j1F1+b4PD7wAw48PfwRTnP387g')]
    [TestCase('69',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,kaeB+wywX68+88UHAIbw46sffPaiP7EB09iTsDwg+dO/+OkgPA++BRFYT0EEucNt9w5ckJx69Q')]
    [TestCase('70',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,orth+wqsry8/9bWDnwXwA0RfffikZ4FB0b+HoD0m/Yf/eO0dwN89Ahdw7wL+uhNutw5Z6Oz59Q')]
    [TestCase('71',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,hYehexCuiy++8ssfAIbvxNfPe/ekZ3FCz9ZroDwfB+AvuOoiOL8+CANc7/8Is4sddxB0PAx67w')]
    [TestCase('72',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,mZ+BOw6p58/+9LmDjwXv6V8//fmml0FCzblbsH0mC4R/+O4e0I/+Awp0z0EBs+cddxBwoDz57w')]
    [TestCase('73',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,pnePf/XcQ1/AAZUvQL/qsU9APvDR33/E0r6DPzj876vPevPqH1G2A83frrsIyjffOwg9iP1/7Q')]
    [TestCase('74',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,tAfgPfTgZ9+ACH2jj3/vztMPvvPfEzDF0JiLX3kBzGOvevfyN/H1+tYb770Avk/fvApMZC0+8Q')]
    [TestCase('75',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,nvdewPThaz8ABJNPIL7oiZdQPvDcy78Fz7gnX7f0+6+PuvXjb7G1BbTLTvkNxj8/vAhQYE0A5g')]
    [TestCase('76',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,rp9/fvPml88ADHenb37uqwcAPvTsB5DGzIszn/j60l9/uvrsi0F1+70P77sEuFtvvQthNC1+6g')]
    [TestCase('77',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,jbtBOwu4W08+9NMXQEbw53/fe/iiL9HB0t6bYDwh7+dfeekYIA++BhFcTwEEtOMNtwtVZBy6+A')]
    [TestCase('78',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,nMcRewmzq8++9b6X34XvBRgvffqnW6GB0MeLQD0m85OPuesWrN/9Ahlwv8L9syvu9gtSwIx5+Q')]
    [TestCase('79',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,gZ9Rew63i7/989wvQEbvyaN/+/mkV6ECz91vMHwg+/ff+OkYGM8+CQNc338IrbOdtgxsCGw58w')]
    [TestCase('80',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,kqshewyx41++9cSbz4Xu6y//PPyrh2GCzMNfQH0m/5cfuewWuI/+BA10jwEBqwN+9gxocLw49A')]
    [TestCase('81',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,q/MuAPXYh58/EGf3n/vqvAtAPfbh5w/E2f3nvjrvAlCPffj5A3F2/7mvzrsA1GMffv5AnN1/7g')]
    [TestCase('82',  'claPurple,claRed,True,0,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,vXcf/vHdu09AE1hnL3zv3Yd/fPfuE9AE1tkL33v34R/fvfsENIF19sL33n34x3fvPgFNYJ298A')]
    [TestCase('83',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,pFsegfTcv1+/FF0LwDnplkcgPffv1y9F1wJwTrrlEUjP/fv1S9G1AJyTbnkE0nP/fv1SdC0A5w')]
    [TestCase('84',  'claPurple,claRed,True,0,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,uPeuvvDh9w+AGExjfzruvasvfPj9AyAG09ifjnvv6gsffv8AiME09qfj3rv6woffPwBiMI396Q')]
    [TestCase('85',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,mpuBew2tT+++9b0TwMXx48PvO/WkPzGC1duX0Hwf/ZOPeOcmPM+9BA5s34EEvKtttw5VrAy78w')]
    [TestCase('86',  'claPurple,claRed,True,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,qrNxuwuqo3//97GHT4XxA2AP/fejaxFC07+PwL0lAUjvuOsjvG89AROIb0P+vvt9Nw9T+Ez68w')]
    [TestCase('87',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,j3uhOxGpe3/+9cIrsEXxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9zf0AJtm8NdxBrXIx67Q')]
    [TestCase('88',  'claPurple,claRed,True,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,o5eROw+m2y9/+LSHL8Xw6oPfPPmmm+GCz7ln4L0lEEBfuOwmzA++AgWQX8IBuM8d9xBqtKx57Q')]
    [TestCase('89',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,oF//v/fVe8//A4g3wP3tuPc//vLs2/+D1Nlnb3nyBHDPO/f2E4F2As7bLrwG1WdPPQM16B1/8g')]
    [TestCase('90',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,rd8/PvbYo0/ACHGnb77y1X+vPfX5A+AE0rd3fzr54C/fu/r/OwG2+dcjHz7+yX8vfgVCwB399A')]
    [TestCase('91',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,ltvegPfZr49/BoRTULzskjMwfvP7xy9F0dgHr7jpE2y/e/rxX+E1BLXD3rkL1Hf/vQJFzE3/6w')]
    [TestCase('92',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,pm/f/vXc2x9ADGivD33ys6eP/fUK9E8GzrAf/3nx6SPPO/78j0H1+b8XL7wBxJMPfwVUoO287g')]
    [TestCase('93',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,j6eB+wyxX5/+8sYHAEbw46cffPaiO7HB0tiXoDwg+N/POOofOB/+BRFYT0EEucdtdw5djIz69Q')]
    [TestCase('94',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,oLdh+wqtqx+/9LaDn8XvA0BfffikY3EB0cCLkD0m/I8Pue0cvO99Ahhw38L9uRNudw5Z5Nx59g')]
    [TestCase('95',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,g4ehexCvjx9+8s0fAEbvxM/Pu/ekY3ECz9drgDwfBew/+OogNM9+CANY378Iso8d9w91NPz57w')]
    [TestCase('96',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,lpuBOw6p56++9LqDjwXv6Vs//fmnkzECzbpfkD0mCpCP+O4d0J8+BAt0v0EBsusd9w9wnCy58A')]
    [TestCase('97',  'claPurple,claWhite,True,0.15,0.15,,Round,0.15,0.15,0.85,0.85,1,0.98,j3uhOxGpe3/+9cIrsEXxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9zf0AJtm8NdxBrXIx67Q')]
    [TestCase('98',  'claPurple,claWhite,True,0,0.15,,Round,0.15,0.15,0.85,0.85,1,0.98,j3uhOxGpe3/+9cIrsEXxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9zf0AJtm8NdxBrXIx67Q')]
    [TestCase('99',  'claPurple,claWhite,False,0.08,0.15,,Round,0.15,0.15,0.85,0.85,1,0.98,j3uhOxGpe3/+9cIrsEXxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9zf0AJtm8NdxBrXIx67Q')]
    [TestCase('100', 'claPurple,claWhite,True,0.15,0.15,,Bevel,0.15,0.15,0.85,0.85,1,0.98,j3uhOxGpe3/+9cIrsEXxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9zf0AJtm8NdxBrXIx67Q')]
    [TestCase('101', 'claPurple,claWhite,True,0,0.15,,Bevel,0.15,0.15,0.85,0.85,1,0.98,j3uhOxGpe3/+9cIrsEXxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9zf0AJtm8NdxBrXIx67Q')]
    [TestCase('102', 'claPurple,claWhite,False,0.08,0.15,,Bevel,0.15,0.15,0.85,0.85,1,0.98,j3uhOxGpe3/+9cIrsEXxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9zf0AJtm8NdxBrXIx67Q')]
    [TestCase('103', 'claPurple,claWhite,True,0.15,0.15,,InnerRound,0.15,0.15,0.85,0.85,1,0.98,j3uhOxGpe3/+9cIrsEXxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9zf0AJtm8NdxBrXIx67Q')]
    [TestCase('104', 'claPurple,claWhite,True,0,0.15,,InnerRound,0.15,0.15,0.85,0.85,1,0.98,j3uhOxGpe3/+9cIrsEXxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9zf0AJtm8NdxBrXIx67Q')]
    [TestCase('105', 'claPurple,claWhite,False,0.08,0.15,,InnerRound,0.15,0.15,0.85,0.85,1,0.98,j3uhOxGpe3/+9cIrsEXxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9zf0AJtm8NdxBrXIx67Q')]
    [TestCase('106', 'claPurple,claWhite,True,0.15,0.15,,InnerLine,0.15,0.15,0.85,0.85,1,0.98,j3uhOxGpe3/+9cIrsEXxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9zf0AJtm8NdxBrXIx67Q')]
    [TestCase('107', 'claPurple,claWhite,True,0,0.15,,InnerLine,0.15,0.15,0.85,0.85,1,0.98,j3uhOxGpe3/+9cIrsEXxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9zf0AJtm8NdxBrXIx67Q')]
    [TestCase('108', 'claPurple,claWhite,False,0.08,0.15,,InnerLine,0.15,0.15,0.85,0.85,1,0.98,j3uhOxGpe3/+9cIrsEXxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9zf0AJtm8NdxBrXIx67Q')]
    [TestCase('109', 'claPurple,claWhite,True,0.15,0.15,TopLeft|TopRight|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,i4+RexCri19+9Mcb8IXwxN/Pu/ejb6GC0NdjsHweDJTvt+cpOH/+Bv9nLwAJtIMddxFwTDw67g')]
    [TestCase('110', 'claPurple,claWhite,True,0,0.15,TopLeft|TopRight|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,j3uhOxGpe3/+9cIrsEXxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9zf0AJtm8NdxBrXIx67Q')]
    [TestCase('111', 'claPurple,claWhite,False,0.08,0.15,TopLeft|TopRight|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,jIOh+xCrg2++9MUf4MXwxOu/+/akb8FC0ddrsHweDJTvt+cpOH/+Bv9rTwAJtHsdNxFvVGy67Q')]
    [TestCase('112', 'claPurple,claWhite,True,0.15,0.15,TopLeft|TopRight|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,h5+Bew+ujx9+888fIIbvxcPfu/iia4HCztljsHweDJTvt+cpOH/+Bv9b7/8Is5vt9g9wLMw58A')]
    [TestCase('113', 'claPurple,claWhite,True,0,0.15,TopLeft|TopRight|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,j3uhOxGpe3/+9cIrsEXxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9zf0AJtm8NdxBrXIx67Q')]
    [TestCase('114', 'claPurple,claWhite,False,0.08,0.15,TopLeft|TopRight|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,iYuhuxCsi1++88gb8EXwxOPPe/ekb5FC0NZnsHweDJTvt+cpOH/+Bv9jHwAJs4cdNxFySDx67g')]
    [TestCase('115', 'claPurple,claWhite,True,0.15,0.15,TopLeft|TopRight|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,g59huw60i99989crQEbvx6ufe/mkX5HCztxrsHweDJTvt+cpOH/+Bv9b378Ir6u9tg1tEIx58g')]
    [TestCase('116', 'claPurple,claWhite,True,0,0.15,TopLeft|TopRight|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,j3uhOxGpe3/+9cIrsEXxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9zf0AJtm8NdxBrXIx67Q')]
    [TestCase('117', 'claPurple,claWhite,False,0.08,0.15,TopLeft|TopRight|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,hYehexCuiy++8ssfAIbvxNfPe/ekZ3FCz9ZrsHweDJTvt+cpOH/+Bv9f7/8Is4sddxB0PAx67w')]
    [TestCase('118', 'claPurple,claWhite,True,0.15,0.15,TopLeft|TopRight|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,gZ9Rew63i7/989wvQEbvyaN/+/mkV6ECz91vsHweDJTvt+cpOH/+Bv9f338IrbOdtgxsCGw58w')]
    [TestCase('119', 'claPurple,claWhite,True,0,0.15,TopLeft|TopRight|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,j3uhOxGpe3/+9cIrsEXxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9zf0AJtm8NdxBrXIx67Q')]
    [TestCase('120', 'claPurple,claWhite,False,0.08,0.15,TopLeft|TopRight|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,g4ehexCvjx9+8s0fAEbvxM/Pu/ekY3ECz9drsHweDJTvt+cpOH/+Bv9b378Iso8d9w91NPz57w')]
    [TestCase('121', 'claPurple,claWhite,True,0.15,0.15,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,i4+RexCri19+9Mcb8EXxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9nLwAJtIMddxFwTDw67g')]
    [TestCase('122', 'claPurple,claWhite,True,0,0.15,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,j3uhOxGpe3/+9cIrsEXxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9zf0AJtm8NdxBrXIx67Q')]
    [TestCase('123', 'claPurple,claWhite,False,0.08,0.15,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,jIOh+xCrg2++9MUf4EXxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9rTwAJtHsdNxFvVGy67Q')]
    [TestCase('124', 'claPurple,claWhite,True,0.15,0.15,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,h5+Bew+ujx9+888fIEbxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9b7/8Is5vt9g9wLMw58A')]
    [TestCase('125', 'claPurple,claWhite,True,0,0.15,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,j3uhOxGpe3/+9cIrsEXxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9zf0AJtm8NdxBrXIx67Q')]
    [TestCase('126', 'claPurple,claWhite,False,0.08,0.15,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,iYuhuxCsi1++88gb8EXxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9jHwAJs4cdNxFySDx67g')]
    [TestCase('127', 'claPurple,claWhite,True,0.15,0.15,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,g59huw60i99989crQEbxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9b378Ir6u9tg1tEIx58g')]
    [TestCase('128', 'claPurple,claWhite,True,0,0.15,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,j3uhOxGpe3/+9cIrsEXxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9zf0AJtm8NdxBrXIx67Q')]
    [TestCase('129', 'claPurple,claWhite,False,0.08,0.15,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,hYehexCuiy++8ssfAEbxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9f7/8Is4sddxB0PAx67w')]
    [TestCase('130', 'claPurple,claWhite,True,0.15,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,gZ9Rew63i7/989wvQEbxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9f338IrbOdtgxsCGw58w')]
    [TestCase('131', 'claPurple,claWhite,True,0,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,j3uhOxGpe3/+9cIrsEXxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9zf0AJtm8NdxBrXIx67Q')]
    [TestCase('132', 'claPurple,claWhite,False,0.08,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,g4ehexCvjx9+8s0fAEbxxPOPO/amZxFD0tprsHweDJTvt+cpOH/+Bv9b378Iso8d9w91NPz57w')]
    [TestCase('133', '$BF0000FF,claWhite,True,0.5,0.5,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,0.85,0.98,jyeyegzK204+7+NfUAfx/uP/O+yPr2DB4fVjcfkd2O/vt+EkzN78Ayj0zoL82jfOdgUkdCw8Ag')]
    [TestCase('134', '$BF0000FF,claWhite,False,0.5,0.5,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,0.85,0.98,jyeyegzK204+7+NfUAfx/uP/O+yPr2DB4fVjcfkd2O/vt+EkzN78Ayj0zoL82jfOdgUkdCw8Ag')]
    [TestCase('135', '$BF0000FF,claWhite,True,0.5,0.5,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,q2eieQja31697LuvT0jx/+P/O+yOq1AB4vVfcTke1/Pvt+EkyN68AyjcLsP14NvetwEVzFz9Ag')]
    [TestCase('136', '$BF0000FF,claWhite,False,0.5,0.5,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,q2eieQja31697LuvT0jx/+P/O+yOq1AB4vVfcTke1/Pvt+EkyN68AyjcLsP14NvetwEVzFz9Ag')]
    [TestCase('137', '$BF0000FF,claWhite,True,0.5,0.5,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,xy9iOf3dC59/9KvDT4fxBNzPu+uNryAB4/RLsXke1ffft+EizN78AykEz0Xy6w8vuQISYF18Cg')]
    [TestCase('138', '$BF0000FF,claWhite,False,0.5,0.5,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,xy9iOf3dC59/9KvDT4fxBNzPu+uNryAB4/RLsXke1ffft+EizN78AykEz0Xy6w8vuQISYF18Cg')]
    [TestCase('139', '$BF0000FF,claWhite,True,0.5,0.5,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,zDNC+frT4y7A9qfDT0byCNCvO+uLu/AA5PFH0Xkd1e+P9+Eh5N78BCw8H4fw5wtfeQQJpA09DQ')]
    [TestCase('140', '$BF0000FF,claWhite,False,0.5,0.5,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,zDNC+frT4y7A9qfDT0byCNCvO+uLu/AA5PFH0Xkd1e+P9+Eh5N78BCw8H4fw5wtfeQQJpA09DQ')]
    [TestCase('141', '$BF0000FF,claWhite,True,0.75,0.75,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,0.85,0.98,mFsiugzS18797NgrkAfx/uP/O+yPr2DB4fVjcfkd2O/vt+EkzN78AyjgjsL63Yce9wMgiNy8AA')]
    [TestCase('142', '$BF0000FF,claWhite,False,0.75,0.75,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,0.85,0.98,mFsiugzS18797NgrkAfx/uP/O+yPr2DB4fVjcfkd2O/vt+EkzN78AyjgjsL63Yce9wMgiNy8AA')]
    [TestCase('143', '$BF0000FF,claWhite,True,0.75,0.75,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,q2eieQja31697LuvT0jx/+P/O+yOq1AB4vVfcTke1/Pvt+EkyN68AyjcLsP14NvetwEVzFz9Ag')]
    [TestCase('144', '$BF0000FF,claWhite,False,0.75,0.75,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,q2eieQja31697LuvT0jx/+P/O+yOq1AB4vVfcTke1/Pvt+EkyN68AyjcLsP14NvetwEVzFz9Ag')]
    [TestCase('145', '$BF0000FF,claWhite,True,0.75,0.75,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,yCtiufvU437A9qXPP8bxBtS/e+uMtxDB4/JHwfkd1fOvt+Ei2N58BCssv4bw5Bcv+QMMnN38Dg')]
    [TestCase('146', '$BF0000FF,claWhite,False,0.75,0.75,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,yCtiufvU437A9qXPP8bxBtS/e+uMtxDB4/JHwfkd1fOvt+Ei2N58BCssv4bw5Bcv+QMMnN38Dg')]
    [TestCase('147', '$BF0000FF,claWhite,True,0.75,0.75,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,0UfS+fvNg36B+Jm/b4XyD9Cv++qJtwBB5PBTwbkc2OuPN+Ii9L68BCtgDwju2isPOQT26039Ew')]
    [TestCase('148', '$BF0000FF,claWhite,False,0.75,0.75,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,0UfS+fvNg36B+Jm/b4XyD9Cv++qJtwBB5PBTwbkc2OuPN+Ii9L68BCtgDwju2isPOQT26039Ew')]
    [TestCase('149', '$BF0000FF,claWhite,True,0.5,0.5,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,gxuyOhDM87497u+HgEft5s8vevaJt2AC4OqrATgd2xfweeYTiE69AiLknsH/2BNedgUyGNw7/g')]
    [TestCase('150', '$BF0000FF,claWhite,False,0.5,0.5,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,gxuyOhDM87497u+HgEft5s8vevaJt2AC4OqrATgd2xfweeYTiE69AiLknsH/2BNedgUyGNw7/g')]
    [TestCase('151', '$BF0000FF,claWhite,True,0.5,0.5,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,pGtyOQvd+74867+3v0jqB4DfOwGks0B/3uTbUXojtGOfPeoJFP88BCrIDgL44NuOtwAhhDw9/w')]
    [TestCase('152', '$BF0000FF,claWhite,False,0.5,0.5,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,pGtyOQvd+74867+3v0jqB4DfOwGks0B/3uTbUXojtGOfPeoJFP88BCrIDgL44NuOtwAhhDw9/w')]
    [TestCase('153', '$BF0000FF,claWhite,True,0.5,0.5,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,xycSOfzcL2//9K7Lv0f3NiRvPPy2i6A95eJvoXwonH/PPukX3F67ADL8HsXz8hcfOQIhOD18CA')]
    [TestCase('154', '$BF0000FF,claWhite,False,0.5,0.5,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,xycSOfzcL2//9K7Lv0f3NiRvPPy2i6A95eJvoXwonH/PPukX3F67ADL8HsXz8hcfOQIhOD18CA')]
    [TestCase('155', '$BF0000FF,claWhite,True,0.5,0.5,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,0DPCePjL7/7/96mzX4b7WeSePPiplwD96dtL4TwnmnsvPegeMG+7AUFQH8fw7gdfuQQSpE39Cg')]
    [TestCase('156', '$BF0000FF,claWhite,False,0.5,0.5,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,0DPCePjL7/7/96mzX4b7WeSePPiplwD96dtL4TwnmnsvPegeMG+7AUFQH8fw7gdfuQQSpE39Cg')]
    [TestCase('157', '$BF0000FF,claWhite,True,0.75,0.75,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,jVsCOhDV6y696+JH4Mfq5stv+vqMs5DB3+HXgbgf0r8/u+YNxN49AibMTsH93HOudgMuMKx8/A')]
    [TestCase('158', '$BF0000FF,claWhite,False,0.75,0.75,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,jVsCOhDV6y696+JH4Mfq5stv+vqMs5DB3+HXgbgf0r8/u+YNxN49AibMTsH93HOudgMuMKx8/A')]
    [TestCase('159', '$BF0000FF,claWhite,True,0.75,0.75,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,pGtyOQvd+74867+3v0jqB4DfOwGks0B/3uTbUXojtGOfPeoJFP88BCrIDgL44NuOtwAhhDw9/w')]
    [TestCase('160', '$BF0000FF,claWhite,False,0.75,0.75,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,pGtyOQvd+74867+3v0jqB4DfOwGks0B/3uTbUXojtGOfPeoJFP88BCrIDgL44NuOtwAhhDw9/w')]
    [TestCase('161', '$BF0000FF,claWhite,True,0.75,0.75,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,yyPyuPnO817A96bLb8b5VuDO/Piqf1D95+JDkfwonIdv/eYfDE+7/0A0f0bx6hcf+QMXjP08DQ')]
    [TestCase('162', '$BF0000FF,claWhite,False,0.75,0.75,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,yyPyuPnO817A96bLb8b5VuDO/Piqf1D95+JDkfwonIdv/eYfDE+7/0A0f0bx6hcf+QMXjP08DQ')]
    [TestCase('163', '$BF0000FF,claWhite,True,0.75,0.75,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,2VOSefrFe55B+pWvTwX5iKweffWQZ5C96N1jwXwlqmvv++YihI+6/Et0nwju4C/PuAPzE579Eg')]
    [TestCase('164', '$BF0000FF,claWhite,False,0.75,0.75,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,2VOSefrFe55B+pWvTwX5iKweffWQZ5C96N1jwXwlqmvv++YihI+6/Et0nwju4C/PuAPzE579Eg')]
    [TestCase('165', '$000000FF,claWhite,False,0.75,0.75,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('166', '$FF0000FF,claWhite,False,0.75,0.75,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    procedure TestFillRoundRectSolid(const AColor, AModulateColor: string; ABlending: Boolean; AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType; ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',   'claPurple,claWhite,True,0.15,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,rFP+wPXj119/EnQDIDrrjjtQfff34x+F2/qXjnrkFHgf/vz6Q7G2AafLPjkE1Hdff/1QuC2A6A')]
    [TestCase('2',   'claPurple,claRed,True,0.15,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,u9MeP/Tm/+//FmFzr7rvrcev/PgEBBCG2Nevvjvs8TdPPgACeBH2+LITP7v7yI8fAABehB1+6w')]
    [TestCase('3',   'claPurple,claWhite,True,0.15,0.15,AllCorners,Round,0,0,1,1,1,0.98,plvuAPTes3//EmQD0PnojzfwvPXw118F1/mHDjrlE2DfPfv6T4E1AqO3XrkDzWPPfv1TdC0A5w')]
    [TestCase('4',   'claPurple,claRed,True,0.15,0.15,AllCorners,Round,0,0,1,1,1,0.98,t++uPvLi4y8AGE5bb3rus6svfPf+B3DG08+jbjvu6hMP/v4EkMG0968Hv7v5wH+vvwBjOL196g')]
    [TestCase('5',   'claPurple,claWhite,True,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,qwdegPv0T3A9GKwvMP36gQtQOwD9g89F6Ma3TjriBixQ/AMOoIC5BsIbf/gD8WdPgflTQA7/6Q')]
    [TestCase('6',   'claPurple,claRed,True,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,vB8+gPnwnxA+Gp6jz7z6oquPfAL9q58F5qyrPjvoC+Sv/AcLJEE5A8czD7r98rtPgflQkE4+6g')]
    [TestCase('7',   'claPurple,claWhite,True,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,pQMuwPryQ0C9GaYzcL37gPuv+v/2Zx8G5ryrvnniAzTAewMSkNC4B8ITfzgD8E8fwfhWFM5+6A')]
    [TestCase('8',   'claPurple,claRed,True,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,uBsOgPjun/D9G5WT73z7posfvAL2m++F452f3nrpCNwv/AcOLIG4A8gvX/r78a8PAflTcO796A')]
    [TestCase('9',   'claPurple,claWhite,True,0,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,qlMuQfbi3z//E28LEDrrlEtw/fj33//E2wKEvjrlE2Qfvv31PwF3AKKvTvkE2Id/P/1QuC2A6A')]
    [TestCase('10',  'claPurple,claRed,True,0,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,vNceP/PmE9B/F2B7r3rvtcev/PkDBMAF2N6r3jvt8jNfvgAAdBG296vzTrv8y5dPgP9dfO096w')]
    [TestCase('11',  'claPurple,claWhite,True,0,0.15,AllCorners,Round,0,0,1,1,1,0.98,pFsegfTcv1+/FF0LwDnplkcgPffv1y9F1wJwTrrlEUjP/fv1S9G1AJyTbnkE0nP/fv1SdC0A5w')]
    [TestCase('12',  'claPurple,claRed,True,0,0.15,AllCorners,Round,0,0,1,1,1,0.98,uPeuvvDh9w+AGExjfzruvasvfPj9AyAG09ifjnvv6gsffv8AiME09qfj3rv6woffPwBiMI396Q')]
    [TestCase('13',  'claPurple,claWhite,True,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,qgc+wPv0T4A9GKsvMP36ggtg+//+f79F6MezXvrhByBgPAQOqIC5BsEfj/gD8GM/AflTQA4/6g')]
    [TestCase('14',  'claPurple,claRed,True,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,ux8uwPnxoyB+Gp+jv7z6oquPvAL9q5/F5ayvXjvoC9i/fAgLKDH5AsY7H7r98rNfgflSjE5+6g')]
    [TestCase('15',  'claPurple,claWhite,True,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,pP8NAPvyP1D9GaQzYL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8AXj3gD70sPQfhWGN7+6A')]
    [TestCase('16',  'claPurple,claRed,True,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,uB/+//junwB+HJaP3zz7p4cPvAL2m+9F452j/nrpCNA/vAgOMGF4A8Y3X/r78asfAflVcP796A')]
    [TestCase('17',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,q1sOwfXi31//EnIHEDrrkD9g/ff53/8E2/yTnnrkFGwPPv34Q8E2AaO/PnkE1n9vP/1QtC1A6A')]
    [TestCase('18',  'claPurple,claRed,False,0.08,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,u9sev/PmC+C/FmF7r/rvsMOvPPkFBPDF19mzvrvs8jNPPgABfAE2+K4HPzv8yZMfwP9egA3+6g')]
    [TestCase('19',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Round,0,0,1,1,1,0.98,pWP+APTdu39/E2EHwDnpkjcQffbx1z/F1vuDHjrlE1DPffv4U5F1AZ6nXvkD0Gvffv1TcC2A5g')]
    [TestCase('20',  'claPurple,claRed,False,0.08,0.15,AllCorners,Round,0,0,1,1,1,0.98,uPe+fvHh7y/AF01jf7rut6cvvPf/A1DG0tGnfrvu6wsf/v4CkLH09qr7vjv6wYO/fwBjMK296Q')]
    [TestCase('21',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,qgc+wPv0T3A9GKwvMP36ggtgOwD9g89F6MazXjriByhQ/AMOpIB5BsIfj/gD8GNPQflTQA4/6g')]
    [TestCase('22',  'claPurple,claRed,False,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,ux8uwPnxnxA+Gp+jv3z6oqePfAL9q58F5qyrTnvoC9yvPAgLKDH5Asc3H7r98rdPgflRkE5+6g')]
    [TestCase('23',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,pAMOAPvyP0D9GaU3cH37gfu/+v/2Zx8G5ryr3nniBCzAewMSmMC4B8IXj3gD70sfgfhWGM7+6A')]
    [TestCase('24',  'claPurple,claRed,False,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,tx/+v/jun/A9HJWT7zz7p4cfvAL2m++F452f/rrpCdQvfAgOLHF4A8gzb/r78K8fwfhUcO496Q')]
    [TestCase('25',  'claPurple,claWhite,True,0.15,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,qjtOgPTkz08/E33XbzrshkcwPff1x29F3vCbTjrkFKQ/vvz5O9G2A673jvkDzW9vP/xNzC1A6g')]
    [TestCase('26',  'claPurple,claRed,True,0.15,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,uL+OvvPp79//GGdH37rwo8/PvPkC9E8G3Myrfnvq8F9PvgACYFE2+7c3n3v7wotPAP9anD1+7Q')]
    [TestCase('27',  'claPurple,claWhite,True,0.15,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,oz8egPLgp2+/E27PL3rqhkPgfPXst6+F2u2PvvnkE5T/vfr5R6E1BKrjvjkDxVvfPvxPjC0A6Q')]
    [TestCase('28',  'claPurple,claRed,True,0.15,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,tNsOfvHlzw9AGlQnr7rvqLdffPj887/G18OfHjvs6UMPfv8DcBE1+rUvL3z5uHvvv/9fVO297A')]
    [TestCase('29',  'claPurple,claWhite,True,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,rQ++APv2T6A9F7EvUD38fiNguwD9h78F6cm3DvriBzBgfAMSmKA5B8MbX/gC9XtvgflQOA6/6A')]
    [TestCase('30',  'claPurple,claRed,True,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,vR+eAPnymzD+GKCr33z7nb+//AL/r48F57Gn7nroDOSvvAYPIHF5A8kv/7n89ctPgflNjF4+6Q')]
    [TestCase('31',  'claPurple,claWhite,True,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,pwueAPr1P3B9GKs3kP38fBewugD2bx8G57+rfjnjBDjQuwIWiPB4CMMTXzgC9Gc/AflTDN4+5w')]
    [TestCase('32',  'claPurple,claRed,True,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,uht+wPfwmyC+GpebD338oaM/PAP4n9+F5KKbjvrpCdwvfAYTKME4BMorP/r69McfAflPbA6+5w')]
    [TestCase('33',  'claPurple,claWhite,True,0,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,qlMuQfbi3z//E28LEDrrlEtw/fj33//E2wKEvjrlE2Qfvv31PwF3AKKvTvkE2Id/P/1QuC2A6A')]
    [TestCase('34',  'claPurple,claRed,True,0,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,vNceP/PmE9B/F2B7r3rvtcev/PkDBMAF2N6r3jvt8jNfvgAAdBG296vzTrv8y5dPgP9dfO096w')]
    [TestCase('35',  'claPurple,claWhite,True,0,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,pFsegfTcv1+/FF0LwDnplkcgPffv1y9F1wJwTrrlEUjP/fv1S9G1AJyTbnkE0nP/fv1SdC0A5w')]
    [TestCase('36',  'claPurple,claRed,True,0,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,uPeuvvDh9w+AGExjfzruvasvfPj9AyAG09ifjnvv6gsffv8AiME09qfj3rv6woffPwBiMI396Q')]
    [TestCase('37',  'claPurple,claWhite,True,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,qgc+wPv0T4A9GKsvMP36ggtg+//+f79F6MezXvrhByBgPAQOqIC5BsEfj/gD8GM/AflTQA4/6g')]
    [TestCase('38',  'claPurple,claRed,True,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,ux8uwPnxoyB+Gp+jv7z6oquPvAL9q5/F5ayvXjvoC9i/fAgLKDH5AsY7H7r98rNfgflSjE5+6g')]
    [TestCase('39',  'claPurple,claWhite,True,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,pP8NAPvyP1D9GaQzYL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8AXj3gD70sPQfhWGN7+6A')]
    [TestCase('40',  'claPurple,claRed,True,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,uB/+//junwB+HJaP3zz7p4cPvAL2m+9F452j/nrpCNA/vAgOMGF4A8Y3X/r78asfAflVcP796A')]
    [TestCase('41',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,ql/eQPXh148/Enn3L7rrjTdgPff71w9F2/SrbnrkE4Affvz2R+G2AaLbbvkD0YN/v/xOsC0A6A')]
    [TestCase('42',  'claPurple,claRed,False,0.08,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,udv+vvPkAxBAFmVrv3rwq7+v/PgH/A+G2NG/jvvr8EM/fv/+e0H2+Kwjb3v7xZs/AP9bgB2+6g')]
    [TestCase('43',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,pGe+gPPbt6+/Emn33/npji8AffX0y08F1/Gf7jnlEmjfffr1V7H1AZzHnnkDynP/fvxQbC1A5g')]
    [TestCase('44',  'claPurple,claRed,False,0.08,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,tfuefvHg41+AF1FPj3rvsaM/fPcC+H+G08i3Pvvt6B8PPv7/jwG196gb73v5vIvff/9fNL196Q')]
    [TestCase('45',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,qg9egPv1T3D9F60zQP36gQtQewD8g79F6MazTrriCCxQvAMPnID5BsMfj/gD8WtPQflSQA7/6Q')]
    [TestCase('46',  'claPurple,claRed,False,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,uyM+gPnxnwD+GZ6rz7z6oqufvAL9q49F5q2rPvvoDeCvvAcMIEF5A8k3L3r98rtPQflQkE4+6g')]
    [TestCase('47',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,owsuwPr0P0B9GaY7gL37gf+vOgD1Zx9G5ryrvjnjBTDAOwMTjND4B8MXnzgD71MfgfhVFN5+6A')]
    [TestCase('48',  'claPurple,claRed,False,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,tyMOQPjvm+C9G5Wb/zz7posfvAL2m9/F456f3jrqCtgvvAcPJIH4A8ozf7r78LMPgfhSdP796A')]
    [TestCase('49',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,qi8eAPTnx08/E37Hn7rshEdAvff2r59F3+ubHjrkFrBPPv36P9H2BLIDf/kDy2t/P/xL0B2A6g')]
    [TestCase('50',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,t7tuPvPr58+/GWc77zrxoc///PoE3F9G3canPrvp8m8/PgEDWGE2/Lo/r/v7v4dvwP5ZqE0+7g')]
    [TestCase('51',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,oy/eP/Ljn2+/E3C/b/rqhEPg/PXum++F2+ePjrnkFqAPPvv6S6G1Ba/3vnkDw1fvPvxNkB1A6Q')]
    [TestCase('52',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,s9fu/fDnxw9AG1QXzzrwpbOPvPn+189G2bub3jrr60//PQAEaCG1+7k/T/z5tXMfQP9dZP297Q')]
    [TestCase('53',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,rhMOgfr5S+B9FrYzcP38fDeAewH9i6+F6cyz3jnjCCyA/AIWmLB5B8MXX3gC+I9/gflPOB7/5w')]
    [TestCase('54',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,vh/uwPj1l2A+GKOz/zz8mtPffAMCsH+F57WnvnroDNzPfAUUIJH5A8sv3zn89tdvgflMkH6+6A')]
    [TestCase('55',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,qQ/+gPn4O8C9F7E7sP39ejPgOgH3cw+G58KrPrnjBTQA/AEbhBC5CMMPX3gB939fAflRDO4+5g')]
    [TestCase('56',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,uxvOgPfzl2C+GZqjPz39nbtvvAP8o89F5aebPvrpCtRPPAUYJOF4BMwnHzr69tMvAflNdC7+5g')]
    [TestCase('57',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,qlMuQfbi3z//E28LEDrrlEtw/fj33//E2wKEvjrlE2Qfvv31PwF3AKKvTvkE2Id/P/1QuC2A6A')]
    [TestCase('58',  'claPurple,claRed,True,0,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,vNceP/PmE9B/F2B7r3rvtcev/PkDBMAF2N6r3jvt8jNfvgAAdBG296vzTrv8y5dPgP9dfO096w')]
    [TestCase('59',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,pFsegfTcv1+/FF0LwDnplkcgPffv1y9F1wJwTrrlEUjP/fv1S9G1AJyTbnkE0nP/fv1SdC0A5w')]
    [TestCase('60',  'claPurple,claRed,True,0,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,uPeuvvDh9w+AGExjfzruvasvfPj9AyAG09ifjnvv6gsffv8AiME09qfj3rv6woffPwBiMI396Q')]
    [TestCase('61',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,qgc+wPv0T4A9GKsvMP36ggtg+//+f79F6MezXvrhByBgPAQOqIC5BsEfj/gD8GM/AflTQA4/6g')]
    [TestCase('62',  'claPurple,claRed,True,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,ux8uwPnxoyB+Gp+jv7z6oquPvAL9q5/F5ayvXjvoC9i/fAgLKDH5AsY7H7r98rNfgflSjE5+6g')]
    [TestCase('63',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,pP8NAPvyP1D9GaQzYL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8AXj3gD70sPQfhWGN7+6A')]
    [TestCase('64',  'claPurple,claRed,True,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,uB/+//junwB+HJaP3zz7p4cPvAL2m+9F452j/nrpCNA/vAgOMGF4A8Y3X/r78asfAflVcP796A')]
    [TestCase('65',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,qFuOwPTf048/En/nT7rsjDdgPff8xx+F3OuvTnrkFJAv/vvzRwF3AqHrnrkDzoOPv/tLtC1A6A')]
    [TestCase('66',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,ttfOfvPj+x8AF2lX73rxqb/PPPkJ7A/G2cnDbnvr8E9PPv/8c4G2+aozjzv7wptvAP5YjB3+6g')]
    [TestCase('67',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,oWNuwPLZr7+/EnDjDzrrjS8QffX1t09F2OenzjnlE3j/PfryU+H1ApvbzvkCx3MPv/tMcC2A5g')]
    [TestCase('68',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,sfdOPvHe219AGFY7v7rwr6Nf/PcE5G8G1b+7Hnvt6S8P/v38h0G1+KYvH/z4uIsPQP5cQL296Q')]
    [TestCase('69',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,qRN+QPv3S4B9F683UD37gRdwuwD7f7+F6MivPnrjCixgfAMRlJA5B8Qjn3gD8nNfAflRPB4/6Q')]
    [TestCase('70',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,uideQPnzlwA+GZ+v7/z6oLOv/AL9p4/F5q+jLnvpDuCv/AYOGGG5A8s3Pzr988NPwfhOkG6+6Q')]
    [TestCase('71',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,oxNOgPr2N1C9GKk/kP37gAfAugD0Zw+G5r2nrrnjBzDQuwIVhOB4CMUbn7gC8V8fQfhUEO6+5w')]
    [TestCase('72',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,ticuAPjxk/D9GpefH737pZM//AL3l99F5KCXzvrqC9gv/AYRHKF4BMwzj3r78rsfQfhQdB4+6A')]
    [TestCase('73',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,qiseAPTow18/E37Hv3rshUdAvff2p58F3+ufHjrkFrQ/Pv37Q8E2BbMHbzkEy3OPP/xLzB3A6g')]
    [TestCase('74',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,uLuO/vLs59//GWY3/zrxocsPPfsD2F8G3cWnPnvp9G8ffgEDWHF2/LtDr3v8vouPwP5ZpF2+7g')]
    [TestCase('75',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,pCveP/Ljm3+/E2+7j7rqhEPg/PXtl+9F2+eTjrnkFqT/ffv7T5H1BbD7nrkDwlv/PvxMjB2A6Q')]
    [TestCase('76',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,tNMOvvDoxw+AG1MT3zrwpq+f/Pn9z88G2bubzvrq7VPfPQAFaDH1+7pDP3z6tHsvAP9dXP397Q')]
    [TestCase('77',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,rxMuQfr6S/B9FrgzcD39fD+QewH+i6/F6cyzvnnjCCygvAIYlNC5B8MXXzgC+ZN/gflPPC7/5w')]
    [TestCase('78',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,vh/+wPj1m3D+F6SzD738mdvvfAMDsH/F57annnroDNjf/AQWIKH5A8srzzn899tvwflMlI5+6A')]
    [TestCase('79',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,qQ8uQfn5O+C9F7M7wH3+ejfwegH3cw8G6MOnHvnjBTAg/AEdhCD5CMMLX3gB+INfAflREP7+5Q')]
    [TestCase('80',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,uxvugPf0l3B+GZynT739ncN//AP9o8+F5aiXLvrpCtBfvAQaJPG4BM0nDzr69tc/QflNeE7+5g')]
    [TestCase('81',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,qlMuQfbi3z//E28LEDrrlEtw/fj33//E2wKEvjrlE2Qfvv31PwF3AKKvTvkE2Id/P/1QuC2A6A')]
    [TestCase('82',  'claPurple,claRed,True,0,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,vNceP/PmE9B/F2B7r3rvtcev/PkDBMAF2N6r3jvt8jNfvgAAdBG296vzTrv8y5dPgP9dfO096w')]
    [TestCase('83',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,pFsegfTcv1+/FF0LwDnplkcgPffv1y9F1wJwTrrlEUjP/fv1S9G1AJyTbnkE0nP/fv1SdC0A5w')]
    [TestCase('84',  'claPurple,claRed,True,0,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,uPeuvvDh9w+AGExjfzruvasvfPj9AyAG09ifjnvv6gsffv8AiME09qfj3rv6woffPwBiMI396Q')]
    [TestCase('85',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,qgc+wPv0T4A9GKsvMP36ggtg+//+f79F6MezXvrhByBgPAQOqIC5BsEfj/gD8GM/AflTQA4/6g')]
    [TestCase('86',  'claPurple,claRed,True,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,ux8uwPnxoyB+Gp+jv7z6oquPvAL9q5/F5ayvXjvoC9i/fAgLKDH5AsY7H7r98rNfgflSjE5+6g')]
    [TestCase('87',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,pP8NAPvyP1D9GaQzYL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8AXj3gD70sPQfhWGN7+6A')]
    [TestCase('88',  'claPurple,claRed,True,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,uB/+//junwB+HJaP3zz7p4cPvAL2m+9F452j/nrpCNA/vAgOMGF4A8Y3X/r78asfAflVcP796A')]
    [TestCase('89',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,p1duwPTfz4//EYLjXzrtjTdwPff8vw8F3eevPrrkFZQ/PvzyRwH3AqHvrrkDzn+Pf/tKuC1A6A')]
    [TestCase('90',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,tdOuvvPi9w8AF2pT//rxqr/vfPkJ5P9F2sW/Xnvr8VNPfv/7b4E2+qo3nzv7wZdvgP1XkB2+6g')]
    [TestCase('91',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,oF9OwPLZq69/EnTbH7rrji8QffX1r0/F2OOjvjnlFIAPPvrxU+F1A5zf3vkCxm8PP/tLdB2A5g')]
    [TestCase('92',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,sPM+fvHd10+AGFgzzzrxsKN//PcE3G/G1bu3/jrt6jMfPv78g1E1+aYzL/z4t4cfwP1aSL196Q')]
    [TestCase('93',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,qReOQPv5R5D9FrE3YH37gBuA+wD8f6/F6MmvLrrjCihgPAMSkJB5B8Ujn3gD9HtfwfhQPC7/6A')]
    [TestCase('94',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,uSt+APn0kxD+GKCz//z6oLe//AL+p38F57CjHrvpDtyvfAYPFGH5A8s7Pzr99MdfwfhNlH5+6Q')]
    [TestCase('95',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,ohduQPr3N2B9GKs/oD38fw/Q+gD0Zw+G5r+jnjnkCDDQewIXgOC4CMUbr7gC82cvAfhTEO5+5w')]
    [TestCase('96',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,titOwPfyj/B9GpijL737pJtPPAP3k8+F5KKTvjrrDNQvfAYTGLG4BM03jzr7878vAfhOdC7+5w')]
    [TestCase('97',  'claPurple,claWhite,True,0.15,0.15,,Round,0.15,0.15,0.85,0.85,1,0.98,pP8NAPvyP1D9GaQzYL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8AXj3gD70sPQfhWGN7+6A')]
    [TestCase('98',  'claPurple,claWhite,True,0,0.15,,Round,0.15,0.15,0.85,0.85,1,0.98,pP8NAPvyP1D9GaQzYL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8AXj3gD70sPQfhWGN7+6A')]
    [TestCase('99',  'claPurple,claWhite,False,0.08,0.15,,Round,0.15,0.15,0.85,0.85,1,0.98,pP8NAPvyP1D9GaQzYL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8AXj3gD70sPQfhWGN7+6A')]
    [TestCase('100', 'claPurple,claWhite,True,0.15,0.15,,Bevel,0.15,0.15,0.85,0.85,1,0.98,pP8NAPvyP1D9GaQzYL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8AXj3gD70sPQfhWGN7+6A')]
    [TestCase('101', 'claPurple,claWhite,True,0,0.15,,Bevel,0.15,0.15,0.85,0.85,1,0.98,pP8NAPvyP1D9GaQzYL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8AXj3gD70sPQfhWGN7+6A')]
    [TestCase('102', 'claPurple,claWhite,False,0.08,0.15,,Bevel,0.15,0.15,0.85,0.85,1,0.98,pP8NAPvyP1D9GaQzYL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8AXj3gD70sPQfhWGN7+6A')]
    [TestCase('103', 'claPurple,claWhite,True,0.15,0.15,,InnerRound,0.15,0.15,0.85,0.85,1,0.98,pP8NAPvyP1D9GaQzYL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8AXj3gD70sPQfhWGN7+6A')]
    [TestCase('104', 'claPurple,claWhite,True,0,0.15,,InnerRound,0.15,0.15,0.85,0.85,1,0.98,pP8NAPvyP1D9GaQzYL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8AXj3gD70sPQfhWGN7+6A')]
    [TestCase('105', 'claPurple,claWhite,False,0.08,0.15,,InnerRound,0.15,0.15,0.85,0.85,1,0.98,pP8NAPvyP1D9GaQzYL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8AXj3gD70sPQfhWGN7+6A')]
    [TestCase('106', 'claPurple,claWhite,True,0.15,0.15,,InnerLine,0.15,0.15,0.85,0.85,1,0.98,pP8NAPvyP1D9GaQzYL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8AXj3gD70sPQfhWGN7+6A')]
    [TestCase('107', 'claPurple,claWhite,True,0,0.15,,InnerLine,0.15,0.15,0.85,0.85,1,0.98,pP8NAPvyP1D9GaQzYL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8AXj3gD70sPQfhWGN7+6A')]
    [TestCase('108', 'claPurple,claWhite,False,0.08,0.15,,InnerLine,0.15,0.15,0.85,0.85,1,0.98,pP8NAPvyP1D9GaQzYL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8AXj3gD70sPQfhWGN7+6A')]
    [TestCase('109', 'claPurple,claWhite,True,0.15,0.15,TopLeft|TopRight|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,pQMuwPryQ0C9GaYzcL37gPuv+v/2Zx8G5ryr3vnhBCTQuwMSmNC4B8ATfzgD8E8fwfhWFM5+6A')]
    [TestCase('110', 'claPurple,claWhite,True,0,0.15,TopLeft|TopRight|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,pP8NAPvyP1D9GaQzYL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8AXj3gD70sPQfhWGN7+6A')]
    [TestCase('111', 'claPurple,claWhite,False,0.08,0.15,TopLeft|TopRight|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,pAMOAPvyP0D9GaU3cH37gfu/+v/2Zx8G5ryr3vnhBCTQuwMSmNC4B8AXj3gD70sfgfhWGM7+6A')]
    [TestCase('112', 'claPurple,claWhite,True,0.15,0.15,TopLeft|TopRight|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,pwueAPr1P3B9GKs3kP38fBewugD2bx8G57+r3vnhBCTQuwMSmNC4B8ATXzgC9Gc/AflTDN4+5w')]
    [TestCase('113', 'claPurple,claWhite,True,0,0.15,TopLeft|TopRight|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,pP8NAPvyP1D9GaQzYL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8AXj3gD70sPQfhWGN7+6A')]
    [TestCase('114', 'claPurple,claWhite,False,0.08,0.15,TopLeft|TopRight|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,owsuwPr0P0B9GaY7gL37gf+vOgD1Zx9G5ryr3vnhBCTQuwMSmNC4B8AXnzgD71MfgfhVFN5+6A')]
    [TestCase('115', 'claPurple,claWhite,True,0.15,0.15,TopLeft|TopRight|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,qQ/+gPn4O8C9F7E7sP39ejPgOgH3cw+G58Kr3vnhBCTQuwMSmNC4B8APX3gB939fAflRDO4+5g')]
    [TestCase('116', 'claPurple,claWhite,True,0,0.15,TopLeft|TopRight|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,pP8NAPvyP1D9GaQzYL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8AXj3gD70sPQfhWGN7+6A')]
    [TestCase('117', 'claPurple,claWhite,False,0.08,0.15,TopLeft|TopRight|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,oxNOgPr2N1C9GKk/kP37gAfAugD0Zw+G5r2n3vnhBCTQuwMSmNC4B8Abn7gC8V8fQfhUEO6+5w')]
    [TestCase('118', 'claPurple,claWhite,True,0.15,0.15,TopLeft|TopRight|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,qQ8uQfn5O+C9F7M7wH3+ejfwegH3cw8G6MOn3vnhBCTQuwMSmNC4B8ALX3gB+INfAflREP7+5Q')]
    [TestCase('119', 'claPurple,claWhite,True,0,0.15,TopLeft|TopRight|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,pP8NAPvyP1D9GaQzYL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8AXj3gD70sPQfhWGN7+6A')]
    [TestCase('120', 'claPurple,claWhite,False,0.08,0.15,TopLeft|TopRight|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,ohduQPr3N2B9GKs/oD38fw/Q+gD0Zw+G5r+j3vnhBCTQuwMSmNC4B8Abr7gC82cvAfhTEO5+5w')]
    [TestCase('121', 'claPurple,claWhite,True,0.15,0.15,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,pQMuwPryQ0C9GaYzcL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8ATfzgD8E8fwfhWFM5+6A')]
    [TestCase('122', 'claPurple,claWhite,True,0,0.15,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,pP8NAPvyP1D9GaQzYL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8AXj3gD70sPQfhWGN7+6A')]
    [TestCase('123', 'claPurple,claWhite,False,0.08,0.15,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,pAMOAPvyP0D9GaU3cL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8AXj3gD70sfgfhWGM7+6A')]
    [TestCase('124', 'claPurple,claWhite,True,0.15,0.15,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,pwueAPr1P3B9GKs3kL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8ATXzgC9Gc/AflTDN4+5w')]
    [TestCase('125', 'claPurple,claWhite,True,0,0.15,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,pP8NAPvyP1D9GaQzYL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8AXj3gD70sPQfhWGN7+6A')]
    [TestCase('126', 'claPurple,claWhite,False,0.08,0.15,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,owsuwPr0P0B9GaY7gL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8AXnzgD71MfgfhVFN5+6A')]
    [TestCase('127', 'claPurple,claWhite,True,0.15,0.15,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,qQ/+gPn4O8C9F7E7sL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8APX3gB939fAflRDO4+5g')]
    [TestCase('128', 'claPurple,claWhite,True,0,0.15,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,pP8NAPvyP1D9GaQzYL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8AXj3gD70sPQfhWGN7+6A')]
    [TestCase('129', 'claPurple,claWhite,False,0.08,0.15,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,oxNOgPr2N1C9GKk/kL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8Abn7gC8V8fQfhUEO6+5w')]
    [TestCase('130', 'claPurple,claWhite,True,0.15,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,qQ8uQfn5O+C9F7M7wL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8ALX3gB+INfAflREP7+5Q')]
    [TestCase('131', 'claPurple,claWhite,True,0,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,pP8NAPvyP1D9GaQzYL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8AXj3gD70sPQfhWGN7+6A')]
    [TestCase('132', 'claPurple,claWhite,False,0.08,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,ohduQPr3N2B9GKs/oL37gfu/uv/3Zx8G5r2n3vnhBCTQuwMSmNC4B8Abr7gC82cvAfhTEO5+5w')]
    [TestCase('133', '$BF0000FF,claWhite,True,0.5,0.5,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,0.85,0.98,rEtOQv78F/B9EOKPUD74cYfwu/j3T68F8+9Xr3fe/H+gu/8LRJB6B8a/nvcGDogfAflFNG5A7A')]
    [TestCase('134', '$BF0000FF,claWhite,False,0.5,0.5,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,0.85,0.98,rEtOQv78F/B9EOKPUD74cYfwu/j3T68F8+9Xr3fe/H+gu/8LRJB6B8a/nvcGDogfAflFNG5A7A')]
    [TestCase('135', '$BF0000FF,claWhite,True,0.5,0.5,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,uFsOAgAPUBD+EtdbAD74cIfgu/j3T68F8+9Trzfe/H+wu/8LRIB6B8f3vrcMBfQPwvpCjA4B7Q')]
    [TestCase('136', '$BF0000FF,claWhite,False,0.5,0.5,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,uFsOAgAPUBD+EtdbAD74cIfgu/j3T68F8+9Trzfe/H+wu/8LRIB6B8f3vrcMBfQPwvpCjA4B7Q')]
    [TestCase('137', '$BF0000FF,claWhite,True,0.5,0.5,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,w0eOAQENkOA+Fc8/gHz4boPAO/j4V48F8+9Przfe+4Pgu/8KRIC6B8YzL7gNCfCvAvtKzM7A7g')]
    [TestCase('138', '$BF0000FF,claWhite,False,0.5,0.5,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,w0eOAQENkOA+Fc8/gHz4boPAO/j4V48F8+9Przfe+4Pgu/8KRIC6B8YzL7gNCfCvAvtKzM7A7g')]
    [TestCase('139', '$BF0000FF,claWhite,True,0.5,0.5,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,yC+OAQEIjAD/EtMz8Dv4bYfgO/f5W2/F8vFHzzfe/G8QPAAJTLD6B8Y7f7gLC+R/QvxM1L6A7w')]
    [TestCase('140', '$BF0000FF,claWhite,False,0.5,0.5,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,yC+OAQEIjAD/EtMz8Dv4bYfgO/f5W2/F8vFHzzfe/G8QPAAJTLD6B8Y7f7gLC+R/QvxM1L6A7w')]
    [TestCase('141', '$BF0000FF,claWhite,True,0.75,0.75,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,0.85,0.98,rld+Av8IHBB+EOODID74cYfwu/j3T68F8+9Tr3fe/H+gu/8LRJB6B8bPjrcKDLR/AftBUO6A7A')]
    [TestCase('142', '$BF0000FF,claWhite,False,0.75,0.75,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,0.85,0.98,rld+Av8IHBB+EOODID74cYfwu/j3T68F8+9Tr3fe/H+gu/8LRJB6B8bPjrcKDLR/AftBUO6A7A')]
    [TestCase('143', '$BF0000FF,claWhite,True,0.75,0.75,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,uFsOAgAPUBD+EtdbAD74cIfgu/j3T68F8+9Trzfe/H+wu/8LRIB6B8f3vrcMBfQPwvpCjA4B7Q')]
    [TestCase('144', '$BF0000FF,claWhite,False,0.75,0.75,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,uFsOAgAPUBD+EtdbAD74cIfgu/j3T68F8+9Trzfe/H+wu/8LRIB6B8f3vrcMBfQPwvpCjA4B7Q')]
    [TestCase('145', '$BF0000FF,claWhite,True,0.75,0.75,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,xjuegQEHfAA/E9Qr8Dv4bYPQu/f5W38F8/BLvzfe+3vw+/8KSKC6B8Yzf3gLCeiPQvtN0L4A8A')]
    [TestCase('146', '$BF0000FF,claWhite,False,0.75,0.75,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,xjuegQEHfAA/E9Qr8Dv4bYPQu/f5W38F8/BLvzfe+3vw+/8KSKC6B8Yzf3gLCeiPQvtN0L4A8A')]
    [TestCase('147', '$BF0000FF,claWhite,True,0.75,0.75,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,zCtuQQEEbPC+EdQbwHv4b4/we/f5V5/F8vFP77fd/WcAvAAIUMD6B8c/T3gJBuSPQvxG7I7A8A')]
    [TestCase('148', '$BF0000FF,claWhite,False,0.75,0.75,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,zCtuQQEEbPC+EdQbwHv4b4/we/f5V5/F8vFP77fd/WcAvAAIUMD6B8c/T3gJBuSPQvxG7I7A8A')]
    [TestCase('149', '$BF0000FF,claWhite,True,0.5,0.5,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,plNuwv38+999EOWn0H78a9Pg+/3qV/+F9edDr3biAcAw/AIdECB6B7GjfvcGEXTfQPhGBH5A6w')]
    [TestCase('150', '$BF0000FF,claWhite,False,0.5,0.5,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,plNuwv38+999EOWn0H78a9Pg+/3qV/+F9edDr3biAcAw/AIdECB6B7GjfvcGEXTfQPhGBH5A6w')]
    [TestCase('151', '$BF0000FF,claWhite,True,0.5,0.5,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,tGM+AgARSPD9E9hrgP76eqsgfQT/q79E+OYvP7niDrRwfQIgLFA6BazrjvcNBvzvQfpCcD4B7A')]
    [TestCase('152', '$BF0000FF,claWhite,False,0.5,0.5,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,tGM+AgARSPD9E9hrgP76eqsgfQT/q79E+OYvP7niDrRwfQIgLFA6BazrjvcNBvzvQfpCcD4B7A')]
    [TestCase('153', '$BF0000FF,claWhite,True,0.5,0.5,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,vz+ewQAMoPD+FtFHsPz4dlugPQIWAGAD9uzz/jvhE4ygfgQZgID7Abk3HzgPCfyvAvpLuB4B7g')]
    [TestCase('154', '$BF0000FF,claWhite,False,0.5,0.5,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,vz+ewQAMoPD+FtFHsPz4dlugPQIWAGAD9uzz/jvhE4ygfgQZgID7Abk3HzgPCfyvAvpLuB4B7g')]
    [TestCase('155', '$BF0000FF,claWhite,True,0.5,0.5,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,xxOeQQAHsDB/E9U/8Lv2clMA/v8mJGCD9fffrrzgF0DA/wgK1KC8ALpHj/gMDfA/AvxMyB4B7g')]
    [TestCase('156', '$BF0000FF,claWhite,False,0.5,0.5,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,xxOeQQAHsDB/E9U/8Lv2clMA/v8mJGCD9fffrrzgF0DA/wgK1KC8ALpHj/gMDfA/AvxMyB4B7g')]
    [TestCase('157', '$BF0000FF,claWhite,True,0.75,0.75,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,qGOuAv8JBAB+EOWbgP77bs9gfALwg7/F9uM3n7fjCdDQfAMg+F86B6e7XncLD6xPwfpBJA7B6w')]
    [TestCase('158', '$BF0000FF,claWhite,False,0.75,0.75,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,qGOuAv8JBAB+EOWbgP77bs9gfALwg7/F9uM3n7fjCdDQfAMg+F86B6e7XncLD6xPwfpBJA7B6w')]
    [TestCase('159', '$BF0000FF,claWhite,True,0.75,0.75,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,tGM+AgARSPD9E9hrgP76eqsgfQT/q79E+OYvP7niDrRwfQIgLFA6BazrjvcNBvzvQfpCcD4B7A')]
    [TestCase('160', '$BF0000FF,claWhite,False,0.75,0.75,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,tGM+AgARSPD9E9hrgP76eqsgfQT/q79E+OYvP7niDrRwfQIgLFA6BazrjvcNBvzvQfpCcD4B7A')]
    [TestCase('161', '$BF0000FF,claWhite,True,0.75,0.75,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,wyuuQQEElDA/FNczAPz2dEfQ/QElHHDD9ffvfvzgFWRwvwcLxGA8ALc3j3gMCfRvgvpOxA4B7w')]
    [TestCase('162', '$BF0000FF,claWhite,False,0.75,0.75,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,wyuuQQEElDA/FNczAPz2dEfQ/QElHHDD9ffvfvzgFWRwvwcLxGA8ALc3j3gMCfRvgvpOxA4B7w')]
    [TestCase('163', '$BF0000FF,claWhite,True,0.75,0.75,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,zAd+wQACjAA/EtYn4Pv1fWdQfgEoGBCE9AD8Lr3fHBSgfwr7BxH9/7hLT7gJB/BfAvxC6L7A7w')]
    [TestCase('164', '$BF0000FF,claWhite,False,0.75,0.75,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,zAd+wQACjAA/EtYn4Pv1fWdQfgEoGBCE9AD8Lr3fHBSgfwr7BxH9/7hLT7gJB/BfAvxC6L7A7w')]
    [TestCase('165', '$000000FF,claWhite,False,0.75,0.75,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALsXv3gGgyAfwf6/5A9AMJPb3gH+u9/gvxCPD/A8Q')]
    [TestCase('166', '$FF0000FF,claWhite,False,0.75,0.75,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0,0.98,zyduQQD/p4C+D9IH0Lv2h2NAvwEkEMBD9ALsXv3gGgyAfwf6/5A9AMJPb3gH+u9/gvxCPD/A8Q')]
    procedure TestFillRoundRectSolidWithChessBackground(const AColor, AModulateColor: string; ABlending: Boolean; AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType; ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,z1tieve+c17B9JqTv0XwP/je+Qi2P/H+0sfDEH0msHt/fOUWUM+6910kH4PwAECONgYubJ28AQ')]
    procedure TestFillRoundRectSolidWithClipping(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,0Udieve7V14B9Z2jf4XuMSQfu/6QD6G+18e3Eb0mr39ffOQVSI9691vU/kHvB4jutQQhQN29Ag')]
    procedure TestFillRoundRectSolidWithClipping2(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,6C+yOAG/N98+6LozPwbqThyvffak4zD+3e/TAf4lu8fO/ezm/9/9/VQoDwTs+MsutwcP1P28Cw')]
    procedure TestFillRoundRectSolidWithResource(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,8DuvfukDlBDAG01D7zb8zqtf+gAlBPBG09C7Db/z6pc+QAkBvNE09G7D77z6pQ9QQgBvNA292w')]
    procedure TestSaveState(const AMinSimilarity: Double; const AExpectedImageHash: string);
  end;

implementation

uses
  { Delphi }
  System.IOUtils,
  System.UIConsts,
  System.Math;

type
  TOpenTransform = class(TTransform);

{$IF CompilerVersion <= 30}
type
  TRectFHelper = record helper for TRectF
    function CenterAt(const ADesignatedArea: TRectF): TRectF;
  end;

{ TRectFHelper }

function TRectFHelper.CenterAt(const ADesignatedArea: TRectF): TRectF;
begin
  Result := Self;
  RectCenter(Result, ADesignatedArea);
end;
{$ENDIF}

function CirclesPath(const ABounds: TRectF): TPathData;

  function CircleBounds(ACenterX, ACenterY, ARadius: Single): TRectF;
  begin
    Result := TRectF.Create(PointF(ACenterX - ARadius, ACenterY - ARadius), 2 * ARadius, 2 * ARadius);
  end;

var
  LRadius: SIngle;
begin
  LRadius := Min(ABounds.Width, ABounds.Height) / 4;
  Result := TPathData.Create;
  Result.AddEllipse(CircleBounds(ABounds.CenterPoint.X - LRadius / 2, ABounds.CenterPoint.Y - LRadius / 2, LRadius));
  Result.AddEllipse(CircleBounds(ABounds.CenterPoint.X - LRadius / 2, ABounds.CenterPoint.Y + LRadius / 2, LRadius));
  Result.AddEllipse(CircleBounds(ABounds.CenterPoint.X + LRadius / 2, ABounds.CenterPoint.Y - LRadius / 2, LRadius));
  Result.AddEllipse(CircleBounds(ABounds.CenterPoint.X + LRadius / 2, ABounds.CenterPoint.Y + LRadius / 2, LRadius));
end;

function StarPath: ISkPath;
const
  C = 128.0;
  R = 115.2;
var
  I: Integer;
  A: Single;
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.MoveTo(C + R, C);
  for I := 1 to 7 do
  begin
    A := 2.6927937 * I;
    LPathBuilder.LineTo(C + R * Cos(A), C + R * Sin(A));
  end;
  Result := LPathBuilder.Detach;
end;

{ TSkFMXCanvasTests }

procedure TSkFMXCanvasTests.ApplyGradientPoints(AGradient: TGradient; APoints: string);
var
  LGradientPoint: TGradientPoint;
  LRecords: string;
begin
  AGradient.Points.Clear;
  APoints := APoints.Substring(1, APoints.Length - 2);
  for LRecords in APoints.Split([' ']) do
  begin
    LGradientPoint := TGradientPoint(AGradient.Points.Add);
    LGradientPoint.Color := StringToAlphaColor(LRecords.Split([';'])[0]);
    LGradientPoint.Offset := StrToFloat(LRecords.Split([';'])[1], TFormatSettings.Invariant);
  end;
end;

function TSkFMXCanvasTests.CreateBitmap(const AImageFileName: string): TBitmap;
var
  LBitmap: TBitmap;
  LSvgBrush: TSkSvgBrush;
begin
  LBitmap := TBitmap.Create;
  try
    if AImageFileName.EndsWith('.svg') then
    begin
      LSvgBrush := TSkSvgBrush.Create;
      try
        LSvgBrush.Source := TFile.ReadAllText(SvgAssetsPath + AImageFileName);
        LBitmap.SetSize(Round(LSvgBrush.OriginalSize.Width), Round(LSvgBrush.OriginalSize.Height));
        LBitmap.SkiaDraw(
          procedure(const ACanvas: ISkCanvas)
          begin
            LSvgBrush.Render(ACanvas, RectF(0, 0, LBitmap.Width, LBitmap.Height), 1);
          end, False);
      finally
        LSvgBrush.Free;
      end;
    end
    else
      LBitmap.LoadFromFile(ImageAssetsPath + AImageFileName);
  except
    LBitmap.Free;
    raise;
  end;
  Result := LBitmap;
end;

function TSkFMXCanvasTests.CreateMatrix(AScaleX, AScaleY, ADeltaX, ADeltaY, ARotationDegree: Single): TMatrix;
begin
  Result := TMatrix.CreateScaling(AScaleX, AScaleY) * TMatrix.CreateRotation(DegToRad(ARotationDegree));
  Result := Result * TMatrix.CreateTranslation(ADeltaX, ADeltaY);
end;

procedure TSkFMXCanvasTests.DrawChessBackground(ACanvas: TCanvas; ASquareSize: Single; AEvenSquareColor,
  AOddSquareColor: TAlphaColor);
var
  X, Y: Integer;
begin
  ACanvas.Clear(AEvenSquareColor);
  ACanvas.Fill.Kind := TBrushKind.Solid;
  ACanvas.Fill.Color := AOddSquareColor;
  for X := 0 to Ceil(Abs(ACanvas.Width / ACanvas.Scale / ASquareSize)) do
    for Y := 0 to Ceil(Abs(ACanvas.Height / ACanvas.Scale / ASquareSize)) do
      if Odd(X + Y) then
        ACanvas.FillRect(TRectF.Create(PointF(X, Y) * ASquareSize, ASquareSize, ASquareSize), 0, 0, [], 1);
end;

function TSkFMXCanvasTests.StringToCorners(const ACornersString: string): TCorners;
var
  LString: string;
begin
  if SameText(ACornersString, 'AllCorners') then
    Exit(AllCorners);
  Result := [];
  for LString in ACornersString.Split([' ', ',', '|', ';']) do
  begin
    if SameText(LString, 'TopLeft') then
      Result := Result + [TCorner.TopLeft]
    else if SameText(LString, 'TopRight') then
      Result := Result + [TCorner.TopRight]
    else if SameText(LString, 'BottomLeft') then
      Result := Result + [TCorner.BottomLeft]
    else if SameText(LString, 'BottomRight') then
      Result := Result + [TCorner.BottomRight];
  end;
end;

function TSkFMXCanvasTests.StringToPolygon(APolygonString: string; const ABounds: TRectF): TPolygon;
var
  LPoint: TPointF;
  LPointText: string;
begin
  APolygonString := APolygonString.Substring(1, APolygonString.Length - 2);
  for LPointText in APolygonString.Split([' ']) do
  begin
    // Check if it is the FMX.Types.PolygonPointBreak constant
    if LPointText = '$FFFFFF;$FFFFFF' then
      LPoint := PointF($FFFFFF, $FFFFFF)
    else
    begin
      LPoint := PointF(StrToFloat(LPointText.Split([';'])[0], TFormatSettings.Invariant),
        StrToFloat(LPointText.Split([';'])[1], TFormatSettings.Invariant));
      LPoint.X := ABounds.Left + ABounds.Width * LPoint.X;
      LPoint.Y := ABounds.Top + ABounds.Height * LPoint.Y;
    end;
    Result := Result + [LPoint];
  end;
end;

function TSkFMXCanvasTests.StringToSides(const ASidesString: string): TSides;
var
  LString: string;
begin
  if SameText(ASidesString, 'AllSides') then
    Exit(AllSides);
  Result := [];
  for LString in ASidesString.Split([' ', ',', '|', ';']) do
  begin
    if SameText(LString, 'Top') then
      Result := Result + [TSide.Top]
    else if SameText(LString, 'Left') then
      Result := Result + [TSide.Left]
    else if SameText(LString, 'Bottom') then
      Result := Result + [TSide.Bottom]
    else if SameText(LString, 'Right') then
      Result := Result + [TSide.Right];
  end;
end;

procedure TSkFMXCanvasTests.TestClear(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Clear(TAlphaColors.Black);
        LBitmap.Canvas.Clear(TAlphaColors.Red);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestClear2(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Clear(TAlphaColors.Black);
        LBitmap.Canvas.Clear(MakeColor(TAlphaColors.Red, 0.3));
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestClearRect(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Clear(TAlphaColors.Black);
        LBitmap.Canvas.ClearRect(RectF(25, 25, 75, 75), MakeColor(TAlphaColors.Red, 0.3));
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestClearRect2(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Clear(TAlphaColors.Black);
        LBitmap.Canvas.ClearRect(RectF(0, 0, 50, 50), TAlphaColors.Null);
        LBitmap.Canvas.ClearRect(RectF(50, 50, 100, 100), TAlphaColors.Null);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestClipRect(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.IntersectClipRect(RectF(0, 0, LBitmap.Width, LBitmap.Height));
        LBitmap.Canvas.Clear(TAlphaColors.Red);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestClipRect2(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.IntersectClipRect(RectF(0, 0, LBitmap.Width, LBitmap.Height));
        LBitmap.Canvas.IntersectClipRect(RectF(LBitmap.Width / 2, LBitmap.Height / 2, LBitmap.Width, LBitmap.Height));
        LBitmap.Canvas.Clear(TAlphaColors.Red);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestClipRect3(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.IntersectClipRect(RectF(LBitmap.Width / 2, LBitmap.Height / 2, LBitmap.Width, LBitmap.Height));
        LBitmap.Canvas.ExcludeClipRect(RectF(0, 0, LBitmap.Width / 2, LBitmap.Height / 2)
          .CenterAt(RectF(0, 0, LBitmap.Width, LBitmap.Height)));
        LBitmap.Canvas.Clear(TAlphaColors.Red);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestClipRect4(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.ExcludeClipRect(RectF(0, 0, LBitmap.Width / 2, LBitmap.Height / 2)
          .CenterAt(RectF(0, 0, LBitmap.Width, LBitmap.Height)));
        LBitmap.Canvas.Clear(TAlphaColors.Red);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestClipRect5(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LClipRects: TArray<TRectF>;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    LClipRects := [RectF(0, 0, LBitmap.Width, LBitmap.Height),
      RectF(LBitmap.Width / 2, LBitmap.Height / 2, LBitmap.Width, LBitmap.Height)];
    if LBitmap.Canvas.BeginScene(@LClipRects) then
      try
        LBitmap.Canvas.Clear(TAlphaColors.Red);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestClipRect6(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LClipRects: TArray<TRectF>;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    LClipRects := [RectF(LBitmap.Width / 2, LBitmap.Height / 2, LBitmap.Width, LBitmap.Height)];
    if LBitmap.Canvas.BeginScene(@LClipRects) then
      try
        LBitmap.Canvas.Clear(TAlphaColors.Red);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestClipRect7(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.ExcludeClipRect(RectF(0, 0, LBitmap.Width / 2, LBitmap.Height / 2));
        LBitmap.Canvas.ExcludeClipRect(RectF(LBitmap.Width / 2, LBitmap.Height / 2, LBitmap.Width, LBitmap.Height));
        LBitmap.Canvas.Clear(TAlphaColors.Red);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestClipRect8(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.IntersectClipRect(RectF(0, 0, LBitmap.Width / 2, LBitmap.Height / 2));
        LBitmap.Canvas.IntersectClipRect(RectF(LBitmap.Width / 2, LBitmap.Height / 2, LBitmap.Width, LBitmap.Height));
        LBitmap.Canvas.Clear(TAlphaColors.Red);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestClipRect9(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Blending := False;
        if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
          Exit;
        LModulateCanvas.ModulateColor := MakeColor(TAlphaColors.Blue, 0.5);
        try
          LBitmap.Canvas.ClearRect(RectF(25, 25, 90, 90), TAlphaColors.Black);
          LBitmap.Canvas.ClearRect(RectF(0, 0, 50, 50), TAlphaColors.Null);
        finally
          LModulateCanvas.ModulateColor := TAlphaColors.White;
          LModulateCanvas := nil;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestDrawBitmap(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer;
  ASurfaceScale, ASrcLeft, ASrcTop, ASrcRight, ASrcBottom, ADestLeft, ADestTop, ADestRight, ADestBottom,
  AOpacity: Single; AHighSpeed, ABlending: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImage: TBitmap;
  LSurface: TBitmap;
begin
  LSurface := TBitmap.Create;
  try
    LSurface.SetSize(ASurfaceWidth, ASurfaceHeight);
    LSurface.BitmapScale := ASurfaceScale;
    if LSurface.Canvas.BeginScene then
      try
        LSurface.Canvas.Blending := ABlending;
        LImage := CreateBitmap(AImageFileName);
        try
          LSurface.Canvas.DrawBitmap(LImage, RectF(ASrcLeft, ASrcTop, ASrcRight, ASrcBottom),
            RectF(ADestLeft, ADestTop, ADestRight, ADestBottom), AOpacity, AHighSpeed);
        finally
          LImage.Free;
        end;
      finally
        LSurface.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LSurface.ToSkImage, AMinSimilarity);
  finally
    LSurface.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestDrawBitmapScaledKeepsHighSampling;
const
  CSurfaceScale = 1.25;
  CSurfaceWidth = 320;
  CSurfaceHeight = 180;
var
  LActual: TBitmap;
  LActualImage: ISkImage;
  LDestRect: TRectF;
  LExpected: TBitmap;
  LExpectedImage: ISkImage;
  LPaint: ISkPaint;
  LSource: TBitmap;
  LSourceImage: ISkImage;
  LSourceRect: TRectF;
begin
  LSource := CreateBitmap('3d-shapes.svg');
  try
    LActual := TBitmap.Create;
    try
      LActual.SetSize(CSurfaceWidth, CSurfaceHeight);
      LActual.BitmapScale := CSurfaceScale;
      LDestRect := RectF(20, 15, 180, 95);
      LSourceRect := RectF(0, 0, LSource.Width, LSource.Height);
      if LActual.Canvas.BeginScene then
        try
          LActual.Canvas.Clear(TAlphaColors.Null);
          LActual.Canvas.DrawBitmap(LSource, LSourceRect, LDestRect, 1, False);
        finally
          LActual.Canvas.EndScene;
        end;

      LExpected := TBitmap.Create;
      try
        LExpected.SetSize(CSurfaceWidth, CSurfaceHeight);
        LExpected.BitmapScale := CSurfaceScale;
        LSourceImage := LSource.ToSkImage;
        LPaint := TSkPaint.Create;
        LPaint.AntiAlias := True;
        LExpected.SkiaDraw(
          procedure(const ACanvas: ISkCanvas)
          begin
            ACanvas.DrawImageRect(LSourceImage, LSourceRect, LDestRect, TSkSamplingOptions.High, LPaint);
          end);

        LExpectedImage := LExpected.ToSkImage;
        LActualImage := LActual.ToSkImage;
        Assert.AreEqualPixels(LExpectedImage.PeekPixels, LActualImage.PeekPixels,
          'Scaled bitmap draws must use high sampling (issue #404).');
      finally
        LExpected.Free;
      end;
    finally
      LActual.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestDrawBitmapSubpixelOffset(const AOffset: Single; AHighSpeed: Boolean);
const
  CTolerance = 2;
var
  LImage: ISkImage;
  LPixmap: ISkPixmap;
  LSource: TBitmap;
  LSurface: TBitmap;
begin
  LSource := TBitmap.Create;
  try
    LSource.SetSize(20, 20);
    if LSource.Canvas.BeginScene then
      try
        LSource.Canvas.Clear(TAlphaColors.Black);
      finally
        LSource.Canvas.EndScene;
      end;
    LSurface := TBitmap.Create;
    try
      LSurface.SetSize(40, 40);
      if LSurface.Canvas.BeginScene then
        try
          LSurface.Canvas.Clear(TAlphaColors.Null);
          LSurface.Canvas.DrawBitmap(LSource, RectF(0, 0, LSource.Width, LSource.Height),
            RectF(10 + AOffset, 10 + AOffset, 30 + AOffset, 30 + AOffset), 1, AHighSpeed);
        finally
          LSurface.Canvas.EndScene;
        end;
      LImage := LSurface.ToSkImage;
      LPixmap := LImage.PeekPixels;
      Assert.AreEqual((1 - AOffset) * 255, TAlphaColorRec(LPixmap.GetColor(20, 10)).A, CTolerance,
        'Top edge must be partially covered at a sub-pixel position.');
      Assert.AreEqual((1 - AOffset) * 255, TAlphaColorRec(LPixmap.GetColor(10, 20)).A, CTolerance,
        'Left edge must be partially covered at a sub-pixel position.');
      Assert.AreEqual(AOffset * 255, TAlphaColorRec(LPixmap.GetColor(20, 30)).A, CTolerance,
        'Bottom edge must be partially covered at a sub-pixel position.');
      Assert.AreEqual(AOffset * 255, TAlphaColorRec(LPixmap.GetColor(30, 20)).A, CTolerance,
        'Right edge must be partially covered at a sub-pixel position.');
    finally
      LSurface.Free;
    end;
  finally
    LSource.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestDrawBitmapWithChessBackground(const AImageFileName: string; ASurfaceWidth,
  ASurfaceHeight: Integer; ASurfaceScale, ASrcLeft, ASrcTop, ASrcRight, ASrcBottom, ADestLeft, ADestTop, ADestRight,
  ADestBottom, AOpacity: Single; AHighSpeed, ABlending: Boolean; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImage: TBitmap;
  LSurface: TBitmap;
begin
  LSurface := TBitmap.Create;
  try
    LSurface.SetSize(ASurfaceWidth, ASurfaceHeight);
    LSurface.BitmapScale := ASurfaceScale;
    if LSurface.Canvas.BeginScene then
      try
        DrawChessBackground(LSurface.Canvas, Min(ASurfaceWidth, ASurfaceHeight) / 15 / LSurface.BitmapScale,
          TAlphaColors.Black, TAlphaColors.White);
        LSurface.Canvas.Blending := ABlending;
        LImage := CreateBitmap(AImageFileName);
        try
          LSurface.Canvas.DrawBitmap(LImage, RectF(ASrcLeft, ASrcTop, ASrcRight, ASrcBottom),
            RectF(ADestLeft, ADestTop, ADestRight, ADestBottom), AOpacity, AHighSpeed);
        finally
          LImage.Free;
        end;
      finally
        LSurface.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LSurface.ToSkImage, AMinSimilarity);
  finally
    LSurface.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestDrawBitmapWithClipping(const AImageFileName: string; ASurfaceWidth,
  ASurfaceHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImage: TBitmap;
  LSurface: TBitmap;
begin
  LSurface := TBitmap.Create;
  try
    LSurface.SetSize(ASurfaceWidth, ASurfaceHeight);
    if LSurface.Canvas.BeginScene then
      try
        LSurface.Canvas.IntersectClipRect(RectF(0, 0, ASurfaceWidth / 2, ASurfaceHeight / 2)
          .CenterAt(RectF(0, 0, ASurfaceWidth, ASurfaceHeight)));
        LImage := CreateBitmap(AImageFileName);
        try
          LSurface.Canvas.DrawBitmap(LImage, RectF(0, 0, LImage.Width, LImage.Height),
            RectF(0, 0, ASurfaceWidth, ASurfaceHeight), 1, True);
        finally
          LImage.Free;
        end;
      finally
        LSurface.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LSurface.ToSkImage, AMinSimilarity);
  finally
    LSurface.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestDrawBitmapWithClipping2(const AImageFileName: string; ASurfaceWidth,
  ASurfaceHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LClipRects: TArray<TRectF>;
  LImage: TBitmap;
  LSurface: TBitmap;
begin
  LSurface := TBitmap.Create;
  try
    LSurface.SetSize(ASurfaceWidth, ASurfaceHeight);
    LClipRects := [RectF(0, 0, ASurfaceWidth / 2, ASurfaceHeight / 2)
      .CenterAt(RectF(0, 0, ASurfaceWidth, ASurfaceHeight))];
    if LSurface.Canvas.BeginScene(@LClipRects) then
      try
        LImage := CreateBitmap(AImageFileName);
        try
          LSurface.Canvas.DrawBitmap(LImage, RectF(0, 0, LImage.Width, LImage.Height),
            RectF(0, 0, ASurfaceWidth, ASurfaceHeight), 1, True);
        finally
          LImage.Free;
        end;
      finally
        LSurface.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LSurface.ToSkImage, AMinSimilarity);
  finally
    LSurface.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestDrawBitmapWithClipping3(
  const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LClipRects: TArray<TRectF>;
  LImage: TBitmap;
  LSurface: TBitmap;
begin
  LSurface := TBitmap.Create;
  try
    LSurface.SetSize(ASurfaceWidth, ASurfaceHeight);
    LClipRects := [RectF(0, 0, ASurfaceWidth / 2, ASurfaceHeight / 2)
      .CenterAt(RectF(0, 0, ASurfaceWidth, ASurfaceHeight))];
    if LSurface.Canvas.BeginScene(@LClipRects) then
      try
        LSurface.Canvas.IntersectClipRect(RectF(0, 0, ASurfaceWidth / 2, ASurfaceHeight / 2));
        LImage := CreateBitmap(AImageFileName);
        try
          LSurface.Canvas.DrawBitmap(LImage, RectF(0, 0, LImage.Width, LImage.Height),
            RectF(0, 0, ASurfaceWidth, ASurfaceHeight), 1, True);
        finally
          LImage.Free;
        end;
      finally
        LSurface.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LSurface.ToSkImage, AMinSimilarity);
  finally
    LSurface.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestDrawBitmapWithMatrix(const AImageFileName: string; ASurfaceWidth,
  ASurfaceHeight: Integer; ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg, ASrcLeft,
  ASrcTop, ASrcRight, ASrcBottom, ADestLeft, ADestTop, ADestRight, ADestBottom, AOpacity: Single; AHighSpeed,
  ABlending: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImage: TBitmap;
  LSurface: TBitmap;
begin
  LSurface := TBitmap.Create;
  try
    LSurface.SetSize(ASurfaceWidth, ASurfaceHeight);
    LSurface.BitmapScale := 1;
    if LSurface.Canvas.BeginScene then
      try
        LSurface.Canvas.Blending := ABlending;
        LSurface.Canvas.SetMatrix(
          CreateMatrix(ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg));
        LImage := CreateBitmap(AImageFileName);
        try
          LSurface.Canvas.DrawBitmap(LImage, RectF(ASrcLeft, ASrcTop, ASrcRight, ASrcBottom),
            RectF(ADestLeft, ADestTop, ADestRight, ADestBottom), AOpacity, AHighSpeed);
        finally
          LImage.Free;
        end;
      finally
        LSurface.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LSurface.ToSkImage, AMinSimilarity);
  finally
    LSurface.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestDrawBitmapWithModulateColor(const AImageFileName: string; ASurfaceWidth,
  ASurfaceHeight: Integer; ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg, ASrcLeft,
  ASrcTop, ASrcRight, ASrcBottom, ADestLeft, ADestTop, ADestRight, ADestBottom, AOpacity: Single; AHighSpeed,
  ABlending: Boolean; const AModulateColor: string; AModulateColorOpacity: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImage: TBitmap;
  LModulateCanvas: IModulateCanvas;
  LSurface: TBitmap;
begin
  LSurface := TBitmap.Create;
  try
    LSurface.SetSize(ASurfaceWidth, ASurfaceHeight);
    LSurface.BitmapScale := 1;
    if LSurface.Canvas.BeginScene then
      try
        LSurface.Canvas.Blending := ABlending;
        if not Supports(LSurface.Canvas, IModulateCanvas, LModulateCanvas) then
          Exit;
        LModulateCanvas.ModulateColor := MakeColor(StringToAlphaColor(AModulateColor), AModulateColorOpacity);
        try
          LSurface.Canvas.SetMatrix(
            CreateMatrix(ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg));
          LImage := CreateBitmap(AImageFileName);
          try
            LSurface.Canvas.DrawBitmap(LImage, RectF(ASrcLeft, ASrcTop, ASrcRight, ASrcBottom),
              RectF(ADestLeft, ADestTop, ADestRight, ADestBottom), AOpacity, AHighSpeed);
          finally
            LImage.Free;
          end;
        finally
          LModulateCanvas.ModulateColor := TAlphaColors.White;
          LModulateCanvas := nil;
        end;
      finally
        LSurface.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LSurface.ToSkImage, AMinSimilarity);
  finally
    LSurface.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestDrawBitmapWithModulateColor2(const AImageFileName: string; ASurfaceWidth,
  ASurfaceHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImage: TBitmap;
  LModulateCanvas: IModulateCanvas;
  LSurface: TBitmap;
begin
  LSurface := TBitmap.Create;
  try
    LSurface.SetSize(ASurfaceWidth, ASurfaceHeight);
    LSurface.BitmapScale := 1;
    if LSurface.Canvas.BeginScene then
      try
        if not Supports(LSurface.Canvas, IModulateCanvas, LModulateCanvas) then
          Exit;
        LModulateCanvas.ModulateColor := MakeColor(TAlphaColors.Black, 0.5);
        try
          LImage := CreateBitmap(AImageFileName);
          try
            LSurface.Canvas.DrawBitmap(LImage, RectF(0, 0, LImage.Width, LImage.Height),
              RectF(0, 0, ASurfaceWidth, ASurfaceHeight), 1, False);
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LSurface.Canvas.IntersectClipRect(RectF(LSurface.Width / 2, 0, LSurface.Width, LSurface.Height));
            LSurface.Canvas.Clear(TAlphaColors.Null);
            LSurface.Canvas.DrawBitmap(LImage, RectF(0, 0, LImage.Width, LImage.Height),
              RectF(0, 0, ASurfaceWidth, ASurfaceHeight), 1, False);
          finally
            LImage.Free;
          end;
        finally
          LModulateCanvas.ModulateColor := TAlphaColors.White;
          LModulateCanvas := nil;
        end;
      finally
        LSurface.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LSurface.ToSkImage, AMinSimilarity);
  finally
    LSurface.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestDrawPathWithConsecutiveMoveTo(ACap: TStrokeCap; AThickness: Single);

  procedure CheckInk(const APixmap: ISkPixmap; X, Y: Integer; AExpected: Boolean);
  begin
    if AExpected then
      Assert.IsTrue(TAlphaColorRec(APixmap.GetColor(X, Y)).A > 0, Format('Missing ink at (%d, %d)', [X, Y]))
    else
      Assert.IsTrue(TAlphaColorRec(APixmap.GetColor(X, Y)).A = 0, Format('Unexpected ink at (%d, %d)', [X, Y]));
  end;

var
  LBitmap: TBitmap;
  LImage: ISkImage;
  LPath: TPathData;
  LPixmap: ISkPixmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    LPath := TPathData.Create;
    try
      LPath.MoveTo(PointF(20, 20));
      LPath.MoveTo(PointF(20, 20));
      LPath.MoveTo(PointF(80, 20));
      LPath.MoveTo(PointF(20, 80));
      LPath.LineTo(PointF(80, 80));
      LPath.MoveTo(PointF(50, 50));
      if LBitmap.Canvas.BeginScene then
        try
          LBitmap.Canvas.Clear(TAlphaColors.Null);
          LBitmap.Canvas.Stroke.Kind := TBrushKind.Solid;
          LBitmap.Canvas.Stroke.Color := TAlphaColors.Black;
          LBitmap.Canvas.Stroke.Thickness := AThickness;
          LBitmap.Canvas.Stroke.Cap := ACap;
          LBitmap.Canvas.DrawPath(LPath, 1);
        finally
          LBitmap.Canvas.EndScene;
        end;
    finally
      LPath.Free;
    end;
    LImage := LBitmap.ToSkImage;
    LPixmap := LImage.PeekPixels;
    CheckInk(LPixmap, 20, 20, False);
    CheckInk(LPixmap, 80, 20, False);
    CheckInk(LPixmap, 50, 50, False);
    CheckInk(LPixmap, 50, 80, True);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestDrawRectSides(const ASidesString, ACornersString: string; ARadius: Single;
  ACornerType: TCornerType; AThickness: Single; ACap: TStrokeCap);
const
  L = 20;
  T = 20;
  R = 80;
  B = 80;
var
  LBitmap: TBitmap;
  LBottomLeft: Boolean;
  LBottomRight: Boolean;
  LCorners: TCorners;
  LImage: ISkImage;
  LPixmap: ISkPixmap;
  LSides: TSides;
  LTopLeft: Boolean;
  LTopRight: Boolean;

  function HasInk(const APoint: TPoint): Boolean;
  begin
    Result := TAlphaColorRec(LPixmap.GetColor(APoint.X, APoint.Y)).A > 0;
  end;

  function IsCornerDrawn(ACorner: TCorner; ASide1, ASide2: TSide): Boolean;
  begin
    // FMX also strokes the arc of a rounded corner when none of its sides is drawn (before XE8, even without radius)
    {$IF CompilerVersion < 29}
    Result := (ASide1 in LSides) or (ASide2 in LSides) or (ACorner in LCorners);
    {$ELSE}
    Result := (ASide1 in LSides) or (ASide2 in LSides) or ((ACorner in LCorners) and (ARadius > 0));
    {$ENDIF}
  end;

  procedure CheckSide(ASide: TSide; const AStart, AEnd: TPoint; AStartCornerDrawn, AEndCornerDrawn: Boolean);
  var
    I: Integer;
    LLength: Integer;
    LMargin: Integer;
    LPoint: TPoint;
  begin
    LLength := Max(AEnd.X - AStart.X, AEnd.Y - AStart.Y);
    if ASide in LSides then
    begin
      LPoint := Point((AStart.X + AEnd.X) div 2, (AStart.Y + AEnd.Y) div 2);
      Assert.IsTrue(HasInk(LPoint), Format('Missing ink at (%d, %d), in the middle of a drawn side', [LPoint.X, LPoint.Y]));
      Exit;
    end;
    LMargin := Ceil(ARadius + AThickness) + 1;
    for I := 0 to LLength do
    begin
      if (AStartCornerDrawn and (I <= LMargin)) or (AEndCornerDrawn and (I >= LLength - LMargin)) then
        Continue;
      LPoint := Point(AStart.X + (AEnd.X - AStart.X) * I div LLength, AStart.Y + (AEnd.Y - AStart.Y) * I div LLength);
      Assert.IsFalse(HasInk(LPoint), Format('Unexpected ink at (%d, %d), on a side that is not drawn', [LPoint.X, LPoint.Y]));
    end;
  end;

begin
  LSides := StringToSides(ASidesString);
  LCorners := StringToCorners(ACornersString);
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Clear(TAlphaColors.Null);
        LBitmap.Canvas.Stroke.Kind := TBrushKind.Solid;
        LBitmap.Canvas.Stroke.Color := TAlphaColors.Black;
        LBitmap.Canvas.Stroke.Thickness := AThickness;
        LBitmap.Canvas.Stroke.Cap := ACap;
        LBitmap.Canvas.DrawRectSides(RectF(L, T, R, B), ARadius, ARadius, LCorners, 1, LSides, ACornerType);
      finally
        LBitmap.Canvas.EndScene;
      end;
    LImage := LBitmap.ToSkImage;
    LPixmap := LImage.PeekPixels;
    LTopLeft := IsCornerDrawn(TCorner.TopLeft, TSide.Top, TSide.Left);
    LTopRight := IsCornerDrawn(TCorner.TopRight, TSide.Top, TSide.Right);
    LBottomLeft := IsCornerDrawn(TCorner.BottomLeft, TSide.Bottom, TSide.Left);
    LBottomRight := IsCornerDrawn(TCorner.BottomRight, TSide.Bottom, TSide.Right);
    CheckSide(TSide.Top, Point(L, T), Point(R, T), LTopLeft, LTopRight);
    CheckSide(TSide.Bottom, Point(L, B), Point(R, B), LBottomLeft, LBottomRight);
    CheckSide(TSide.Left, Point(L, T), Point(L, B), LTopLeft, LBottomLeft);
    CheckSide(TSide.Right, Point(R, T), Point(R, B), LTopRight, LBottomRight);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillArcBitmap(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode;
  const ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AStartAngle, ASweepAngle, AOpacity,
  AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LDest: TRectF;
  LImage: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(500, 400);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Bitmap;
          LBitmap.Canvas.Fill.Bitmap.WrapMode := AWrapMode;
          LImage := CreateBitmap(AImageFileName);
          try
            LBitmap.Canvas.Fill.Bitmap.Bitmap := LImage;
          finally
            LImage.Free;
          end;
          LDest := RectF(ALeftPercent * LBitmap.Width, ATopPercent * LBitmap.Height,
            ARightPercent * LBitmap.Width, ABottomPercent * LBitmap.Height);
          LBitmap.Canvas.FillArc(LDest.CenterPoint, PointF(LDest.Width / 2, LDest.Height / 2),
            AStartAngle, ASweepAngle, AOpacity);
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillEllipseBitmap(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode;
  ABlending: Boolean; const ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LImage: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(400, 400);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Blending := ABlending;
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Bitmap;
          LBitmap.Canvas.Fill.Bitmap.WrapMode := AWrapMode;
          LImage := CreateBitmap(AImageFileName);
          try
            LBitmap.Canvas.Fill.Bitmap.Bitmap := LImage;
          finally
            LImage.Free;
          end;
          LBitmap.Canvas.FillEllipse(RectF(ALeftPercent * LBitmap.Width, ATopPercent * LBitmap.Height,
            ARightPercent * LBitmap.Width, ABottomPercent * LBitmap.Height), AOpacity);
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillEllipseBitmapWithChessBackground(const AImageFileName, AModulateColor: string;
  AWrapMode: TWrapMode; ABlending: Boolean; const ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity,
  AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LImage: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(400, 400);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        DrawChessBackground(LBitmap.Canvas, Min(LBitmap.Width, LBitmap.Height) / 15 / LBitmap.BitmapScale,
          TAlphaColors.Black, TAlphaColors.White);
        LBitmap.Canvas.Blending := ABlending;
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Bitmap;
          LBitmap.Canvas.Fill.Bitmap.WrapMode := AWrapMode;
          LImage := CreateBitmap(AImageFileName);
          try
            LBitmap.Canvas.Fill.Bitmap.Bitmap := LImage;
          finally
            LImage.Free;
          end;
          LBitmap.Canvas.FillEllipse(RectF(ALeftPercent * LBitmap.Width, ATopPercent * LBitmap.Height,
            ARightPercent * LBitmap.Width, ABottomPercent * LBitmap.Height), AOpacity);
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillEllipseBitmapWithClipping(const AImageFileName, AModulateColor: string;
  AWrapMode: TWrapMode; ABlending: Boolean; const AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LImage: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(400, 400);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.ExcludeClipRect(RectF(0.35 * LBitmap.Width, 0.35 * LBitmap.Height,
          0.65 * LBitmap.Width, 0.65 * LBitmap.Height));
        LBitmap.Canvas.Blending := ABlending;
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Bitmap;
          LBitmap.Canvas.Fill.Bitmap.WrapMode := AWrapMode;
          LImage := CreateBitmap(AImageFileName);
          try
            LBitmap.Canvas.Fill.Bitmap.Bitmap := LImage;
          finally
            LImage.Free;
          end;
          LBitmap.Canvas.FillEllipse(RectF(0, 0, LBitmap.Width, LBitmap.Height), AOpacity);
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillEllipseBitmapWithMatrix(const AImageFileName: string; ASurfaceWidth,
  ASurfaceHeight: Integer; AWrapMode: TWrapMode; ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY,
  ARotationDeg, ADestLeft, ADestTop, ADestRight, ADestBottom, AOpacity: Single; ABlending: Boolean;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LImage: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(ASurfaceWidth, ASurfaceHeight);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Blending := ABlending;
        LBitmap.Canvas.SetMatrix(
          CreateMatrix(ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg));
        LBitmap.Canvas.Fill.Kind := TBrushKind.Bitmap;
        LBitmap.Canvas.Fill.Bitmap.WrapMode := AWrapMode;
        LImage := CreateBitmap(AImageFileName);
        try
          LBitmap.Canvas.Fill.Bitmap.Bitmap := LImage;
        finally
          LImage.Free;
        end;
        LBitmap.Canvas.FillEllipse(RectF(ADestLeft, ADestTop, ADestRight, ADestBottom), AOpacity);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillEllipseGradient(const APoints, AModulateColor: string;
  AGradientStyle: TGradientStyle; ABlending: Boolean; const AScale, AOpacity, AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    LBitmap.BitmapScale := AScale;
    if LBitmap.Canvas.BeginScene then
      try
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.SetMatrix(TMatrix.CreateTranslation(10, 10));
          LBitmap.Canvas.Blending := ABlending;
          LBitmap.Canvas.Fill.Kind := TBrushKind.Gradient;
          LBitmap.Canvas.Fill.Gradient.Style := AGradientStyle;
          ApplyGradientPoints(LBitmap.Canvas.Fill.Gradient, APoints);
          LBitmap.Canvas.FillEllipse(RectF(0, 0, 80, 80), AOpacity);
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillEllipseGradient2(const APoints, AModulateColor: string;
  AGradientStyle: TGradientStyle; ABlending: Boolean; const AScale, AOpacity, AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    LBitmap.BitmapScale := AScale;
    if LBitmap.Canvas.BeginScene then
      try
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.SetMatrix(TMatrix.CreateTranslation(10, 10));
          LBitmap.Canvas.Blending := ABlending;
          LBitmap.Canvas.Fill.Kind := TBrushKind.Gradient;
          LBitmap.Canvas.Fill.Gradient.Style := AGradientStyle;
          ApplyGradientPoints(LBitmap.Canvas.Fill.Gradient, APoints);
          LBitmap.Canvas.FillEllipse(RectF(0, 20, 80, 60), AOpacity);
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillEllipseGradientRadial(const APoints, AModulateColor: string; AGradientScaleX,
  AGradientScaleY, AGradientPosX, AGradientPosY, AGradientRotationDegree, AGradientRotationCenterX,
  AGradientRotationCenterY, AGradientSkewX, AGradientSkewY, ADestLeftPercent, ADestTopPercent, ADestRightPercent,
  ADestBottomPercent, ACanvasScaleX, ACanvasScaleY, ACanvasOffsetX, ACanvasOffsetY, ACanvasRotationDeg: Single;
  AApplyClip, ABlending, ADrawBackgroundChess: Boolean; const ABitmapScale, AOpacity, AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    LBitmap.BitmapScale := ABitmapScale;
    if LBitmap.Canvas.BeginScene then
      try
        if ADrawBackgroundChess then
        begin
          DrawChessBackground(LBitmap.Canvas, Min(LBitmap.Width, LBitmap.Height) / 15 / LBitmap.BitmapScale,
            TAlphaColors.Black, TAlphaColors.White);
        end;
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.SetMatrix(
            CreateMatrix(ACanvasScaleX, ACanvasScaleY, ACanvasOffsetX, ACanvasOffsetY, ACanvasRotationDeg));
          if AApplyClip then
          begin
            LBitmap.Canvas.IntersectClipRect(RectF(0.1 * LBitmap.Width, 0.1 * LBitmap.Height,
              0.9 * LBitmap.Width, 0.9 * LBitmap.Height));
          end;
          LBitmap.Canvas.Blending := ABlending;
          LBitmap.Canvas.Fill.Kind := TBrushKind.Gradient;
          LBitmap.Canvas.Fill.Gradient.Style := TGradientStyle.Radial;
          ApplyGradientPoints(LBitmap.Canvas.Fill.Gradient, APoints);
          LBitmap.Canvas.Fill.Gradient.RadialTransform.Scale.Point := PointF(AGradientScaleX, AGradientScaleY);
          LBitmap.Canvas.Fill.Gradient.RadialTransform.Position.Point := PointF(AGradientPosX, AGradientPosY);
          LBitmap.Canvas.Fill.Gradient.RadialTransform.RotationAngle := AGradientRotationDegree;
          LBitmap.Canvas.Fill.Gradient.RadialTransform.RotationCenter.Point := PointF(AGradientRotationCenterX, AGradientRotationCenterY);
          TOpenTransform(LBitmap.Canvas.Fill.Gradient.RadialTransform).Skew.Point := PointF(AGradientSkewX, AGradientSkewY);
          LBitmap.Canvas.FillEllipse(RectF(ADestLeftPercent * LBitmap.Width, ADestTopPercent * LBitmap.Height,
            ADestRightPercent * LBitmap.Width, ADestBottomPercent * LBitmap.Height), AOpacity);
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillEllipseGradientStartEndPoint(const APoints, AModulateColor: string; AStartX,
  AStartY, AStopX, AStopY, ADestLeftPercent, ADestTopPercent, ADestRightPercent, ADestBottomPercent, ACanvasScaleX,
  ACanvasScaleY, ACanvasOffsetX, ACanvasOffsetY, ACanvasRotationDeg: Single; AApplyClip, ABlending: Boolean;
  const ABitmapScale, AOpacity: Single; ADrawBackgroundChess: Boolean; AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    LBitmap.BitmapScale := ABitmapScale;
    if LBitmap.Canvas.BeginScene then
      try
        if ADrawBackgroundChess then
        begin
          DrawChessBackground(LBitmap.Canvas, Min(LBitmap.Width, LBitmap.Height) / 15 / LBitmap.BitmapScale,
            TAlphaColors.Black, TAlphaColors.White);
        end;
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.SetMatrix(
            CreateMatrix(ACanvasScaleX, ACanvasScaleY, ACanvasOffsetX, ACanvasOffsetY, ACanvasRotationDeg));
          if AApplyClip then
          begin
            LBitmap.Canvas.IntersectClipRect(RectF(0.1 * LBitmap.Width, 0.1 * LBitmap.Height,
              0.9 * LBitmap.Width, 0.9 * LBitmap.Height));
          end;
          LBitmap.Canvas.Blending := ABlending;
          LBitmap.Canvas.Fill.Kind := TBrushKind.Gradient;
          LBitmap.Canvas.Fill.Gradient.Style := TGradientStyle.Linear;
          ApplyGradientPoints(LBitmap.Canvas.Fill.Gradient, APoints);
          LBitmap.Canvas.Fill.Gradient.StartPosition.Point := PointF(AStartX, AStartY);
          LBitmap.Canvas.Fill.Gradient.StopPosition.Point := PointF(AStopX, AStopY);
          LBitmap.Canvas.FillEllipse(RectF(ADestLeftPercent * LBitmap.Width, ADestTopPercent * LBitmap.Height,
            ADestRightPercent * LBitmap.Width, ADestBottomPercent * LBitmap.Height), AOpacity);
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillEllipseGradientWithChessBackground(const APoints, AModulateColor: string;
  AGradientStyle: TGradientStyle; ABlending: Boolean; const AScale, AOpacity, AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    LBitmap.BitmapScale := AScale;
    if LBitmap.Canvas.BeginScene then
      try
        DrawChessBackground(LBitmap.Canvas, Min(LBitmap.Width, LBitmap.Height) / 15 / LBitmap.BitmapScale,
          TAlphaColors.Black, TAlphaColors.White);
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.SetMatrix(TMatrix.CreateTranslation(10, 10));
          LBitmap.Canvas.Blending := ABlending;
          LBitmap.Canvas.Fill.Kind := TBrushKind.Gradient;
          LBitmap.Canvas.Fill.Gradient.Style := AGradientStyle;
          ApplyGradientPoints(LBitmap.Canvas.Fill.Gradient, APoints);
          LBitmap.Canvas.FillEllipse(RectF(0, 20, 80, 60), AOpacity);
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillEllipseSolid(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
        LBitmap.Canvas.Fill.Color := TAlphaColors.Black;
        LBitmap.Canvas.FillEllipse(RectF(25, 25, 75, 75), 0.5);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillEllipseSolid2(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
        LBitmap.Canvas.Fill.Color := MakeColor(TAlphaColors.Black, 0.5);
        LBitmap.Canvas.FillEllipse(RectF(25, 25, 75, 75), 1);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillEllipseSolid3(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.IntersectClipRect(RectF(0, 0, 50, 50));
        LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
        LBitmap.Canvas.Fill.Color := TAlphaColors.Black;
        LBitmap.Canvas.FillEllipse(RectF(25, 25, 75, 75), 1);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillEllipseSolid4(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.IntersectClipRect(RectF(20, 20, 95, 95));
        LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
        LBitmap.Canvas.Fill.Color := TAlphaColors.Red;
        LBitmap.Canvas.FillEllipse(RectF(25, 25, 75, 75), 1);
        LBitmap.Canvas.Fill.Color := TAlphaColors.Green;
        LBitmap.Canvas.FillEllipse(RectF(50, 50, 100, 100), 0.7);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillEllipseSolid5(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
          Exit;
        LModulateCanvas.ModulateColor := TAlphaColors.Blue;
        try
          LBitmap.Canvas.IntersectClipRect(RectF(20, 20, 95, 95));
          LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
          LBitmap.Canvas.Fill.Color := TAlphaColors.Red;
          LBitmap.Canvas.FillEllipse(RectF(25, 25, 75, 75), 1);
          LBitmap.Canvas.Fill.Color := TAlphaColors.Green;
          LBitmap.Canvas.FillEllipse(RectF(50, 50, 100, 100), 0.7);
        finally
          LModulateCanvas.ModulateColor := TAlphaColors.White;
          LModulateCanvas := nil;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillEllipseSolid6(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Blending := False;
        LBitmap.Canvas.IntersectClipRect(RectF(20, 20, 95, 95));
        LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
        LBitmap.Canvas.Fill.Color := TAlphaColors.Red;
        LBitmap.Canvas.FillEllipse(RectF(25, 25, 75, 75), 1);
        LBitmap.Canvas.Fill.Color := TAlphaColors.Green;
        LBitmap.Canvas.FillEllipse(RectF(50, 50, 100, 100), 0.7);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillEllipseSolidWithMatrix(ASurfaceWidth, ASurfaceHeight: Integer; ASurfaceScaleX,
  ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg, ADestLeft, ADestTop, ADestRight, ADestBottom,
  AOpacity: Single; ABlending: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(ASurfaceWidth, ASurfaceHeight);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Blending := ABlending;
        LBitmap.Canvas.SetMatrix(
          CreateMatrix(ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg));
        LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
        LBitmap.Canvas.Fill.Color := TAlphaColors.Red;
        LBitmap.Canvas.FillEllipse(RectF(ADestLeft, ADestTop, ADestRight, ADestBottom), AOpacity);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillEllipseSolidWithModulateColor(const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
          Exit;
        LModulateCanvas.ModulateColor := TAlphaColors.Blue;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
          LBitmap.Canvas.Fill.Color := TAlphaColors.Red;
          LBitmap.Canvas.FillEllipse(RectF(25, 25, 75, 75), 0.6);
        finally
          LModulateCanvas.ModulateColor := TAlphaColors.White;
          LModulateCanvas := nil;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillEllipseSolidWithResource(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LBrushObject: TBrushObject;
  LBrushObject2: TBrushObject;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBrushObject := TBrushObject.Create(nil);
        try
          LBrushObject2 := TBrushObject.Create(nil);
          try
            LBrushObject2.Brush.Kind := TBrushKind.Solid;
            LBrushObject2.Brush.Color := TAlphaColors.Blue;
            LBrushObject.Brush.Kind := TBrushKind.Resource;
            LBrushObject.Brush.Resource.StyleResource := LBrushObject2;
            LBitmap.Canvas.Fill.Kind := TBrushKind.Resource;
            LBitmap.Canvas.Fill.Resource.StyleResource := LBrushObject;
            LBitmap.Canvas.FillEllipse(RectF(25, 25, 75, 75), 0.7);
          finally
            LBrushObject2.Free;
          end;
        finally
          LBrushObject.Free;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillPathBitmap(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode;
  ABlending: Boolean; const ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double;
  const AExpectedImageHash: string);
const
  PathDataString = 'M253.706 364.063C164.912 364.063 92.9062 290.606 92.9062 200.006C92.9062 109.394 164.912 35.9375 ' +
    '253.706 35.9375C267.531 35.9375 280.944 37.725 293.75 41.0688C264.673 22.3955 230.838 12.4784 196.281 ' +
    '12.5C94.7813 12.5 12.5 96.45 12.5 200.006C12.5 303.556 94.7813 387.5 196.281 387.5C232.094 387.5 265.5 377.013 ' +
    '293.75 358.938C280.944 362.281 267.531 364.063 253.706 364.063Z M311.331 237.325L358.413 270.313L340.625 ' +
    '216.669L387.5 183.406L329.431 183.238L311.331 129.694L293.225 183.238L235.156 183.406L282.044 216.669L264.244 ' +
    '270.313L311.331 237.325Z';
var
  LBitmap: TBitmap;
  LImage: TBitmap;
  LModulateCanvas: IModulateCanvas;
  LPathData: TPathData;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(400, 400);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Blending := ABlending;
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Bitmap;
          LBitmap.Canvas.Fill.Bitmap.WrapMode := AWrapMode;
          LImage := CreateBitmap(AImageFileName);
          try
            LBitmap.Canvas.Fill.Bitmap.Bitmap := LImage;
          finally
            LImage.Free;
          end;
          LPathData := TPathData.Create;
          try
            LPathData.Data := PathDataString;
            LPathData.ApplyMatrix(CreateMatrix(ARightPercent - ALeftPercent, ABottomPercent - ATopPercent,
              ALeftPercent * LBitmap.Width, ATopPercent * LBitmap.Height, 0));
            LBitmap.Canvas.FillPath(LPathData, AOpacity);
          finally
            LPathData.Free;
          end;
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillPathBitmap2(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode;
  ABlending: Boolean; const ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LImage: TBitmap;
  LModulateCanvas: IModulateCanvas;
  LPathData: TPathData;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(400, 400);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Blending := ABlending;
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Bitmap;
          LBitmap.Canvas.Fill.Bitmap.WrapMode := AWrapMode;
          LImage := CreateBitmap(AImageFileName);
          try
            LBitmap.Canvas.Fill.Bitmap.Bitmap := LImage;
          finally
            LImage.Free;
          end;
          LPathData := TPathData.CreateFromSkPath(StarPath);
          try
            LPathData.ApplyMatrix(CreateMatrix((ARightPercent - ALeftPercent) * 1.5,
              (ABottomPercent - ATopPercent) * 1.5, ALeftPercent * LBitmap.Width, ATopPercent * LBitmap.Height, 0));
            LBitmap.Canvas.FillPath(LPathData, AOpacity);
          finally
            LPathData.Free;
          end;
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillPathBitmap3(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode;
  ABlending: Boolean; const ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LImage: TBitmap;
  LModulateCanvas: IModulateCanvas;
  LPathData: TPathData;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(400, 400);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Blending := ABlending;
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Bitmap;
          LBitmap.Canvas.Fill.Bitmap.WrapMode := AWrapMode;
          LImage := CreateBitmap(AImageFileName);
          try
            LBitmap.Canvas.Fill.Bitmap.Bitmap := LImage;
          finally
            LImage.Free;
          end;
          LPathData := CirclesPath(RectF(0, 0, LBitmap.Width, LBitmap.Height));
          try
            LPathData.ApplyMatrix(CreateMatrix(ARightPercent - ALeftPercent,
              ABottomPercent - ATopPercent, ALeftPercent * LBitmap.Width, ATopPercent * LBitmap.Height, 0));
            LBitmap.Canvas.FillPath(LPathData, AOpacity);
          finally
            LPathData.Free;
          end;
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillPathBitmapWithChessBackground(const AImageFileName, AModulateColor: string;
  AWrapMode: TWrapMode; ABlending: Boolean; const AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LImage: TBitmap;
  LModulateCanvas: IModulateCanvas;
  LPathData: TPathData;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(400, 400);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        DrawChessBackground(LBitmap.Canvas, Min(LBitmap.Width, LBitmap.Height) / 15 / LBitmap.BitmapScale,
          TAlphaColors.Black, TAlphaColors.White);
        LBitmap.Canvas.Blending := ABlending;
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Bitmap;
          LBitmap.Canvas.Fill.Bitmap.WrapMode := AWrapMode;
          LImage := CreateBitmap(AImageFileName);
          try
            LBitmap.Canvas.Fill.Bitmap.Bitmap := LImage;
          finally
            LImage.Free;
          end;
          LPathData := CirclesPath(RectF(0, 0, LBitmap.Width, LBitmap.Height));
          try
            LBitmap.Canvas.FillPath(LPathData, AOpacity);
          finally
            LPathData.Free;
          end;
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillPathBitmapWithClipping(const AImageFileName, AModulateColor: string;
  AWrapMode: TWrapMode; ABlending: Boolean; const AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LImage: TBitmap;
  LModulateCanvas: IModulateCanvas;
  LPathData: TPathData;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(400, 400);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.IntersectClipRect(RectF(0.5 * LBitmap.Width, 0, LBitmap.Width, LBitmap.Height));
        LBitmap.Canvas.Blending := ABlending;
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Bitmap;
          LBitmap.Canvas.Fill.Bitmap.WrapMode := AWrapMode;
          LImage := CreateBitmap(AImageFileName);
          try
            LBitmap.Canvas.Fill.Bitmap.Bitmap := LImage;
          finally
            LImage.Free;
          end;
          LPathData := CirclesPath(RectF(0, 0, LBitmap.Width, LBitmap.Height));
          try
            LBitmap.Canvas.FillPath(LPathData, AOpacity);
          finally
            LPathData.Free;
          end;
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillPathBitmapWithMatrix(const AImageFileName: string; ASurfaceWidth,
  ASurfaceHeight: Integer; AWrapMode: TWrapMode; ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY,
  ARotationDeg, ADestLeft, ADestTop, ADestRight, ADestBottom, AOpacity: Single; ABlending: Boolean;
  const AMinSimilarity: Double; const AExpectedImageHash: string);

  function HeptagonPath(const ABounds: TRectF): ISkPath;
  const
    VerticesCount = 7;
  var
    LPathBuilder: ISkPathBuilder;
    LPolygon: TPolygon;
    LRadius: Single;
    LVertexAngle: Double;
    I: Integer;
  begin
    LRadius := 0.45 * Min(ABounds.Width, ABounds.Height);
    SetLength(LPolygon, VerticesCount);
    LVertexAngle := -0.5 * Pi;
    for I := 0 to Length(LPolygon) - 1 do
    begin
      LPolygon[I] := PointF(LRadius * Cos(LVertexAngle), LRadius * Sin(LVertexAngle));
      LVertexAngle := LVertexAngle + (2 * Pi / VerticesCount);
    end;

    LPathBuilder := TSkPathBuilder.Create;
    LPathBuilder.AddPolygon(LPolygon, True);
    LPathBuilder.Offset(ABounds.CenterPoint.X, ABounds.CenterPoint.Y);
    Result := LPathBuilder.Detach;
  end;

var
  LBitmap: TBitmap;
  LImage: TBitmap;
  LPathData: TPathData;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(ASurfaceWidth, ASurfaceHeight);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Blending := ABlending;
        LBitmap.Canvas.SetMatrix(
          CreateMatrix(ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg));
        LBitmap.Canvas.Fill.Kind := TBrushKind.Bitmap;
        LBitmap.Canvas.Fill.Bitmap.WrapMode := AWrapMode;
        LImage := CreateBitmap(AImageFileName);
        try
          LBitmap.Canvas.Fill.Bitmap.Bitmap := LImage;
        finally
          LImage.Free;
        end;
        LPathData := TPathData.CreateFromSkPath(HeptagonPath(RectF(ADestLeft, ADestTop, ADestRight, ADestBottom)));
        try
          LBitmap.Canvas.FillPath(LPathData, AOpacity);
        finally
          LPathData.Free;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillPathGradient(const APoints, AModulateColor: string; AGradientStyle: TGradientStyle;
  ABlending: Boolean; const AScale, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LModulateCanvas: IModulateCanvas;
  LPathData: TPathData;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    LBitmap.BitmapScale := AScale;
    if LBitmap.Canvas.BeginScene then
      try
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.SetMatrix(TMatrix.CreateTranslation(10, 10));
          LBitmap.Canvas.Blending := ABlending;
          LBitmap.Canvas.Fill.Kind := TBrushKind.Gradient;
          LBitmap.Canvas.Fill.Gradient.Style := AGradientStyle;
          ApplyGradientPoints(LBitmap.Canvas.Fill.Gradient, APoints);
          LPathData := TPathData.CreateFromSkPath(StarPath);
          try
            LPathData.ApplyMatrix(CreateMatrix(0.33, 0.33, 0, 0, 0));
            LBitmap.Canvas.FillPath(LPathData, AOpacity);
          finally
            LPathData.Free;
          end;
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillPathSolid(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LPathData: TPathData;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
        LBitmap.Canvas.Fill.Color := TAlphaColors.Black;
        LPathData := CirclesPath(RectF(15, 15, 85, 85));
        try
          LBitmap.Canvas.FillPath(LPathData, 0.5);
        finally
          LPathData.Free;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillPathSolid2(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LPathData: TPathData;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
        LBitmap.Canvas.Fill.Color := MakeColor(TAlphaColors.Black, 0.5);
        LPathData := CirclesPath(RectF(15, 15, 85, 85));
        try
          LBitmap.Canvas.FillPath(LPathData, 1);
        finally
          LPathData.Free;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillPathSolid3(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LPathData: TPathData;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.IntersectClipRect(RectF(0, 0, 50, 50));
        LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
        LBitmap.Canvas.Fill.Color := TAlphaColors.Black;
        LPathData := CirclesPath(RectF(15, 15, 85, 85));
        try
          LBitmap.Canvas.FillPath(LPathData, 1);
        finally
          LPathData.Free;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillPathSolid4(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LPathData: TPathData;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.IntersectClipRect(RectF(20, 20, 95, 95));
        LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
        LBitmap.Canvas.Fill.Color := TAlphaColors.Red;
        LPathData := CirclesPath(RectF(15, 15, 85, 85));
        try
          LBitmap.Canvas.FillPath(LPathData, 1);
        finally
          LPathData.Free;
        end;
        LBitmap.Canvas.Fill.Color := TAlphaColors.Green;
        LPathData := CirclesPath(RectF(50, 50, 100, 100));
        try
          LBitmap.Canvas.FillPath(LPathData, 0.7);
        finally
          LPathData.Free;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillPathSolid5(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LModulateCanvas: IModulateCanvas;
  LPathData: TPathData;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
          Exit;
        LModulateCanvas.ModulateColor := TAlphaColors.Blue;
        try
          LBitmap.Canvas.IntersectClipRect(RectF(20, 20, 95, 95));
          LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
          LBitmap.Canvas.Fill.Color := TAlphaColors.Red;
          LPathData := CirclesPath(RectF(15, 15, 85, 85));
          try
            LBitmap.Canvas.FillPath(LPathData, 1);
          finally
            LPathData.Free;
          end;
          LBitmap.Canvas.Fill.Color := TAlphaColors.Green;
          LPathData := CirclesPath(RectF(50, 50, 100, 100));
          try
            LBitmap.Canvas.FillPath(LPathData, 0.7);
          finally
            LPathData.Free;
          end;
        finally
          LModulateCanvas.ModulateColor := TAlphaColors.White;
          LModulateCanvas := nil;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillPathSolid6(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LPathData: TPathData;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Blending := False;
        LBitmap.Canvas.IntersectClipRect(RectF(20, 20, 95, 95));
        LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
        LBitmap.Canvas.Fill.Color := TAlphaColors.Red;
        LPathData := CirclesPath(RectF(15, 15, 85, 85));
        try
          LBitmap.Canvas.FillPath(LPathData, 1);
        finally
          LPathData.Free;
        end;
        LBitmap.Canvas.Fill.Color := TAlphaColors.Green;
        LPathData := CirclesPath(RectF(50, 50, 100, 100));
        try
          LBitmap.Canvas.FillPath(LPathData, 0.7);
        finally
          LPathData.Free;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillPathSolidWithMatrix(ASurfaceWidth, ASurfaceHeight: Integer; ASurfaceScaleX,
  ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg, ADestLeft, ADestTop, ADestRight, ADestBottom,
  AOpacity: Single; ABlending: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LPathData: TPathData;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(ASurfaceWidth, ASurfaceHeight);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Blending := ABlending;
        LBitmap.Canvas.SetMatrix(
          CreateMatrix(ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg));
        LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
        LBitmap.Canvas.Fill.Color := TAlphaColors.Red;
        LPathData := CirclesPath(RectF(ADestLeft, ADestTop, ADestRight, ADestBottom));
        try
          LBitmap.Canvas.FillPath(LPathData, AOpacity);
        finally
          LPathData.Free;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillPathSolidWithModulateColor(const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LModulateCanvas: IModulateCanvas;
  LPathData: TPathData;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
          Exit;
        LModulateCanvas.ModulateColor := TAlphaColors.Blue;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
          LBitmap.Canvas.Fill.Color := TAlphaColors.Red;
          LPathData := CirclesPath(RectF(15, 15, 85, 85));
          try
            LBitmap.Canvas.FillPath(LPathData, 0.6);
          finally
            LPathData.Free;
          end;
        finally
          LModulateCanvas.ModulateColor := TAlphaColors.White;
          LModulateCanvas := nil;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillPathSolidWithResource(const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LBrushObject: TBrushObject;
  LBrushObject2: TBrushObject;
  LPathData: TPathData;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBrushObject := TBrushObject.Create(nil);
        try
          LBrushObject2 := TBrushObject.Create(nil);
          try
            LBrushObject2.Brush.Kind := TBrushKind.Solid;
            LBrushObject2.Brush.Color := TAlphaColors.Blue;
            LBrushObject.Brush.Kind := TBrushKind.Resource;
            LBrushObject.Brush.Resource.StyleResource := LBrushObject2;
            LBitmap.Canvas.Fill.Kind := TBrushKind.Resource;
            LBitmap.Canvas.Fill.Resource.StyleResource := LBrushObject;
            LPathData := CirclesPath(RectF(15, 15, 85, 85));
            try
              LBitmap.Canvas.FillPath(LPathData, 0.7);
            finally
              LPathData.Free;
            end;
          finally
            LBrushObject2.Free;
          end;
        finally
          LBrushObject.Free;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillPolygonBitmap(const APoints, AImageFileName, AModulateColor: string;
  AWrapMode: TWrapMode; ABlending: Boolean; const AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LImage: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(500, 400);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Blending := ABlending;
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Bitmap;
          LBitmap.Canvas.Fill.Bitmap.WrapMode := AWrapMode;
          LImage := CreateBitmap(AImageFileName);
          try
            LBitmap.Canvas.Fill.Bitmap.Bitmap := LImage;
          finally
            LImage.Free;
          end;
          LBitmap.Canvas.FillPolygon(StringToPolygon(APoints, RectF(0, 0, LBitmap.Width, LBitmap.Height)), AOpacity);
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectBitmap(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode;
  const ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LImage: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(500, 400);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Bitmap;
          LBitmap.Canvas.Fill.Bitmap.WrapMode := AWrapMode;
          LImage := CreateBitmap(AImageFileName);
          try
            LBitmap.Canvas.Fill.Bitmap.Bitmap := LImage;
          finally
            LImage.Free;
          end;
          LBitmap.Canvas.FillRect(RectF(ALeftPercent * LBitmap.Width, ATopPercent * LBitmap.Height,
            ARightPercent * LBitmap.Width, ABottomPercent * LBitmap.Height), 0, 0, [], AOpacity);
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectBitmapWithChessBackground(const AImageFileName, AModulateColor: string;
  AWrapMode: TWrapMode; ABlending: Boolean; const AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LImage: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(400, 400);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        DrawChessBackground(LBitmap.Canvas, Min(LBitmap.Width, LBitmap.Height) / 15 / LBitmap.BitmapScale,
          TAlphaColors.Black, TAlphaColors.White);
        LBitmap.Canvas.Blending := ABlending;
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Bitmap;
          LBitmap.Canvas.Fill.Bitmap.WrapMode := AWrapMode;
          LImage := CreateBitmap(AImageFileName);
          try
            LBitmap.Canvas.Fill.Bitmap.Bitmap := LImage;
          finally
            LImage.Free;
          end;
          LBitmap.Canvas.FillRect(RectF(0, 0, LBitmap.Width, LBitmap.Height), 0, 0, [], AOpacity);
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectBitmapWithClipping(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode;
  ABlending: Boolean; const AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LImage: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(400, 400);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.IntersectClipRect(RectF(0.15 * LBitmap.Width, 0.15 * LBitmap.Height,
          0.85 * LBitmap.Width, 0.85 * LBitmap.Height));
        LBitmap.Canvas.Blending := ABlending;
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Bitmap;
          LBitmap.Canvas.Fill.Bitmap.WrapMode := AWrapMode;
          LImage := CreateBitmap(AImageFileName);
          try
            LBitmap.Canvas.Fill.Bitmap.Bitmap := LImage;
          finally
            LImage.Free;
          end;
          LBitmap.Canvas.FillRect(RectF(0, 0, LBitmap.Width, LBitmap.Height), 0, 0, [], AOpacity);
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectBitmapWithMatrix(const AImageFileName: string; ASurfaceWidth,
  ASurfaceHeight: Integer; AWrapMode: TWrapMode; ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY,
  ARotationDeg, ADestLeft, ADestTop, ADestRight, ADestBottom, AOpacity: Single; ABlending: Boolean;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LImage: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(ASurfaceWidth, ASurfaceHeight);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Blending := ABlending;
        LBitmap.Canvas.SetMatrix(
          CreateMatrix(ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg));
        LBitmap.Canvas.Fill.Kind := TBrushKind.Bitmap;
        LBitmap.Canvas.Fill.Bitmap.WrapMode := AWrapMode;
        LImage := CreateBitmap(AImageFileName);
        try
          LBitmap.Canvas.Fill.Bitmap.Bitmap := LImage;
        finally
          LImage.Free;
        end;
        LBitmap.Canvas.FillRect(RectF(ADestLeft, ADestTop, ADestRight, ADestBottom), 0, 0, [], AOpacity);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectGradient(const APoints, AModulateColor: string; AGradientStyle: TGradientStyle;
  ABlending: Boolean; const AScale, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    LBitmap.BitmapScale := AScale;
    if LBitmap.Canvas.BeginScene then
      try
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.SetMatrix(TMatrix.CreateTranslation(10, 10));
          LBitmap.Canvas.Blending := ABlending;
          LBitmap.Canvas.Fill.Kind := TBrushKind.Gradient;
          LBitmap.Canvas.Fill.Gradient.Style := AGradientStyle;
          ApplyGradientPoints(LBitmap.Canvas.Fill.Gradient, APoints);
          LBitmap.Canvas.FillRect(RectF(0, 0, 80, 80), 0, 0, [], AOpacity);
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectGradient2(const APoints, AModulateColor: string; AGradientStyle: TGradientStyle;
  ABlending: Boolean; const AScale, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    LBitmap.BitmapScale := AScale;
    if LBitmap.Canvas.BeginScene then
      try
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.SetMatrix(TMatrix.CreateTranslation(10, 10));
          LBitmap.Canvas.Blending := ABlending;
          LBitmap.Canvas.Fill.Kind := TBrushKind.Gradient;
          LBitmap.Canvas.Fill.Gradient.Style := AGradientStyle;
          ApplyGradientPoints(LBitmap.Canvas.Fill.Gradient, APoints);
          LBitmap.Canvas.FillRect(RectF(0, 20, 80, 60), 0, 0, [], AOpacity);
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectGradientRadial(const APoints, AModulateColor: string; AGradientScaleX,
  AGradientScaleY, AGradientPosX, AGradientPosY, AGradientRotationDegree, AGradientRotationCenterX,
  AGradientRotationCenterY, AGradientSkewX, AGradientSkewY, ADestLeftPercent, ADestTopPercent, ADestRightPercent,
  ADestBottomPercent, ACanvasScaleX, ACanvasScaleY, ACanvasOffsetX, ACanvasOffsetY, ACanvasRotationDeg: Single;
  AApplyClip, ABlending, ADrawBackgroundChess: Boolean; const ABitmapScale, AOpacity, AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    LBitmap.BitmapScale := ABitmapScale;
    if LBitmap.Canvas.BeginScene then
      try
        if ADrawBackgroundChess then
        begin
          DrawChessBackground(LBitmap.Canvas, Min(LBitmap.Width, LBitmap.Height) / 15 / LBitmap.BitmapScale,
            TAlphaColors.Black, TAlphaColors.White);
        end;
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.SetMatrix(
            CreateMatrix(ACanvasScaleX, ACanvasScaleY, ACanvasOffsetX, ACanvasOffsetY, ACanvasRotationDeg));
          if AApplyClip then
          begin
            LBitmap.Canvas.IntersectClipRect(RectF(0.1 * LBitmap.Width, 0.1 * LBitmap.Height,
              0.9 * LBitmap.Width, 0.9 * LBitmap.Height));
          end;
          LBitmap.Canvas.Blending := ABlending;
          LBitmap.Canvas.Fill.Kind := TBrushKind.Gradient;
          LBitmap.Canvas.Fill.Gradient.Style := TGradientStyle.Radial;
          ApplyGradientPoints(LBitmap.Canvas.Fill.Gradient, APoints);
          LBitmap.Canvas.Fill.Gradient.RadialTransform.Scale.Point := PointF(AGradientScaleX, AGradientScaleY);
          LBitmap.Canvas.Fill.Gradient.RadialTransform.Position.Point := PointF(AGradientPosX, AGradientPosY);
          LBitmap.Canvas.Fill.Gradient.RadialTransform.RotationAngle := AGradientRotationDegree;
          LBitmap.Canvas.Fill.Gradient.RadialTransform.RotationCenter.Point := PointF(AGradientRotationCenterX, AGradientRotationCenterY);
          TOpenTransform(LBitmap.Canvas.Fill.Gradient.RadialTransform).Skew.Point := PointF(AGradientSkewX, AGradientSkewY);
          LBitmap.Canvas.FillRect(RectF(ADestLeftPercent * LBitmap.Width, ADestTopPercent * LBitmap.Height,
            ADestRightPercent * LBitmap.Width, ADestBottomPercent * LBitmap.Height), 0, 0, [], AOpacity);
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectGradientStartEndPoint(const APoints, AModulateColor: string; AStartX, AStartY,
  AStopX, AStopY, ADestLeftPercent, ADestTopPercent, ADestRightPercent, ADestBottomPercent, ACanvasScaleX,
  ACanvasScaleY, ACanvasOffsetX, ACanvasOffsetY, ACanvasRotationDeg: Single; AApplyClip, ABlending: Boolean;
  const ABitmapScale, AOpacity: Single; ADrawBackgroundChess: Boolean; AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    LBitmap.BitmapScale := ABitmapScale;
    if LBitmap.Canvas.BeginScene then
      try
        if ADrawBackgroundChess then
        begin
          DrawChessBackground(LBitmap.Canvas, Min(LBitmap.Width, LBitmap.Height) / 15 / LBitmap.BitmapScale,
            TAlphaColors.Black, TAlphaColors.White);
        end;
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.SetMatrix(
            CreateMatrix(ACanvasScaleX, ACanvasScaleY, ACanvasOffsetX, ACanvasOffsetY, ACanvasRotationDeg));
          if AApplyClip then
          begin
            LBitmap.Canvas.IntersectClipRect(RectF(0.1 * LBitmap.Width, 0.1 * LBitmap.Height,
              0.9 * LBitmap.Width, 0.9 * LBitmap.Height));
          end;
          LBitmap.Canvas.Blending := ABlending;
          LBitmap.Canvas.Fill.Kind := TBrushKind.Gradient;
          LBitmap.Canvas.Fill.Gradient.Style := TGradientStyle.Linear;
          ApplyGradientPoints(LBitmap.Canvas.Fill.Gradient, APoints);
          LBitmap.Canvas.Fill.Gradient.StartPosition.Point := PointF(AStartX, AStartY);
          LBitmap.Canvas.Fill.Gradient.StopPosition.Point := PointF(AStopX, AStopY);
          LBitmap.Canvas.FillRect(RectF(ADestLeftPercent * LBitmap.Width, ADestTopPercent * LBitmap.Height,
            ADestRightPercent * LBitmap.Width, ADestBottomPercent * LBitmap.Height), 0, 0, [], AOpacity);
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectSolid(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
        LBitmap.Canvas.Fill.Color := TAlphaColors.Black;
        LBitmap.Canvas.FillRect(RectF(25, 25, 75, 75), 0, 0, [], 0.5);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectSolid2(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
        LBitmap.Canvas.Fill.Color := MakeColor(TAlphaColors.Black, 0.5);
        LBitmap.Canvas.FillRect(RectF(25, 25, 75, 75), 0, 0, [], 1);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectSolid3(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.IntersectClipRect(RectF(0, 0, 50, 50));
        LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
        LBitmap.Canvas.Fill.Color := TAlphaColors.Black;
        LBitmap.Canvas.FillRect(RectF(25, 25, 75, 75), 0, 0, [], 1);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectSolid4(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.IntersectClipRect(RectF(20, 20, 95, 95));
        LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
        LBitmap.Canvas.Fill.Color := TAlphaColors.Red;
        LBitmap.Canvas.FillRect(RectF(25, 25, 75, 75), 0, 0, [], 1);
        LBitmap.Canvas.Fill.Color := TAlphaColors.Green;
        LBitmap.Canvas.FillRect(RectF(50, 50, 100, 100), 0, 0, [], 0.7);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectSolid5(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
          Exit;
        LModulateCanvas.ModulateColor := TAlphaColors.Blue;
        try
          LBitmap.Canvas.IntersectClipRect(RectF(20, 20, 95, 95));
          LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
          LBitmap.Canvas.Fill.Color := TAlphaColors.Red;
          LBitmap.Canvas.FillRect(RectF(25, 25, 75, 75), 0, 0, [], 1);
          LBitmap.Canvas.Fill.Color := TAlphaColors.Green;
          LBitmap.Canvas.FillRect(RectF(50, 50, 100, 100), 0, 0, [], 0.7);
        finally
          LModulateCanvas.ModulateColor := TAlphaColors.White;
          LModulateCanvas := nil;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectSolid6(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Blending := False;
        LBitmap.Canvas.IntersectClipRect(RectF(20, 20, 95, 95));
        LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
        LBitmap.Canvas.Fill.Color := TAlphaColors.Red;
        LBitmap.Canvas.FillRect(RectF(25, 25, 75, 75), 0, 0, [], 1);
        LBitmap.Canvas.Fill.Color := TAlphaColors.Green;
        LBitmap.Canvas.FillRect(RectF(50, 50, 100, 100), 0, 0, [], 0.7);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectSolidWithMatrix(ASurfaceWidth, ASurfaceHeight: Integer; ASurfaceScaleX,
  ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg, ADestLeft, ADestTop, ADestRight, ADestBottom,
  AOpacity: Single; ABlending: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(ASurfaceWidth, ASurfaceHeight);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Blending := ABlending;
        LBitmap.Canvas.SetMatrix(
          CreateMatrix(ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg));
        LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
        LBitmap.Canvas.Fill.Color := TAlphaColors.Red;
        LBitmap.Canvas.FillRect(RectF(ADestLeft, ADestTop, ADestRight, ADestBottom), 0, 0, [], AOpacity);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectSolidWithModulateColor(const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
          Exit;
        LModulateCanvas.ModulateColor := TAlphaColors.Blue;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
          LBitmap.Canvas.Fill.Color := TAlphaColors.Red;
          LBitmap.Canvas.FillRect(RectF(25, 25, 75, 75), 0, 0, [], 0.6);
        finally
          LModulateCanvas.ModulateColor := TAlphaColors.White;
          LModulateCanvas := nil;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectSolidWithResource(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LBrushObject: TBrushObject;
  LBrushObject2: TBrushObject;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBrushObject := TBrushObject.Create(nil);
        try
          LBrushObject2 := TBrushObject.Create(nil);
          try
            LBrushObject2.Brush.Kind := TBrushKind.Solid;
            LBrushObject2.Brush.Color := TAlphaColors.Blue;
            LBrushObject.Brush.Kind := TBrushKind.Resource;
            LBrushObject.Brush.Resource.StyleResource := LBrushObject2;
            LBitmap.Canvas.Fill.Kind := TBrushKind.Resource;
            LBitmap.Canvas.Fill.Resource.StyleResource := LBrushObject;
            LBitmap.Canvas.FillRect(RectF(25, 25, 75, 75), 0, 0, [], 0.7);
          finally
            LBrushObject2.Free;
          end;
        finally
          LBrushObject.Free;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRoundRectBitmap(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode;
  AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType; ASmallSize: Boolean; ALeftPercent,
  ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LDestRect: TRectF;
  LImage: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    if ASmallSize then
      LBitmap.SetSize(300, 300)
    else
      LBitmap.SetSize(600, 600);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Bitmap;
          LBitmap.Canvas.Fill.Bitmap.WrapMode := AWrapMode;
          LImage := CreateBitmap(AImageFileName);
          try
            LBitmap.Canvas.Fill.Bitmap.Bitmap := LImage;
          finally
            LImage.Free;
          end;
          LDestRect := RectF(ALeftPercent * LBitmap.Width, ATopPercent * LBitmap.Height,
            ARightPercent * LBitmap.Width, ABottomPercent * LBitmap.Height);
          if AXRadius < 1 + TEpsilon.Vector then
            AXRadius := AXRadius * LDestRect.Width;
          if AYRadius < 1 + TEpsilon.Vector then
            AYRadius := AYRadius * LDestRect.Height;
          LBitmap.Canvas.FillRect(LDestRect, AXRadius, AYRadius,
            StringToCorners(ACornersString), AOpacity, ACornerType);
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRoundRectBitmapWithChessBackground(const AImageFileName, AModulateColor: string;
  AWrapMode: TWrapMode; AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType;
  ASmallSize: Boolean; ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LDestRect: TRectF;
  LImage: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    if ASmallSize then
      LBitmap.SetSize(300, 300)
    else
      LBitmap.SetSize(600, 600);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        DrawChessBackground(LBitmap.Canvas, Min(LBitmap.Width, LBitmap.Height) / 15 / LBitmap.BitmapScale,
          TAlphaColors.Black, TAlphaColors.White);
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Bitmap;
          LBitmap.Canvas.Fill.Bitmap.WrapMode := AWrapMode;
          LImage := CreateBitmap(AImageFileName);
          try
            LBitmap.Canvas.Fill.Bitmap.Bitmap := LImage;
          finally
            LImage.Free;
          end;
          LDestRect := RectF(ALeftPercent * LBitmap.Width, ATopPercent * LBitmap.Height,
            ARightPercent * LBitmap.Width, ABottomPercent * LBitmap.Height);
          if AXRadius < 1 + TEpsilon.Vector then
            AXRadius := AXRadius * LDestRect.Width;
          if AYRadius < 1 + TEpsilon.Vector then
            AYRadius := AYRadius * LDestRect.Height;
          LBitmap.Canvas.FillRect(LDestRect, AXRadius, AYRadius,
            StringToCorners(ACornersString), AOpacity, ACornerType);
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRoundRectBitmapWithClipping(const AImageFileName, AModulateColor: string;
  AWrapMode: TWrapMode; AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType;
  ASmallSize: Boolean; ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LDestRect: TRectF;
  LImage: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    if ASmallSize then
      LBitmap.SetSize(300, 300)
    else
      LBitmap.SetSize(600, 600);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.IntersectClipRect(RectF(LBitmap.Width * 0.5, 0, LBitmap.Width, LBitmap.Height));
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Bitmap;
          LBitmap.Canvas.Fill.Bitmap.WrapMode := AWrapMode;
          LImage := CreateBitmap(AImageFileName);
          try
            LBitmap.Canvas.Fill.Bitmap.Bitmap := LImage;
          finally
            LImage.Free;
          end;
          LDestRect := RectF(ALeftPercent * LBitmap.Width, ATopPercent * LBitmap.Height,
            ARightPercent * LBitmap.Width, ABottomPercent * LBitmap.Height);
          LBitmap.Canvas.FillRect(LDestRect, AXRadius * LDestRect.Width, AYRadius * LDestRect.Height,
            StringToCorners(ACornersString), AOpacity, ACornerType);
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRoundRectBitmapWithClipping2(const AImageFileName, AModulateColor: string;
  AWrapMode: TWrapMode; AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType;
  ASmallSize: Boolean; ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LClipRects: TArray<TRectF>;
  LDestRect: TRectF;
  LImage: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    if ASmallSize then
      LBitmap.SetSize(300, 300)
    else
      LBitmap.SetSize(600, 600);
    LBitmap.BitmapScale := 1;
    LClipRects := [RectF(LBitmap.Width * 0.5, 0, LBitmap.Width, LBitmap.Height)];
    if LBitmap.Canvas.BeginScene(@LClipRects) then
      try
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Bitmap;
          LBitmap.Canvas.Fill.Bitmap.WrapMode := AWrapMode;
          LImage := CreateBitmap(AImageFileName);
          try
            LBitmap.Canvas.Fill.Bitmap.Bitmap := LImage;
          finally
            LImage.Free;
          end;
          LDestRect := RectF(ALeftPercent * LBitmap.Width, ATopPercent * LBitmap.Height,
            ARightPercent * LBitmap.Width, ABottomPercent * LBitmap.Height);
          LBitmap.Canvas.FillRect(LDestRect, AXRadius * LDestRect.Width, AYRadius * LDestRect.Height,
            StringToCorners(ACornersString), AOpacity, ACornerType);
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRoundRectBitmapWithMatrix(const AImageFileName, AModulateColor: string;
  AWrapMode: TWrapMode; AScaleX, AScaleY, ADeltaX, ADeltaY, ARotationDegree, AXRadius, AYRadius: Single;
  const ACornersString: string; ACornerType: TCornerType; ASmallSize: Boolean; ALeftPercent, ATopPercent, ARightPercent,
  ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LDestRect: TRectF;
  LImage: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    if ASmallSize then
      LBitmap.SetSize(300, 300)
    else
      LBitmap.SetSize(600, 600);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.SetMatrix(
          CreateMatrix(AScaleX, AScaleY, ADeltaX * LBitmap.Width, ADeltaY * LBitmap.Height, ARotationDegree));
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Bitmap;
          LBitmap.Canvas.Fill.Bitmap.WrapMode := AWrapMode;
          LImage := CreateBitmap(AImageFileName);
          try
            LBitmap.Canvas.Fill.Bitmap.Bitmap := LImage;
          finally
            LImage.Free;
          end;
          LDestRect := RectF(ALeftPercent * LBitmap.Width, ATopPercent * LBitmap.Height,
            ARightPercent * LBitmap.Width, ABottomPercent * LBitmap.Height);
          LBitmap.Canvas.FillRect(LDestRect, AXRadius * LDestRect.Width, AYRadius * LDestRect.Height,
            StringToCorners(ACornersString), AOpacity, ACornerType);
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRoundRectGradient(const APoints, AModulateColor: string;
  AGradientStyle: TGradientStyle; AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType;
  ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LDestRect: TRectF;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Gradient;
          LBitmap.Canvas.Fill.Gradient.Style := AGradientStyle;
          ApplyGradientPoints(LBitmap.Canvas.Fill.Gradient, APoints);
          LDestRect := RectF(ALeftPercent * LBitmap.Width, ATopPercent * LBitmap.Height,
            ARightPercent * LBitmap.Width, ABottomPercent * LBitmap.Height);
          LBitmap.Canvas.FillRect(LDestRect, AXRadius * LDestRect.Width, AYRadius * LDestRect.Height,
            StringToCorners(ACornersString), AOpacity, ACornerType);
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRoundRectGradientWithChessBackground(const APoints, AModulateColor: string;
  AGradientStyle: TGradientStyle; AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType;
  ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LDestRect: TRectF;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        DrawChessBackground(LBitmap.Canvas, Min(LBitmap.Width, LBitmap.Height) / 15 / LBitmap.BitmapScale,
          TAlphaColors.Black, TAlphaColors.White);
        LBitmap.Canvas.Blending := False;
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Gradient;
          LBitmap.Canvas.Fill.Gradient.Style := AGradientStyle;
          ApplyGradientPoints(LBitmap.Canvas.Fill.Gradient, APoints);
          LDestRect := RectF(ALeftPercent * LBitmap.Width, ATopPercent * LBitmap.Height,
            ARightPercent * LBitmap.Width, ABottomPercent * LBitmap.Height);
          LBitmap.Canvas.FillRect(LDestRect, AXRadius * LDestRect.Width, AYRadius * LDestRect.Height,
            StringToCorners(ACornersString), AOpacity, ACornerType);
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRoundRectGradientWithResource(const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LBrushObject: TBrushObject;
  LBrushObject2: TBrushObject;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBrushObject := TBrushObject.Create(nil);
        try
          LBrushObject2 := TBrushObject.Create(nil);
          try
            LBrushObject2.Brush.Kind := TBrushKind.Gradient;
            LBrushObject2.Brush.Gradient.Style := TGradientStyle.Linear;
            ApplyGradientPoints(LBrushObject2.Brush.Gradient, '[claRed;0 claNull;0.5 claBlue;1]');
            LBrushObject.Brush.Kind := TBrushKind.Resource;
            LBrushObject.Brush.Resource.StyleResource := LBrushObject2;
            LBitmap.Canvas.Fill.Kind := TBrushKind.Resource;
            LBitmap.Canvas.Fill.Resource.StyleResource := LBrushObject;
            LBitmap.Canvas.FillRect(RectF(25, 25, 75, 75), 16, 16, AllCorners, 1);
          finally
            LBrushObject2.Free;
          end;
        finally
          LBrushObject.Free;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRoundRectSolid(const AColor, AModulateColor: string; ABlending: Boolean; AXRadius,
  AYRadius: Single; const ACornersString: string; ACornerType: TCornerType; ALeftPercent, ATopPercent, ARightPercent,
  ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LDestRect: TRectF;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Blending := ABlending;
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
          LBitmap.Canvas.Fill.Color := StringToAlphaColor(AColor);
          LDestRect := RectF(ALeftPercent * LBitmap.Width, ATopPercent * LBitmap.Height,
            ARightPercent * LBitmap.Width, ABottomPercent * LBitmap.Height);
          LBitmap.Canvas.FillRect(LDestRect, AXRadius * LDestRect.Width, AYRadius * LDestRect.Height,
            StringToCorners(ACornersString), AOpacity, ACornerType);
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRoundRectSolidWithChessBackground(const AColor, AModulateColor: string;
  ABlending: Boolean; AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType; ALeftPercent,
  ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LDestRect: TRectF;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        DrawChessBackground(LBitmap.Canvas, Min(LBitmap.Width, LBitmap.Height) / 15 / LBitmap.BitmapScale,
          TAlphaColors.Black, TAlphaColors.White);
        LBitmap.Canvas.Blending := ABlending;
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
          LBitmap.Canvas.Fill.Color := StringToAlphaColor(AColor);
          LDestRect := RectF(ALeftPercent * LBitmap.Width, ATopPercent * LBitmap.Height,
            ARightPercent * LBitmap.Width, ABottomPercent * LBitmap.Height);
          LBitmap.Canvas.FillRect(LDestRect, AXRadius * LDestRect.Width, AYRadius * LDestRect.Height,
            StringToCorners(ACornersString), AOpacity, ACornerType);
        finally
          if LModulateCanvas <> nil then
          begin
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LModulateCanvas := nil;
          end;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRoundRectSolidWithClipping(const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.IntersectClipRect(RectF(LBitmap.Width * 0.5, 0, LBitmap.Width, LBitmap.Height));
        LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
        LBitmap.Canvas.Fill.Color := TAlphaColors.Red;
        LBitmap.Canvas.FillRect(RectF(25, 25, 75, 75), 16, 16, AllCorners, 0.85);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRoundRectSolidWithClipping2(const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
          Exit;
        LModulateCanvas.ModulateColor := TAlphaColors.Blue;
        try
          LBitmap.Canvas.IntersectClipRect(RectF(LBitmap.Width * 0.5, 0, LBitmap.Width, LBitmap.Height));
          LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
          LBitmap.Canvas.Fill.Color := TAlphaColors.Red;
          LBitmap.Canvas.FillRect(RectF(25, 25, 75, 75), 16, 16, AllCorners, 0.85);
        finally
          LModulateCanvas.ModulateColor := TAlphaColors.White;
          LModulateCanvas := nil;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRoundRectSolidWithResource(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LBrushObject: TBrushObject;
  LBrushObject2: TBrushObject;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBrushObject := TBrushObject.Create(nil);
        try
          LBrushObject2 := TBrushObject.Create(nil);
          try
            LBrushObject2.Brush.Kind := TBrushKind.Solid;
            LBrushObject2.Brush.Color := MakeColor(TAlphaColors.Green, 0.6);
            LBrushObject.Brush.Kind := TBrushKind.Resource;
            LBrushObject.Brush.Resource.StyleResource := LBrushObject2;
            LBitmap.Canvas.Fill.Kind := TBrushKind.Resource;
            LBitmap.Canvas.Fill.Resource.StyleResource := LBrushObject;
            LBitmap.Canvas.FillRect(RectF(25, 25, 75, 75), 16, 16, AllCorners, 1);
          finally
            LBrushObject2.Free;
          end;
        finally
          LBrushObject.Free;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestSaveState(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LSaveState: TCanvasSaveState;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LSaveState := LBitmap.Canvas.SaveState;
        LBitmap.Canvas.IntersectClipRect(RectF(25, 25, 75, 75));
        LBitmap.Canvas.RestoreState(LSaveState);
        LBitmap.Canvas.Clear(TAlphaColors.Red);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TSkFMXCanvasTests);
end.
