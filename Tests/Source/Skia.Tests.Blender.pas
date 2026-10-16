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
unit Skia.Tests.Blender;

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
  { TSkBlenderTests }

  [TestFixture]
  TSkBlenderTests = class(TTestBase)
  public
    [TestCase('Clear',      '256,Clear,0.99,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('Src',        '256,Src,0.99,vz8hvArGa/A76zAwwIPjnG+AhPPY6/FE2/i7IPz90xP//PXo22C7+J5zffkN/Fd+OgJFcD2/8g')]
    [TestCase('Dest',       '256,Dest,0.99,k2PBfAu5h0/+8wKQcIj04FNvPPvhj4HBx8CrH38WC6zP/fSxi5/D8QFgYIAFzTtfugw/GN98Bw')]
    [TestCase('SrcOver',    '256,SrcOver,0.99,udtQ/QTGKxD96ysUMAToqGPAhfHWtxHE3fOTQPz90xcPffXo50C7+JxzjbkN/FdeegJGcG1/8g')]
    [TestCase('DestOver',   '256,DestOver,0.99,nAPRvgOz88/9/zAYMUj4qt+/P/fn39FEw67rjjny5JufvQDBi+C85KE/j/sVFOBP/AAAlBDBBw')]
    [TestCase('SrcIn',      '256,SrcIn,0.99,tp8RehLM+3983wKsD4Tf0uM/QffSm6HB3wp4sQEi+SsvPerY2x/CBf6XXr79ta+N+A2E+Os68g')]
    [TestCase('DestIn',     '256,DestIn,0.99,mceROxG6xy998wisAAjw1F8v+/zjw2FCxcXT/74WC6y/ffWxf88D8gRgQMAFzTtvegw+HK+8Bw')]
    [TestCase('SrcOut',     '256,SrcOut,0.99,3vfBfPC6x+5AAc4HwEX5XzhfQeybq6CA48/DwDcBhGvfO/EmWFDz6gAwfsT6J6hPu/mW+0+CGA')]
    [TestCase('DestOut',    '256,DestOut,0.99,z/fhe/K/F44C9ptnf8b5oaBOP+6TI3C86tpbcX0lqocfPOUVYN85+F1Uf4nq4ANP+QXWgz7+Fw')]
    [TestCase('SrcATop',    '256,SrcATop,0.99,sDtBuwzMu5/93/2TfwTk3td/QvXQY8FA4gRQ0QEi+Ssv/enX4//BBfuXfr79ta99eA6F9Cv78Q')]
    [TestCase('DestATop',   '256,DestATop,0.99,oWOhfQm0M6A8/zU0wQf0nut/PvnpE7LFwLQTb3ny5ZeP/QDBgxD95KQ/b/sVFOBffAD/l+BACA')]
    [TestCase('&Xor',       '256,Xor,0.99,2Jfxveq5hw7CAcnvP8b9ayyfQuqac8A/5smb4DcBhG/vu/AmYCDz6v4znsT6J6g/+/mX+39CGA')]
    [TestCase('Plus',       '256,Plus,0.99,t8+gPQjH/19970rgIAXtoYNww/UdQOKD0fs7/4Dy/DOPQf23wyCA6o4DD3kgClTffQQbuB9B/g')]
    [TestCase('Modulate',   '256,Modulate,0.99,n88xOw63+z+97ubrH8fp3U/fffefQ5FC0b8vMbog5Ydv+ezfr/8+ARLUvgDyvkv+NgtqyOx6/A')]
    [TestCase('Screen',     '256,Screen,0.99,s9PA/QfI9w+98EzcIEXun3MAQ/caMOKD0fkzD4Hz+T9fwf25txCA6Y7/LnkhCkDfvQMawB/B/Q')]
    [TestCase('Overlay',    '256,Overlay,0.99,kxuR/gO27089/DMAgQj1sMsvwPjpy2HFw6nTzvnz6re//f7CZ9C845UvfzwTGXwfvAEFhKDACA')]
    [TestCase('Darken',     '256,Darken,0.99,pgdB/gCvMyA++g5UUAfxtd/fAvClb/FEz6tPoHT7wmN/+fftxzD49LCz3XsBBgTPeAAtOF5//Q')]
    [TestCase('Lighten',    '256,Lighten,0.99,r9fg/QfJ68+88U3YIEXvnWOggvgYJPLD0fYrL4H09UsfAf68qwCA6I37PjkiCjDv/QIZyC8B/Q')]
    [TestCase('ColorDodge', '256,ColorDodge,0.99,nROBvgWu6x9+/CwEAcj0qBNAQPTw34HExLnzrvnx35/f/vy5WxB+6J43DzwTB8w/PQQfUCAABQ')]
    [TestCase('ColorBurn',  '256,ColorBurn,0.99,hAfhvgW90988+C8YcUj1sW8fwP3Uz/EExKbnTrn45Lf/vPvSW9A74Jq7Dn0VIwBPu/76h/DABw')]
    [TestCase('HardLight',  '256,HardLight,0.99,iudgfQGp+9+97wqUAIbvpEswQ/O0d1HF0chDkDz81BvvvPXaBzF886ZnbXkL+2/uuQNFqM3+8g')]
    [TestCase('SoftLight',  '256,SoftLight,0.99,lROx/gO16399/TMIYUj2rdP/P/jozxGFw6zfrnnz56u//f7Cb+A85JsvH7wUFZgvvAEFhLDABw')]
    [TestCase('Difference', '256,Difference,0.99,qs8g/gbK0x989E/Y4ATxlUvAwfsQJGKE0PI370D46zf/v//RjyC+6o3/3vgnE/y+fQEJlP/B+Q')]
    [TestCase('Exclusion',  '256,Exclusion,0.99,ssfQPQfI56888k3g4ITumWuQAvkVQFIE0PhHv0D28htvgP7Mp1C+7I8DvzgmEyCv/QIKhO9B+w')]
    [TestCase('Multiply',   '256,Multiply,0.99,ogtxvgCwJ9A9+w9UQAfys89/gvGjXwGFz6hHwHT8v3NPOfjwuxD486+z7XsCBfTOuP8sQF6//A')]
    [TestCase('Hue',        '256,Hue,0.99,zN9gfv3LYxC89igAgEXqnmvABfDb5wHD3vmfwD373jNvfvrWq/D89o2LXrkd9XPvPQMvmH6A9g')]
    [TestCase('Saturation', '256,Saturation,0.99,rBNxfwS8M6B9ACfc0Mf1ps+vAPXY2/GDy71brznz5Zcfvv/Ce6D855ovrzsWFtyPfAAJXEBBBw')]
    [TestCase('Color',      '256,Color,0.99,zOMwvv3JW0C89igEcIXqn3eAxe/c71HD3PiPwP363TM/fvrWrwA99o+Pbrkd9XPffQMtpH6A9g')]
    [TestCase('Luminosity', '256,Luminosity,0.99,lesQ/gqv449+9jAY8cb2s7+fwPjhu4GFxawn/zf113NP/PvOz0C76LEX/voEFOTu+P4aQP9/AQ')]
    procedure TestModes(const ASize: Integer; const ABlendMode: TSkBlendMode; const AMinSimilarity: Double; const AExpectedImageHash: string);
  end;

implementation

uses
  { Delphi }
  System.Math,
  System.Math.Vectors,
  System.Classes,
  System.Types,
  System.UITypes,
  System.IOUtils;

{ TSkBlenderTests }

procedure TSkBlenderTests.TestModes(const ASize: Integer;
  const ABlendMode: TSkBlendMode; const AMinSimilarity: Double;
  const AExpectedImageHash: string);

  procedure DrawBackground(const ACanvas: ISkCanvas);
  var
    LPaint: ISkPaint;
    LComp: ISkShader;
    LRotMatrix: TMatrix;
    LRadialShader: ISkShader;
    LSweepShader: ISkShader;
  begin
    LRadialShader := TSkShader.MakeGradientRadial(PointF(ASize / 2, ASize / 2), ASize / 2, [$FFFFFFFF, $FFFFFFFF, $00FFFFFF]);
    LRotMatrix := TMatrix.CreateTranslation(- ASize / 2, - ASize / 2) * TMatrix.CreateRotation(DegToRad(-90)) * TMatrix.CreateTranslation(ASize / 2, ASize / 2);
    LSweepShader := TSkShader.MakeGradientSweep(PointF(ASize / 2, ASize / 2), [$FFFF00FF, $FFFF0000, $FFFFFF00, $FF00FF00, $FF00FFFF, $FF0000FF, $FFFF00FF], LRotMatrix);
    LComp := TSkShader.MakeBlend(TSkBlendMode.Modulate, LRadialShader, LSweepShader);
    LPaint := TSkPaint.Create;
    LPaint.Shader := LComp;
    ACanvas.DrawPaint(LPaint);
  end;

var
  LSurface: ISkSurface;
  LLayerPaint: ISkPaint;
  LDiscPaint: ISkPaint;
  LRadius: Single;
begin
  LSurface := TSkSurface.MakeRaster(ASize, ASize);
  Assert.IsNotNull(LSurface, 'Invalid ISkSurface (nil)');
  LSurface.Canvas.Clear(TAlphaColors.Null);

  LLayerPaint := TSkPaint.Create;
  LLayerPaint.Blender := TSkBlender.MakeMode(ABlendMode);

  DrawBackground(LSurface.Canvas);

  LRadius := ASize / 3.2;
  LSurface.Canvas.SaveLayer(LLayerPaint);
  try
    LDiscPaint := TSkPaint.Create;
    LDiscPaint.AntiAlias := True;
    LDiscPaint.Blender := TSkBlender.MakeMode(TSkBlendMode.Plus);

    LDiscPaint.Color := TAlphaColors.Green;
    LSurface.Canvas.DrawCircle(ASize / 2, LRadius, LRadius, LDiscPaint);

    LDiscPaint.setColor(TAlphaColors.Red);
    LSurface.Canvas.DrawCircle(LRadius, ASize - LRadius, LRadius, LDiscPaint);

    LDiscPaint.setColor(TAlphaColors.Blue);
    LSurface.Canvas.DrawCircle(ASize - LRadius, ASize - LRadius, LRadius, LDiscPaint);
  finally
    LSurface.Canvas.Restore;
  end;
  Assert.AreSimilar(AExpectedImageHash, LSurface.MakeImageSnapshot, AMinSimilarity, Format(' w:%d h:%d', [ASize, ASize]));
end;

initialization
  TDUnitX.RegisterTestFixture(TSkBlenderTests);
end.
