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
unit Skia.Tests.Vcl.Svg;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  System.UITypes,
  DUnitX.TestFramework,

  { Skia }
  System.Skia,
  Vcl.Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkSvgBrushTests }

  [TestFixture]
  TSkSvgBrushTests = class(TTestBase)
  private
    procedure TestRender(const ASvgFileName: string; const AWidth, AHeight: Integer; const AWrapMode: TSkSvgWrapMode; const AOverrideColor: TAlphaColor; const AExpectedImageHash: string);
  public
    [TestCase('android.svg.1',      'android.svg,80,80,2/8Aug4IgGA77hN0b0Tr4x9Pwfzdp8G/3QMsAcMf/hcPP/GcH1AC9UrMD0D8rWfP+wthwB17Dg')]
    [TestCase('android.svg.2',      'android.svg,160,80,CGTCOPvzb2957QtMzsPsDew+vPqzRyEB3tszMX8jpQ++P/WoJ1AB/z5AH0Ln1bt+ugVDhM37BA')]
    [TestCase('chinese-text.svg.1', 'chinese-text.svg,80,80,0b9xuwXG3y4D9333z4X4ddDOv/esk2D85gCwIbwgupuPPeAewM+5+lScnwjxyK+/dwHfbz59Gg')]
    [TestCase('chinese-text.svg.2', 'chinese-text.svg,160,80,s3MxuwXLa69C83FTEIb1Y8BOg/y7k/B65QusEbwav/vPPd4ZyC+6+EOUX8bxxpevOALvn/57Gg')]
    [TestCase('delphi.svg.1',       'delphi.svg,80,80,Hligfvo8rHCB9RA8gHvxsLuAAuQ3FGGD3vLXPwYKDjzPPfihJ2GD8pQf0HsOoFuP/he2EF5A6Q')]
    [TestCase('delphi.svg.2',       'delphi.svg,160,80,BHgiePr0r3/48EsIUIH64XNfuffKNzGHy997n8Qwvmf+f/d4z6/G+RN83/z2vxOP9xV3jAz56w')]
    [TestCase('gorilla.svg.1',      'gorilla.svg,80,80,y+PxeQv+c1/66QcUMEPw6S8Pev+Hb1GC0sajID0b63ef9uQgNEB7ARioLwEGEbC9OAtfqE29+g')]
    [TestCase('gorilla.svg.2',      'gorilla.svg,160,80,v2diefbm8+788OAnH0T2QJieO/qpx2DA2MYHoX4tqB+/O+71f7+9CFgoD0Ls1Ns+Ngxa5Gz8/A')]
    [TestCase('lion.svg.1',         'lion.svg,80,80,vz8R/QfPYw/C9LujH8bycMhOPfaajxB+5NZXUUAb89Pf++vxvx/9+jpQn8fq1NuOuQjvK469EA')]
    [TestCase('lion.svg.2',         'lion.svg,160,80,8Qti+gPXY7+78/+bP4Xzd6CevfKae+A+4tszocEswYPuffHLwx9A/lFU7wfp2Pu+uQb2J269EQ')]
    [TestCase('tesla.svg.1',        'tesla.svg,40,40,xU+S+/iEo25A+rerDwP9aPievO+MO3FB4cgroLsmuU/PvOMdPI+7/D9kz8bq0yPPeAYEzA09EA')]
    [TestCase('tesla.svg.2',        'tesla.svg,80,40,tzPiuveyV67/87Tf74Pza4hOO/R3ZyBA3tmvgDwmtWNv/OMbTM+6+VBoL0jq2BcfOQbxG179Ew')]
    [TestCase('youtube.svg.1',      'youtube.svg,80,80,xSOAvfEXkE+EAn63bn7mF7g+/vbg06DCzq3vX4IOMIyeeOsu8JF466MbgEQGmHv+OhFxFE7/4A')]
    [TestCase('youtube.svg.2',      'youtube.svg,160,80,whtiuQrGZ0998gBkf0TuEXTPN/+o8xEExK03EIIu71+evPn012+BAUTUjsH02DceNhNFMCy77Q')]
    procedure TestFitRender(const ASvgFileName: string; const AWidth, AHeight: Integer; const AExpectedImageHash: string);
    [TestCase('android.svg.1',      'android.svg,80,80,5p/Q+w4T4GC97hJkb8Xt81f/gvrqsyG/4vNH0QMfBVhf//KM98+B9DjcL//6q2tfvA1h1D28DQ')]
    [TestCase('android.svg.2',      'android.svg,160,80,5pvAew4V3GC97hNkfwXu9Ff/gvrqsyG/4vNH0UMfBVxf//KM97+B9DncL//6q2tfvA1h1D38DQ')]
    [TestCase('chinese-text.svg.1', 'chinese-text.svg,80,80,h+uw+vzPy++B8HOLoMbvQ5z+Bf3Ua/B65xR0Qf0V0VfQPeAEqA/69Sa8H8T1xAOv+QQGsM56Gg')]
    [TestCase('chinese-text.svg.2', 'chinese-text.svg,160,80,huuwuvzPy++B8HKLoMbvRJz+Bf3UZ/B65xRwQT0W0VfAPeAGoP/59Si8L0T1wgefuQQFtM56Gg')]
    [TestCase('delphi.svg.1',       'delphi.svg,80,80,Hligfvo8rHCB9RA8gHvxsLuAAuQ3FGGD3vLXPwYKDjzPPfihJ2GD8pQf0HsOoFuP/he2EF5A6Q')]
    [TestCase('delphi.svg.2',       'delphi.svg,160,80,Hlygfvo8rHCB9RE8gHvxsL+QAuQ3FGHD3vLbTwYKDjzP/fehJ2GD8pMf0HsOoVefPhi2DF5A6Q')]
    [TestCase('gorilla.svg.1',      'gorilla.svg,80,80,y+PxeQv+c1/66QcUMEPw6S8Pev+Hb1GC0sajID0b63ef9uQgNEB7ARioLwEGEbC9OAtfqE29+g')]
    [TestCase('gorilla.svg.2',      'gorilla.svg,160,80,zOPxOQv+b1/66QcUMIPw6i8Puv+Hc1GC0sWfED0b7HeftuQgMFA7ARioLwEGEbCtOAtfqD39+g')]
    [TestCase('lion.svg.1',         'lion.svg,80,80,2bdA/wPGn4/A+8qnL8bzbLweffSal8A+5NovYcEd9wPweu3vk689/CJMX8fq1OvOuQnvFz69Dw')]
    [TestCase('lion.svg.2',         'lion.svg,160,80,26tgvwTIr2+A+86nP8bzarwevfSal8A+5NkroYEd+QMA++3tl8/9/CFMT8fq1OfOuQnwEy69Dw')]
    [TestCase('tesla.svg.1',        'tesla.svg,40,40,xU+S+/iEo25A+rerDwP9aPievO+MO3FB4cgroLsmuU/PvOMdPI+7/D9kz8bq0yPPeAYEzA09EA')]
    [TestCase('tesla.svg.2',        'tesla.svg,80,40,xEuCu/iDo26A+rerDwP9aPievO+NO3GB4cgnoLsmuU/PfOMcNJ97/D9gz8bq1CPPuAYFyP18EA')]
    [TestCase('youtube.svg.1',      'youtube.svg,80,80,xSOAvfEXkE+EAn63bn7mF7g+/vbg06DCzq3vX4IOMIyeeOsu8JF466MbgEQGmHv+OhFxFE7/4A')]
    [TestCase('youtube.svg.2',      'youtube.svg,160,80,xSOAvfEXkD/EAn63bn7mF7g+/vbg06DCzq3vX4IOMIyeeOsu8JF466MbgEQGmHv+OhFxFE7/4A')]
    procedure TestStretchRender(const ASvgFileName: string; const AWidth, AHeight: Integer; const AExpectedImageHash: string);
    [TestCase('android.svg',      'android.svg,450,450,+U+A/fbWB2A+/NOzH8Px/PMOPeIP0C+B/aUjPz8J1HMvQP0MuE+8/iHkPkHv1Jte/wYXoD/76w')]
    [TestCase('chinese-text.svg', 'chinese-text.svg,450,450,sluSef2PV+4/5OqDX4L2q2yuPOeU+175+LvfgPsbpUthffkX7E75AZK4H8viCijvO/zM6169GQ')]
    [TestCase('delphi.svg',       'delphi.svg,1600,1600,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBYfwnq5Ref+QXZay6+Fg')]
    [TestCase('gorilla.svg',      'gorilla.svg,1500,1500,80eRwPVsA88/+tLrfkfsYsjO//PvBy+8AiOYgLsax5uhuvoaTL97/pFcvwnjCzRQvOuhF198Eg')]
    [TestCase('lion.svg',         'lion.svg,1200,1200,vUugPwyn945GA3fTz4H6orDOfPi9o149/upbQLsK4g+gfvFqKC/4+k9EX4vhAzzvPPGle188Ew')]
    [TestCase('tesla.svg',        'tesla.svg,450,450,ydtBu/fBfy4B+pKL3wTzdszuvfChS5B+5OFHgfwdtH/fe+cSmG85+E0wf8fs4Pf+eQToV/49FA')]
    [TestCase('youtube.svg',      'youtube.svg,450,450,43fQvOzsq28AD21PXz35HjQ/O/vqG8BD29NTX34F1htP//wJvLD29MCXX0H1xtM/PwIvrC198A')]
    procedure TestTileRender(const ASvgFileName: string; const AWidth, AHeight: Integer; const AExpectedImageHash: string);
    [TestCase('android.svg',      'android.svg,450,450,jwMhuwsOiH868RDg4EXzuaNPu/yyp7DC0uy7sD0h5TNQ/OvN0w6BAzOQX78EuGuu+QpkPPy5/w')]
    [TestCase('chinese-text.svg', 'chinese-text.svg,550,550,6LdSPPmIz52D87+nPwT2qvQeAOSKhxB89OoPUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('delphi.svg',       'delphi.svg,1000,1000,wrPfQPz632+A9hYwoUDrhI9wQebSX3DE0uqL33wLBAigeOvDbxD9Bppf3/gTnPuuOw5zuG3B3g')]
    [TestCase('gorilla.svg',      'gorilla.svg,1000,1000,uDtyuQ/zC4+56B+AwEP24nsfN/6lH7GC2cM3oP4PEbAveOQqoN+6BQ7Y7/8J7h89eAVcLN199Q')]
    [TestCase('lion.svg',         'lion.svg,1000,1000,ves/wgqd/85B+LyzEIbzatzufO+da/C+495PAX0bzWM/fOYLHP959mdcn0jr3/tu+QXlU779FQ')]
    [TestCase('tesla.svg',        'tesla.svg,60,60,C8yR/Qa8M+798dcz4IL0eZy+/eyVexB/49Nbob0mp3sffOYYQN85+GVUX8nq4ANfeQXVgw4+GA')]
    [TestCase('youtube.svg',      'youtube.svg,100,100,rdvPgPXq625B9eQP8EHj7aOvPuzVNxBA6AjggH4IN2BPd90Q5LD49axnf8EJxPdNeQs4nO4A5Q')]
    procedure TestDefaultRenderWithACustomColor(const ASvgFileName: string; const AWidth, AHeight: Integer; const AExpectedImageHash: string);
  end;

implementation

uses
  { Delphi }
  System.Types,
  System.IOUtils;

{ TSkSvgBrushTests }

procedure TSkSvgBrushTests.TestDefaultRenderWithACustomColor(
  const ASvgFileName: string; const AWidth, AHeight: Integer;
  const AExpectedImageHash: string);
begin
  TestRender(ASvgFileName, AWidth, AHeight, TSkSvgWrapMode.Default, TAlphaColors.Blueviolet, AExpectedImageHash);
end;

procedure TSkSvgBrushTests.TestFitRender(const ASvgFileName: string;
  const AWidth, AHeight: Integer; const AExpectedImageHash: string);
begin
  TestRender(ASvgFileName, AWidth, AHeight, TSkSvgWrapMode.Fit, TAlphaColors.Null, AExpectedImageHash);
end;

procedure TSkSvgBrushTests.TestRender(const ASvgFileName: string;
  const AWidth, AHeight: Integer; const AWrapMode: TSkSvgWrapMode;
  const AOverrideColor: TAlphaColor; const AExpectedImageHash: string);
var
  LSurface: ISkSurface;
  LBrush: TSkSvgBrush;
begin
  LSurface := TSkSurface.MakeRaster(AWidth, AHeight, TSkColorType.BGRA8888, TSkAlphaType.Premul, TSkColorSpace.MakeSRGB);
  LSurface.Canvas.Clear(TAlphaColors.Null);

  LBrush := TSkSvgBrush.Create;
  try
    LBrush.OverrideColor := AOverrideColor;
    LBrush.WrapMode := AWrapMode;
    LBrush.Source := TFile.ReadAllText(SvgAssetsPath + ASvgFileName);
    LBrush.Render(LSurface.Canvas, TRectF.Create(0, 0, AWidth, AHeight), 1);
  finally
    LBrush.Free;
  end;
  Assert.AreSimilar(AExpectedImageHash, LSurface.MakeImageSnapshot);
end;

procedure TSkSvgBrushTests.TestStretchRender(const ASvgFileName: string; const AWidth, AHeight: Integer;
  const AExpectedImageHash: string);
begin
  TestRender(ASvgFileName, AWidth, AHeight, TSkSvgWrapMode.Stretch, TAlphaColors.Null, AExpectedImageHash);
end;

procedure TSkSvgBrushTests.TestTileRender(const ASvgFileName: string;
  const AWidth, AHeight: Integer; const AExpectedImageHash: string);
begin
  TestRender(ASvgFileName, AWidth, AHeight, TSkSvgWrapMode.Tile, TAlphaColors.Null, AExpectedImageHash);
end;

initialization
  TDUnitX.RegisterTestFixture(TSkSvgBrushTests);
end.
