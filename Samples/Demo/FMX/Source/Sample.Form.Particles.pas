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
unit Sample.Form.Particles;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Diagnostics, FMX.Types, FMX.Controls, FMX.Forms, FMX.StdCtrls,
  FMX.Layouts, FMX.Objects, System.IOUtils,

  { Skia }
  Skia, Skia.FMX,

  { Sample }
  Sample.Form.Base;

type
  TfrmParticles = class(TfrmBase)
    btnParticlesAssets: TSpeedButton;
    lblParticlesAssetsTitle: TSkLabel;
    lblParticlesAssetsDescription: TSkLabel;
    btnParticlesWithMouse: TSpeedButton;
    lblParticlesWithMouseTitle: TSkLabel;
    lblParticlesWithMouseDescription: TSkLabel;
    lytContentTopOffset: TLayout;
    procedure btnParticlesAssetsClick(Sender: TObject);
    procedure btnParticlesWithMouseClick(Sender: TObject);
  public
    { Public declarations }
  end;

implementation

uses
  { Sample }
  Sample.Form.Base.Viewer,
  Sample.Form.Viewer.AnimatedPaintBox;

{$R *.fmx}

type
  IParticlesAssets = interface
    function GetAssetFiles: TArray<string>;
    function GetSize: TSize;
    procedure Render(const ACanvas: ISkCanvas; const ADest: TRectF);
    procedure SetAssetFileIndex(const AValue: Integer);
    procedure SetMousePosition(const APoint: TPointF);
    property AssetFileIndex: Integer write SetAssetFileIndex;
    property AssetFiles: TArray<string> read GetAssetFiles;
    property MousePosition: TPointF write SetMousePosition;
    property Size: TSize read GetSize;
  end;

  TParticlesAssets = class(TInterfacedObject, IParticlesAssets)
  strict private
    FParticles: ISkParticleEffect;
    FParticlesSamplesPath: string;
    FResourceProvider: ISkResourceProvider;
    FSampleIndex: Integer;
    FSampleTimer: TStopWatch;
    function GetAssetFiles: TArray<string>;
    function GetSize: TSize;
    procedure SetAssetFileIndex(const AValue: Integer);
    procedure SetMousePosition(const APoint: TPointF);
  public
    constructor Create(const AAssetsPath, AParticlesSamplesPath: string);
    procedure Render(const ACanvas: ISkCanvas; const ADest: TRectF);
    property AssetFileIndex: Integer read FSampleIndex write SetAssetFileIndex;
    property AssetFiles: TArray<string> read GetAssetFiles;
    property MousePosition: TPointF write SetMousePosition;
    property Size: TSize read GetSize;
  end;

{ TfrmParticles }

procedure TfrmParticles.btnParticlesAssetsClick(Sender: TObject);
var
  LParticlesAssets: IParticlesAssets;
  LOptions: IViewerOptions;
begin
  LParticlesAssets := TParticlesAssets.Create(AssetsPath, AssetsPath + 'Particles');
  LOptions := TViewerOptions.Create;
  LOptions.AddStrings('Assets file', LParticlesAssets.AssetFiles, 0);
  LParticlesAssets.AssetFileIndex := LOptions['Assets file'];

  ChildForm<TfrmAnimatedPaintBoxViewer>.OnMouseMove :=
    procedure (const AX, AY: Single)
    begin
      LParticlesAssets.MousePosition := PointF(AX, AY);
    end;

  ChildForm<TfrmAnimatedPaintBoxViewer>.OnOptionsChange :=
    procedure
    begin
      LParticlesAssets.AssetFileIndex := LOptions['Assets file'];
      ChildForm<TfrmAnimatedPaintBoxViewer>.DrawSize := LParticlesAssets.Size;
    end;

  ChildForm<TfrmAnimatedPaintBoxViewer>.Show('Particles Assets', '',
    LParticlesAssets.Size.Width, LParticlesAssets.Size.Height,
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF; const ASeconds: Double)
    begin
      LParticlesAssets.Render(ACanvas, ADest);
    end, LOptions);
end;

procedure TfrmParticles.btnParticlesWithMouseClick(Sender: TObject);
const
  DrawPosition: TPointF = (X: 25; Y: 150);
  Size: TSize = (cx: 800; cy: 600);
var
  LParticles: ISkParticleEffect;
begin
  LParticles := TSkParticleEffect.Make(TFile.ReadAllText(AssetsPath + TPath.Combine('Particles', 'writing.json')));
  LParticles.Position := PointF(0, 0);
  LParticles.Start(0, True);

  ChildForm<TfrmAnimatedPaintBoxViewer>.OnMouseMove :=
    procedure (const AX, AY: Single)
    begin
      LParticles.SetUniform('mouse_pos', [AX - DrawPosition.X, AY - DrawPosition.Y]);
    end;

  ChildForm<TfrmAnimatedPaintBoxViewer>.Show('Particles with Mouse', 'Move the mouse (or tap moving the screen)', Size.Width, Size.Height,
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF; const ASeconds: Double)
    begin
      ACanvas.Save;
      try
        ACanvas.Translate(DrawPosition.X, DrawPosition.Y);
        LParticles.Update(ASeconds);
        LParticles.Render(ACanvas);
      finally
        ACanvas.Restore;
      end;
    end);
end;

{ TParticlesAssets }

type
  TResourceProvider = class abstract(TSkResourceProviderBaseClass)
  strict private
    FAssetsPath: string;
  strict protected
    function Load(const APath, AName: string): TBytes; override;
  public
    constructor Create(const AAssetsPath: string);
  end;

  TParticleUniform = record
    Name: string;
    Data: array[0..2] of Single;
    DataCount: Integer;
  end;
  TParticleUniforms = array of TParticleUniform;

  TParticlesSample = record
    FileName: string;
    Width: Single;
    Height: Single;
    X: Single;
    Y: Single;
    UseMousePos: Boolean;
    Uniforms: ^TParticleUniform;
    UniformsCount: Integer;
  end;

const
  UniformsOfUniforms: array[0..2] of TParticleUniform = (
    (Name: 'rate';  Data: (2, 0, 0);          DataCount: 1),
    (Name: 'spin';  Data: (4, 0, 0);          DataCount: 1),
    (Name: 'color'; Data: (0.25, 0.75, 0.75); DataCount: 3));

  ParticlesSamples: array[0..16] of TParticlesSample = (
    (FileName: 'confetti.json';           Width: 400;  Height: 400; X: 200; Y: 200; UseMousePos: False; Uniforms: nil; UniformsCount: 0),
    (FileName: 'cube.json';               Width: 400;  Height: 400; X: 200; Y: 200; UseMousePos: False; Uniforms: nil; UniformsCount: 0),
    (FileName: 'curves.json';             Width: 150;  Height: 200; X: 75;  Y: 200; UseMousePos: False; Uniforms: nil; UniformsCount: 0),
    (FileName: 'fireworks.json';          Width: 400;  Height: 400; X: 200; Y: 350; UseMousePos: False; Uniforms: nil; UniformsCount: 0),
    (FileName: 'mandrill.json';           Width: 250;  Height: 250; X: 25;  Y: 25;  UseMousePos: False; Uniforms: nil; UniformsCount: 0),
    (FileName: 'mouse_track.json';        Width: 400;  Height: 400; X: 200; Y: 200; UseMousePos: True;  Uniforms: nil; UniformsCount: 0),
    (FileName: 'mouse_trail.json';        Width: 400;  Height: 400; X: 200; Y: 200; UseMousePos: False; Uniforms: nil; UniformsCount: 0),
    (FileName: 'orientation.json';        Width: 1200; Height: 800; X: 100; Y: 500; UseMousePos: False; Uniforms: nil; UniformsCount: 0),
    (FileName: 'path_spawn.json';         Width: 300;  Height: 300; X: 0;   Y: 0;   UseMousePos: False; Uniforms: nil; UniformsCount: 0),
    (FileName: 'sinusoidal_emitter.json'; Width: 400;  Height: 400; X: 150; Y: 200; UseMousePos: False; Uniforms: nil; UniformsCount: 0),
    (FileName: 'spiral.json';             Width: 400;  Height: 400; X: 200; Y: 200; UseMousePos: False; Uniforms: nil; UniformsCount: 0),
    (FileName: 'sprite_frame.json';       Width: 200;  Height: 200; X: 100; Y: 100; UseMousePos: False; Uniforms: nil; UniformsCount: 0),
    (FileName: 'text.json';               Width: 250;  Height: 160; X: 10;  Y: 150; UseMousePos: False; Uniforms: nil; UniformsCount: 0),
    (FileName: 'uniforms.json';           Width: 250;  Height: 250; X: 125; Y: 125; UseMousePos: False; Uniforms: @UniformsOfUniforms[0]; UniformsCount: Length(UniformsOfUniforms)),
    (FileName: 'variable_rate.json';      Width: 400;  Height: 400; X: 200; Y: 200; UseMousePos: False; Uniforms: nil; UniformsCount: 0),
    (FileName: 'warp.json';               Width: 400;  Height: 400; X: 200; Y: 200; UseMousePos: False; Uniforms: nil; UniformsCount: 0),
    (FileName: 'writing.json';            Width: 800;  Height: 600; X: 25;  Y: 150; UseMousePos: True;  Uniforms: nil; UniformsCount: 0));

constructor TParticlesAssets.Create(const AAssetsPath,
  AParticlesSamplesPath: string);
begin
  inherited Create;
  FParticlesSamplesPath := AParticlesSamplesPath;
  FResourceProvider := TResourceProvider.Create(AAssetsPath);
  FSampleIndex := -1;
  AssetFileIndex := FSampleIndex;
end;

function TParticlesAssets.GetAssetFiles: TArray<string>;
var
  I: Integer;
begin
  SetLength(Result, Length(ParticlesSamples));
  for I := 0 to Length(ParticlesSamples) - 1 do
    Result[I] := ParticlesSamples[I].FileName;
end;

function TParticlesAssets.GetSize: TSize;
begin
  Result := TSize.Create(Round(ParticlesSamples[FSampleIndex].Width), Round(ParticlesSamples[FSampleIndex].Height));
end;

procedure TParticlesAssets.Render(const ACanvas: ISkCanvas;
  const ADest: TRectF);
begin
  if Assigned(FParticles) then
  begin
    ACanvas.Save;
    try
      ACanvas.Translate(ParticlesSamples[FSampleIndex].X, ParticlesSamples[FSampleIndex].Y);
      FParticles.Update(FSampleTimer.Elapsed.TotalSeconds);
      FParticles.Render(ACanvas);
    finally
      ACanvas.Restore;
    end;
  end;
end;

procedure TParticlesAssets.SetAssetFileIndex(const AValue: Integer);
var
  LSample: TParticlesSample;
  LData: TArray<Single>;
  I: Integer;
begin
  if FSampleIndex <> AValue then
  begin
    FSampleIndex := AValue;
    LSample := ParticlesSamples[FSampleIndex];
    FParticles := TSkParticleEffect.Make(TFile.ReadAllText(TPath.Combine(FParticlesSamplesPath, LSample.FileName)), FResourceProvider);
    for I := 0 to LSample.UniformsCount - 1 do
    begin
      SetLength(LData, TParticleUniforms(LSample.Uniforms)[I].DataCount);
      Move(TParticleUniforms(LSample.Uniforms)[I].Data, LData[0], Length(LData) * SizeOf(Single));
      FParticles.SetUniform(TParticleUniforms(LSample.Uniforms)[I].Name, LData);
    end;
    FParticles.Position := PointF(0, 0);
    FParticles.Start(0, True);
    FSampleTimer := TStopWatch.StartNew;
  end;
end;

procedure TParticlesAssets.SetMousePosition(const APoint: TPointF);
begin
  if Assigned(FParticles) and ParticlesSamples[FSampleIndex].UseMousePos then
    FParticles.SetUniform('mouse_pos', [APoint.X - ParticlesSamples[FSampleIndex].X, APoint.Y - ParticlesSamples[FSampleIndex].Y]);
end;

{ TResourceProvider }

constructor TResourceProvider.Create(const AAssetsPath: string);
begin
  inherited Create(False);
  FAssetsPath := AAssetsPath;
end;

function TResourceProvider.Load(const APath, AName: string): TBytes;

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

begin
  if AName = 'star' then
    Result := StarPath.Serialize
  else
    Result := TFile.ReadAllBytes(FAssetsPath + TPath.Combine(APath, AName));
end;

end.
