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
unit FMX.Skia.Designtime;

interface

{$SCOPEDENUMS ON}

procedure Register;

implementation

uses
  { Delphi }
  System.UITypes,
  System.SysUtils,
  System.Classes,
  FMX.Forms,
  DesignEditors,
  DesignIntf,
  StrEdit,

  { Skia }
  FMX.Skia,
  FMX.Skia.Designtime.Editor.AnimatedImage,
  FMX.Skia.Designtime.Editor.SVG;

type
  { TSkAnimatedImageSourcePropertyEditor }

  TSkAnimatedImageSourcePropertyEditor = class(TPropertyEditor)
  protected
    function GetIsDefault: Boolean; override;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    class function TryEdit(var AData: TBytes): Boolean; static;
  end;

  { TSkAnimatedImageComponentEditor }

  TSkAnimatedImageComponentEditor = class(TDefaultEditor)
  public
    procedure Edit; override;
  end;

  { TSkLabelTextPropertyEditor }

  TSkLabelTextPropertyEditor = class(TStringListProperty)
  private
    FStrings: TStrings;
  protected
    function GetStrings: TStrings; override;
    procedure SetStrings(const AValue: TStrings); override;
  public
    procedure Edit; override;
    function GetValue: string; override;
  end;

  { TSkSvgSourcePropertyEditor }

  TSkSvgSourcePropertyEditor = class(TPropertyEditor)
  protected
    function GetIsDefault: Boolean; override;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    class function TryEdit(var ASource: TSkSvgSource): Boolean; static;
  end;

  { TSkSvgComponentEditor }

  TSkSvgComponentEditor = class(TDefaultEditor)
  public
    procedure Edit; override;
  end;

  { TSkSkiaFMXSelectionEditor }

  TSkSkiaFMXSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(AProc: TGetStrProc); override;
  end;

{$R ..\..\..\Assets\Resources\Components.dcr}

{ TSkAnimatedImageSourcePropertyEditor }

procedure TSkAnimatedImageSourcePropertyEditor.Edit;
var
  LData: TBytes;
begin
  LData := TSkAnimatedImage(GetComponent(0)).Source.Data;
  if TryEdit(LData) then
  begin
    TSkAnimatedImage(GetComponent(0)).Source.Data := LData;
    Modified;
  end;
end;

function TSkAnimatedImageSourcePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TSkAnimatedImageSourcePropertyEditor.GetIsDefault: Boolean;
begin
  Result := TSkAnimatedImage(GetComponent(0)).Source.Data <> nil;
end;

function TSkAnimatedImageSourcePropertyEditor.GetValue: string;
begin
  Result := 'TSkAnimatedImage.TSource';
end;

class function TSkAnimatedImageSourcePropertyEditor.TryEdit(
  var AData: TBytes): Boolean;
var
  LAnimatedImageEditorForm: TSkAnimatedImageEditorForm;
begin
  LAnimatedImageEditorForm := TSkAnimatedImageEditorForm.Create(Application);
  try
    Result := LAnimatedImageEditorForm.ShowModal(AData) = mrOk;
  finally
    LAnimatedImageEditorForm.Free;
  end;
end;

{ TSkAnimatedImageComponentEditor }

procedure TSkAnimatedImageComponentEditor.Edit;
var
  LData: TBytes;
begin
  LData := TSkAnimatedImage(Component).Source.Data;
  if TSkAnimatedImageSourcePropertyEditor.TryEdit(LData) then
  begin
    TSkAnimatedImage(Component).Source.Data := LData;
    if Designer <> nil then
      Designer.Modified;
  end;
end;

{ TSkLabelTextPropertyEditor }

procedure TSkLabelTextPropertyEditor.Edit;
begin
  inherited;
  FreeAndNil(FStrings);
end;

function TSkLabelTextPropertyEditor.GetStrings: TStrings;
begin
  if FStrings = nil then
  begin
    FStrings := TStringList.Create;
    {$IF CompilerVersion >= 31}
    FStrings.Options := FStrings.Options - [TStringsOption.soTrailingLineBreak];
    {$ENDIF}
  end;
  FStrings.Text := GetStrValue;
  Result := FStrings;
end;

function TSkLabelTextPropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TSkLabelTextPropertyEditor.SetStrings(const AValue: TStrings);
begin
  if AValue.Text.EndsWith(AValue.LineBreak) then
    SetStrValue(AValue.Text.Substring(0, Length(AValue.Text) - Length(AValue.LineBreak)))
  else
    SetStrValue(AValue.Text);
end;

{ TSkSvgSourcePropertyEditor }

procedure TSkSvgSourcePropertyEditor.Edit;
var
  LSource: TSkSvgSource;
begin
  LSource := TSkSvgSource(GetStrValue);
  if TryEdit(LSource) then
    SetStrValue(LSource);
end;

function TSkSvgSourcePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TSkSvgSourcePropertyEditor.GetIsDefault: Boolean;
var
  LSource: TSkSvgSource;
begin
  LSource := TSkSvgSource(GetStrValue);
  Result := LSource <> '';
end;

function TSkSvgSourcePropertyEditor.GetValue: string;
begin
  Result := 'TSkSvgSource';
end;

class function TSkSvgSourcePropertyEditor.TryEdit(
  var ASource: TSkSvgSource): Boolean;
var
  LSvgEditorForm: TSkSvgEditorForm;
begin
  LSvgEditorForm := TSkSvgEditorForm.Create(Application);
  try
    Result := LSvgEditorForm.ShowModal(ASource) = mrOk;
  finally
    LSvgEditorForm.Free;
  end;
end;

{ TSkSvgComponentEditor }

procedure TSkSvgComponentEditor.Edit;
var
  LSource: TSkSvgSource;
begin
  LSource := TSkSvg(Component).Svg.Source;
  if TSkSvgSourcePropertyEditor.TryEdit(LSource) then
  begin
    TSkSvg(Component).Svg.Source := LSource;
    if Designer <> nil then
      Designer.Modified;
  end;
end;

{ TSkSkiaFMXSelectionEditor }

procedure TSkSkiaFMXSelectionEditor.RequiresUnits(AProc: TGetStrProc);
begin
  inherited;
  AProc('System.Skia');
end;

{ Register }

procedure Register;
begin
  RegisterComponents('Skia', [TSkAnimatedImage, TSkAnimatedPaintBox, TSkLabel, TSkPaintBox, TSkSvg]);
  RegisterPropertyEditor(TypeInfo(TSkAnimatedImage.TSource), TSkAnimatedImage, 'Source', TSkAnimatedImageSourcePropertyEditor);
  RegisterComponentEditor(TSkAnimatedImage, TSkAnimatedImageComponentEditor);
  RegisterPropertyEditor(TypeInfo(string), TSkLabel.TCustomWordsItem, 'Text', TSkLabelTextPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TSkSvgSource), nil, '', TSkSvgSourcePropertyEditor);
  RegisterComponentEditor(TSkSvg, TSkSvgComponentEditor);
  RegisterSelectionEditor(TSkCustomControl, TSkSkiaFMXSelectionEditor);
  RegisterSelectionEditor(TSkStyledControl, TSkSkiaFMXSelectionEditor);
end;

end.
