{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2011-2021 Google LLC.                                    }
{ Copyright (c) 2021 Skia4Delphi Project.                                }
{                                                                        }
{ Use of this source code is governed by a BSD-style license that can be }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit Skia.Vcl.Designtime;

interface

{$SCOPEDENUMS ON}

procedure Register;

implementation

uses
  { Delphi }
  System.UITypes,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  DesignEditors,
  DesignIntf,

  { Skia }
  Skia.Vcl,
  Skia.Vcl.Designtime.Editor.Lottie,
  Skia.Vcl.Designtime.Editor.SVG;

type
  { TSkLottieSourcePropertyEditor }

  TSkLottieSourcePropertyEditor = class(TPropertyEditor)
  protected
    function GetIsDefault: Boolean; override;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
    class function TryEdit(var ASource: TSkLottieSource): Boolean; static;
  end;

  { TSkLottieComponentEditor }

  TSkLottieComponentEditor = class(TDefaultEditor)
  public
    procedure Edit; override;
  end;

  { TSkSvgSourcePropertyEditor }

  TSkSvgSourcePropertyEditor = class(TPropertyEditor)
  protected
    function GetIsDefault: Boolean; override;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
    class function TryEdit(var ASource: TSkSvgSource): Boolean; static;
  end;

  { TSkSvgComponentEditor }

  TSkSvgComponentEditor = class(TDefaultEditor)
  public
    procedure Edit; override;
  end;

{$R ..\..\Assets\Resources\Components.dcr}

{ TSkLottieSourcePropertyEditor }

procedure TSkLottieSourcePropertyEditor.Edit;
var
  LSource: TSkLottieSource;
begin
  LSource := TSkLottieSource(GetStrValue);
  if TryEdit(LSource) then
    SetStrValue(LSource);
end;

function TSkLottieSourcePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TSkLottieSourcePropertyEditor.GetIsDefault: Boolean;
var
  LSource: TSkLottieSource;
begin
  LSource := TSkLottieSource(GetStrValue);
  Result := LSource <> '';
end;

function TSkLottieSourcePropertyEditor.GetValue: string;
begin
  Result := '(TSkLottieSource)';
end;

class function TSkLottieSourcePropertyEditor.TryEdit(
  var ASource: TSkLottieSource): Boolean;
var
  LLottieEditorForm: TSkLottieEditorForm;
begin
  LLottieEditorForm := TSkLottieEditorForm.Create(Application);
  try
    Result := LLottieEditorForm.ShowModal(ASource) = mrOk;
  finally
    LLottieEditorForm.Free;
  end;
end;

{ TSkLottieComponentEditor }

procedure TSkLottieComponentEditor.Edit;
var
  LSource: TSkLottieSource;
begin
  LSource := TSkLottieAnimation(Component).Source;
  if TSkLottieSourcePropertyEditor.TryEdit(LSource) then
    TSkLottieAnimation(Component).Source := LSource;
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
  Result := '(TSkSvgSource)';
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
    TSkSvg(Component).Svg.Source := LSource;
end;

{ Register }

procedure Register;
begin
  RegisterComponents('Skia', [TSkLottieAnimation, TSkPaintBox, TSkSvg]);
  RegisterPropertyEditor(TypeInfo(TSkLottieSource), nil, '', TSkLottieSourcePropertyEditor);
  RegisterComponentEditor(TSkLottieAnimation, TSkLottieComponentEditor);
  RegisterPropertyEditor(TypeInfo(TSkSvgSource), nil, '', TSkSvgSourcePropertyEditor);
  RegisterComponentEditor(TSkSvg, TSkSvgComponentEditor);
end;

end.
