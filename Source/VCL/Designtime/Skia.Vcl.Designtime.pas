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

uses
  { Delphi }
  DesignEditors,
  DesignIntf;

type
  { TSkLottieSourcePropertyEditor }

  TSkLottieSourcePropertyEditor = class(TPropertyEditor)
  protected
    function GetIsDefault: Boolean; override;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
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
  end;

procedure Register;

implementation

uses
  { Delphi }
  System.UITypes,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,

  { Skia }
  Skia.Vcl,
  Skia.Vcl.Designtime.Editor.Lottie,
  Skia.Vcl.Designtime.Editor.SVG;

{$R ..\..\Assets\Resources\Components.dcr}

{ TSkLottieSourcePropertyEditor }

procedure TSkLottieSourcePropertyEditor.Edit;
var
  LSource: TSkLottieSource;
  LLottieEditorForm: TSkLottieEditorForm;
begin
  LSource := TSkLottieSource(GetStrValue);
  LLottieEditorForm := TSkLottieEditorForm.Create(Application);
  try
    if LLottieEditorForm.ShowModal(LSource) = mrOk then
      SetStrValue(LSource);
  finally
    LLottieEditorForm.Free;
  end;
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

{ TSkSvgSourcePropertyEditor }

procedure TSkSvgSourcePropertyEditor.Edit;
var
  LSource: TSkSvgSource;
  LSvgEditorForm: TSkSvgEditorForm;
begin
  LSource := TSkSvgSource(GetStrValue);
  LSvgEditorForm := TSkSvgEditorForm.Create(Application);
  try
    if LSvgEditorForm.ShowModal(LSource) = mrOk then
      SetStrValue(LSource);
  finally
    LSvgEditorForm.Free;
  end;
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

{ Register }

procedure Register;
begin
  RegisterComponents('Skia', [TSkLottieAnimation, TSkPaintBox, TSkSvg]);
  RegisterPropertyEditor(TypeInfo(TSkLottieSource), nil, '', TSkLottieSourcePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TSkSvgSource), nil, '', TSkSvgSourcePropertyEditor);
end;

end.
