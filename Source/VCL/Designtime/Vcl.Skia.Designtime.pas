{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2021-2025 Skia4Delphi Project.                           }
{                                                                        }
{ Use of this source code is governed by the MIT license that can be     }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit Vcl.Skia.Designtime;

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
  StrEdit,

  { Skia }
  Vcl.Skia,
  Vcl.Skia.AnimatedImageList,
  Vcl.Skia.AnimatedBitmap,
  Vcl.Skia.Designtime.Editor.AnimatedImage,
  Vcl.Skia.Designtime.Editor.SVG,
  Vcl.Skia.Designtime.Editor.Image;

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


  { TSkAnimatedBitmapSourcePropertyEditor }

  TSkAnimatedBitmapSourcePropertyEditor = class(TPropertyEditor)
  protected
    function GetIsDefault: Boolean; override;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    class function TryEdit(var AData: TBytes): Boolean; static;
  end;

  { TSkAnimatedItemDataPropertyEditor }

  TSkAnimatedItemDataPropertyEditor = class(TPropertyEditor)
  protected
    function GetIsDefault: Boolean; override;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    class function TryEdit(var AData: TBytes; aType: TSkSourceType): Boolean; static;
  end;


  { TSkAnimatedImageComponentEditor }

  TSkAnimatedBitmapComponentEditor = class(TDefaultEditor)
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
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const AValue: string); override;
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

  { TSkSkiaVclSelectionEditor }

  TSkSkiaVclSelectionEditor = class(TSelectionEditor)
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

{ TSkAnimatedItemDataPropertyEditor }

procedure TSkAnimatedItemDataPropertyEditor.Edit;
var
  LData: TBytes;
  LItem: TSkAnimatedItem;
begin
  LItem := TSkAnimatedItem(GetComponent(0));
  LData := Litem.Source.Data;
  if TryEdit(LData,LItem.SourceType) then
  begin
    Litem.Source.Data := LData;
    Modified;
  end;
end;

function TSkAnimatedItemDataPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TSkAnimatedItemDataPropertyEditor.GetIsDefault: Boolean;
begin
  Result := TSkAnimatedItem(GetComponent(0)).Source.Data <> nil;
end;

function TSkAnimatedItemDataPropertyEditor.GetValue: string;
begin
  Result := 'TSkAnimatedItem.TSource';
end;

class function TSkAnimatedItemDataPropertyEditor.TryEdit(
  var AData: TBytes; aType: TSkSourceType): Boolean;
begin
  case aType of
    anim: begin
        var LAnimatedImageEditorForm := TSkAnimatedImageEditorForm.Create(Application);
        try
          Result := LAnimatedImageEditorForm.ShowModal(AData) = mrOk;
        finally
          LAnimatedImageEditorForm.Free;
        end;
    end;
    svg: begin
        var LSvgEditForm := TSkSvgEditorForm.Create(Application);
        var LSource: TSkSvgSource := TEncoding.UTF8.GetString(AData);
        try
          result := LSvgEditForm.ShowModal(LSource) = mrOk;
          if result then
            aData := Tencoding.UTF8.GetBytes(LSource);
        finally
          LSvgEditForm.Free;
        end;
    end;
    image: begin
      var LImageEditForm := TSkImageEditorForm.Create(Application);
      try
        Result := LImageEditForm.ShowModal(AData) = mrOk;
      finally
        LImageEditForm.Free;
      end;
    end;
    else result := False;
  end;
end;

{ TSkAnimatedBitmapSourcePropertyEditor }

procedure TSkAnimatedBitmapSourcePropertyEditor.Edit;
var
  LData: TBytes;
begin
  LData := TSkAnimatedBitmap(GetComponent(0)).Source.Data;
  if TryEdit(LData) then
  begin
    TSkAnimatedBitmap(GetComponent(0)).Source.Data := LData;
    Modified;
  end;
end;

function TSkAnimatedBitmapSourcePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TSkAnimatedBitmapSourcePropertyEditor.GetIsDefault: Boolean;
begin
  Result := TSkAnimatedBitmap(GetComponent(0)).Source.Data <> nil;
end;

function TSkAnimatedBitmapSourcePropertyEditor.GetValue: string;
begin
  Result := 'TSkAnimatedBitmap.TSource';
end;

class function TSkAnimatedBitmapSourcePropertyEditor.TryEdit(
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

{ TSkAnimatedBitmapComponentEditor }

procedure TSkAnimatedBitmapComponentEditor.Edit;
var
  LData: TBytes;
begin
  LData := TSkAnimatedBitmap(Component).Source.Data;
  if TSkAnimatedBitmapSourcePropertyEditor.TryEdit(LData) then
  begin
    TSkAnimatedBitmap(Component).Source.Data := LData;
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

function TSkLabelTextPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paMultiSelect, paAutoUpdate];
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

procedure TSkLabelTextPropertyEditor.SetValue(const AValue: string);
begin
  SetStrValue(AValue);
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

{ TSkSkiaVclSelectionEditor }

procedure TSkSkiaVclSelectionEditor.RequiresUnits(AProc: TGetStrProc);
begin
  inherited;
  AProc('System.Skia');
end;

{ Register }

procedure Register;
begin
  RegisterComponents('Skia', [TSkAnimatedImage, TSkAnimatedPaintBox, TSkLabel, TSkPaintBox, TSkSvg, TSkAnimatedImageList, TSkAnimatedBitmap]);
  RegisterPropertyEditor(TypeInfo(TSkAnimatedImage.TSource), TSkAnimatedImage, 'Source', TSkAnimatedImageSourcePropertyEditor);
  RegisterComponentEditor(TSkAnimatedImage, TSkAnimatedImageComponentEditor);
  RegisterPropertyEditor(TypeInfo(string), TSkLabel, 'Caption', TSkLabelTextPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TSkLabel.TCustomWordsItem, 'Caption', TSkLabelTextPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TSkSvgSource), nil, '', TSkSvgSourcePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TSkAnimatedBitmap.TSource), TSkAnimatedBitmap, 'Source', TSkAnimatedBitmapSourcePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TSkAnimatedItem.TSource), TSkAnimatedItem, 'Source', TSkAnimatedItemDataPropertyEditor);
  RegisterComponentEditor(TSkAnimatedBitmap, TSkAnimatedBitmapComponentEditor);
  RegisterComponentEditor(TSkSvg, TSkSvgComponentEditor);
  RegisterSelectionEditor(TSkCustomControl, TSkSkiaVclSelectionEditor);
  RegisterSelectionEditor(TSkCustomWinControl, TSkSkiaVclSelectionEditor);
end;

end.
