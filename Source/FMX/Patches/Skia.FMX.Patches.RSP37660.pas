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
unit Skia.FMX.Patches.RSP37660;

interface

{$SCOPEDENUMS ON}
{$IFDEF IOS}

uses
  { Delphi }
  FMX.MediaLibrary.iOS;

// - ---------------------------------------------------------------------------
// - WORKAROUND
// - ---------------------------------------------------------------------------
// -
// - Description:
// -   The functions BitmapToUIImage and UIImageToBitmap of unit FMX.Helpers.iOS
// -   are ignoring the PixelFormat of bitmaps. This issue generates wrong
// -   colors in several parts of the code, and this workaround will fix some
// -   important services that use these functions: IFMXTakenImageService,
// -   IFMXCameraService, IFMXPhotoLibrary and IFMXShareSheetActionsService
// -
// - Bug report:
// -   https://quality.embarcadero.com/browse/RSP-37651
// -   https://quality.embarcadero.com/browse/RSP-37660
// -
// - ---------------------------------------------------------------------------
{$IF CompilerVersion > 35.0}
  {$MESSAGE WARN 'Check if the issue has been fixed'}
{$ENDIF}
procedure ApplyPatchesRSP37660;

implementation

uses
  { Delphi }
  System.SysUtils,
  System.Types,
  System.Classes,
  System.Threading,
  FMX.Types,
  FMX.MediaLibrary,
  FMX.Controls,
  FMX.Platform,
  FMX.Graphics;

type
  { TSkImageManagerCocoa }

  TSkImageManagerCocoa = class(TInterfacedPersistent, IFMXTakenImageService, IFMXCameraService, IFMXPhotoLibrary)
  strict private
    type
      TTakenImageServiceExecutor = class
      strict private
        FControl: TControl;
        FParams: TParamsPhotoQuery;
        procedure DidCancelTaking;
        procedure DidFinishTaking(AImage: TBitmap);
      public
        constructor Create(const AControl: TControl; const AParams: TParamsPhotoQuery);
        procedure TakeImageFromLibrary(const ADefaultService: IFMXTakenImageService);
        procedure TakePhoto(const ADefaultService: IFMXCameraService);
      end;
  strict private
    FDefaultTakenImageService: IFMXTakenImageService;
    FDefaultCameraService: IFMXCameraService;
    FDefaultPhotoLibrary: IFMXPhotoLibrary;
  public
    constructor Create;
    destructor Destroy; override;
    { IFMXTakenImageService }
    procedure TakeImageFromLibrary(const AControl: TControl; const ARequiredResolution: TSize; const AEditable: Boolean;
      const AOnDidFinishTaking: TOnDidFinishTaking; const AOnDidCancelTaking: TOnDidCancelTaking); overload;
    procedure TakeImageFromLibrary(const AControl: TControl; const AParams: TParamsPhotoQuery); overload;
    { IFMXPhotoLibrary }
    procedure AddImageToSavedPhotosAlbum(const ABitmap: TBitmap; const AWriteImageCompletionEvent: TWriteImageCompletionEvent = nil);
    { IFMXCameraService }
    procedure TakePhoto(const AControl: TControl; const ARequiredResolution: TSize; const AEditable: Boolean;
      const AOnDidFinishTaking: TOnDidFinishTaking; const AOnDidCancelTaking: TOnDidCancelTaking); overload;
    procedure TakePhoto(const AControl: TControl; const AParams: TParamsPhotoQuery); overload;
  end;

  { TSkShareService }

  TSkShareService = class(TInterfacedPersistent, IFMXShareSheetActionsService)
  strict private
    FDefaultShareSheetActionsService: IFMXShareSheetActionsService;
  public
    constructor Create;
    destructor Destroy; override;
    { IFMXShareSheetActionsService }
    procedure Share(const AControl: TControl; const AText: string; const ABitmap: TBitmap);
  end;

var
  FImageManagerCocoa: TSkImageManagerCocoa;
  FShareService: TSkShareService;

procedure SetRGBAPixelFormat(const ABitmap: TBitmap);
var
  LBitmapData: TBitmapData;
begin
  if Assigned(ABitmap) and (not ABitmap.IsEmpty) and ABitmap.Map(TMapAccess.ReadWrite, LBitmapData) then
  begin
    try
      ChangePixelFormat(LBitmapData.Data, LBitmapData.Data, (LBitmapData.Pitch * LBitmapData.Height) div LBitmapData.BytesPerPixel,
        LBitmapData.PixelFormat, TPixelFormat.RGBA);
    finally
      ABitmap.Unmap(LBitmapData);
    end;
  end;
end;

{ TSkImageManagerCocoa.TTakenImageServiceExecutor }

constructor TSkImageManagerCocoa.TTakenImageServiceExecutor.Create(
  const AControl: TControl; const AParams: TParamsPhotoQuery);
begin
  inherited Create;
  FControl := AControl;
  FParams := AParams;
end;

procedure TSkImageManagerCocoa.TTakenImageServiceExecutor.DidCancelTaking;
var
  LSelf: TObject;
begin
  if Assigned(FParams.OnDidCancelTaking) then
    FParams.OnDidCancelTaking;
  LSelf := Self;
  TTask.Run(
    procedure
    begin
      LSelf.Free;
    end);
end;

procedure TSkImageManagerCocoa.TTakenImageServiceExecutor.DidFinishTaking(
  AImage: TBitmap);
var
  LSelf: TObject;
begin
  if Assigned(FParams.OnDidFinishTaking) then
  begin
    SetRGBAPixelFormat(AImage);
    FParams.OnDidFinishTaking(AImage);
  end;
  LSelf := Self;
  TTask.Run(
    procedure
    begin
      LSelf.Free;
    end);
end;

procedure TSkImageManagerCocoa.TTakenImageServiceExecutor.TakeImageFromLibrary(
  const ADefaultService: IFMXTakenImageService);
var
  LParams: TParamsPhotoQuery;
begin
  LParams := FParams;
  LParams.OnDidCancelTaking := DidCancelTaking;
  LParams.OnDidFinishTaking := DidFinishTaking;
  ADefaultService.TakeImageFromLibrary(FControl, LParams);
end;

procedure TSkImageManagerCocoa.TTakenImageServiceExecutor.TakePhoto(
  const ADefaultService: IFMXCameraService);
var
  LParams: TParamsPhotoQuery;
begin
  LParams := FParams;
  LParams.OnDidCancelTaking := DidCancelTaking;
  LParams.OnDidFinishTaking := DidFinishTaking;
  ADefaultService.TakePhoto(FControl, LParams);
end;

{ TSkImageManagerCocoa }

procedure TSkImageManagerCocoa.AddImageToSavedPhotosAlbum(
  const ABitmap: TBitmap;
  const AWriteImageCompletionEvent: TWriteImageCompletionEvent);
var
  LBitmap: TBitmap;
begin
  if Assigned(FDefaultPhotoLibrary) then
  begin
    LBitmap := TBitmap.Create;
    try
      LBitmap.Assign(ABitmap);
      SetRGBAPixelFormat(LBitmap);
      FDefaultPhotoLibrary.AddImageToSavedPhotosAlbum(LBitmap, AWriteImageCompletionEvent);
    finally
      LBitmap.Free;
    end;
  end
  else
    raise Exception.Create('Unsupported service');
end;

constructor TSkImageManagerCocoa.Create;
begin
  inherited Create;
  { IFMXTakenImageService }
  if TPlatformServices.Current.SupportsPlatformService(IFMXTakenImageService, FDefaultTakenImageService) then
    TPlatformServices.Current.RemovePlatformService(IFMXTakenImageService);
  TPlatformServices.Current.AddPlatformService(IFMXTakenImageService, IInterface(Self));

  { IFMXPhotoLibrary }
  if TPlatformServices.Current.SupportsPlatformService(IFMXPhotoLibrary, FDefaultPhotoLibrary) then
    TPlatformServices.Current.RemovePlatformService(IFMXPhotoLibrary);
  TPlatformServices.Current.AddPlatformService(IFMXPhotoLibrary, IInterface(Self));

  { IFMXCameraService }
  if TPlatformServices.Current.SupportsPlatformService(IFMXCameraService, FDefaultCameraService) then
    TPlatformServices.Current.RemovePlatformService(IFMXCameraService);
  TPlatformServices.Current.AddPlatformService(IFMXCameraService, IInterface(Self));
end;

destructor TSkImageManagerCocoa.Destroy;
begin
  { IFMXTakenImageService }
  TPlatformServices.Current.RemovePlatformService(IFMXTakenImageService);
  if Assigned(FDefaultTakenImageService) then
    TPlatformServices.Current.AddPlatformService(IFMXTakenImageService, FDefaultTakenImageService);

  { IFMXPhotoLibrary }
  TPlatformServices.Current.RemovePlatformService(IFMXPhotoLibrary);
  if Assigned(FDefaultPhotoLibrary) then
    TPlatformServices.Current.AddPlatformService(IFMXPhotoLibrary, FDefaultPhotoLibrary);

  { IFMXCameraService }
  TPlatformServices.Current.RemovePlatformService(IFMXCameraService);
  if Assigned(FDefaultCameraService) then
    TPlatformServices.Current.AddPlatformService(IFMXCameraService, FDefaultCameraService);
  inherited;
end;

procedure TSkImageManagerCocoa.TakeImageFromLibrary(const AControl: TControl;
  const ARequiredResolution: TSize; const AEditable: Boolean;
  const AOnDidFinishTaking: TOnDidFinishTaking;
  const AOnDidCancelTaking: TOnDidCancelTaking);
var
  LParamTmp: TParamsPhotoQuery;
begin
  LParamTmp.Editable := AEditable;
  LParamTmp.RequiredResolution := ARequiredResolution;
  LParamTmp.NeedSaveToAlbum := False;
  LParamTmp.OnDidFinishTaking := AOnDidFinishTaking;
  LParamTmp.OnDidCancelTaking := AOnDidCancelTaking;
  TakeImageFromLibrary(AControl, LParamTmp);
end;

procedure TSkImageManagerCocoa.TakeImageFromLibrary(const AControl: TControl;
  const AParams: TParamsPhotoQuery);
var
  LExecutor: TTakenImageServiceExecutor;
begin
  if Assigned(FDefaultTakenImageService) then
  begin
    LExecutor := TTakenImageServiceExecutor.Create(AControl, AParams);
    LExecutor.TakeImageFromLibrary(FDefaultTakenImageService);
  end
  else
    raise Exception.Create('Unsupported service');
end;

procedure TSkImageManagerCocoa.TakePhoto(const AControl: TControl;
  const ARequiredResolution: TSize; const AEditable: Boolean;
  const AOnDidFinishTaking: TOnDidFinishTaking;
  const AOnDidCancelTaking: TOnDidCancelTaking);
var
  LParamTmp: TParamsPhotoQuery;
begin
  LParamTmp.Editable := AEditable;
  LParamTmp.RequiredResolution := ARequiredResolution;
  LParamTmp.NeedSaveToAlbum := False;
  LParamTmp.OnDidFinishTaking := AOnDidFinishTaking;
  LParamTmp.OnDidCancelTaking := AOnDidCancelTaking;
  TakePhoto(AControl, LParamTmp);
end;

procedure TSkImageManagerCocoa.TakePhoto(const AControl: TControl;
  const AParams: TParamsPhotoQuery);
var
  LExecutor: TTakenImageServiceExecutor;
begin
  if Assigned(FDefaultCameraService) then
  begin
    LExecutor := TTakenImageServiceExecutor.Create(AControl, AParams);
    LExecutor.TakePhoto(FDefaultCameraService);
  end
  else
    raise Exception.Create('Unsupported service');
end;

{ TSkShareService }

constructor TSkShareService.Create;
begin
  inherited Create;
  { IFMXShareSheetActionsService }
  if TPlatformServices.Current.SupportsPlatformService(IFMXShareSheetActionsService, FDefaultShareSheetActionsService) then
    TPlatformServices.Current.RemovePlatformService(IFMXShareSheetActionsService);
  TPlatformServices.Current.AddPlatformService(IFMXShareSheetActionsService, IInterface(Self));
end;

destructor TSkShareService.Destroy;
begin
  { IFMXShareSheetActionsService }
  TPlatformServices.Current.RemovePlatformService(IFMXShareSheetActionsService);
  if Assigned(FDefaultShareSheetActionsService) then
    TPlatformServices.Current.AddPlatformService(IFMXShareSheetActionsService, FDefaultShareSheetActionsService);
  inherited;
end;

procedure TSkShareService.Share(const AControl: TControl; const AText: string;
  const ABitmap: TBitmap);
var
  LBitmap: TBitmap;
begin
  if Assigned(FDefaultShareSheetActionsService) then
  begin
    LBitmap := TBitmap.Create;
    try
      LBitmap.Assign(ABitmap);
      SetRGBAPixelFormat(LBitmap);
      FDefaultShareSheetActionsService.Share(AControl, AText, LBitmap);
    finally
      LBitmap.Free;
    end;
  end
  else
    raise Exception.Create('Unsupported service');
end;

procedure ApplyPatchesRSP37660;
begin
  FImageManagerCocoa := TSkImageManagerCocoa.Create;
  FShareService := TSkShareService.Create;
end;

initialization
finalization
  FImageManagerCocoa.Free;
  FShareService.Free;
{$ELSE}
implementation
{$ENDIF}
end.
