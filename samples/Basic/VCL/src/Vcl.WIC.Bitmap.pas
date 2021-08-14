unit Vcl.WIC.Bitmap;

interface

uses
  { Delphi }
  System.Classes,
  System.SysUtils,
  System.Types,
  Vcl.Graphics,
  Winapi.Wincodec,
  Winapi.Windows;

type
  EWICBitmap = class(Exception);

  { TWICBitmap }

  TWICBitmap = class (TGraphic)
  private
    FImage: TWICImage;
    FWidth: Integer;
    FHeight: Integer;
    FUpdateCount: Integer;
    FLock: IWICBitmapLock;
    FLockCount: Integer;
  protected
    procedure AssignTo(ADest: TPersistent); override;
    procedure BeginUpdate;
    procedure Draw(ACanvas: TCanvas; const ARect: TRect); override;
    procedure EndUpdate;
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure SetHeight(AValue: Integer); override;
    procedure SetWidth(AValue: Integer); override;
  public
    constructor Create(AWidth, AHeight: Integer); overload;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPALETTE); override;
    procedure LoadFromStream(AStream: TStream); override;
    function LockPixels(out AData: Pointer; out AStride: Integer): Boolean;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle; var APalette: HPALETTE); override;
    procedure SaveToStream(AStream: TStream); override;
    procedure SetSize(AWidth, AHeight: Integer); override;
    procedure UnlockPixels;
    property Image: TWICImage read FImage;
  end;

implementation

uses
  { Delphi }
  Winapi.ActiveX;

{ TWICBitmap }

procedure TWICBitmap.Assign(ASource: TPersistent);
begin
  BeginUpdate;
  try
    if not Assigned(FImage) then
      FImage := TWICImage.Create;
    FImage.Assign(ASource);
    FWidth  := FImage.Width;
    FHeight := FImage.Height;
  finally
    EndUpdate;
  end;
end;

procedure TWICBitmap.AssignTo(ADest: TPersistent);
begin
  if Assigned(FImage) then
    ADest.Assign(FImage)
  else
    inherited;
end;

procedure TWICBitmap.BeginUpdate;
begin
  TMonitor.Enter(Self);
  try
    if FUpdateCount = 0 then
      TMonitor.Enter(Self);
    Inc(FUpdateCount);
  finally
    TMonitor.Exit(Self);
  end;
end;

constructor TWICBitmap.Create(AWidth, AHeight: Integer);
begin
  Create;
  SetSize(AWidth, AHeight);
end;

destructor TWICBitmap.Destroy;
begin
  FImage.Free;
  inherited;
end;

procedure TWICBitmap.Draw(ACanvas: TCanvas; const ARect: TRect);
begin
  if Assigned(FImage) then
    ACanvas.Draw(ARect.Left, ARect.Top, FImage);
end;

procedure TWICBitmap.EndUpdate;
begin
  TMonitor.Enter(Self);
  try
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
    begin
      TMonitor.Exit(Self);
      Changed(Self);
    end;
  finally
    TMonitor.Exit(Self);
  end;
end;

function TWICBitmap.GetEmpty: Boolean;
begin
  Result := not Assigned(FImage);
end;

function TWICBitmap.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TWICBitmap.GetWidth: Integer;
begin
  Result := FWidth;
end;

procedure TWICBitmap.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
begin
  BeginUpdate;
  try
    if not Assigned(FImage) then
      FImage := TWICImage.Create;
    FImage.LoadFromClipboardFormat(AFormat, AData, APalette);
    FWidth  := FImage.Width;
    FHeight := FImage.Height;
  finally
    EndUpdate;
  end;
end;

procedure TWICBitmap.LoadFromStream(AStream: TStream);
begin
  BeginUpdate;
  try
    if not Assigned(FImage) then
      FImage := TWICImage.Create;
    FImage.LoadFromStream(AStream);
    FWidth  := FImage.Width;
    FHeight := FImage.Height;
  finally
    EndUpdate;
  end;
end;

function TWICBitmap.LockPixels(out AData: Pointer;
  out AStride: Integer): Boolean;

  procedure BitmapNeeded;
  var
    LPixelFormat: TGUID;
    LBitmap: IWICBitmap;
    LBitmapSource: IWICBitmapSource;
  begin
    if not Assigned(FImage) then
    begin
      if (FWidth > 1) and (FHeight > 1) then
      begin
        FImage := TWICImage.Create;
        try
          LPixelFormat := GUID_WICPixelFormat32bppPBGRA;
          if Failed(TWICImage.ImagingFactory.CreateBitmap(FWidth, FHeight, @LPixelFormat, WICBitmapCacheOnLoad, LBitmap)) then
            raise EWICBitmap.Create('Could not create bitmap.');
          FImage.Handle := LBitmap;
        except
          FreeAndNil(FImage);
          raise;
        end;
      end;
    end
    else
    begin
      if Failed(WICConvertBitmapSource(GUID_WICPixelFormat32bppBGRA, FImage.Handle, LBitmapSource)) then
        raise EWICBitmap.Create('Could not convert bitmap source.');
      if Failed(TWICImage.ImagingFactory.CreateBitmapFromSource(LBitmapSource, WICBitmapCacheOnLoad, LBitmap)) then
        raise EWICBitmap.Create('Could not create bitmap.');
      FImage.Handle := LBitmap;
    end;
  end;

var
  LBufferSize: UINT;
  LData: WICInProcPointer;
  LLock: IWICBitmapLock;
  LRect: WICRect;
  LStride: UINT;
begin
  TMonitor.Enter(Self);
  try
    if FLockCount = 0 then
    begin
      BitmapNeeded;
      if not Assigned(FImage) then
        Exit(False);
      LRect.X      := 0;
      LRect.Y      := 0;
      LRect.Width  := FWidth;
      LRect.Height := FHeight;
      if Failed(FImage.Handle.Lock(LRect, WICBitmapLockRead or WICBitmapLockWrite, LLock)) then
        raise EWICBitmap.Create('Could not lock bitmap.');
      if Failed(LLock.GetStride(LStride)) then
        raise EWICBitmap.Create('Could not get bitmap stride.');
      if Failed(LLock.GetDataPointer(LBufferSize, LData)) then
        raise EWICBitmap.Create('Could not get bitmap data pointer.');
      FLock   := LLock;
      AData   := LData;
      AStride := LStride;
      BeginUpdate;
    end;
    Inc(FLockCount);
    Result := True;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TWICBitmap.SaveToClipboardFormat(var AFormat: Word;
  var AData: THandle; var APalette: HPALETTE);
begin
  if Assigned(FImage) then
    FImage.SaveToClipboardFormat(AFormat, AData, APalette);
end;

procedure TWICBitmap.SaveToStream(AStream: TStream);
begin
  if Assigned(FImage) then
  begin
    FImage.ImageFormat := TWICImageFormat.wifPng;
    FImage.SaveToStream(AStream);
  end;
end;

procedure TWICBitmap.SetHeight(AValue: Integer);
begin
  SetSize(FWidth, AValue);
end;

procedure TWICBitmap.SetSize(AWidth, AHeight: Integer);
begin
  if (AWidth <> FWidth) or (AHeight <> FHeight) then
  begin
    BeginUpdate;
    try
      FreeAndNil(FImage);
      FWidth  := AWidth;
      FHeight := AHeight;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TWICBitmap.SetWidth(AValue: Integer);
begin
  SetSize(AValue, FHeight);
end;

procedure TWICBitmap.UnlockPixels;
begin
  TMonitor.Enter(Self);
  try
    Dec(FLockCount);
    if FLockCount = 0 then
    begin
      FLock := nil;
      EndUpdate;
    end;
  finally
    TMonitor.Exit(Self);
  end;
end;

end.
