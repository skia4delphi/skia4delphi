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
unit Skia.Bindings;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils;

type
  ESkBindings = class(Exception);

  { ISkNativeObject }

  ISkNativeObject = interface
    procedure Dispose;
    function GetHandle: THandle;
    function GetOwnsHandle: Boolean;
    function IsDisposed: Boolean;
    property Handle: THandle read GetHandle;
    property OwnsHandle: Boolean read GetOwnsHandle;
  end;

  { TSkNativeObject }

  TSkNativeObject = class abstract(TInterfacedObject, ISkNativeObject)
  private
    FHandle: THandle;
    FOwnsHandle: Boolean;
  strict private
    procedure Dispose;
    function GetHandle: THandle;
    function GetOwnsHandle: Boolean;
    function IsDisposed: Boolean; inline;
  protected
    function GetSelf: THandle; inline;
  strict protected
    class procedure DestroyHandle(const AHandle: THandle); virtual; abstract;
  public
    constructor Wrap(const AHandle: THandle; const AOwnsHandle: Boolean = True); virtual;
    destructor Destroy; override;
  end;

  { ISkObject }

  ISkObject = interface(ISkNativeObject)
    ['{D1D9EE04-B65D-43D7-AA7F-B165DE5FA334}']
  end;

  { TSkObject }

  TSkObject = class abstract(TSkNativeObject, ISkObject)
  strict protected
    class procedure DestroyHandle(const AHandle: THandle); override;
  end;

  { ISkReferenceCountedBase }

  ISkReferenceCountedBase = interface(ISkNativeObject)
    procedure Release;
    procedure Retain;
  end;

  { TSkReferenceCountedBase }

  TSkReferenceCountedBase = class abstract(TSkNativeObject)
  strict protected
    procedure Release;
    procedure Retain;
    class procedure DestroyHandle(const AHandle: THandle); override; final;
    class procedure RefHandle(const AHandle: THandle); virtual;
    class procedure UnrefHandle(const AHandle: THandle); virtual;
  end;

  { ISkReferenceCounted }

  ISkReferenceCounted = interface(ISkReferenceCountedBase)
    ['{80B556E0-204B-4A64-82E9-D45056F4FE06}']
  end;

  { TSkReferenceCounted }

  TSkReferenceCounted = class abstract(TSkReferenceCountedBase, ISkReferenceCounted)
  strict protected
    class procedure RefHandle(const AHandle: THandle); override; final;
    class procedure UnrefHandle(const AHandle: THandle); override; final;
  end;

  { ISkNonVirtualReferenceCounted }

  ISkNonVirtualReferenceCounted = interface(ISkReferenceCountedBase)
    ['{96733457-7335-4ADC-B82B-E18A2E5808A3}']
  end;

  { TSkNonVirtualReferenceCounted }

  TSkNonVirtualReferenceCounted = class abstract(TSkReferenceCountedBase, ISkNonVirtualReferenceCounted);

  TSkEnumerable<T> = class;

  { TSkEnumerator<T> }

  TSkEnumerator<T> = class
  strict private
    FEnumerable: TSkEnumerable<T>;
  public
    constructor Create(const AEnumerable: TSkEnumerable<T>);
    function GetCurrent: T;
    function MoveNext: Boolean;
    property Current: T read GetCurrent;
  end;

  { ISkEnumerable<T> }

  ISkEnumerable<T> = interface(ISkObject)
    ['{A0D2696B-E080-4F15-8B60-45245B0D0D7B}']
    function GetEnumerator: TSkEnumerator<T>;
  end;

  { TSkEnumerable<T> }

  TSkEnumerable<T> = class abstract(TSkObject, ISkEnumerable<T>)
  strict private
    FRun: Boolean;
  protected
    function GetCurrent: T; virtual; abstract;
    function GetEnumerator: TSkEnumerator<T>;
    function MoveNext: Boolean; virtual; abstract;
    procedure Reset; virtual;
  end;

  TSkDelegateInitializeFunc<I: ISkNativeObject> = reference to function (const AContextProc: Pointer): I;

  TSkDelegateInvokeProc<T> = reference to procedure (const AProc: T);

  { TSkDelegate<T> }

  TSkDelegate<T> = record
  strict private type
    TProcWrapper = record
      Proc: T;
    end;
    PProcWrapper = ^TProcWrapper;

  public
    class function Initialize<I: ISkNativeObject>(const AProc: T; const AInitializeFunc: TSkDelegateInitializeFunc<I>): I; static; inline;
    class procedure Finalize(const AContextProc: Pointer); static; inline;
    class procedure Invoke(const AContextProc: Pointer; const AInvokeProc: TSkDelegateInvokeProc<T>); static; inline;
  end;

  TSkGetStringsFunc = reference to function (const AHandle: THandle; const AStrings: PMarshaledAString): Integer;

  TSkSetStringsProc = reference to procedure (const AHandle: THandle; const AStrings: PMarshaledAString; const ALength: Integer);

  { TSkBindings }

  TSkBindings = record
    class function GetSelf(const AObject: ISkNativeObject): THandle; static; inline;
    class function GetStrings(const AHandle: THandle; const AFunc: TSkGetStringsFunc): TArray<string>; static; inline;
    class function RevokeOwnership(const AObject: ISkNativeObject): THandle; static; inline;
    class function SafeCreate<T: TSkNativeObject, constructor>(const AHandle: THandle; const AOwnsHandle: Boolean = True): T; static; inline;
    class function SafeGetSelf(const AObject: ISkNativeObject): THandle; static; inline;
    class procedure SetStrings(const AHandle: THandle; const AStrings: TArray<string>; const AProc: TSkSetStringsProc); static; inline;
  end;

implementation

uses
  { Skia }
  Skia.API;

{ TSkNativeObject }

destructor TSkNativeObject.Destroy;
begin
  Dispose;
  inherited;
end;

procedure TSkNativeObject.Dispose;
begin
  if not IsDisposed then
  begin
    if FOwnsHandle then
      DestroyHandle(FHandle);
    FHandle := 0;
  end;
end;

function TSkNativeObject.GetHandle: THandle;
begin
  Result := FHandle;
end;

function TSkNativeObject.GetOwnsHandle: Boolean;
begin
  Result := FOwnsHandle;
end;

function TSkNativeObject.GetSelf: THandle;
begin
  if IsDisposed then
    raise ESkBindings.Create('Handle is already disposed');
  Result := FHandle;
end;

function TSkNativeObject.IsDisposed: Boolean;
begin
  Result := (FHandle = 0);
end;

constructor TSkNativeObject.Wrap(const AHandle: THandle;
  const AOwnsHandle: Boolean);
begin
  inherited Create;
  if AHandle = 0 then
    raise ESkBindings.Create('Handle is invalid');
  FHandle     := AHandle;
  FOwnsHandle := AOwnsHandle;
end;

{ TSkObject }

class procedure TSkObject.DestroyHandle(const AHandle: THandle);
begin
  raise ESkBindings.CreateFmt('%s.DestroyHandle not implemented', [ClassName]);
end;

{ TSkReferenceCountedBase }

class procedure TSkReferenceCountedBase.DestroyHandle(const AHandle: THandle);
begin
  UnrefHandle(AHandle);
end;

class procedure TSkReferenceCountedBase.RefHandle(const AHandle: THandle);
begin
  raise ESkBindings.CreateFmt('%s.RefHandle not implemented', [ClassName]);
end;

procedure TSkReferenceCountedBase.Release;
begin
  UnrefHandle(GetSelf);
end;

procedure TSkReferenceCountedBase.Retain;
begin
  RefHandle(GetSelf);
end;

class procedure TSkReferenceCountedBase.UnrefHandle(const AHandle: THandle);
begin
  raise ESkBindings.CreateFmt('%s.UnrefHandle not implemented', [ClassName]);
end;

{ TSkReferenceCounted }

class procedure TSkReferenceCounted.RefHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_refcnt_ref(AHandle);
end;

class procedure TSkReferenceCounted.UnrefHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_refcnt_unref(AHandle);
end;

{ TSkEnumerator<T> }

constructor TSkEnumerator<T>.Create(const AEnumerable: TSkEnumerable<T>);
begin
  inherited Create;
  FEnumerable := AEnumerable;
end;

function TSkEnumerator<T>.GetCurrent: T;
begin
  Result := FEnumerable.GetCurrent;
end;

function TSkEnumerator<T>.MoveNext: Boolean;
begin
  Result := FEnumerable.MoveNext;
end;

{ TSkEnumerable<T> }

function TSkEnumerable<T>.GetEnumerator: TSkEnumerator<T>;
begin
  if FRun then
    Reset;
  Result := TSkEnumerator<T>.Create(Self);
  FRun   := True;
end;

procedure TSkEnumerable<T>.Reset;
begin
  raise ESkBindings.Create('No reset support for this iterator');
end;

{ TSkDelegate<T> }

class procedure TSkDelegate<T>.Finalize(const AContextProc: Pointer);
begin
  Dispose(PProcWrapper(AContextProc));
end;

class function TSkDelegate<T>.Initialize<I>(const AProc: T;
  const AInitializeFunc: TSkDelegateInitializeFunc<I>): I;
var
  LProcWrapper: PProcWrapper;
begin
  New(LProcWrapper);
  try
    LProcWrapper.Proc := AProc;
    Result := AInitializeFunc(LProcWrapper);
    if not Assigned(Result) then
      Dispose(LProcWrapper);
  except
    Dispose(LProcWrapper);
    raise;
  end;
end;

class procedure TSkDelegate<T>.Invoke(const AContextProc: Pointer;
  const AInvokeProc: TSkDelegateInvokeProc<T>);
begin
  AInvokeProc(PProcWrapper(AContextProc).Proc)
end;

{ TSkBindings }

class function TSkBindings.GetSelf(const AObject: ISkNativeObject): THandle;
var
  LObject: TSkNativeObject;
begin
  Assert(Assigned(AObject));
  LObject := AObject as TSkNativeObject;
  Result  := LObject.GetSelf;
end;

class function TSkBindings.GetStrings(const AHandle: THandle;
  const AFunc: TSkGetStringsFunc): TArray<string>;
var
  I: Integer;
  LMarshaledAStrings: TArray<MarshaledAString>;
begin
  SetLength(LMarshaledAStrings, AFunc(AHandle, nil));
  if Length(LMarshaledAStrings) = 0 then
    Result := nil
  else
  begin
    AFunc(AHandle, @LMarshaledAStrings[0]);
    SetLength(Result, Length(LMarshaledAStrings));
    for I := 0 to Length(LMarshaledAStrings) - 1 do
      Result[I] := string(LMarshaledAStrings[I]);
  end;
end;

class function TSkBindings.RevokeOwnership(
  const AObject: ISkNativeObject): THandle;
var
  LObject: TSkNativeObject;
begin
  Assert(Assigned(AObject));
  LObject := AObject as TSkNativeObject;
  Result  := LObject.GetSelf;
  if not LObject.FOwnsHandle then
    raise ESkBindings.Create('Current object is not owner of the handle');
  LObject.FOwnsHandle := False;
end;

class function TSkBindings.SafeCreate<T>(const AHandle: THandle;
  const AOwnsHandle: Boolean): T;
begin
  if AHandle = 0 then
    Exit(nil);
  Result := T.Wrap(AHandle, AOwnsHandle);
end;

class function TSkBindings.SafeGetSelf(const AObject: ISkNativeObject): THandle;
begin
  if not Assigned(AObject) then
    Exit(0);
  Result := GetSelf(AObject);
end;

class procedure TSkBindings.SetStrings(const AHandle: THandle;
  const AStrings: TArray<string>; const AProc: TSkSetStringsProc);
var
  I: integer;
  LMarshaledAStrings: TArray<MarshaledAString>;
  LUTF8Strings: TArray<UTF8String>;
begin
  if Length(AStrings) = 0 then
    AProc(AHandle, nil, 0)
  else
  begin
    SetLength(LUTF8Strings, Length(AStrings));
    SetLength(LMarshaledAStrings, Length(AStrings));
    for I := 0 to Length(AStrings) - 1 do
    begin
      LUTF8Strings[I]       := UTF8String(AStrings[I]);
      LMarshaledAStrings[I] := MarshaledAString(LUTF8Strings[I]);
    end;
    AProc(AHandle, @LMarshaledAStrings[0], Length(AStrings));
  end;
end;

end.
