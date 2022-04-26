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
{$IFNDEF SKIA_EMBEDDED}
unit Skia.Bindings;
{$ENDIF}

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils;

type
  ESkBindings = class(Exception);

  { ISkObject }

  ISkObject = interface
    function GetHandle: THandle;
    function GetOwnsHandle: Boolean;
    property Handle: THandle read GetHandle;
    property OwnsHandle: Boolean read GetOwnsHandle;
  end;

  { TSkObject }

  TSkObject = class abstract(TInterfacedObject, ISkObject)
  {$IFNDEF AUTOREFCOUNT}
  strict private const
    objDestroyingFlag = Integer($80000000);
  {$ENDIF}
  strict private
    FHandle: THandle;
    FOwnsHandle: Boolean;
    {$IFNDEF AUTOREFCOUNT}
    [Volatile] FRefCount: Integer;
    {$ENDIF}
    function GetOwnsHandle: Boolean;
    {$IFNDEF AUTOREFCOUNT}
    function GetRefCount: Integer; inline;
    {$ENDIF}
  protected
    function GetHandle: THandle; inline;
  strict protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    {$IFNDEF AUTOREFCOUNT}
    class procedure __MarkDestroying(const Obj); static; inline;
    {$ENDIF}
    class procedure DestroyHandle(const AHandle: THandle); virtual;
    class procedure RefHandle(const AHandle: THandle); virtual;
    class procedure UnrefHandle(const AHandle: THandle); virtual;
  public
    constructor Create(const AHandle: THandle);
    constructor Wrap(const AHandle: THandle; const AOwnsHandle: Boolean = True); virtual;
    destructor Destroy; override;
    class function ReleaseHandle(const AObject: ISkObject): THandle;
    {$IFNDEF AUTOREFCOUNT}
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property RefCount: Integer read GetRefCount;
    class function NewInstance: TObject; override;
    {$ENDIF}
  end;

  { ISkReferenceCountedBase }

  ISkReferenceCountedBase = interface(ISkObject)
    procedure Release;
    procedure Retain;
  end;

  { TSkReferenceCountedBase }

  TSkReferenceCountedBase = class abstract(TSkObject)
  strict protected
    procedure Release;
    procedure Retain;
  end;

  { ISkReferenceCounted }

  ISkReferenceCounted = interface(ISkReferenceCountedBase)
    ['{80B556E0-204B-4A64-82E9-D45056F4FE06}']
  end;

  { TSkReferenceCounted }

  TSkReferenceCounted = class abstract(TSkReferenceCountedBase, ISkReferenceCounted)
  public
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

  TSkDelegateInitializeFunc<I: ISkObject> = reference to function (const AContextProc: Pointer): I;

  TSkDelegateInvokeProc<T> = reference to procedure (const AProc: T);

  { TSkDelegate<T> }

  TSkDelegate<T> = record
  strict private type
    TProcWrapper = record
      Proc: T;
    end;
    PProcWrapper = ^TProcWrapper;

  public
    class function Initialize<I: ISkObject>(const AProc: T; const AInitializeFunc: TSkDelegateInitializeFunc<I>): I; static; inline;
    class procedure Finalize(const AContextProc: Pointer); static; inline;
    class procedure Invoke(const AContextProc: Pointer; const AInvokeProc: TSkDelegateInvokeProc<T>); static; inline;
  end;

  TSkGetStringsFunc = reference to function (const AHandle: THandle; const AStrings: PMarshaledAString): Integer;

  TSkSetStringsProc = reference to procedure (const AHandle: THandle; const AStrings: PMarshaledAString; const ALength: Integer);

  { TSkBindings }

  TSkBindings = record
    class function GetStrings(const AHandle: THandle; const AFunc: TSkGetStringsFunc): TArray<string>; static; inline;
    class function SafeCreate<T: TSkObject, constructor>(const AHandle: THandle; const AOwnsHandle: Boolean = True): T; static; inline;
    class function SafeGetHandle(const AObject: ISkObject): THandle; static; inline;
    class procedure SetStrings(const AHandle: THandle; const AStrings: TArray<string>; const AProc: TSkSetStringsProc); static; inline;
  end;

implementation

{$IFNDEF SKIA_EMBEDDED}
uses
  { Skia }
  Skia.API;
{$ENDIF}

{ TSkObject }

{$IFNDEF AUTOREFCOUNT}

procedure TSkObject.AfterConstruction;
begin
  AtomicDecrement(FRefCount);
  inherited;
end;

procedure TSkObject.BeforeDestruction;
begin
  inherited;
  if RefCount <> 0 then
    Error(reInvalidPtr);
end;

{$ENDIF}

constructor TSkObject.Create(const AHandle: THandle);
begin
  inherited Create;
  Assert(AHandle <> 0);
  FHandle     := AHandle;
  FOwnsHandle := True;
end;

destructor TSkObject.Destroy;
begin
  if FOwnsHandle then
    DestroyHandle(FHandle);
  inherited;
end;

class procedure TSkObject.DestroyHandle(const AHandle: THandle);
begin
  UnrefHandle(AHandle);
end;

function TSkObject.GetHandle: THandle;
begin
  Result := FHandle;
end;

function TSkObject.GetOwnsHandle: Boolean;
begin
  Result := FOwnsHandle;
end;

{$IFNDEF AUTOREFCOUNT}

function TSkObject.GetRefCount: Integer;
begin
  Result := FRefCount and not objDestroyingFlag;
end;

class function TSkObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TSkObject(Result).FRefCount := 1;
end;

{$ENDIF}

function TSkObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

class procedure TSkObject.RefHandle(const AHandle: THandle);
begin
end;

class function TSkObject.ReleaseHandle(const AObject: ISkObject): THandle;
var
  LObject: TSkObject;
begin
  if not Assigned(AObject) then
    Exit(0);
  LObject := AObject as TSkObject;
  Result  := LObject.GetHandle;
  if not LObject.FOwnsHandle then
    raise ESkBindings.Create('Current object is not owner of the handle');
  LObject.FOwnsHandle := False;
end;

class procedure TSkObject.UnrefHandle(const AHandle: THandle);
begin
end;

constructor TSkObject.Wrap(const AHandle: THandle; const AOwnsHandle: Boolean);
begin
  inherited Create;
  Assert(AHandle <> 0);
  FHandle     := AHandle;
  FOwnsHandle := AOwnsHandle;
end;

function TSkObject._AddRef: Integer;
begin
  {$IFNDEF AUTOREFCOUNT}
  Result := AtomicIncrement(FRefCount);
  {$ELSE}
  Result := __ObjAddRef;
  {$ENDIF}
  if Result > 1 then
    RefHandle(FHandle);
end;

function TSkObject._Release: Integer;
begin
  {$IFNDEF AUTOREFCOUNT}
  Result := AtomicDecrement(FRefCount);
  if Result = 0 then
  begin
    __MarkDestroying(Self);
    Destroy;
  end;
  {$ELSE}
  Result := __ObjRelease;
  {$ENDIF}
  if Result > 0 then
    UnrefHandle(FHandle);
end;

{$IFNDEF AUTOREFCOUNT}

class procedure TSkObject.__MarkDestroying(const Obj);
var
  LRef: Integer;
begin
  repeat
    LRef := TSkObject(Obj).FRefCount;
  until AtomicCmpExchange(TSkObject(Obj).FRefCount, LRef or objDestroyingFlag, LRef) = LRef;
end;

{$ENDIF}

{ TSkReferenceCountedBase }

procedure TSkReferenceCountedBase.Release;
begin
  UnrefHandle(GetHandle);
end;

procedure TSkReferenceCountedBase.Retain;
begin
  RefHandle(GetHandle);
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

class function TSkBindings.SafeCreate<T>(const AHandle: THandle;
  const AOwnsHandle: Boolean): T;
begin
  if AHandle = 0 then
    Exit(nil);
  Result := T.Wrap(AHandle, AOwnsHandle);
end;

class function TSkBindings.SafeGetHandle(const AObject: ISkObject): THandle;
begin
  if not Assigned(AObject) then
    Exit(0);
  Result := AObject.Handle;
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
