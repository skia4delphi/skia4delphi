#ifndef UnitStringUtils
#define UnitStringUtils

[Code]
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
// unit String.Utils;

// interface

/// <summary> Add an string to the array </summary>
function AppendString(const AStrings: TArrayOfString; const ANewString: string; const AAllowDuplicates: Boolean): TArrayOfString; forward;
/// <summary> Convert booleans to string in word format (True or False) </summary>
function BoolToStr(const AValue: Boolean): string; forward;
/// <summary> Check if one text ends with another text with case insentive comparison </summary>
function EndsWithText(const AText, ASubText: string): Boolean; forward;
/// <summary> Insert an string to the array at index 0 </summary>
function InsertStringAtBeginning(const AStrings: TArrayOfString; const ANewString: string): TArrayOfString; forward;
/// <summary> Concatenates strings into one, separating each string by a delimiter </summary>
function JoinStrings(const AStrings: TArrayOfString; const ADelimiter: string; const AJoinEmptyStrings: Boolean): string; forward;
/// <summary> Remove all matches of a string in an array </summary>
function RemoveString(const AStrings: TArrayOfString; const AValue: string; const ACaseSensitive: Boolean): TArrayOfString; forward;
/// <summary> Break a string into an array of substrings </summary>
function SplitString(AText: string; const ADelimiter: string): TArrayOfString; forward;
/// <summary> Check if one text starts with another text with case insentive comparison </summary>
function StartsWithText(const AText, ASubText: string): Boolean; forward;
/// <summary> Try to convert strings to booleans. If fail, the ADefault value will be returned. </summary>
function StrToBoolDef(const AValue: string; const ADefault: Boolean): Boolean; forward;

// implementation

function AppendString(const AStrings: TArrayOfString; const ANewString: string; const AAllowDuplicates: Boolean): TArrayOfString;
var
  I: Integer;
begin
  Result := AStrings;
  if not AAllowDuplicates then
  begin
    for I := 0  to GetArrayLength(AStrings) - 1 do
      if SameText(AStrings[I], ANewString) then
        Exit;
  end;
  SetArrayLength(Result, GetArrayLength(Result) + 1);
  Result[GetArrayLength(Result) - 1] := ANewString;
end;

function BoolToStr(const AValue: Boolean): string;
begin
if AValue then
  Result := 'True'
else
  Result := 'False';
end;

function EndsWithText(const AText, ASubText: string): Boolean;
var
  LSubTextLocation: Integer;
begin
  if ASubText = '' then
    Result := True
  else
  begin
    LSubTextLocation := Length(AText) - Length(ASubText);
    if LSubTextLocation >= 0 then
      Result := SameText(Copy(AText, LSubTextLocation + 1, Length(ASubText)), ASubText)
    else
      Result := False;
  end;
end;

function InsertStringAtBeginning(const AStrings: TArrayOfString; const ANewString: string): TArrayOfString;
var
  I: Integer;
begin
  Result := AStrings;
  SetArrayLength(Result, GetArrayLength(Result) + 1);
  for I := GetArrayLength(Result) - 1 downto 1 do
    Result[I] := Result[I - 1];
  Result[0] := ANewString;
end;

function JoinStrings(const AStrings: TArrayOfString; const ADelimiter: string; const AJoinEmptyStrings: Boolean): string;
var
  LString: string;
  I: Integer;
begin
  Result := '';
  for I := 0 to GetArrayLength(AStrings) - 1 do
  begin
    LString := AStrings[I];
    if (Length(LString) > 0) or AJoinEmptyStrings then
    begin
      if Length(Result) = 0 then
        Result := LString
       else
        Result := Result + ADelimiter + LString;
    end;
  end;
end;

function RemoveString(const AStrings: TArrayOfString; const AValue: string; const ACaseSensitive: Boolean): TArrayOfString;
var
  LString: string;
  I: Integer;
begin
  Result := [];
  if ACaseSensitive then
  begin
    for I := 0 to GetArrayLength(AStrings) - 1 do
    begin
      LString := AStrings[I];
      if LString <> AValue then
        Result := AppendString(Result, LString, True);
    end;
  end
  else
    for I := 0 to GetArrayLength(AStrings) - 1 do
    begin
      LString := AStrings[I];
      if not SameText(LString, AValue) then
        Result := AppendString(Result, LString, True);
    end;
end;

function SplitString(AText: string; const ADelimiter: string): TArrayOfString;
var
  LTemp: string;
  I, P: Integer;
begin
  if AText = '' then
  begin
    Result := [];
    Exit;
  end;
  LTemp := AText;
  SetArrayLength(Result, StringChangeEx(LTemp, ADelimiter, '', True) + 1);
  for I := 0 to GetArrayLength(Result) - 1 do
  begin
    P := Pos(ADelimiter, AText);
    if P > 0 then
    begin
      Result[I] := Copy(AText, 1, P - 1);
      Delete(AText, 1, P + Length(ADelimiter) - 1);
    end
    else
      Result[I] := AText;
  end;
end;

function StartsWithText(const AText, ASubText: string): Boolean;
begin
  if ASubText = '' then
    Result := True
  else if Length(AText) >= Length(ASubText) then
    Result := SameText(Copy(AText, 0, Length(ASubText)), ASubText)
  else
    Result := False;
end;

function StrToBoolDef(const AValue: string; const ADefault: Boolean): Boolean;
begin
  if SameText(AValue, 'True') or SameText(AValue, 'Yes') or (AValue = '1') then
    Result := True
  else if SameText(AValue, 'False') or SameText(AValue, 'No') or (AValue = '0') then
    Result := False
  else
    Result := ADefault;
end;

// end.
#endif
