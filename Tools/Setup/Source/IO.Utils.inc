#ifndef UnitIOUtils
#define UnitIOUtils

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
// unit IO.Utils;

// interface

type
  TSearchOption = (soTopDirectoryOnly, soAllDirectories);

/// <summary> Combine two paths (including combination of directory and filename) </summary>
function CombinePath(const APath, ASubPath: string): string; forward;
/// <summary> Get files of a path with an optional filename. The filename accepts the wildcard *, like "*.json" </summary>
function GetFiles(APath, AFileName: string; const ASearchOption: TSearchOption): TArrayOfString; forward;

// implementation

// uses
  #include "Source\String.Utils.inc"

function CombinePath(const APath, ASubPath: string): string;
begin
  Result := ASubPath;
  if StartsWithText(Result, '\') then
    Result := Copy(Result, 2, Length(Result) - 1);
  Result := AddBackslash(APath) + Result;
end;

function GetFiles(APath, AFileName: string; const ASearchOption: TSearchOption): TArrayOfString;
var
  LFindRec: TFindRec;
  LSubDirResult: TArrayOfString;
  I: Integer;
begin
  Result := [];
  APath := AddBackslash(APath);

  if FindFirst(APath + AFileName, LFindRec) then
  begin
    try
      repeat
        if (LFindRec.Name <> '.') and (LFindRec.Name <> '..') and
          ((LFindRec.Attributes and FILE_ATTRIBUTE_DIRECTORY) = 0) and FileExists(APath + LFindRec.Name) then
        begin
          SetArrayLength(Result, GetArrayLength(Result) + 1);
          Result[GetArrayLength(Result) - 1] := APath + LFindRec.Name;
        end;
      until not FindNext(LFindRec);
    finally
      FindClose(LFindRec);
    end;
  end;

  if (ASearchOption = soAllDirectories) and FindFirst(APath + '*', LFindRec) then
  begin
    try
      repeat
        if (LFindRec.Name <> '.') and (LFindRec.Name <> '..') and
          DirExists(AddBackslash(APath + LFindRec.Name)) then
        begin
          LSubDirResult := GetFiles(AddBackslash(APath + LFindRec.Name), AFileName, ASearchOption);
          for I := 0 to GetArrayLength(LSubDirResult) -1 do
          begin
            SetArrayLength(Result, GetArrayLength(Result) + 1);
            Result[GetArrayLength(Result) - 1] := LSubDirResult[I];
          end;
        end;
      until not FindNext(LFindRec);
    finally
      FindClose(LFindRec);
    end;
  end;
end;

// end.
#endif
