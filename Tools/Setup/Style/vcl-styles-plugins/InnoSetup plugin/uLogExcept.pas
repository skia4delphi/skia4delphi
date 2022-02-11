// **************************************************************************************************
//
// Unit uLogExcept
// unit for the VCL Styles Plugin for Inno Setup
// https://github.com/RRUZ/vcl-styles-plugins
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is uLogExcept.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2021 Rodrigo Ruz V.
// All Rights Reserved.
//
// *************************************************************************************************

unit uLogExcept;

interface

Uses
  System.SysUtils,
  System.Classes;

type
  TLogFile = class
  private
    FLogStream: TStream;
  public
    property LogStream: TStream read FLogStream write FLogStream;
    class procedure Add(const AMessage: string); overload;
    class procedure Add(const AException: Exception); overload;
  end;

implementation

uses
  Windows,
  IOUtils;

var
  sLogFile: string;

{.$DEFINE ENABLELOG}

procedure AppendAllText(const FileName, Contents: string);
{$IFDEF ENABLELOG}
var
  LFileStream: TFileStream;
  LBuffer: TBytes;
{$ENDIF}
begin
{$IFDEF ENABLELOG}
  if (TFile.Exists(FileName)) then
    LFileStream := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyNone)
  else
    LFileStream := TFileStream.Create(FileName, fmCreate or fmShareDenyNone);

  try
    LFileStream.Seek(0, soFromEnd);
    LBuffer := TEncoding.ANSI.GetBytes(Contents);
    LFileStream.WriteBuffer(LBuffer, Length(LBuffer));
  finally
    LFileStream.Free;
  end;
{$ENDIF}
end;

{ TLogException }
class procedure TLogFile.Add(const AMessage: string);
begin
  try
    AppendAllText(sLogFile, FormatDateTime('hh:nn:ss.zzz', Now) + ' ' + AMessage + sLineBreak);
  except
    on e: EFOpenError do;
  end;
end;

class procedure TLogFile.Add(const AException: Exception);
begin
  try
    AppendAllText(sLogFile, Format('%s %s StackTrace %s %s', [FormatDateTime('hh:nn:ss.zzz', Now), AException.Message,
      AException.StackTrace, sLineBreak]));
  except
    on e: EFOpenError do;
  end;
end;

function GetTempDirectory: string;
var
  lpBuffer: array [0 .. MAX_PATH] of Char;
begin
  GetTempPath(MAX_PATH, @lpBuffer);
  Result := StrPas(lpBuffer);
end;

initialization

sLogFile := IncludeTrailingPathDelimiter(GetTempDirectory) + 'VclStylesInno.log';

end.
