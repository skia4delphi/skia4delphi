unit Skia.FMX.HtmlLabel;

interface

uses
  System.SysUtils, System.Classes, System.UITypes, System.UIConsts,
  System.Generics.Collections, System.Math, System.Character,
  FMX.Types, FMX.Controls, FMX.Graphics,
  {$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.ShellAPI,
  {$ENDIF MSWINDOWS}
  {$IFDEF MACOS}
  Posix.Stdlib,
  {$ENDIF MACOS}
  // Note: If using an older Skia4Delphi version before Delphi 12 native integration:
  System.Skia, FMX.Skia;

type
  TSkHtmlLinkClickEvent = procedure(Sender: TObject; const HRef: string) of object;

  [ComponentPlatforms(SkSupportedPlatformsMask)]
  TSkHtmlLabel = class(TSkLabel)
  private
    FHTMLText: string;
    FAutoOpenURL: Boolean;
    FLinkColor: TAlphaColor;
    FPreserveWhitespace: Boolean;
    FOnLinkClick: TSkHtmlLinkClickEvent;
    fBRLineHeight: Single;
    FAutosizeHeightToContents: Boolean;

    function GetAutoSize: Boolean;
    procedure SetAutoSize(const Value: Boolean);
    procedure SetAutosizeHeightToContents(const Value: Boolean);
    procedure SetHTMLText(const Value: string);
    procedure SetLinkColor(const Value: TAlphaColor);
    procedure SetPreserveWhitespace(const Value: Boolean);
    procedure HandleWordClick(Sender: TObject);
    procedure OpenURL(const AURL: string);
    procedure SetBRLineHeight(const New: Single);
  protected
    function DecodeEntities(const S: string): string; virtual;
    procedure ParseHTMLToWords; virtual;
    procedure DoFontChange(Sender:TObject);
    function DoSetSize(const ASize: TControlSize; const ANewPlatformDefault: Boolean;
      ANewWidth, ANewHeight: Single; var ALastWidth, ALastHeight: Single): Boolean; override;
    procedure DoEndUpdate; override;
    procedure AdjustHeightToContents; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;
    procedure Resize;
  published
    { Setting this property automatically parses the HTML and generates the Skia Words }
    property HTMLText: string read FHTMLText write SetHTMLText;
    { Automatically adjusts the height to fit wrapped content without altering the width }
    property AutosizeHeightToContents: Boolean read FAutosizeHeightToContents write SetAutosizeHeightToContents default False;
    { Standard AutoSize; automatically unchecked when AutosizeHeightToContents is True }
    property AutoSize: Boolean read GetAutoSize write SetAutoSize default True;
    { Automatically launch external web browser on link click }
    property AutoOpenURL: Boolean read FAutoOpenURL write FAutoOpenURL default False;
    { Default link color for <a href> tags }
    property LinkColor: TAlphaColor read FLinkColor write SetLinkColor default $FF1A73E8;
    { When false, collapses multiple spaces/newlines as standard HTML does }
    property PreserveWhitespace: Boolean read FPreserveWhitespace write SetPreserveWhitespace default False;
    { Event fired when an HTML hyperlink is clicked }
    property OnLinkClick: TSkHtmlLinkClickEvent read FOnLinkClick write FOnLinkClick;
    property BRLineHeight:Single read fBRLineHeight write SetBRLineHeight;

  end;

procedure Register;

implementation

uses
  System.Math.Vectors;

function IfThenString(IfThenTest:Boolean;IfTrue,IfFalse:string):string;
begin
  if IfThenTest then
    result:=IfTrue
  else
   Result:=IfFalse;
end;

procedure Register;
begin
  RegisterComponents('Skia', [TSkHtmlLabel]);
end;

type
  THTMLStyleState = record
    TagName: string;
    FontFamily: string;
    FontSize: Single;
    FontWeight: TFontWeight;
    FontSlant: TFontSlant;
    FontStretch: TFontStretch;
    FontColor: TAlphaColor;
    BackgroundColor: TAlphaColor;
    Decorations: TSkTextDecorations;
    DecorationColor: TAlphaColor;
    LetterSpacing: Single;
    HeightMultiplier: Single;
    Cursor: TCursor;
    HRef: string;
    IsPre: Boolean;
    IsCode: Boolean;
    IsSub: Boolean;
    IsSup: Boolean;
    ListLevel: Integer;
    ListIndex: Integer;
    IsOrderedList: Boolean;
  end;

{ Color & Size Parsing Helpers }

function TryParseHTMLColor(const S: string; out Color: TAlphaColor): Boolean;
var
  CleanStr: string;
  R, G, B, A: Byte;
  FloatA: Single;
  Parts: TArray<string>;
  HexVal: UInt32;
begin
  Result := False;
  CleanStr := Trim(S);
  if CleanStr = '' then Exit;

  // Remove surrounding quotes if present
  if (CleanStr.Length >= 2) and (((CleanStr.Chars[0] = '"') and (CleanStr.Chars[CleanStr.Length - 1] = '"')) or
     ((CleanStr.Chars[0] = '''') and (CleanStr.Chars[CleanStr.Length - 1] = ''''))) then
    CleanStr := CleanStr.Substring(1, CleanStr.Length - 2).Trim;

  if SameText(CleanStr, 'transparent') or SameText(CleanStr, 'none') then
  begin
    Color := TAlphaColors.Null;
    Exit(True);
  end;

  if CleanStr.StartsWith('$') then
   begin
      if TryStrToUInt(CleanStr,HexVal) then
        begin
          Color := $FF000000 or HexVal;
          Exit(True);
        end;
   end
else
  // Hex: #RGB, #RGBA, #RRGGBB, #RRGGBBAA
  if CleanStr.StartsWith('#') then
  begin
    CleanStr := CleanStr.Substring(1);
    case CleanStr.Length of
      3: // #RGB -> #RRGGBB
        if TryStrToUInt('$' + CleanStr.Chars[0] + CleanStr.Chars[0] +
                              CleanStr.Chars[1] + CleanStr.Chars[1] +
                              CleanStr.Chars[2] + CleanStr.Chars[2], HexVal) then
        begin
          Color := $FF000000 or HexVal;
          Exit(True);
        end;
      4: // #RGBA
        if TryStrToUInt('$' + CleanStr.Chars[3] + CleanStr.Chars[3] +
                              CleanStr.Chars[0] + CleanStr.Chars[0] +
                              CleanStr.Chars[1] + CleanStr.Chars[1] +
                              CleanStr.Chars[2] + CleanStr.Chars[2], HexVal) then
        begin
          Color := HexVal;
          Exit(True);
        end;
      6: // #RRGGBB
        if TryStrToUInt('$' + CleanStr, HexVal) then
        begin
          Color := $FF000000 or HexVal;
          Exit(True);
        end;
      8: // #RRGGBBAA (CSS standard: AA at end -> converted to Delphi ARGB)
        if TryStrToUInt('$' + CleanStr.Substring(6, 2) + CleanStr.Substring(0, 6), HexVal) then
        begin
          Color := HexVal;
          Exit(True);
        end;
    end;
  end;

  // rgb(r, g, b) or rgba(r, g, b, a)
  if CleanStr.ToLower.StartsWith('rgb') then
  begin
    var OpenP := CleanStr.IndexOf('(');
    var CloseP := CleanStr.IndexOf(')');
    if (OpenP >= 0) and (CloseP > OpenP) then
    begin
      Parts := CleanStr.Substring(OpenP + 1, CloseP - OpenP - 1).Split([',', ' '], TStringSplitOptions.ExcludeEmpty);
      if Length(Parts) >= 3 then
      begin
        R := EnsureRange(StrToIntDef(Trim(Parts[0]), 0), 0, 255);
        G := EnsureRange(StrToIntDef(Trim(Parts[1]), 0), 0, 255);
        B := EnsureRange(StrToIntDef(Trim(Parts[2]), 0), 0, 255);
        A := 255;
        if Length(Parts) >= 4 then
        begin
          var AlphaStr := Trim(Parts[3]);
          if TryStrToFloat(AlphaStr, FloatA, TFormatSettings.Invariant) then
          begin
            if FloatA <= 1.0 then
              A := EnsureRange(Round(FloatA * 255), 0, 255)
            else
              A := EnsureRange(Round(FloatA), 0, 255);
          end;
        end;
        Color := (A shl 24) or (R shl 16) or (G shl 8) or B;
        Exit(True);
      end;
    end;
  end;

  // Named Color via FMX/UIConsts
  try
    Color := StringToAlphaColor(CleanStr);
    Result := True;
  except
    try
      Color := StringToAlphaColor('cla' + CleanStr);
      Result := True;
    except
      Result := False;
    end;
  end;
end;

function ParseFontSize(const AVal: string; ACurrentSize, ABaseSize: Single): Single;
var
  S: string;
  Num: Single;
begin
  S := LowerCase(Trim(AVal));
  if S = '' then Exit(ACurrentSize);

  // Standard HTML <font size="1..7">
  if S = '1' then Exit(10);
  if S = '2' then Exit(12);
  if S = '3' then Exit(ABaseSize);
  if S = '4' then Exit(18);
  if S = '5' then Exit(24);
  if S = '6' then Exit(32);
  if S = '7' then Exit(48);

  // Relative sizes (+1, -1, etc.)
  if S = '+1' then Exit(ACurrentSize * 1.2);
  if S = '+2' then Exit(ACurrentSize * 1.4);
  if S = '+3' then Exit(ACurrentSize * 1.8);
  if S = '+4' then Exit(ACurrentSize * 2.2);
  if S = '-1' then Exit(Max(6, ACurrentSize * 0.85));
  if S = '-2' then Exit(Max(6, ACurrentSize * 0.7));
  if S = '-3' then Exit(Max(6, ACurrentSize * 0.55));

  // CSS keywords
  if S = 'xx-small' then Exit(9);
  if S = 'x-small'  then Exit(10);
  if S = 'small'    then Exit(12);
  if S = 'medium'   then Exit(ABaseSize);
  if S = 'large'    then Exit(18);
  if S = 'x-large'  then Exit(24);
  if S = 'xx-large' then Exit(32);
  if S = 'smaller'  then Exit(Max(6, ACurrentSize * 0.8));
  if S = 'larger'   then Exit(ACurrentSize * 1.25);

  // Units: px, pt, em, %
  if S.EndsWith('px') then
  begin
    if TryStrToFloat(S.Substring(0, S.Length - 2), Num, TFormatSettings.Invariant) then
      Exit(Max(1, Num));
  end
  else if S.EndsWith('pt') then
  begin
    if TryStrToFloat(S.Substring(0, S.Length - 2), Num, TFormatSettings.Invariant) then
      Exit(Max(1, Num));
  end
  else if S.EndsWith('em') then
  begin
    if TryStrToFloat(S.Substring(0, S.Length - 2), Num, TFormatSettings.Invariant) then
      Exit(Max(1, Num * ACurrentSize));
  end
  else if S.EndsWith('%') then
  begin
    if TryStrToFloat(S.Substring(0, S.Length - 1), Num, TFormatSettings.Invariant) then
      Exit(Max(1, (Num / 100) * ACurrentSize));
  end
  else if TryStrToFloat(S, Num, TFormatSettings.Invariant) then
    Exit(Max(1, Num));

  Result := ACurrentSize;
end;

procedure ParseAttributes(const ATagContent: string; out ATagName: string;
  var AAttrs: TDictionary<string, string>);
var
  I, Len, KeyStart, ValStart: Integer;
  Key, Val: string;
  QuoteChar: Char;
begin
  AAttrs.Clear;
  Len := ATagContent.Length;
  I := 1;

  // 1. Extract Tag Name
  while (I <= Len) and (ATagContent[I] <= ' ') do Inc(I);
  KeyStart := I;
  while (I <= Len) and (ATagContent[I] > ' ') and (ATagContent[I] <> '/') do Inc(I);
  ATagName := LowerCase(Copy(ATagContent, KeyStart, I - KeyStart));

  // 2. Extract Key/Value Pairs
  while I <= Len do
  begin
    while (I <= Len) and ((ATagContent[I] <= ' ') or (ATagContent[I] = '/')) do Inc(I);
    if I > Len then Break;

    KeyStart := I;
    while (I <= Len) and (ATagContent[I] > ' ') and (ATagContent[I] <> '=') and (ATagContent[I] <> '/') do Inc(I);
    Key := LowerCase(Copy(ATagContent, KeyStart, I - KeyStart));

    while (I <= Len) and (ATagContent[I] <= ' ') do Inc(I);

    if (I <= Len) and (ATagContent[I] = '=') then
    begin
      Inc(I);
      while (I <= Len) and (ATagContent[I] <= ' ') do Inc(I);

      if (I <= Len) and ((ATagContent[I] = '"') or (ATagContent[I] = '''')) then
      begin
        QuoteChar := ATagContent[I];
        Inc(I);
        ValStart := I;
        while (I <= Len) and (ATagContent[I] <> QuoteChar) do Inc(I);
        Val := Copy(ATagContent, ValStart, I - ValStart);
        if (I <= Len) and (ATagContent[I] = QuoteChar) then Inc(I);
      end
      else
      begin
        ValStart := I;
        while (I <= Len) and (ATagContent[I] > ' ') and (ATagContent[I] <> '/') do Inc(I);
        Val := Copy(ATagContent, ValStart, I - ValStart);
      end;
    end
    else
      Val := Key; // Boolean attribute

    if Key <> '' then
      AAttrs.AddOrSetValue(Key, Val);
  end;
end;

procedure ApplyInlineCSS(const AStyleStr: string; var AState: THTMLStyleState; ABaseFontSize: Single);
var
  Declarations, Pair: TArray<string>;
  PropertyKey, PropertyVal: string;
  ParsedColor: TAlphaColor;
  NumVal: Single;
begin
  Declarations := AStyleStr.Split([';'], TStringSplitOptions.ExcludeEmpty);
  for var Decl in Declarations do
  begin
    Pair := Decl.Split([':'], 2);
    if Length(Pair) < 2 then Continue;

    PropertyKey := LowerCase(Trim(Pair[0]));
    PropertyVal := Trim(Pair[1]);

    if PropertyKey = 'color' then
    begin
      if TryParseHTMLColor(PropertyVal, ParsedColor) then
        AState.FontColor := ParsedColor;
    end
    else if (PropertyKey = 'background-color') or (PropertyKey = 'background') then
    begin
      if TryParseHTMLColor(PropertyVal, ParsedColor) then
        AState.BackgroundColor := ParsedColor;
    end
    else if PropertyKey = 'font-size' then
    begin
      AState.FontSize := ParseFontSize(PropertyVal, AState.FontSize, ABaseFontSize);
    end
    else if (PropertyKey = 'font-family') or (PropertyKey = 'font-name') then
    begin
      var CleanFamily := StringReplace(PropertyVal, '"', '', [rfReplaceAll]);
      CleanFamily := StringReplace(CleanFamily, '''', '', [rfReplaceAll]);
      AState.FontFamily := CleanFamily;
    end
    else if PropertyKey = 'font-weight' then
    begin
      PropertyVal := LowerCase(PropertyVal);
      if (PropertyVal = 'bold') or (PropertyVal = 'bolder') or (PropertyVal = '700') or (PropertyVal = '800') or (PropertyVal = '900') then
        AState.FontWeight := TFontWeight.Bold
      else if (PropertyVal = 'semibold') or (PropertyVal = '600') then
        AState.FontWeight := TFontWeight.SemiBold
      else if (PropertyVal = 'medium') or (PropertyVal = '500') then
        AState.FontWeight := TFontWeight.Medium
      else if (PropertyVal = 'light') or (PropertyVal = '300') then
        AState.FontWeight := TFontWeight.Light
      else if (PropertyVal = 'ultralight') or (PropertyVal = 'extralight') or (PropertyVal = '200') then
        AState.FontWeight := TFontWeight.UltraLight
      else if (PropertyVal = 'thin') or (PropertyVal = '100') then
        AState.FontWeight := TFontWeight.Thin
      else
        AState.FontWeight := TFontWeight.Regular;
    end
    else if PropertyKey = 'font-style' then
    begin
      PropertyVal := LowerCase(PropertyVal);
      if PropertyVal = 'italic' then
        AState.FontSlant := TFontSlant.Italic
      else if PropertyVal = 'oblique' then
        AState.FontSlant := TFontSlant.Oblique
      else
        AState.FontSlant := TFontSlant.Regular;
    end
    else if (PropertyKey = 'text-decoration') or (PropertyKey = 'text-decoration-line') then
    begin
      PropertyVal := LowerCase(PropertyVal);
      if PropertyVal.Contains('none') then
        AState.Decorations := []
      else
      begin
        if PropertyVal.Contains('underline') then
          AState.Decorations := AState.Decorations + [TSkTextDecoration.Underline];
        if PropertyVal.Contains('line-through') or PropertyVal.Contains('strike') then
          AState.Decorations := AState.Decorations + [TSkTextDecoration.LineThrough];
        if PropertyVal.Contains('overline') then
          AState.Decorations := AState.Decorations + [TSkTextDecoration.Overline];
      end;
    end
    else if PropertyKey = 'text-decoration-color' then
    begin
      if TryParseHTMLColor(PropertyVal, ParsedColor) then
        AState.DecorationColor := ParsedColor;
    end
    else if PropertyKey = 'letter-spacing' then
    begin
      PropertyVal := StringReplace(LowerCase(PropertyVal), 'px', '', [rfReplaceAll]);
      PropertyVal := StringReplace(PropertyVal, 'pt', '', [rfReplaceAll]);
      if TryStrToFloat(Trim(PropertyVal), NumVal, TFormatSettings.Invariant) then
        AState.LetterSpacing := NumVal;
    end
    else if (PropertyKey = 'line-height') or (PropertyKey = 'height-multiplier') then
    begin
      if TryStrToFloat(Trim(PropertyVal), NumVal, TFormatSettings.Invariant) then
        AState.HeightMultiplier := NumVal;
    end
    else if PropertyKey = 'cursor' then
    begin
      PropertyVal := LowerCase(PropertyVal);
      if (PropertyVal = 'pointer') or (PropertyVal = 'hand') then
        AState.Cursor := crHandPoint
      else if (PropertyVal = 'text') or (PropertyVal = 'ibeam') then
        AState.Cursor := crIBeam
      else if PropertyVal = 'crosshair' then
        AState.Cursor := crCross
      else if PropertyVal = 'help' then
        AState.Cursor := crHelp
      else
        AState.Cursor := crDefault;
    end;
  end;
end;

{ TSkHtmlLabel }

constructor TSkHtmlLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHTMLText := '';
  FAutoOpenURL := False;
  FLinkColor := $FF1A73E8;
  FPreserveWhitespace := False;
  Autosize:=false;
  FAutosizeHeightToContents := true;
end;

function TSkHtmlLabel.GetAutoSize: Boolean;
begin
  Result := inherited AutoSize;
end;

procedure TSkHtmlLabel.SetAutoSize(const Value: Boolean);
begin
  if inherited AutoSize <> Value then
  begin
    if Value and FAutosizeHeightToContents then
      FAutosizeHeightToContents := False;
    inherited AutoSize := Value;
  end;
end;

procedure TSkHtmlLabel.SetAutosizeHeightToContents(const Value: Boolean);
begin
  if FAutosizeHeightToContents <> Value then
  begin
    FAutosizeHeightToContents := Value;
    if FAutosizeHeightToContents then
    begin
      if inherited AutoSize then
        inherited AutoSize := False;
      AdjustHeightToContents;
    end;
  end;
end;

procedure TSkHtmlLabel.AdjustHeightToContents;
var
  LParagraph: ISkParagraph;
  LHeight: Single;
begin
  if (csLoading in ComponentState) or (csDestroying in ComponentState) then
    Exit;

  LParagraph := Paragraph;
  if Assigned(LParagraph) then
  begin
    if Width > 0 then
      LParagraph.Layout(Width)
    else
      LParagraph.Layout(Infinity);
    LHeight := LParagraph.Height;
    if (LHeight > 0) and not SameValue(Height, LHeight, TEpsilon.Position) then
      SetSize(Width, LHeight);
  end;
end;

procedure TSkHtmlLabel.DoEndUpdate;
begin
  inherited DoEndUpdate;
  if not (csLoading in ComponentState) and FAutosizeHeightToContents then
    AdjustHeightToContents;
end;

function TSkHtmlLabel.DoSetSize(const ASize: TControlSize; const ANewPlatformDefault: Boolean;
  ANewWidth, ANewHeight: Single; var ALastWidth, ALastHeight: Single): Boolean;
var
  LParagraph: ISkParagraph;
begin
  if FAutosizeHeightToContents and not (csLoading in ComponentState) then
  begin
    LParagraph := Paragraph;
    if Assigned(LParagraph) then
    begin
      if ANewWidth > 0 then
        LParagraph.Layout(ANewWidth)
      else
        LParagraph.Layout(Infinity);
      ANewHeight := LParagraph.Height;
    end;
  end;
  Result := inherited DoSetSize(ASize, ANewPlatformDefault, ANewWidth, ANewHeight, ALastWidth, ALastHeight);
end;

procedure TSkHtmlLabel.SetHTMLText(const Value: string);
begin
  if FHTMLText <> Value then
  begin
    FHTMLText := Value;
    ParseHTMLToWords;
  end;
end;

procedure TSkHtmlLabel.SetLinkColor(const Value: TAlphaColor);
begin
  if FLinkColor <> Value then
  begin
    FLinkColor := Value;
    ParseHTMLToWords;
  end;
end;

procedure TSkHtmlLabel.SetPreserveWhitespace(const Value: Boolean);
begin
  if FPreserveWhitespace <> Value then
  begin
    FPreserveWhitespace := Value;
    ParseHTMLToWords;
  end;
end;

procedure TSkHtmlLabel.HandleWordClick(Sender: TObject);
var
  LWord: TCustomWordsItem;
begin
  if Sender is TCustomWordsItem then
  begin
    LWord := TCustomWordsItem(Sender);
    if LWord.TagString <> '' then
    begin
      if Assigned(FOnLinkClick) then
        FOnLinkClick(Self, LWord.TagString);
      if FAutoOpenURL then
        OpenURL(LWord.TagString);
    end;
  end;
end;

procedure TSkHtmlLabel.Loaded;
begin
  inherited Loaded;
  if assigned(TextSettings) then
    TextSettings.OnChange:=DoFontChange;
  ParseHTMLToWords;
  if FAutosizeHeightToContents then
    AdjustHeightToContents;
end;

procedure TSkHtmlLabel.OpenURL(const AURL: string);
begin
  if Trim(AURL) = '' then Exit;
  {$IFDEF MSWINDOWS}
  Winapi.ShellAPI.ShellExecute(0, 'open', PChar(AURL), nil, nil, SW_SHOWNORMAL);
  {$ENDIF MSWINDOWS}
  {$IFDEF MACOS}
  _system(PAnsiChar('open ' + AnsiQuotedStr(AURL, '"')));
  {$ENDIF MACOS}
end;

function TSkHtmlLabel.DecodeEntities(const S: string): string;
var
  I, J, Len, CodePoint: Integer;
  Entity, Replacement: string;
  IsHex: Boolean;
begin
  Len := Length(S);
  if Len = 0 then Exit('');

  SetLength(Result, Len);
  var OutIdx := 1;
  I := 1;

  while I <= Len do
  begin
    if S[I] = '&' then
    begin
      J := I + 1;
      while (J <= Len) and (J - I < 12) and (S[J] <> ';') and (S[J] <> '&') and (S[J] > ' ') do
        Inc(J);

      if (J <= Len) and (S[J] = ';') then
      begin
        Entity := Copy(S, I + 1, J - I - 1);
        Replacement := '';

        // Numeric entity: &#160; or &#x00A0;
        if Entity.StartsWith('#') then
        begin
          IsHex := (Entity.Length > 2) and ((Entity.Chars[1] = 'x') or (Entity.Chars[1] = 'X'));
          if IsHex then
            TryStrToUInt('$' + Entity.Substring(2), UInt32(CodePoint))
          else
            CodePoint := StrToIntDef(Entity.Substring(1), 0);

          if (CodePoint > 0) and (CodePoint <= $FFFF) then
            Replacement := Char(CodePoint)
          else if (CodePoint > $FFFF) and (CodePoint <= $10FFFF) then
          begin
            Dec(CodePoint, $10000);
            Replacement := Char($D800 + (CodePoint shr 10)) + Char($DC00 + (CodePoint and $3FF));
          end;
        end
        else
        begin
          // Named Entities
          var LowerEnt := LowerCase(Entity);
          if LowerEnt = 'nbsp' then Replacement := #160
          else if LowerEnt = 'amp' then Replacement := '&'
          else if LowerEnt = 'lt' then Replacement := '<'
          else if LowerEnt = 'gt' then Replacement := '>'
          else if LowerEnt = 'quot' then Replacement := '"'
          else if LowerEnt = 'apos' then Replacement := ''''
          else if LowerEnt = 'copy' then Replacement := '©'
          else if LowerEnt = 'reg' then Replacement := '®'
          else if LowerEnt = 'trade' then Replacement := '™'
          else if LowerEnt = 'mdash' then Replacement := '—'
          else if LowerEnt = 'ndash' then Replacement := '–'
          else if LowerEnt = 'lsquo' then Replacement := '‘'
          else if LowerEnt = 'rsquo' then Replacement := '’'
          else if LowerEnt = 'ldquo' then Replacement := '“'
          else if LowerEnt = 'rdquo' then Replacement := '”'
          else if LowerEnt = 'bull' then Replacement := '•'
          else if LowerEnt = 'hellip' then Replacement := '…'
          else if LowerEnt = 'euro' then Replacement := '€'
          else if LowerEnt = 'pound' then Replacement := '£'
          else if LowerEnt = 'yen' then Replacement := '¥'
          else if LowerEnt = 'cent' then Replacement := '¢'
          else if LowerEnt = 'deg' then Replacement := '°'
          else if LowerEnt = 'plusmn' then Replacement := '±'
          else if LowerEnt = 'times' then Replacement := '×'
          else if LowerEnt = 'divide' then Replacement := '÷'
          else if LowerEnt = 'micro' then Replacement := 'µ'
          else if LowerEnt = 'sect' then Replacement := '§'
          else if LowerEnt = 'para' then Replacement := '¶'
          else if LowerEnt = 'middot' then Replacement := '·'
          else if LowerEnt = 'check' then Replacement := '✓'
          else if LowerEnt = 'cross' then Replacement := '✗'
          else if LowerEnt = 'star' then Replacement := '★'
          else if LowerEnt = 'spades' then Replacement := '♠'
          else if LowerEnt = 'clubs' then Replacement := '♣'
          else if LowerEnt = 'hearts' then Replacement := '♥'
          else if LowerEnt = 'diams' then Replacement := '♦'
          else if LowerEnt = 'frac12' then Replacement := '½'
          else if LowerEnt = 'frac14' then Replacement := '¼'
          else if LowerEnt = 'frac34' then Replacement := '¾'
          else if LowerEnt = 'ne' then Replacement := '≠'
          else if LowerEnt = 'le' then Replacement := '≤'
          else if LowerEnt = 'ge' then Replacement := '≥'
          else if LowerEnt = 'infin' then Replacement := '∞';
        end;

        if Replacement <> '' then
        begin
          for var C in Replacement do
          begin
            if OutIdx > Length(Result) then SetLength(Result, OutIdx + 16);
            Result[OutIdx] := C;
            Inc(OutIdx);
          end;
          I := J + 1;
          Continue;
        end;
      end;
    end;

    if OutIdx > Length(Result) then SetLength(Result, OutIdx + 16);
    Result[OutIdx] := S[I];
    Inc(OutIdx);
    Inc(I);
  end;

  SetLength(Result, OutIdx - 1);
end;

procedure TSkHtmlLabel.DoFontChange(Sender: TObject);
begin
    ParseHTMLToWords;
end;

procedure TSkHtmlLabel.SetBRLineHeight(const New:Single);
begin
   fBRLineHeight:=New;
   ParseHTMLToWOrds;
end;

procedure TSkHtmlLabel.ParseHTMLToWords;
var
  I, StartIdx, Len: Integer;
  TagContent, TagName, Segment: string;
  IsClosing, IsSelfClosing: Boolean;
  BaseFontSize: Single;
  StateStack: TList<THTMLStyleState>;
  CurrentState, NewState: THTMLStyleState;
  Attrs: TDictionary<string, string>;
  ParsedColor: TAlphaColor;

  procedure AddWordSegment(const AText: string; const AState: THTMLStyleState);
  var
    DecodedText, ProcessedText: string;
    LWord: TCustomWordsItem;
  begin
    if AText = '' then Exit;

    DecodedText := DecodeEntities(AText);

    if not AState.IsPre and not FPreserveWhitespace then
    begin
      // Collapse redundant whitespaces into single space
      var SB := TStringBuilder.Create;
      try
        var InSpace := False;
        for var C in DecodedText do
        begin
          if (C = ' ') or (C = #9) or (C = #13) or (C = #10) then
          begin
            if not InSpace then
            begin
              SB.Append(' ');
              InSpace := True;
            end;
          end
          else
          begin
            SB.Append(C);
            InSpace := False;
          end;
        end;
        ProcessedText := SB.ToString;
      finally
        SB.Free;
      end;
    end
    else
      ProcessedText := DecodedText;

    if ProcessedText = '' then Exit;

    LWord := Words.Add;
    LWord.Text := ProcessedText;

    // Font Family
    if AState.FontFamily <> '' then
      LWord.Font.Families := AState.FontFamily;

    // Font Size
    if AState.FontSize > 0 then
      LWord.Font.Size := AState.FontSize;

    // Weight & Slant
    LWord.Font.Weight := AState.FontWeight;
    LWord.Font.Slant := AState.FontSlant;
    LWord.Font.Stretch := AState.FontStretch;

    // Colors
    if AState.FontColor <> TAlphaColors.Null then
      LWord.FontColor := AState.FontColor;

    if AState.BackgroundColor <> TAlphaColors.Null then
      LWord.BackgroundColor := AState.BackgroundColor;

    // Decorations
    LWord.Decorations.Decorations := AState.Decorations;
    if AState.DecorationColor <> TAlphaColors.Null then
      LWord.Decorations.Color := AState.DecorationColor;

    // Spacing & Multiplier
    if AState.LetterSpacing <> 0 then
      LWord.LetterSpacing := AState.LetterSpacing;
    if AState.HeightMultiplier <> 0 then
      LWord.HeightMultiplier := AState.HeightMultiplier;

    // Cursor & Links
    LWord.Cursor := AState.Cursor;
    if AState.HRef <> '' then
    begin
      LWord.TagString := AState.HRef;
      LWord.OnClick := HandleWordClick;
    end;
  end;

  procedure PopTag(const ATag: string);
  begin
    for var StackIdx := StateStack.Count - 1 downto 1 do
    begin
      if SameText(StateStack[StackIdx].TagName, ATag) then
      begin
        while StateStack.Count > StackIdx do
          StateStack.Delete(StateStack.Count - 1);
        CurrentState := StateStack.Last;
        Exit;
      end;
    end;

    if StateStack.Count > 1 then
    begin
      StateStack.Delete(StateStack.Count - 1);
      CurrentState := StateStack.Last;
    end;
  end;

begin
  Words.Clear;

  if FHTMLText = '' then
  begin
    Text := '';
    if FAutosizeHeightToContents then
      AdjustHeightToContents;
    Exit;
  end;

  BaseFontSize := TextSettings.Font.Size;
  if BaseFontSize <= 0 then
    BaseFontSize := 14;

  StateStack := TList<THTMLStyleState>.Create;
  Attrs := TDictionary<string, string>.Create;
  try
    // Initialize Base State
    FillChar(CurrentState, SizeOf(CurrentState), 0);
    CurrentState.TagName := '';
    CurrentState.FontFamily := TextSettings.Font.Families;
    CurrentState.FontSize := BaseFontSize;
    CurrentState.FontWeight := TextSettings.Font.Weight;
    CurrentState.FontSlant := TextSettings.Font.Slant;
    CurrentState.FontStretch := TextSettings.Font.Stretch;
    CurrentState.FontColor := TextSettings.FontColor;
    CurrentState.BackgroundColor := TAlphaColors.Null;
    CurrentState.Decorations := TextSettings.Decorations.Decorations;
    CurrentState.DecorationColor := TextSettings.Decorations.Color;
    CurrentState.LetterSpacing := TextSettings.LetterSpacing;
    CurrentState.HeightMultiplier := TextSettings.HeightMultiplier;
    CurrentState.Cursor := crDefault;
    CurrentState.HRef := '';
    CurrentState.ListLevel := 0;

    StateStack.Add(CurrentState);

    Len := Length(FHTMLText);
    I := 1;
    StartIdx := 1;

    while I <= Len do
    begin
      if FHTMLText[I] = '<' then
      begin
        // Add pending text before tag
        if I > StartIdx then
        begin
          Segment := Copy(FHTMLText, StartIdx, I - StartIdx);
          AddWordSegment(Segment, CurrentState);
        end;

        // Skip HTML comments: <!-- ... -->
        if (I + 3 <= Len) and (Copy(FHTMLText, I, 4) = '<!--') then
        begin
          var CommentEnd := Pos('-->', FHTMLText, I + 4);
          if CommentEnd > 0 then
            I := CommentEnd + 2
          else
            I := Len;
          StartIdx := I + 1;
          Inc(I);
          Continue;
        end;

        // Read Tag Content
        Inc(I);
        var TagStart := I;
        var InQuote := False;
        var QuoteCh: Char := #0;

        while (I <= Len) do
        begin
          if (FHTMLText[I] = '"') or (FHTMLText[I] = '''') then
          begin
            if not InQuote then
            begin
              InQuote := True;
              QuoteCh := FHTMLText[I];
            end
            else if FHTMLText[I] = QuoteCh then
              InQuote := False;
          end
          else if (FHTMLText[I] = '>') and not InQuote then
            Break;
          Inc(I);
        end;

        TagContent := Trim(Copy(FHTMLText, TagStart, I - TagStart));
        StartIdx := I + 1;

        if TagContent <> '' then
        begin
          IsClosing := TagContent.StartsWith('/');
          IsSelfClosing := TagContent.EndsWith('/');

          if IsClosing then
          begin
            TagName := LowerCase(Trim(TagContent.Substring(1)));
            PopTag(TagName);
          end
          else
          begin
            ParseAttributes(TagContent, TagName, Attrs);

            // Skip script & style tags with their contents
            if (TagName = 'script') or (TagName = 'style') then
            begin
              var CloseMarker := '</' + TagName + '>';
              var ClosePos := Pos(CloseMarker, LowerCase(FHTMLText), I);
              if ClosePos > 0 then
                I := ClosePos + Length(CloseMarker) - 1
              else
                I := Len;
              StartIdx := I + 1;
              Inc(I);
              Continue;
            end;

            NewState := CurrentState;
            NewState.TagName := TagName;

            // --- Apply Tag-Specific Rules ---
            if (TagName = 'b') or (TagName = 'strong') then
              NewState.FontWeight := TFontWeight.Bold
            else if (TagName = 'i') or (TagName = 'em') or (TagName = 'cite') or (TagName = 'var') or (TagName = 'dfn') or (TagName = 'address') then
              NewState.FontSlant := TFontSlant.Italic
            else if (TagName = 'u') or (TagName = 'ins') then
              NewState.Decorations := NewState.Decorations + [TSkTextDecoration.Underline]
            else if (TagName = 's') or (TagName = 'strike') or (TagName = 'del') then
              NewState.Decorations := NewState.Decorations + [TSkTextDecoration.LineThrough]
            else if TagName = 'overline' then
              NewState.Decorations := NewState.Decorations + [TSkTextDecoration.Overline]
            else if TagName = 'mark' then
              NewState.BackgroundColor := $FFFFFF00 // Yellow highlight default
            else if (TagName = 'code') or (TagName = 'tt') or (TagName = 'kbd') or (TagName = 'samp') then
            begin
              NewState.FontFamily := 'Consolas, Courier New, monospace';
              NewState.IsCode := True;
            end
            else if TagName = 'pre' then
            begin
              NewState.FontFamily := 'Consolas, Courier New, monospace';
              NewState.IsPre := True;
              AddWordSegment(sLineBreak, NewState);
            end
            else if TagName = 'small' then
              NewState.FontSize := Max(6, NewState.FontSize * 0.8)
            else if TagName = 'big' then
              NewState.FontSize := NewState.FontSize * 1.25
            else if TagName = 'sub' then
            begin
              NewState.FontSize := Max(6, NewState.FontSize * 0.75);
              NewState.IsSub := True;
            end
            else if TagName = 'sup' then
            begin
              NewState.FontSize := Max(6, NewState.FontSize * 0.75);
              NewState.IsSup := True;
            end
            else if (TagName = 'p') or (TagName = 'br') or (TagName = 'div') or
              (TagName = 'section') or (TagName = 'article') or (TagName = 'header') or (TagName = 'footer') then
            begin
              CurrentState.IsPre:=true;
              var bufferedFSize:=CurrentState.FontSize;
              CurrentState.FontSize:=IfThen(fBRLineHeight=0,TextSettings.Font.Size,fBRLineHeight);
              AddWordSegment(sLineBreak, CurrentState);
              CurrentState.FontSize:=bufferedFSize;
            end
            else if (TagName = 'hr') then
            begin
              CurrentState.IsPre:=true;
              var bufferedFSize:=CurrentState.FontSize;
              CurrentState.FontSize:=IfThen(fBRLineHeight=0,TextSettings.Font.Size,fBRLineHeight);
              AddWordSegment(sLineBreak + '────────────────────────────────────────' + sLineBreak, CurrentState);
              CurrentState.FontSize:=bufferedFSize;
            end
            else if TagName = 'blockquote' then
            begin
              NewState.FontSlant := TFontSlant.Italic;
              AddWordSegment(sLineBreak + '    ', CurrentState);
            end
            else if TagName = 'h1' then
            begin
              NewState.FontSize := BaseFontSize * 2.0;
              NewState.FontWeight := TFontWeight.Bold;
              AddWordSegment(sLineBreak, CurrentState);
            end
            else if TagName = 'h2' then
            begin
              NewState.FontSize := BaseFontSize * 1.5;
              NewState.FontWeight := TFontWeight.Bold;
              AddWordSegment(sLineBreak, CurrentState);
            end
            else if TagName = 'h3' then
            begin
              NewState.FontSize := BaseFontSize * 1.25;
              NewState.FontWeight := TFontWeight.Bold;
              AddWordSegment(sLineBreak, CurrentState);
            end
            else if TagName = 'h4' then
            begin
              NewState.FontSize := BaseFontSize * 1.1;
              NewState.FontWeight := TFontWeight.Bold;
              AddWordSegment(sLineBreak, CurrentState);
            end
            else if TagName = 'h5' then
            begin
              NewState.FontSize := BaseFontSize * 0.9;
              NewState.FontWeight := TFontWeight.Bold;
              AddWordSegment(sLineBreak, CurrentState);
            end
            else if TagName = 'h6' then
            begin
              NewState.FontSize := BaseFontSize * 0.8;
              NewState.FontWeight := TFontWeight.Bold;
              AddWordSegment(sLineBreak, CurrentState);
            end
            else if TagName = 'ul' then
            begin
              NewState.IsOrderedList := False;
              NewState.ListLevel := CurrentState.ListLevel + 1;
              NewState.ListIndex := 0;
            end
            else if TagName = 'ol' then
            begin
              NewState.IsOrderedList := True;
              NewState.ListLevel := CurrentState.ListLevel + 1;
              NewState.ListIndex := 0;
            end
            else if TagName = 'li' then
            begin
              Inc(CurrentState.ListIndex);
              NewState.ListIndex := CurrentState.ListIndex;
              CurrentState.IsPre:=true;
              if CurrentState.IsOrderedList then
                AddWordSegment(sLineBreak + StringOfChar(' ', (CurrentState.ListLevel - 1) * 2) + IntToStr(CurrentState.ListIndex) + '. ', CurrentState)
              else
                AddWordSegment(sLineBreak + StringOfChar(' ', (CurrentState.ListLevel - 1) * 2) + '• ', CurrentState);
            end
            else if TagName = 'a' then
            begin
              if Attrs.ContainsKey('href') then
                NewState.HRef := DecodeEntities(Attrs['href']);
              NewState.Cursor := crHandPoint;
              NewState.Decorations := NewState.Decorations + [TSkTextDecoration.Underline];
              NewState.FontColor := FLinkColor;
            end
            else if TagName = 'font' then
            begin
              // <font color="...">
              if Attrs.ContainsKey('color') and TryParseHTMLColor(Attrs['color'], ParsedColor) then
                NewState.FontColor := ParsedColor;

              // <font face="..." or family="...">
              if Attrs.ContainsKey('face') then
                NewState.FontFamily := Attrs['face']
              else if Attrs.ContainsKey('family') then
                NewState.FontFamily := Attrs['family'];

              // <font size="...">
              if Attrs.ContainsKey('size') then
                NewState.FontSize := ParseFontSize(Attrs['size'], NewState.FontSize, BaseFontSize);

              // <font letterspacing="...">
              if Attrs.ContainsKey('letterspacing') or Attrs.ContainsKey('letter-spacing') then
              begin
                var LsVal := Attrs.ContainsKey('letterspacing');
                var KeyStr := IfThenString(LsVal, 'letterspacing', 'letter-spacing');
                var SpacingVal: Single;
                if TryStrToFloat(Attrs[KeyStr], SpacingVal, TFormatSettings.Invariant) then
                  NewState.LetterSpacing := SpacingVal;
              end;
            end;

            // Direct Color attribute on tags like <span color="red">
            if Attrs.ContainsKey('color') and (TagName <> 'font') and TryParseHTMLColor(Attrs['color'], ParsedColor) then
              NewState.FontColor := ParsedColor;

            // Direct BGColor attribute on tags like <p bgcolor="yellow">
            if Attrs.ContainsKey('bgcolor') and TryParseHTMLColor(Attrs['bgcolor'], ParsedColor) then
              NewState.BackgroundColor := ParsedColor;

            // --- Apply Inline CSS style="..." (supported on ANY tag) ---
            if Attrs.ContainsKey('style') then
              ApplyInlineCSS(Attrs['style'], NewState, BaseFontSize);

            if not IsSelfClosing and (TagName <> 'br') and (TagName <> 'hr') and (TagName <> 'img') then
            begin
              StateStack.Add(NewState);
              CurrentState := NewState;
            end;
          end;
        end;
      end;
      Inc(I);
    end;

    // Trailing text segment after the last tag
    if StartIdx <= Len then
    begin
      Segment := Copy(FHTMLText, StartIdx, Len - StartIdx + 1);
      AddWordSegment(Segment, CurrentState);
    end;
  finally
    Attrs.Free;
    StateStack.Free;
  end;

  if FAutosizeHeightToContents then
    AdjustHeightToContents;
end;

procedure TSkHtmlLabel.Resize;
begin
  AdjustHeightToContents;
end;

end.
