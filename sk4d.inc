{$IF (NOT defined(IOS)) and (NOT defined(SK_DYNAMIC_LINKING))}
  {$DEFINE SK_DYNAMIC_LOADING}
{$ENDIF}

{$IF CompilerVersion < 31}
type
  {$IFDEF NEXTGEN}
  UTF8Char = _AnsiChar;
  {$ELSE}
  UTF8Char = AnsiChar;
  {$ENDIF}
{$ENDIF}
