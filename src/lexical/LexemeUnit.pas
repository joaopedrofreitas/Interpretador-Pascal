unit LexemeUnit;

interface

uses
  TokenType;

type
  TLexeme = class
  public
    token: string;
    tokenType: TTokenType;
    line: Integer;
    column: Integer;
    constructor Create;
    constructor Create(const aToken: string; aType: TTokenType; aLine, aColumn: Integer);
    function str: string;
  end;

  TLexemeArray = array of TLexeme;

implementation

uses
  SysUtils;

constructor TLexeme.Create;
begin
  token := '';
  tokenType := ttEND_OF_FILE;
  line := 0;
  column := 0;
end;

constructor TLexeme.Create(const aToken: string; aType: TTokenType; aLine, aColumn: Integer);
begin
  token := aToken;
  tokenType := aType;
  line := aLine;
  column := aColumn;
end;

function TLexeme.str: string;
begin
  Result := Format('("%s", %s, %d, %d)',
    [token, TokenTypeToStr(tokenType), line, column]);
end;

end.
