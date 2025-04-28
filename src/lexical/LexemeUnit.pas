unit LexemeUnit;

{$mode objfpc}

interface

uses TokenType, SysUtils;

type
  TLexeme = class
  public
    TokenType: TokenT;
    token: string;
    line: integer;
    column: integer;
    constructor Create;
    function str: string;
  end;

implementation

constructor TLexeme.Create;
begin
  token := '';
  TokenType := TT_END_OF_FILE;
  line := 0;
  column := 0;
end;

function TLexeme.str: string;
begin
  Result := '(' + tt2str(TokenType) + ', ' + token 
    + ', "' + IntToStr(line) + '", ' + IntToStr(column) + ')';
end;

end.
