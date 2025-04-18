unit LexemeUnit;

{$mode objfpc}

interface

uses TokenType, SysUtils;

type
  Lexeme = class
  public
    TokenType: TokenT;
    token: string;
    line: integer;
    column: integer;
    constructor Create;
    function str: string;
  end;

implementation

constructor Lexeme.Create;
begin
  token := '';
  TokenType := TT_END_OF_FILE;
  line := 0;
  column := 0;
end;

function Lexeme.str: string;
begin
  Result := '(' + IntToStr(line) + ', ' + IntToStr(column) 
    + ', "' + token + '", ' + tt2str(TokenType) + ')';
end;

end.