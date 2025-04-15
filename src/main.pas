program Interpreter;

uses
  crt, TokenType;

type
  Lexeme = record
    TokenType: token;
    token: string;
    line_lex: integer;
    column_lex: integer;
  

var
  token: TokenType;  
begin
  token:= TT_ADD;
  writeln('Token ordinal: ', Ord(token));
  
end.
