program Interpreter;

uses
  crt, TokenType;

type
  Lexeme = record
    TokenType: TokenT;
    token: string;
    line_lex: integer;
    column_lex: integer;
  end;
  
var
  lex: Lexeme;  
  f: Text;
  line: string;
  
begin
  Assign(f,'input.txt');
  Reset(f);

  while not Eof(f) do
    begin
      Readln(f, line);
      writeln('Line read: ', line);
    end;
  
  lex.TokenType:= TT_SUB;
  writeln('Token ordinal: ', Ord(lex.TokenType));
  
end.
