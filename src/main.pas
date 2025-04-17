program Interpreter;

uses
  crt,LEXER;
  
var
  lex: Lexeme;  
  f: Text;
  line: string;
  filename: string;
  
begin
  
  if ParamCount < 1 then
    begin
      writeln('Usage: ', ParamStr(0), ' <filename>');
      Halt(1);
  end;

  filename := ParamStr(1);
  
  Assign(f,filename);
  Reset(f);

   
end.
