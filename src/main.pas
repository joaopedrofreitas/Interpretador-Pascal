program Main;

{$mode objfpc}{$H+}

uses
  SysUtils,
  Lexer,
  LexemeUnit,
  Parser;

var
  lexemes: TLexemeArray;
  i: Integer;
  lex: TLexeme;
  L: TLexer;
  P: TParser;

begin
  if ParamCount <> 1 then
  begin
    WriteLn('Usage: ', ExtractFileName(ParamStr(0)), ' <program>');
    Halt(1);
  end;

  try
    L := TLexer.Create();
    try
      lexemes := L.ScanFile(ParamStr(1));
      P := TParser.Create(lexemes);
    
      for i := 0 to High(lexemes) do
      begin
        lex := lexemes[i];
        WriteLn(lex.str);
      end;
      
      P.start();
      
      WriteLn('As análises syntática e léxica não encontraram erro!');
    finally
      L.Free;
    end;
  except
    on E: ELexicalError do
      WriteLn('Lexical Error: ', E.Message);
    on E: ESyntaticalError do
          WriteLn('Syntatic Error: ', E.Message);
    on E: Exception do
      WriteLn('Error: ', E.ClassName, ': ', E.Message);
  end;
end.


