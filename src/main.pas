program Main;

{$mode objfpc}{$H+}

uses
  SysUtils,
  Lexer,
  LexemeUnit;

var
  lexemes: TLexemeArray;
  i: Integer;
  lex: TLexeme;
  L: TLexer;

begin
  if ParamCount <> 1 then
  begin
    WriteLn('Usage: ', ExtractFileName(ParamStr(0)), ' <program>');
    Halt(1);
  end;

  try
    L := TLexer.Create;
    try
      lexemes := L.ScanFile(ParamStr(1));
      for i := 0 to High(lexemes) do
      begin
        lex := lexemes[i];
        WriteLn(lex.str);
      end;
    finally
      L.Free;
    end;
  except
    on E: ELexicalError do
      WriteLn('Error: ', E.Message);
    on E: Exception do
      WriteLn('Error: ', E.ClassName, ': ', E.Message);
  end;
end.



