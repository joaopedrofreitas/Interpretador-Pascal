program Main;

{$mode objfpc}{$H+}

uses
    SysUtils,
    Lexer;  

var
    lexemes: TArray<TLexeme>;
    i: Integer;
    lex: TLexeme;

begin
    // Check command-line arguments
    if ParamCount <> 1 then
    begin
        WriteLn('Usage: ', ExtractFileName(ParamStr(0)), ' <program>');
        Halt(1);
    end;

    try
    lexer := TLexer.Create();
        try
            lexemes := lexer.ScanFile(ParamStr(1));

            // Print each lexeme's token
            for i := 0 to High(lexemes) do
            begin
              lex := lexemes[i];
              WriteLn('Token: ', lex.str());
            end;
          finally
            lexer.Free();
        end;        
    
    except
        on E: ELexicalError do
            WriteLn('Error: ', E.Message);
        on E: Exception do
            WriteLn('Error: ', E.ClassName, ': ', E.Message);
    end;
end.
