program Main;

{$mode objfpc}{$H+}

uses
    SysUtils,
    Lexer;  // Unit where TLexer is declared

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
        // Create lexer and scan file
        with TLexer.Create do
        try
            lexemes := ScanFile(ParamStr(1));

            // Print each lexeme's token
            for i := 0 to High(lexemes) do
            begin
                lex := lexemes[i];
                WriteLn('Token: ', lex.Token);
            end;
        finally
            Free;
        end;
    except
        on E: ELexicalError do
            WriteLn('Error: ', E.Message);
        on E: Exception do
            WriteLn('Error: ', E.ClassName, ': ', E.Message);
    end;
end.
