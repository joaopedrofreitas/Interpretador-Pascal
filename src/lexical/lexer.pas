
unit Lexer;

{$mode objfpc}{$H+}

interface

uses
    SysUtils,
    File,       
    SymbolTable,
    Lexeme;

type
    // Exception for lexical errors
    ELexicalError = class(Exception);

    TLexer = class
    private
        LexerFile: TFile;
        LexerLexemes: TArray<TLexeme>;

        function MakeLexeme: TLexeme;
        function LexicalError(const Msg: string; var L: TLexeme): string;
    public
        constructor Create;
        destructor Destroy; override;

        function ScanFile(const AFileName: string): TArray<TLexeme>;
        property Lexemes: TArray<TLexeme> read LexerLexemes;
    end;

implementation

// State machine states
type
    TState = (
        STATE_INITIAL,
        STATE_ALNUM,
        STATE_ZERO,
        STATE_OCTAL,
        STATE_HEX,
        STATE_DIGIT,
        STATE_REAL,
        STATE_SLASH,
        STATE_COMMENT_SINGLE_LINE,
        STATE_COMMENT_MULTI_LINE,
        STATE_COLON,
        STATE_LESS_THAN,
        STATE_GREATER_THAN,
        STATE_EQUAL,
        STATE_STRING,
        STATE_FINAL
    );

// Helper functions

function IsOctal(C: AnsiChar): Boolean;
begin
    Result := (C >= '0') and (C <= '7');
end;

function IsHex(C: AnsiChar): Boolean;
begin
    Result := (C in ['0'..'9', 'A'..'F']);
end;

{ TLexer }

constructor TLexer.Create;
begin
    inherited Create;
    LexerFile := TFile.Create();
end;

destructor TLexer.Destroy;
begin
    LexerFile.Free;
    inherited Destroy;
end;

function TLexer.ScanFile(const AFileName: string): TArray<TLexeme>;
begin
    SetLength(LexerLexemes, 0);
    LexerFile.Open(AFileName);
    try
        while not LexerFile.IsAtEOF() do
        begin
            var L := MakeLexeme;
            if L.&Type <> TT_END_OF_FILE then
            begin
                // Add lexeme to array
                SetLength(LexerLexemes, Length(LexerLexemes) + 1);
                LexerLexemes[High(LexerLexemes)] := L;
            end;
        end;
    finally
        LexerFile.Close;
    end;

    Result := LexerLexemes;
end;

function TLexer.MakeLexeme: TLexeme;
var
    L: TLexeme;
    C: AnsiChar;
    State: TState;
begin
    // Initialize lexeme
    L.Token := '';
    L.Line := LexerFile.Line;
    L.Column := LexerFile.Column;
    L.&Type := TT_VAR_NAME;

    C := LexerFile.Peek();
    State := STATE_INITIAL;

    while State <> STATE_FINAL do
    begin
        case State of
            STATE_INITIAL:
                if C <= ' ' then
                    C := LexerFile.Advance()
                else if C in ['A'..'Z', 'a'..'z'] then
                begin
                    L.Token := L.Token + C;
                    L.Line := LexerFile.Line;
                    L.Column := LexerFile.Column;
                    C := LexerFile.Advance();
                    State := STATE_ALNUM;
                end
                else if C = '0' then
                begin
                    L.Token := L.Token + C;
                    L.Line := LexerFile.Line;
                    L.Column := LexerFile.Column;
                    C := LexerFile.Advance();
                    State := STATE_ZERO;
                end
                else if C in ['1'..'9'] then
                begin
                    L.Token := L.Token + C;
                    L.Line := LexerFile.Line;
                    L.Column := LexerFile.Column;
                    C := LexerFile.Advance();
                    State := STATE_DIGIT;
                end
                else if C in ['+','-','*',';',',','.','(',')'] then
                begin
                    L.Token := L.Token + C;
                    L.Line := LexerFile.Line;
                    L.Column := LexerFile.Column;
                    L.&Type := SymbolTable.Find(L.Token);
                    C := LexerFile.Advance();
                    State := STATE_FINAL;
                end
                else if C = '/' then
                begin
                    L.Token := L.Token + C;
                    L.Line := LexerFile.Line;
                    L.Column := LexerFile.Column;
                    C := LexerFile.Advance();
                    State := STATE_SLASH;
                end
                else if C = '{' then
                begin
                    C := LexerFile.Advance();
                    State := STATE_COMMENT_MULTI_LINE;
                end
                else if C = ':' then
                begin
                    L.Token := L.Token + C;
                    L.Line := LexerFile.Line;
                    L.Column := LexerFile.Column;
                    C := LexerFile.Advance();
                    State := STATE_COLON;
                end
                else if C = '<' then
                begin
                    L.Token := L.Token + C;
                    L.Line := LexerFile.Line;
                    L.Column := LexerFile.Column;
                    C := LexerFile.Advance();
                    State := STATE_LESS_THAN;
                end
                else if C = '>' then
                begin
                    L.Token := L.Token + C;
                    L.Line := LexerFile.Line;
                    L.Column := LexerFile.Column;
                    C := LexerFile.Advance();
                    State := STATE_GREATER_THAN;
                end
                else if C = '=' then
                begin
                    L.Token := L.Token + C;
                    L.Line := LexerFile.Line;
                    L.Column := LexerFile.Column;
                    C := LexerFile.Advance();
                    State := STATE_EQUAL;
                end
                else if C = '"' then
                begin
                    L.Line := LexerFile.Line;
                    L.Column := LexerFile.Column;
                    C := LexerFile.Advance();
                    State := STATE_STRING;
                end
                else if C = #0 then
                begin
                    L.Line := LexerFile.Line;
                    L.Column := LexerFile.Column;
                    L.&Type := TT_END_OF_FILE;
                    State := STATE_FINAL;
                end
                else
                    raise ELexicalError.Create(LexicalError('invalid token', L));

            STATE_ALNUM:
                if C.IsLetterOrDigit() then
                begin
                    L.Token := L.Token + C;
                    C := LexerFile.Advance();
                end
                else
                begin
                    L.&Type := SymbolTable.Find(L.Token);
                    State := STATE_FINAL;
                end;

            STATE_ZERO:
                if IsOctal(C) then
                begin
                    L.Token := L.Token + C;
                    C := LexerFile.Advance();
                    State := STATE_OCTAL;
                end
                else if C = 'x' then
                begin
                    L.Token := L.Token + C;
                    C := LexerFile.Advance();
                    State := STATE_HEX;
                end
                else if C = '.' then
                begin
                    L.Token := L.Token + C;
                    C := LexerFile.Advance();
                    State := STATE_REAL;
                end
                else if C.IsLetter() then
                    raise ELexicalError.Create(LexicalError('unexpected alphabetical character', L))
                else
                begin
                    L.&Type := TT_LITERAL_DECIMAL;
                    State := STATE_FINAL;
                end;

            STATE_OCTAL:
                if IsOctal(C) then
                begin
                    L.Token := L.Token + C;
                    C := LexerFile.Advance();
                end
                else if C.IsLetter then
                    raise ELexicalError.Create(LexicalError('unexpected alphabetical character', L))
                else
                begin
                    L.&Type := TT_LITERAL_OCTAL;
                    State := STATE_FINAL;
                end;

            STATE_HEX:
                if IsHex(C) then
                begin
                    L.Token := L.Token + C;
                    C := LexerFile.Advance();
                end
                else if (C >= 'a') and (C <= 'f') then
                    raise ELexicalError.Create(LexicalError('hexadecimals must use upper case letters', L))
                else if C.IsLetter then
                    raise ELexicalError.Create(LexicalError('unexpected alphabetical character', L))
                else
                begin
                    L.&Type := TT_LITERAL_HEX;
                    State := STATE_FINAL;
                end;

            STATE_DIGIT:
                if C.IsDigit() then
                begin
                    L.Token := L.Token + C;
                    C := LexerFile.Advance();
                end
                else if C = '.' then
                begin
                    L.Token := L.Token + C;
                    C := LexerFile.Advance();
                    State := STATE_REAL;
                end
                else if C.IsLetter() then
                    raise ELexicalError.Create(LexicalError('unexpected alphabetical character', L))
                else
                begin
                    L.&Type := TT_LITERAL_DECIMAL;
                    State := STATE_FINAL;
                end;

            STATE_REAL:
                if C.IsDigit() then
                begin
                    L.Token := L.Token + C;
                    C := LexerFile.Advance();
                end
                else if C.IsLetter then
                    raise ELexicalError.Create(LexicalError('unexpected alphabetical character', L))
                else
                begin
                    L.&Type := TT_LITERAL_REAL;
                    State := STATE_FINAL;
                end;

            STATE_SLASH:
                if C = '/' then
                begin
                    C := LexerFile.Advance();
                    State := STATE_COMMENT_SINGLE_LINE;
                end
                else
                begin
                    L.&Type := SymbolTable.Find(L.Token);
                    State := STATE_FINAL;
                end;

            STATE_COMMENT_SINGLE_LINE:
                if (C <> #10) and (C <> #0) then
                begin
                    C := LexerFile.Advance();
                end
                else
                begin
                    C := LexerFile.Advance();
                    State := STATE_INITIAL;
                end;

            STATE_COMMENT_MULTI_LINE:
                if C = '}' then
                begin
                    C := LexerFile.Advance();
                    State := STATE_INITIAL;
                end
                else if C = #0 then
                    raise ELexicalError.Create(LexicalError('unexpected end of file inside comment', L))
                else
                    C := LexerFile.Advance();

            STATE_COLON:
                if C = '=' then
                begin
                    L.Token := L.Token + C;
                    C := LexerFile.Advance();
                end;
                L.&Type := SymbolTable.Find(L.Token);
                State := STATE_FINAL;

            STATE_LESS_THAN:
                if C = '=' then
                begin
                    L.Token := L.Token + C;
                    C := LexerFile.Advance();
                end
                else if C = '>' then
                begin
                    L.Token := L.Token + C;
                    C := LexerFile.Advance();
                end;
                L.&Type := SymbolTable.Find(L.Token);
                State := STATE_FINAL;

            STATE_GREATER_THAN:
                if C = '=' then
                begin
                    L.Token := L.Token + C;
                    C := LexerFile.Advance();
                end;
                L.&Type := SymbolTable.Find(L.Token);
                State := STATE_FINAL;

            STATE_EQUAL:
                L.&Type := SymbolTable.Find(L.Token);
                State := STATE_FINAL;

            STATE_STRING:
                if C = '"' then
                begin
                    C := LexerFile.Advance();
                    L.&Type := TT_LITERAL_STRING;
                    State := STATE_FINAL;
                end
                else if C = #0 then
                    raise ELexicalError.Create(LexicalError('unexpected end of file inside string', L))
                else
                begin
                    L.Token := L.Token + C;
                    C := LexerFile.Advance();
                end;
        end;
    end;

    Result := L;
end;

function TLexer.LexicalError(const Msg: string; var L: TLexeme): string;
begin
    Result := Format('Lexical error at line %d, column %d: %s (token so far: "%s")',
        [L.Line, L.Column, Msg, L.Token]);
end;

end.
