unit Lexer;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  MyFile,
  SymbolUnit,
  LexemeUnit,
  TokenType;

type
  TLexemeArray = array of TLexeme;

  ELexicalError = class(Exception);

  TLexer = class
  private
    LexerFile: TFile;
    LexerLexemes: TLexemeArray;
    function MakeLexeme: TLexeme;
    function LexicalError(const Msg: string; var L: TLexeme): string;
  public
    constructor Create;
    destructor Destroy; override;
    function ScanFile(const AFileName: string): TLexemeArray;
    property Lexemes: TLexemeArray read LexerLexemes;
  end;

implementation

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

function IsAlpha(C: AnsiChar): Boolean;
begin
  Result := C in ['A'..'Z', 'a'..'z'];
end;

function IsDigit(C: AnsiChar): Boolean;
begin
  Result := C in ['0'..'9'];
end;

function IsAlphaNum(C: AnsiChar): Boolean;
begin
  Result := IsAlpha(C) or IsDigit(C);
end;

function IsOctal(C: AnsiChar): Boolean;
begin
  Result := (C >= '0') and (C <= '7');
end;

function IsHex(C: AnsiChar): Boolean;
begin
  Result := C in ['0'..'9', 'A'..'F'];
end;

constructor TLexer.Create;
begin
  inherited Create;
  LexerFile := TFile.Create;
end;

destructor TLexer.Destroy;
begin
  LexerFile.Free;
  inherited Destroy;
end;

function TLexer.ScanFile(const AFileName: string): TLexemeArray;
var
  L: TLexeme;
begin
  SetLength(LexerLexemes, 0);
  LexerFile.Open(AFileName);
  try
    repeat
      L := MakeLexeme;
      SetLength(LexerLexemes, Length(LexerLexemes) + 1);  // redimensiona o array dinâmico :contentReference[oaicite:2]{index=2}
      LexerLexemes[High(LexerLexemes)] := L;
    until L.TokenType = TT_END;                  
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
  L := TLexeme.Create;
  L.token := '';
  L.Line := LexerFile.Line;
  L.Column := LexerFile.Column;
  L.TokenType := TT_VAR_NAME;
  C := LexerFile.Peek;
  State := STATE_INITIAL;

  while State <> STATE_FINAL do
    case State of

      STATE_INITIAL: begin
        if C = #0 then                          // trata EOF antes de espaços :contentReference[oaicite:4]{index=4}
        begin
          L.TokenType := TT_END_OF_FILE;
          State := STATE_FINAL;
        end

        { whitespaces are skipped }
        else if C <= ' ' then
          C := LexerFile.Advance

        { this can be either a variable name or a keyword }
        else if IsAlpha(C) then
        begin
          L.token := L.token + C;
          L.Line := LexerFile.Line;
          L.Column := LexerFile.Column;
          C := LexerFile.Advance;
          State := STATE_ALNUM;
        end

        { this can be a octal, hexadecimal or a real number }
        else if C = '0' then
        begin
          L.token := L.token + C;
          L.Line := LexerFile.Line;
          L.Column := LexerFile.Column;
          C := LexerFile.Advance;
          State := STATE_ZERO;
        end

        { this can be either a decimal or real number }
        else if IsDigit(C) then
        begin
          L.token := L.token + C;
          L.Line := LexerFile.Line;
          L.Column := LexerFile.Column;
          C := LexerFile.Advance;
          State := STATE_DIGIT;
        end

        { check if 'c' is one of those symbols }
        else if C in ['+','-','*',';',',','.','(',')'] then
        begin
          L.token := L.token + C;
          L.Line := LexerFile.Line;
          L.Column := LexerFile.Column;
          L.TokenType := SymbolUnit.Find(L.token);
          C := LexerFile.Advance;
          State := STATE_FINAL;
        end

        { this can be either a division or a single-line comment }
        else if C = '/' then
        begin
          L.token := L.token + C;
          L.Line := LexerFile.Line;
          L.Column := LexerFile.Column;
          C := LexerFile.Advance;
          State := STATE_SLASH;
        end

        { beginning of a multi-line comment }
        else if C = '{' then
        begin
          C := LexerFile.Advance;
          State := STATE_COMMENT_MULTI_LINE;
        end

        { this can be either a colon or a assign (:=) }
        else if C = ':' then
        begin
          L.token := L.token + C;
          L.Line := LexerFile.Line;
          L.Column := LexerFile.Column;
          C := LexerFile.Advance;
          State := STATE_COLON;
        end

        { can be less than (<), less or equal (<=) or difference (<>) }
        else if C = '<' then
        begin
          L.token := L.token + C;
          L.Line := LexerFile.Line;
          L.Column := LexerFile.Column;
          C := LexerFile.Advance;
          State := STATE_LESS_THAN;
        end

        { this can be either a greater than (>) or greater or equal (>=) }
        else if C = '>' then
        begin
          L.token := L.token + C;
          L.Line := LexerFile.Line;
          L.Column := LexerFile.Column;
          C := LexerFile.Advance;
          State := STATE_GREATER_THAN;
        end

        { this is a equal to relational operator }
        { (both '=' and '==' are 'equal to') }
        else if C = '=' then
        begin
          L.token := L.token + C;
          L.Line := LexerFile.Line;
          L.Column := LexerFile.Column;
          C := LexerFile.Advance;
          State := STATE_EQUAL;
        end

        { this is the start of a literal string. this token finishes }
        { when another " is found. there must be a closing " before a }
        { new line }
        else if C = '"' then
        begin
          L.Line := LexerFile.Line;
          L.Column := LexerFile.Column;
          C := LexerFile.Advance;
          State := STATE_STRING;
        end

        { end of file or invalid character }
        else
          raise ELexicalError.Create(LexicalError('invalid token', L));
      end;

      STATE_ALNUM: begin
        if IsAlphaNum(C) then
        begin
          L.token := L.token + C;
          C := LexerFile.Advance;
        end

        else
        begin
          { check if the token is in SymbolTable. if it doesn't, it's }
          { a variable name }
          L.TokenType := SymbolUnit.Find(L.token);
          State := STATE_FINAL;
        end;
      end;

      STATE_ZERO: begin
        if IsOctal(C) then
          begin
            L.token := L.token + C;
            C := LexerFile.Advance;
            State := STATE_OCTAL;
          end
        else if C = 'x' then
          begin
            L.token := L.token + C;
            C := LexerFile.Advance;
            State := STATE_HEX;
          end
        else if C = '.' then
          begin
            L.token := L.token + C;
            C := LexerFile.Advance;
            State := STATE_REAL;
          end
        else if (C = '8') or (C = '9') then
          begin
            raise ELexicalError.Create(LexicalError('8 and 9 are not octal digits', L))
          end
        else if IsAlpha(C) then
          raise ELexicalError.Create(LexicalError('unexpected alphabetical character', L))
        else
          begin
            L.TokenType := TT_LITERAL_DECIMAL;
            State := STATE_FINAL;
          end;
      end;

      STATE_OCTAL: begin
        if IsOctal(C) then
          begin
            L.token := L.token + C;
            C := LexerFile.Advance;
          end
        else if IsAlpha(C) then
          raise ELexicalError.Create(LexicalError('unexpected alphabetical character', L))
        else
          begin
            L.TokenType := TT_LITERAL_OCTAL;
            State := STATE_FINAL;
          end;
      end;

      STATE_HEX: begin
        if IsHex(C) then
          begin
            L.token := L.token + C;
            C := LexerFile.Advance;
          end
        else if (C >= 'a') and (C <= 'f') then
          raise ELexicalError.Create(LexicalError('hexadecimals must use upper case letters', L))
        else if IsAlpha(C) then
          raise ELexicalError.Create(LexicalError('unexpected alphabetical character', L))
        else
          begin
            L.TokenType := TT_LITERAL_HEX;
            State := STATE_FINAL;
          end;
      end;

      STATE_DIGIT: begin
        if IsDigit(C) then
          begin
            L.token := L.token + C;
            C := LexerFile.Advance;
          end
        else if C = '.' then
          begin
            L.token := L.token + C;
            C := LexerFile.Advance;
            State := STATE_REAL;
          end
        else if IsAlpha(C) then
          raise ELexicalError.Create(LexicalError('unexpected alphabetical character', L))
        else
          begin
            L.TokenType := TT_LITERAL_DECIMAL;
            State := STATE_FINAL;
          end;
      end;

      STATE_REAL: begin
        if IsDigit(C) then
          begin
            L.token := L.token + C;
            C := LexerFile.Advance;
          end
        else if IsAlpha(C) then
          raise ELexicalError.Create(LexicalError('unexpected alphabetical character', L))
        else
          begin
            L.TokenType := TT_LITERAL_REAL;
            State := STATE_FINAL;
          end;
      end;

      STATE_SLASH: begin
        if C = '/' then
          begin
            L.token := '';
            C := LexerFile.Advance;
            State := STATE_COMMENT_SINGLE_LINE;
          end
        else
          begin
            L.TokenType := SymbolUnit.Find(L.token);
            State := STATE_FINAL;
          end;
      end;

      STATE_COMMENT_SINGLE_LINE: begin
        if (C <> #10) and (C <> #0) then
          C := LexerFile.Advance
        else
          begin
            C := LexerFile.Advance;
            State := STATE_INITIAL;
          end;
      end;

      STATE_COMMENT_MULTI_LINE: begin
        if C = '}' then
          begin
            C := LexerFile.Advance;
            State := STATE_INITIAL;
          end
        else if C = #0 then
          raise ELexicalError.Create(LexicalError('unexpected end of file inside comment', L))
        else
          C := LexerFile.Advance;
      end;

      STATE_COLON: begin
        if C = '=' then
          begin
            L.token := L.token + C;
            C := LexerFile.Advance;
          end;
        L.TokenType := SymbolUnit.Find(L.token);
        State := STATE_FINAL;
      end;

      STATE_LESS_THAN: begin
        if C = '=' then
          begin
            L.token := L.token + C;
            C := LexerFile.Advance;
          end
        else if C = '>' then
          begin
            L.token := L.token + C;
            C := LexerFile.Advance;
          end;
        L.TokenType := SymbolUnit.Find(L.token);
        State := STATE_FINAL;
      end;

      STATE_GREATER_THAN: begin
        if C = '=' then
          begin
            L.token := L.token + C;
            C := LexerFile.Advance;
          end;
        L.TokenType := SymbolUnit.Find(L.token);
        State := STATE_FINAL;
      end;

      STATE_EQUAL: begin
        if C = '=' then
          begin
            L.token := L.token + C;
            C := LexerFile.Advance;
          end;
        L.TokenType := TT_EQUAL;
        State := STATE_FINAL;
      end;
             
      STATE_STRING: begin
          if C = '"' then
          begin
              C := LexerFile.Advance;
              L.TokenType := TT_LITERAL_STRING;
              State := STATE_FINAL;
          end
          else if C = #0 then
              raise ELexicalError.Create(LexicalError('unexpected end of file inside string', L))
          else if C = #10 then   
              raise ELexicalError.Create(LexicalError('new line while trying to tokenize string literal', L))
          else if (Length(L.token) > 0) and (L.token[Length(L.token)] = '\') then
          begin
              // Processamento de escape
              L.token := Copy(L.token, 1, Length(L.token) - 1);  
              case C of
                  'n':  L.token := L.token + #10;  // Nova linha
                  't':  L.token := L.token + #9;   // Tabulação
                  'r':  L.token := L.token + #13;  // Retorno de carro
                  '\':  L.token := L.token + '\';  // Barra invertida literal
                  '"':  L.token := L.token + '"';  // Aspas literal
              else
                  raise ELexicalError.Create(LexicalError('not defined escape code', L));
              end;
              C := LexerFile.Advance;
              State := STATE_STRING;
          end
          else
          begin
              L.token := L.token + C;
              C := LexerFile.Advance;
              State := STATE_STRING;
          end;
      end;      
    end;  // case State
  Result := L;
end;

function TLexer.LexicalError(const Msg: string; var L: TLexeme): string;
begin
  Result := Format(
    'Lexical error at line %d, column %d: %s (token so far: "%s")',
    [L.line, L.Column, Msg, L.token]
  );
end;

end.
