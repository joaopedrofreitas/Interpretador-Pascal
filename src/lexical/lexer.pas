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
  Result := ((C >= 'A') and (C <= 'Z')) or ((C >= 'a') and (C <= 'z'));
end;

function IsDigit(C: AnsiChar): Boolean;
begin
  Result := (C >= '0') and (C <= '9');
end;

function IsHex(C: AnsiChar): Boolean;
begin
  Result := ((C >= '0') and (C <= '9')) or ((C >= 'A') and (C <= 'F'));
end;

function IsAlphaNum(C: AnsiChar): Boolean;
begin
  Result := IsAlpha(C) or IsDigit(C);
end;

function IsOctal(C: AnsiChar): Boolean;
begin
  Result := (C >= '0') and (C <= '7');
end;

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

function TLexer.ScanFile(const AFileName: string): TLexemeArray;
var
  L: TLexeme;
begin
  SetLength(LexerLexemes, 0);
  LexerFile.Open(AFileName);
  try
    repeat
      
      L := MakeLexeme;
      if L.TokenType <> TT_END_OF_FILE then
        begin
          SetLength(LexerLexemes, Length(LexerLexemes) + 1);  // redimensiona o array dinâmico :contentReference[oaicite:2]{index=2}
          LexerLexemes[High(LexerLexemes)] := L;
        end
        
    until L.TokenType = TT_END_OF_FILE;
    
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
  symbol: TokenT;
begin
  L := TLexeme.Create();
  L.token := '';
  L.Line := LexerFile.Line;
  L.Column := LexerFile.Column;
  L.TokenType := TT_EOF;
  C := LexerFile.Peek();
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
          C := LexerFile.Advance()

        { this can be either a variable name or a keyword }
        else if IsAlpha(C) then
        begin
          L.token := L.token + C;
          L.Line := LexerFile.Line;
          L.Column := LexerFile.Column;
          C := LexerFile.Advance();
          State := STATE_ALNUM;
        end

        { this can be a octal, hexadecimal or a real number }
        else if C = '0' then
        begin
          L.token := L.token + C;
          L.Line := LexerFile.Line;
          L.Column := LexerFile.Column;
          C := LexerFile.Advance();
          State := STATE_ZERO;
        end

        { this can be either a decimal or real number }
        else if IsDigit(C) then
        begin
          L.token := L.token + C;
          L.Line := LexerFile.Line;
          L.Column := LexerFile.Column;
          C := LexerFile.Advance();
          State := STATE_DIGIT;
        end

       
        { this can be either a division or a single-line comment }
        else if C = '/' then
        begin
          L.token := L.token + C;
          L.Line := LexerFile.Line;
          L.Column := LexerFile.Column;
          C := LexerFile.Advance();
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
          C := LexerFile.Advance();
          State := STATE_COLON;
        end

        { can be less than (<), less or equal (<=) or difference (<>) }
        else if C = '<' then
        begin
          L.token := L.token + C;
          L.Line := LexerFile.Line;
          L.Column := LexerFile.Column;
          C := LexerFile.Advance();
          State := STATE_LESS_THAN;
        end

        { this can be either a greater than (>) or greater or equal (>=) }
        else if C = '>' then
        begin
          L.token := L.token + C;
          L.Line := LexerFile.Line;
          L.Column := LexerFile.Column;
          C := LexerFile.Advance();
          State := STATE_GREATER_THAN;
        end

        { this is a equal to relational operator }
        { (both '=' and '==' are 'equal to') }
        else if C = '=' then
        begin
          L.token := L.token + C;
          L.Line := LexerFile.Line;
          L.Column := LexerFile.Column;
          C := LexerFile.Advance();
          State := STATE_EQUAL;
        end

        { this is the start of a literal string. this token finishes }
        { when another " is found. there must be a closing " before a }
        { new line }
        else if C = '"' then
        begin
          L.Line := LexerFile.Line;
          L.Column := LexerFile.Column;
          C := LexerFile.Advance();
          State := STATE_STRING;
        end
        
        { '+', '-', '*', ';', ',', '.', '(', ')' }
        { check if 'c' is one of those symbols }
        else if ((Ord(C) >= 40) and (Ord(C) <= 46)) or (Ord(C) = 59) then
        begin
          L.token := L.token + C;
          L.Line := LexerFile.Line;
          L.Column := LexerFile.Column;
          L.TokenType := SymbolUnit.Find(L.token);
          C := LexerFile.Advance();
          State := STATE_FINAL;
        end

        { end of file or invalid character }
        else
        begin
          L.token := L.token + C;
          raise ELexicalError.Create(LexicalError('invalid token', L));
        end
      end;

      STATE_ALNUM: begin
        if IsAlphaNum(C) then
        begin
          L.token := L.token + C;
          C := LexerFile.Advance();
        end

        else
        begin
          { check if the token is in SymbolTable. if it doesn't, it's }
          { a variable name }
          symbol := SymbolUnit.Find(L.token);

          if symbol <> TT_INVALID then
            L.TokenType := symbol
          else
            L.TokenType := TT_IDENT;
            
          State := STATE_FINAL;
        end;
      end;

      STATE_ZERO: begin
        if IsOctal(C) then
        begin
          L.token := L.token + C;
          C := LexerFile.Advance();
          State := STATE_OCTAL;
        end

        else if C = 'x' then
        begin
          L.token := L.token + C;
          C := LexerFile.Advance();
          State := STATE_HEX;
        end

        else if C = '.' then
        begin
          L.token := L.token + C;
          C := LexerFile.Advance();
          State := STATE_REAL;
        end

        else if (C = '8') or (C = '9') then
        begin
          begin
            L.token := L.token + C;
            raise ELexicalError.Create(LexicalError('8 and 9 are not octal digits', L));
          end
        end

        else if IsAlpha(C) then
        begin
          L.token := L.token + C;
          raise ELexicalError.Create(LexicalError('unexpected alphabetical character', L))
        end

        else
        begin
          L.TokenType := TT_LITERAL_DEC;
          State := STATE_FINAL;
        end;
      end;

      STATE_OCTAL: begin
        if IsOctal(C) then
        begin
          L.token := L.token + C;
          C := LexerFile.Advance();
        end

        else if (C = '8') or (C = '9') then
        begin
          L.token := L.token + C;
          raise ELexicalError.Create(LexicalError('8 and 9 are not octal digits', L));
        end

        else if IsAlpha(C) then
        begin
          L.token := L.token + C;
          raise ELexicalError.Create(LexicalError('unexpected alphabetical character', L));
        end

        else
        begin
          L.TokenType := TT_LITERAL_OCT;
          State := STATE_FINAL;
        end;
      end;

      STATE_HEX: begin
        if IsHex(C) then
        begin
          L.token := L.token + C;
          C := LexerFile.Advance();
        end

        else if (C >= 'a') and (C <= 'f') then
        begin
          L.token := L.token + C;
          raise ELexicalError.Create(LexicalError('hexadecimals must use upper case letters', L));
        end

        else if IsAlpha(C) then
        begin
          L.token := L.token + C;
          raise ELexicalError.Create(LexicalError('unexpected alphabetical character', L));
        end

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
          C := LexerFile.Advance();
        end

        else if C = '.' then
        begin
          L.token := L.token + C;
          C := LexerFile.Advance();
          State := STATE_REAL;
        end

        else if IsAlpha(C) then
        begin
          L.token := L.token + C;
          raise ELexicalError.Create(LexicalError('unexpected alphabetical character', L));
        end

        else
        begin
          L.TokenType := TT_LITERAL_DEC;
          State := STATE_FINAL;
        end;
      end;

      STATE_REAL: begin
        if IsDigit(C) then
        begin
          L.token := L.token + C;
          C := LexerFile.Advance();
        end

        else if IsAlpha(C) then
        begin
          L.token := L.token + C;
          raise ELexicalError.Create(LexicalError('unexpected alphabetical character', L));
        end

        else if (c = '.') then
        begin
          L.token := L.token + C;
          raise ELexicalError.Create(LexicalError('more than one period', L));
        end

        else
        begin
          L.token := L.token + '0';
          L.TokenType := TT_LITERAL_REAL;
          State := STATE_FINAL;
        end;
      end;

      STATE_SLASH: begin
        if C = '/' then
        begin
          L.token := '';
          C := LexerFile.Advance();
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
          C := LexerFile.Advance()

        else
        begin
          C := LexerFile.Advance();
          State := STATE_INITIAL;
        end;
      end;

      STATE_COMMENT_MULTI_LINE: begin
        if C = '}' then
        begin
          C := LexerFile.Advance();
          State := STATE_INITIAL;
        end

        else if C = #0 then
        begin
          L.token := L.token + C;
          raise ELexicalError.Create(LexicalError('unexpected end of file inside comment', L));
        end

        else
          C := LexerFile.Advance();
      end;

      STATE_COLON: begin
        if C = '=' then
        begin
          L.token := L.token + C;
          C := LexerFile.Advance();
        end;

        L.TokenType := SymbolUnit.Find(L.token);
        State := STATE_FINAL;
      end;

      STATE_LESS_THAN: begin
        if C = '=' then
        begin
          L.token := L.token + C;
          C := LexerFile.Advance();
        end

        else if C = '>' then
        begin
          L.token := L.token + C;
          C := LexerFile.Advance();
        end;

        L.TokenType := SymbolUnit.Find(L.token);
        State := STATE_FINAL;
      end;

      STATE_GREATER_THAN: begin
        if C = '=' then
        begin
          L.token := L.token + C;
          C := LexerFile.Advance();
        end;

        L.TokenType := SymbolUnit.Find(L.token);
        State := STATE_FINAL;
      end;

      STATE_EQUAL: begin
        if C = '=' then
          begin
            L.token := L.token + C;
            C := LexerFile.Advance();
          end;
        L.TokenType := TT_EQL;
        State := STATE_FINAL;
      end;
             
      STATE_STRING: begin
        if C = '"' then
        begin
          C := LexerFile.Advance();
          L.TokenType := TT_LITERAL_STR;
          State := STATE_FINAL;
        end

        else if C = #0 then
        begin
          L.token := L.token + C;
          raise ELexicalError.Create(LexicalError('unexpected end of file inside string', L));
        end

        else if C = #10 then   
        begin
          L.token := L.token + C;
          raise ELexicalError.Create(LexicalError('new line while trying to tokenize string literal', L));
        end

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
          begin
            L.token := L.token + C;
            raise ELexicalError.Create(LexicalError('not defined escape code', L));
          end
          end;

          C := LexerFile.Advance();
          State := STATE_STRING;
        end

        else
        begin
          L.token := L.token + C;
          C := LexerFile.Advance();
          State := STATE_STRING;
        end;
      end;      
    end;  // case State
  Result := L;
end;

function TLexer.LexicalError(const Msg: string; var L: TLexeme): string;
begin
  Result := Format(
    'error at line %d, column %d: %s  (token so far: "%s")',
    [L.line, L.Column, Msg, L.token]
  );
end;

end.
