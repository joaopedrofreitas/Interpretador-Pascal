unit Lexer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MyFile, SymbolUnit, LexemeUnit, TokenType;

type
  ELexicalError = class(Exception); // Definição da exceção adicionada

  TLexer = class
  private
    FFile: TFile;
    FLexemes: TLexemeArray;
    
    function MakeLexeme: TLexeme;
    function CreateLexicalError(const Msg: string; const L: TLexeme): string;
  public
    constructor Create;
    destructor Destroy; override;
    function ScanFile(const FileName: string): TLexemeArray;
    property Lexemes: TLexemeArray read FLexemes;
  end;

implementation

type
  TState = (
    stInitial,
    stAlnum,
    stZero,
    stOctal,
    stHex,
    stDigit,
    stReal,
    stSlash,
    stCommentSingleLine,
    stCommentMultiLine,
    stColon,
    stLessThan,
    stGreaterThan,
    stEqual,
    stString,
    stFinal
  );

{ Helper functions }
function IsAlpha(C: Char): Boolean;
begin
  Result := (C in ['A'..'Z']) or (C in ['a'..'z']);
end;

function IsDigit(C: Char): Boolean;
begin
  Result := C in ['0'..'9'];
end;

function IsHex(C: Char): Boolean;
begin
  Result := IsDigit(C) or (C in ['A'..'F']) or (C in ['a'..'f']);
end;

function IsAlphaNum(C: Char): Boolean;
begin
  Result := IsAlpha(C) or IsDigit(C);
end;

function IsOctal(C: Char): Boolean;
begin
  Result := C in ['0'..'7'];
end;

constructor TLexer.Create;
begin
  FFile := TFile.Create;
  SetLength(FLexemes, 0);
end;

destructor TLexer.Destroy;
begin
  FFile.Free;
  inherited Destroy;
end;

function TLexer.ScanFile(const FileName: string): TLexemeArray;
var
  Lexeme: TLexeme;
begin
  FFile.Open(FileName);
  try
    while not FFile.IsAtEOF do
    begin
      Lexeme := MakeLexeme;
      if Lexeme.TokenType <> ttEOF then
      begin
        SetLength(FLexemes, Length(FLexemes) + 1);
        FLexemes[High(FLexemes)] := Lexeme;
      end;
    end;
  finally
    FFile.Close;
  end;
  Result := FLexemes;
end;

function TLexer.MakeLexeme: TLexeme;
var
  State: TState;
  C: Char;
  Lex: TLexeme;
begin
  Lex := TLexeme.Create;
  State := stInitial;
  C := FFile.Peek;

  while State <> stFinal do
  begin
    case State of
      stInitial:
        begin
          if FFile.IsAtEOF then
          begin
            Lex.TokenType := ttEOF;
            State := stFinal;
          end
          else if C <= ' ' then
          begin
            FFile.Advance;
            C := FFile.Peek;
          end
          else if IsAlpha(C) then
          begin
            Lex.token := C;
            Lex.Line := FFile.Line;
            Lex.Column := FFile.Column;
            FFile.Advance;
            C := FFile.Peek;
            State := stAlnum;
          end
          else if C = '0' then
          begin
            Lex.token := C;
            Lex.Line := FFile.Line;
            Lex.Column := FFile.Column;
            FFile.Advance;
            C := FFile.Peek;
            State := stZero;
          end
          else if IsDigit(C) then
          begin
            Lex.token := C;
            Lex.Line := FFile.Line;
            Lex.Column := FFile.Column;
            FFile.Advance;
            C := FFile.Peek;
            State := stDigit;
          end
          else if C = '/' then
          begin
            Lex.token := C;
            Lex.Line := FFile.Line;
            Lex.Column := FFile.Column;
            FFile.Advance;
            C := FFile.Peek;
            State := stSlash;
          end
          else if C = '{' then
          begin
            FFile.Advance;
            C := FFile.Peek;
            State := stCommentMultiLine;
          end
          else if C = ':' then
          begin
            Lex.token := C;
            Lex.Line := FFile.Line;
            Lex.Column := FFile.Column;
            FFile.Advance;
            C := FFile.Peek;
            State := stColon;
          end
          else if C = '<' then
          begin
            Lex.token := C;
            Lex.Line := FFile.Line;
            Lex.Column := FFile.Column;
            FFile.Advance;
            C := FFile.Peek;
            State := stLessThan;
          end
          else if C = '>' then
          begin
            Lex.token := C;
            Lex.Line := FFile.Line;
            Lex.Column := FFile.Column;
            FFile.Advance;
            C := FFile.Peek;
            State := stGreaterThan;
          end
          else if C = '=' then
          begin
            Lex.token := C;
            Lex.Line := FFile.Line;
            Lex.Column := FFile.Column;
            FFile.Advance;
            C := FFile.Peek;
            State := stEqual;
          end
          else if C = '"' then
          begin
            Lex.Line := FFile.Line;
            Lex.Column := FFile.Column;
            FFile.Advance;
            C := FFile.Peek;
            State := stString;
          end
          else if C in [';', ',', '.', '(', ')', '+', '-', '*'] then
          begin
            Lex.token := C;
            Lex.Line := FFile.Line;
            Lex.Column := FFile.Column;
            Lex.TokenType := SymbolUnit.Find(Lex.token);
            if Lex.TokenType = ttINVALID then
              raise ELexicalError.Create(CreateLexicalError('Invalid token', Lex));
            FFile.Advance;
            State := stFinal;
          end
          else
            raise ELexicalError.Create(CreateLexicalError('Invalid token', Lex));
        end;

      stAlnum:
        begin
          if IsAlphaNum(C) then
          begin
            Lex.token := Lex.token + C;
            FFile.Advance;
            C := FFile.Peek;
          end
          else
          begin
            Lex.TokenType := SymbolUnit.Find(Lex.token);
            if Lex.TokenType = ttINVALID then
              Lex.TokenType := ttIDENT;
            State := stFinal;
          end;
        end;

      stZero:
        begin
          if IsOctal(C) then
          begin
            Lex.token := Lex.token + C;
            FFile.Advance;
            C := FFile.Peek;
            State := stOctal;
          end
          else if C = 'x' then
          begin
            Lex.token := Lex.token + C;
            FFile.Advance;
            C := FFile.Peek;
            State := stHex;
          end
          else if C = '.' then
          begin
            Lex.token := Lex.token + C;
            FFile.Advance;
            C := FFile.Peek;
            State := stReal;
          end
          else if C in ['8', '9'] then
            raise ELexicalError.Create(CreateLexicalError('8 and 9 are not octal digits', Lex))
          else if IsAlpha(C) then
            raise ELexicalError.Create(CreateLexicalError('Unexpected alphabetical character', Lex))
          else
          begin
            Lex.TokenType := ttLITERAL_DEC;
            State := stFinal;
          end;
        end;

      stOctal:
        begin
          if IsOctal(C) then
          begin
            Lex.token := Lex.token + C;
            FFile.Advance;
            C := FFile.Peek;
          end
          else if C in ['8', '9'] then
            raise ELexicalError.Create(CreateLexicalError('8 and 9 are not octal digits', Lex))
          else if IsAlpha(C) then
            raise ELexicalError.Create(CreateLexicalError('Unexpected alphabetical character', Lex))
          else
          begin
            Lex.TokenType := ttLITERAL_OCT;
            State := stFinal;
          end;
        end;

      stHex:
        begin
          if IsHex(C) then
          begin
            Lex.token := Lex.token + C;
            FFile.Advance;
            C := FFile.Peek;
          end
          else if C in ['a'..'f'] then
            raise ELexicalError.Create(CreateLexicalError('Hexadecimals must use uppercase letters', Lex))
          else if IsAlpha(C) then
            raise ELexicalError.Create(CreateLexicalError('Unexpected alphabetical character', Lex))
          else
          begin
            Lex.TokenType := ttLITERAL_HEX;
            State := stFinal;
          end;
        end;

      stDigit:
        begin
          if IsDigit(C) then
          begin
            Lex.token := Lex.token + C;
            FFile.Advance;
            C := FFile.Peek;
          end
          else if C = '.' then
          begin
            Lex.token := Lex.token + C;
            FFile.Advance;
            C := FFile.Peek;
            State := stReal;
          end
          else if IsAlpha(C) then
            raise ELexicalError.Create(CreateLexicalError('Unexpected alphabetical character', Lex))
          else
          begin
            Lex.TokenType := ttLITERAL_DEC;
            State := stFinal;
          end;
        end;

      stReal:
        begin
          if IsDigit(C) then
          begin
            Lex.token := Lex.token + C;
            FFile.Advance;
            C := FFile.Peek;
          end
          else if IsAlpha(C) then
            raise ELexicalError.Create(CreateLexicalError('Unexpected alphabetical character', Lex))
          else if C = '.' then
            raise ELexicalError.Create(CreateLexicalError('Real number with more than one period', Lex))
          else
          begin
            Lex.TokenType := ttLITERAL_REAL;
            State := stFinal;
          end;
        end;

      stSlash:
        begin
          if C = '/' then
          begin
            Lex.token := '';
            FFile.Advance;
            C := FFile.Peek;
            State := stCommentSingleLine;
          end
          else
          begin
            Lex.TokenType := ttDIV;
            State := stFinal;
          end;
        end;

      stCommentSingleLine:
        begin
          if (C = #10) or (C = #0) then
            State := stInitial
          else
          begin
            FFile.Advance;
            C := FFile.Peek;
          end;
        end;

      stCommentMultiLine:
        begin
          if C = '}' then
          begin
            FFile.Advance;
            C := FFile.Peek;
            State := stInitial;
          end
          else if C = #0 then
            raise ELexicalError.Create(CreateLexicalError('Unexpected end of file inside comment', Lex))
          else
          begin
            FFile.Advance;
            C := FFile.Peek;
          end;
        end;

      stColon:
        begin
          if C = '=' then
          begin
            Lex.token := Lex.token + C;
            FFile.Advance;
            Lex.TokenType := ttASSIGN;
          end
          else
            Lex.TokenType := ttCOLON;
          State := stFinal;
        end;

      stLessThan:
        begin
          if C = '=' then
          begin
            Lex.token := Lex.token + C;
            FFile.Advance;
            Lex.TokenType := ttLEQ;
          end
          else if C = '>' then
          begin
            Lex.token := Lex.token + C;
            FFile.Advance;
            Lex.TokenType := ttNEQ;
          end
          else
            Lex.TokenType := ttLSS;
          State := stFinal;
        end;

      stGreaterThan:
        begin
          if C = '=' then
          begin
            Lex.token := Lex.token + C;
            FFile.Advance;
            Lex.TokenType := ttGEQ;
          end
          else
            Lex.TokenType := ttGTR;
          State := stFinal;
        end;

      stEqual:
        begin
          if C = '=' then
          begin
            Lex.token := Lex.token + C;
            FFile.Advance;
          end;
          Lex.TokenType := ttEQL;
          State := stFinal;
        end;

      stString:
        begin
          if C = '"' then
          begin
            FFile.Advance;
            Lex.TokenType := ttLITERAL_STR;
            State := stFinal;
          end
          else if C = #0 then
            raise ELexicalError.Create(CreateLexicalError('Unexpected end of file inside string', Lex))
          else if C = #10 then
            raise ELexicalError.Create(CreateLexicalError('New line inside string literal', Lex))
          else if (Length(Lex.token) > 0) and (Lex.token[Length(Lex.token)] = '\') then
          begin
            case C of
              'n': Lex.token[Length(Lex.token)] := #10;
              't': Lex.token[Length(Lex.token)] := #9;
              'r': Lex.token[Length(Lex.token)] := #13;
              '\': Lex.token[Length(Lex.token)] := '\';
              '"': Lex.token[Length(Lex.token)] := '"';
              else
                raise ELexicalError.Create(CreateLexicalError('Undefined escape code', Lex));
            end;
            FFile.Advance;
            C := FFile.Peek;
          end
          else
          begin
            Lex.token := Lex.token + C;
            FFile.Advance;
            C := FFile.Peek;
          end;
        end;
    end;
  end;

  Result := Lex;
end;

function TLexer.CreateLexicalError(const Msg: string; const L: TLexeme): string;
begin
  Result := Format('Lexical error at line %d, column %d: %s (token: "%s")',
    [L.Line, L.Column, Msg, L.token]);
end;

end.
