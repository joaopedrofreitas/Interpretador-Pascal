unit SymbolUnit;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  TokenType;

function Contains(const Token: string): Boolean;
function Find(const Token: string): TokenT;

implementation

const
  TableSize = 101;  // a prime ≥ number of keywords

var
  Table: array[0..TableSize - 1] of TokenT;

function SimpleHash(const S: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
    Result := (Result * 31 + Ord(UpCase(S[i]))) mod TableSize;
end;

procedure InitTable;
var
  i: Integer;
  h: Integer;
begin
  for i := 0 to TableSize - 1 do
    Table[i] := TT_INVALID;

  h := SimpleHash('+');       Table[h] := TT_ADD;
  h := SimpleHash('-');       Table[h] := TT_SUB;
  h := SimpleHash('*');       Table[h] := TT_MUL;
  h := SimpleHash('/');       Table[h] := TT_DIV;
  h := SimpleHash('mod');     Table[h] := TT_MOD;
  h := SimpleHash('div');     Table[h] := TT_FLOORDIV;
  h := SimpleHash('or');      Table[h] := TT_OR;
  h := SimpleHash('and');     Table[h] := TT_AND;
  h := SimpleHash('not');     Table[h] := TT_NOT;
  h := SimpleHash('==');      Table[h] := TT_EQL;
  h := SimpleHash('<>');      Table[h] := TT_NEQ;
  h := SimpleHash('>');       Table[h] := TT_GTR;
  h := SimpleHash('>=');      Table[h] := TT_GEQ;
  h := SimpleHash('<');       Table[h] := TT_LSS;
  h := SimpleHash('<=');      Table[h] := TT_LEQ;
  h := SimpleHash(':=');      Table[h] := TT_ASSIGN;
  h := SimpleHash(';');       Table[h] := TT_SEMICOLON;
  h := SimpleHash(',');       Table[h] := TT_COMMA;
  h := SimpleHash('.');       Table[h] := TT_PERIOD;
  h := SimpleHash(':');       Table[h] := TT_COLON;
  h := SimpleHash('(');       Table[h] := TT_LPAREN;
  h := SimpleHash(')');       Table[h] := TT_RPAREN;
  h := SimpleHash('"');       Table[h] := TT_QUOTES;
  h := SimpleHash('program'); Table[h] := TT_PROGRAMSYM;
  h := SimpleHash('var');     Table[h] := TT_VARSYM;
  h := SimpleHash('integer'); Table[h] := TT_TYPE_INT;
  h := SimpleHash('real');    Table[h] := TT_TYPE_REAL;
  h := SimpleHash('string');  Table[h] := TT_TYPE_STR;
  h := SimpleHash('begin');   Table[h] := TT_BEGINSYM;
  h := SimpleHash('end');     Table[h] := TT_ENDSYM;
  h := SimpleHash('for');     Table[h] := TT_FORSYM;
  h := SimpleHash('to');      Table[h] := TT_TOSYM;
  h := SimpleHash('while');   Table[h] := TT_WHILESYM;
  h := SimpleHash('do');      Table[h] := TT_DOSYM;
  h := SimpleHash('break');   Table[h] := TT_BREAKSYM;
  h := SimpleHash('continue');Table[h] := TT_CONTINUESYM;
  h := SimpleHash('if');      Table[h] := TT_IFSYM;
  h := SimpleHash('else');    Table[h] := TT_ELSESYM;
  h := SimpleHash('then');    Table[h] := TT_THENSYM;
  h := SimpleHash('write');   Table[h] := TT_WRITESYM;
  h := SimpleHash('writeln'); Table[h] := TT_WRITELNSYM;
  h := SimpleHash('read');    Table[h] := TT_READSYM;
  h := SimpleHash('readln');  Table[h] := TT_READLNSYM;
end;

function Contains(const Token: string): Boolean;
begin
  Result := Table[SimpleHash(Token)] <> TT_INVALID;
end;

function Find(const Token: string): TokenT;
begin
  Result := Table[SimpleHash(Token)];
end;

initialization
  InitTable;

end.
