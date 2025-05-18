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

  // Arithmetic operators
  h := SimpleHash('+');       Table[h] := TT_ADD;
  h := SimpleHash('-');       Table[h] := TT_SUB;
  h := SimpleHash('*');       Table[h] := TT_MUL;
  h := SimpleHash('/');       Table[h] := TT_DIV;
  h := SimpleHash('mod');     Table[h] := TT_MOD;
  h := SimpleHash('div');     Table[h] := TT_FLOORDIV;

  // Logical, relational operators and assignments
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

  // Symbols
  h := SimpleHash(';');       Table[h] := TT_SEMICOLON;
  h := SimpleHash(',');       Table[h] := TT_COMMA;
  h := SimpleHash('.');       Table[h] := TT_PERIOD;
  h := SimpleHash(':');       Table[h] := TT_COLON;
  h := SimpleHash('(');       Table[h] := TT_LPAREN;
  h := SimpleHash(')');       Table[h] := TT_RPAREN;
  h := SimpleHash('"');       Table[h] := TT_QUOTES;

  // Keywords
  h := SimpleHash('program'); Table[h] := TT_PROGRAM;
  h := SimpleHash('var');     Table[h] := TT_VAR;
  h := SimpleHash('integer'); Table[h] := TT_TYPE_INT;
  h := SimpleHash('real');    Table[h] := TT_TYPE_REAL;
  h := SimpleHash('string');  Table[h] := TT_TYPE_STR;
  h := SimpleHash('begin');   Table[h] := TT_BEGIN;
  h := SimpleHash('end');     Table[h] := TT_END;
  h := SimpleHash('for');     Table[h] := TT_FOR;
  h := SimpleHash('to');      Table[h] := TT_TO;
  h := SimpleHash('while');   Table[h] := TT_WHILE;
  h := SimpleHash('do');      Table[h] := TT_DO;
  h := SimpleHash('break');   Table[h] := TT_BREAK;
  h := SimpleHash('continue');Table[h] := TT_CONTINUE;
  h := SimpleHash('if');      Table[h] := TT_IF;
  h := SimpleHash('else');    Table[h] := TT_ELSE;
  h := SimpleHash('then');    Table[h] := TT_THEN;
  h := SimpleHash('write');   Table[h] := TT_WRITE;
  h := SimpleHash('writeln'); Table[h] := TT_WRITELN;
  h := SimpleHash('read');    Table[h] := TT_READ;
  h := SimpleHash('readln');  Table[h] := TT_READLN;
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
