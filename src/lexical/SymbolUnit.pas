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
  TableSize = 101;

type
  TEntry = record
    Key: string;
    Value: TokenT;
  end;
  TBucket = array of TEntry;

var
  Table: array[0..TableSize - 1] of TBucket;


function SimpleHash(const S: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
    Result := (Result * 31 + Ord(UpCase(S[i]))) mod TableSize;
end;

procedure InitTable;
const
  Tokens: array[0..42] of record Key: string; Value: TokenT end= (
    (Key: '+';        Value: TT_ADD),
    (Key: '-';        Value: TT_SUB),
    (Key: '*';        Value: TT_MUL),
    (Key: '/';        Value: TT_DIV),
    (Key: 'mod';      Value: TT_MOD),
    (Key: 'div';      Value: TT_FLOORDIV),
    (Key: 'or';       Value: TT_OR),
    (Key: 'and';      Value: TT_AND),
    (Key: 'not';      Value: TT_NOT),
    (Key: '==';       Value: TT_EQL),
    (Key: '<>';       Value: TT_NEQ),
    (Key: '>';        Value: TT_GTR),
    (Key: '>=';       Value: TT_GEQ),
    (Key: '<';        Value: TT_LSS),
    (Key: '<=';       Value: TT_LEQ),
    (Key: ':=';       Value: TT_ASSIGN),
    (Key: ';';        Value: TT_SEMICOLON),
    (Key: ',';        Value: TT_COMMA),
    (Key: '.';        Value: TT_PERIOD),
    (Key: ':';        Value: TT_COLON),
    (Key: '(';        Value: TT_LPAREN),
    (Key: ')';        Value: TT_RPAREN),
    (Key: '"';       Value: TT_QUOTES),
    (Key: 'program';  Value: TT_PROGRAM),
    (Key: 'var';      Value: TT_VAR),
    (Key: 'integer';  Value: TT_TYPE_INT),
    (Key: 'real';     Value: TT_TYPE_REAL),
    (Key: 'string';   Value: TT_TYPE_STR),
    (Key: 'begin';    Value: TT_BEGIN),
    (Key: 'end';      Value: TT_END),
    (Key: 'for';      Value: TT_FOR),
    (Key: 'to';       Value: TT_TO),
    (Key: 'while';    Value: TT_WHILE),
    (Key: 'do';       Value: TT_DO),
    (Key: 'break';    Value: TT_BREAK),
    (Key: 'continue'; Value: TT_CONTINUE),
    (Key: 'if';       Value: TT_IF),
    (Key: 'else';     Value: TT_ELSE),
    (Key: 'then';     Value: TT_THEN),
    (Key: 'write';     Value: TT_WRITE),
    (Key: 'writeln';     Value: TT_WRITELN),
    (Key: 'read';     Value: TT_READ),
    (Key: 'readln';     Value: TT_READLN)
  );
var
  i, idx: Integer;
begin
  // Clear buckets
  for i := 0 to TableSize - 1 do
    SetLength(Table[i], 0);

  // Insert each token into its bucket
  for i := Low(Tokens) to High(Tokens) do
  begin
    idx := SimpleHash(Tokens[i].Key);
    SetLength(Table[idx], Length(Table[idx]) + 1);
    Table[idx][High(Table[idx])].Key := Tokens[i].Key;
    Table[idx][High(Table[idx])].Value := Tokens[i].Value;
  end;
end;

function Contains(const Token: string): Boolean;
begin
  Result := Find(Token) <> TT_INVALID;
end;

function Find(const Token: string): TokenT;
var
  bucket: TBucket;
  e: TEntry;
  hashVal: Integer;
begin

  hashVal := SimpleHash(Token);
  bucket := Table[hashVal];
  for e in bucket do
    if SameText(e.Key, Token) then
      Exit(e.Value);
  Result := TT_INVALID;
end;


initialization
  InitTable;
end.
