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
  TableSize = 101; // a prime number bigger than the number of keywords, to reduce collisions

type
  PHashEntry = ^THashEntry;
  THashEntry = record
    Key: string;
    Value: TokenT;
  end;

var
  HashTable: array[0..TableSize - 1] of PHashEntry;
  cleanupIndex: Integer; 

function SimpleHash(const S: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
    Result := (Result * 31 + Ord(UpCase(S[i]))) mod TableSize;
end;

procedure Insert(const Key: string; Value: TokenT);
var
  Index: Integer;
begin
  Index := SimpleHash(Key);
  while Assigned(HashTable[Index]) do
    Index := (Index + 1) mod TableSize; 
  New(HashTable[Index]);
  HashTable[Index]^.Key := Key;
  HashTable[Index]^.Value := Value;
end;

function Contains(const Token: string): Boolean;
var
  Index, StartIndex: Integer;
begin
  Index := SimpleHash(Token);
  StartIndex := Index;
  while Assigned(HashTable[Index]) do
  begin
    if CompareText(HashTable[Index]^.Key, Token) = 0 then
      Exit(True);
    Index := (Index + 1) mod TableSize;
    if Index = StartIndex then
      Break;
  end;
  Result := False;
end;

function Find(const Token: string): TokenT;
var
  Index, StartIndex: Integer;
begin
  Index := SimpleHash(Token);
  StartIndex := Index;
  while Assigned(HashTable[Index]) do
  begin
    if CompareText(HashTable[Index]^.Key, Token) = 0 then
      Exit(HashTable[Index]^.Value);
    Index := (Index + 1) mod TableSize;
    if Index = StartIndex then
      Break;
  end;
  Result := TT_VAR_NAME; // default if not found
end;

procedure InitTable;
begin
  Insert('+'      , TT_ADD);
  Insert('-'      , TT_SUB);
  Insert('*'      , TT_MUL);
  Insert('/'      , TT_DIV);
  Insert('mod'    , TT_MOD);
  Insert('div'    , TT_DIVINT);
  Insert('or'     , TT_OR);
  Insert('and'    , TT_AND);
  Insert('not'    , TT_NOT);
  Insert('=='     , TT_EQUAL);
  Insert('<>'     , TT_DIFFERENCE);
  Insert('>'      , TT_GREATER);
  Insert('>='     , TT_GREATER_EQUAL);
  Insert('<'      , TT_LOWER);
  Insert('<='     , TT_LOWER_EQUAL);
  Insert(':='     , TT_ASSIGN);
  Insert(';'      , TT_SEMICOLON);
  Insert(','      , TT_COMMA);
  Insert('.'      , TT_PERIOD);
  Insert(':'      , TT_COLON);
  Insert('('      , TT_OPEN_PARENTHESES);
  Insert(')'      , TT_CLOSE_PARENTHESES);
  Insert('"'      , TT_QUOTES);
  Insert('program', TT_PROGRAM);
  Insert('var'    , TT_VAR);
  Insert('integer', TT_TYPE_INTEGER);
  Insert('real'   , TT_TYPE_REAL);
  Insert('string' , TT_TYPE_STRING);
  Insert('begin'  , TT_BEGIN);
  Insert('end'    , TT_END);
  Insert('for'    , TT_FOR);
  Insert('to'     , TT_TO);
  Insert('while'  , TT_WHILE);
  Insert('do'     , TT_DO);
  Insert('break'  , TT_BREAK);
  Insert('continue', TT_CONTINUE);
  Insert('if'     , TT_IF);
  Insert('else'   , TT_ELSE);
  Insert('then'   , TT_THEN);
  Insert('write'  , TT_WRITE);
  Insert('writeln', TT_WRITELN);
  Insert('read'   , TT_READ);
  Insert('readln' , TT_READLN);
end;

initialization
  InitTable;

finalization
  for cleanupIndex := 0 to TableSize - 1 do
    if Assigned(HashTable[cleanupIndex]) then
      Dispose(HashTable[cleanupIndex]);

end.
