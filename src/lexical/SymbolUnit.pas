unit SymbolUnit;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  TokenType;

function Contains(const Token: string): Boolean;
function Find(const Token: string): TokenT;

implementation

type
  TSymbolEntry = record
    Name: string;
    Kind: TokenT;
  end;

const
  SymbolEntries: array[0..42] of TSymbolEntry = (
    { Arithmetic operators }
    (Name: '+';        Kind: TT_ADD),
    (Name: '-';        Kind: TT_SUB),
    (Name: '*';        Kind: TT_MUL),
    (Name: '/';        Kind: TT_DIV),
    (Name: 'mod';      Kind: TT_MOD),
    (Name: 'div';      Kind: TT_DIVINT),

    { Logical, relational operators and assignments }
    (Name: 'or';       Kind: TT_OR),
    (Name: 'and';      Kind: TT_AND),
    (Name: 'not';      Kind: TT_NOT),
    (Name: '==';       Kind: TT_EQUAL),
    (Name: '<>';       Kind: TT_DIFFERENCE),
    (Name: '>';        Kind: TT_GREATER),
    (Name: '>=';       Kind: TT_GREATER_EQUAL),
    (Name: '<';        Kind: TT_LOWER),
    (Name: '<=';       Kind: TT_LOWER_EQUAL),
    (Name: ':=';       Kind: TT_ASSIGN),

    { Symbols }
    (Name: ';';        Kind: TT_SEMICOLON),
    (Name: ',';        Kind: TT_COMMA),
    (Name: '.';        Kind: TT_PERIOD),
    (Name: ':';        Kind: TT_COLON),
    (Name: '(';        Kind: TT_OPEN_PARENTHESES),
    (Name: ')';        Kind: TT_CLOSE_PARENTHESES),
    (Name: '"';        Kind: TT_QUOTES),

    { Keywords }
    (Name: 'program';  Kind: TT_PROGRAM),
    (Name: 'var';      Kind: TT_VAR),
    (Name: 'integer';  Kind: TT_TYPE_INTEGER),
    (Name: 'real';     Kind: TT_TYPE_REAL),
    (Name: 'string';   Kind: TT_TYPE_STRING),
    (Name: 'begin';    Kind: TT_BEGIN),
    (Name: 'end';      Kind: TT_END),
    (Name: 'for';      Kind: TT_FOR),
    (Name: 'to';       Kind: TT_TO),
    (Name: 'while';    Kind: TT_WHILE),
    (Name: 'do';       Kind: TT_DO),
    (Name: 'break';    Kind: TT_BREAK),
    (Name: 'continue'; Kind: TT_CONTINUE),
    (Name: 'if';       Kind: TT_IF),
    (Name: 'else';     Kind: TT_ELSE),
    (Name: 'then';     Kind: TT_THEN),
    (Name: 'write';    Kind: TT_WRITE),
    (Name: 'writeln';  Kind: TT_WRITELN),
    (Name: 'read';     Kind: TT_READ),
    (Name: 'readln';   Kind: TT_READLN)
  );

function Contains(const Token: string): Boolean;
var
  i: Integer;
begin
  for i := Low(SymbolEntries) to High(SymbolEntries) do
    if CompareText(Token, SymbolEntries[i].Name) = 0 then
      Exit(True);
  Result := False;
end;

function Find(const Token: string): TokenT;
var
  i: Integer;
begin
  for i := Low(SymbolEntries) to High(SymbolEntries) do
    if CompareText(Token, SymbolEntries[i].Name) = 0 then
      Exit(SymbolEntries[i].Kind);
  Result := TT_VAR_NAME;
end;

end.

