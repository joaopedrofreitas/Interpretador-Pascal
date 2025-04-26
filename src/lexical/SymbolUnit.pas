unit SymbolTable;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fphash,TokenType;

type
 
function Contains(const Token: string): Boolean;
function Find(const Token: string): TokenType;

implementation

var
  SymbolTable: TFPDataHashTable;


procedure InitializeSymbols;
begin
  SymbolTable := TFPDataHashTable.Create;

  // Arithmetic
  AddSymbol('+', TT_ADD);
  AddSymbol('-', TT_SUB);
  AddSymbol('*', TT_MUL);
  AddSymbol('/', TT_DIV);
  AddSymbol('mod', TT_MOD);
  AddSymbol('div', TT_DIVINT);

  // Logical / relational
  AddSymbol('or', TT_OR);
  AddSymbol('and', TT_AND);
  AddSymbol('not', TT_NOT);
  AddSymbol('=', TT_EQUAL);
  AddSymbol('==', TT_EQUAL);
  AddSymbol('<>', TT_DIFFERENCE);
  AddSymbol('>', TT_GREATER);
  AddSymbol('>=', TT_GREATER_EQUAL);
  AddSymbol('<', TT_LOWER);
  AddSymbol('<=', TT_LOWER_EQUAL);
  AddSymbol(':=', TT_ASSIGN);

  // Symbols
  AddSymbol(';', TT_SEMICOLON);
  AddSymbol(',', TT_COMMA);
  AddSymbol('.', TT_PERIOD);
  AddSymbol(':', TT_COLON);
  AddSymbol('(', TT_OPEN_PARENTHESES);
  AddSymbol(')', TT_CLOSE_PARENTHESES);
  AddSymbol('"', TT_QUOTES);

  // Keywords
  AddSymbol('program', TT_PROGRAM);
  AddSymbol('var', TT_VAR);
  AddSymbol('integer', TT_TYPE_INTEGER);
  AddSymbol('real', TT_TYPE_REAL);
  AddSymbol('string', TT_TYPE_STRING);
  AddSymbol('begin', TT_BEGIN);
  AddSymbol('end', TT_END);
  AddSymbol('for', TT_FOR);
  AddSymbol('to', TT_TO);
  AddSymbol('while', TT_WHILE);
  AddSymbol('do', TT_DO);
  AddSymbol('break', TT_BREAK);
  AddSymbol('continue', TT_CONTINUE);
  AddSymbol('if', TT_IF);
  AddSymbol('else', TT_ELSE);
  AddSymbol('then', TT_THEN);
  AddSymbol('write', TT_WRITE);
  AddSymbol('writeln', TT_WRITELN);
  AddSymbol('read', TT_READ);
  AddSymbol('readln', TT_READLN);
end;

procedure AddSymbol(const Name: string; Kind: TokenType);
begin
  SymbolTable.Add(Name, Pointer(Ord(Kind)));
end;

function Contains(const Token: string): Boolean;
begin
  Result := Assigned(SymbolTable.Find(Token));
end;

function Find(const Token: string): TokenType;
  var
    Data: Pointer;
  begin
    Data := SymbolTable.Find(Token);
    if Assigned(Data) then
      Result := TokenType(PtrUInt(Data))
    else
      Result := TT_VAR_NAME;
end;

initialization
  InitializeSymbols;

finalization
  SymbolTable.Free;

end.
