unit SymbolUnit;

{$mode objfpc}{$H+}

interface

uses
  TokenType, Classes, SysUtils;

function Contains(const token: string): Boolean;
function Find(const token: string): TTokenType;

implementation

type
  TTokenMap = class
  private
    FMap: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Key: string; Value: TTokenType);
    function TryGetValue(const Key: string; out Value: TTokenType): Boolean;
    function ContainsKey(const Key: string): Boolean;
  end;

var
  SymbolTable: TTokenMap;

constructor TTokenMap.Create;
begin
  inherited;
  FMap := TStringList.Create;
  FMap.Sorted := True;
  FMap.CaseSensitive := False;
  FMap.Duplicates := dupIgnore;
end;

destructor TTokenMap.Destroy;
begin
  FMap.Free;
  inherited Destroy;
end;

procedure TTokenMap.Add(const Key: string; Value: TTokenType);
begin
  FMap.AddObject(Key, TObject(PtrInt(Value)));
end;

function TTokenMap.TryGetValue(const Key: string; out Value: TTokenType): Boolean;
var
  Index: Integer;
begin
  Index := FMap.IndexOf(Key);
  Result := Index >= 0;
  if Result then
    Value := TTokenType(PtrInt(FMap.Objects[Index]));
end;

function TTokenMap.ContainsKey(const Key: string): Boolean;
begin
  Result := FMap.IndexOf(Key) >= 0;
end;

procedure InitializeSymbolTable;
begin
  SymbolTable := TTokenMap.Create;
  
  // Arithmetic operators
  SymbolTable.Add('+', ttADD);
  SymbolTable.Add('-', ttSUB);
  SymbolTable.Add('*', ttMUL);
  SymbolTable.Add('/', ttDIV);
  SymbolTable.Add('mod', ttMOD);
  SymbolTable.Add('div', ttFLOORDIV);

  // Logical and relational operators
  SymbolTable.Add('or', ttOR);
  SymbolTable.Add('and', ttAND);
  SymbolTable.Add('not', ttNOT);
  SymbolTable.Add('=', ttEQL);
  SymbolTable.Add('==', ttEQL);
  SymbolTable.Add('<>', ttNEQ);
  SymbolTable.Add('>', ttGTR);
  SymbolTable.Add('>=', ttGEQ);
  SymbolTable.Add('<', ttLSS);
  SymbolTable.Add('<=', ttLEQ);
  SymbolTable.Add(':=', ttASSIGN);

  // Symbols
  SymbolTable.Add(';', ttSEMICOLON);
  SymbolTable.Add(',', ttCOMMA);
  SymbolTable.Add('.', ttPERIOD);
  SymbolTable.Add(':', ttCOLON);
  SymbolTable.Add('(', ttLPAREN);
  SymbolTable.Add(')', ttRPAREN);
  SymbolTable.Add('"', ttQUOTES);

  // Keywords
  SymbolTable.Add('program', ttPROGRAM);
  SymbolTable.Add('var', ttVAR);
  SymbolTable.Add('integer', ttTYPE_INT);
  SymbolTable.Add('real', ttTYPE_REAL);
  SymbolTable.Add('string', ttTYPE_STR);
  SymbolTable.Add('begin', ttBEGIN);
  SymbolTable.Add('end', ttEND);
  SymbolTable.Add('for', ttFOR);
  SymbolTable.Add('to', ttTO);
  SymbolTable.Add('while', ttWHILE);
  SymbolTable.Add('do', ttDO);
  SymbolTable.Add('break', ttBREAK);
  SymbolTable.Add('continue', ttCONTINUE);
  SymbolTable.Add('if', ttIF);
  SymbolTable.Add('else', ttELSE);
  SymbolTable.Add('then', ttTHEN);
  SymbolTable.Add('write', ttWRITE);
  SymbolTable.Add('writeln', ttWRITELN);
  SymbolTable.Add('read', ttREAD);
  SymbolTable.Add('readln', ttREADLN);
end;

function Contains(const token: string): Boolean;
begin
  Result := SymbolTable.ContainsKey(token);
end;

function Find(const token: string): TTokenType;
begin
  if not SymbolTable.TryGetValue(token, Result) then
    Result := ttINVALID;
end;

initialization
  InitializeSymbolTable;

finalization
  SymbolTable.Free;

end.
