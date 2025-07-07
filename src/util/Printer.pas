unit Printer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CommandUnit, LexemeUnit, Parser;

type
  ESyntaticalError = class(Exception);

  TPrinter = class
  public
    class procedure PrintLexemes(const lexemes: TLexemeArray);
    class procedure PrintCommands(const commands: TCommandArray);
    class procedure PrintVariableInfoArray(const vars: array of TVariableInfo);
    class procedure PrintTypeErrors(const typeErrors: array of string);
    class function VarTypeToString(vt: VarType): string;
  end;

implementation

class procedure TPrinter.PrintLexemes(const lexemes: TLexemeArray);
var
  i: Integer;
begin
  Writeln('LEXEME LIST');
  Writeln('-----------');
  for i := 0 to High(lexemes) do
    Writeln(lexemes[i].str);
  Writeln;
end;


class procedure TPrinter.PrintCommands(const commands: TCommandArray);
var
  i: Integer;
  cmd: TCommand;
  
  function FormatOperand(const s: string): string;
  begin
    if (s <> '') and (s[1] = 'T') and (Copy(s, 1, 4) = 'Temp') then
      Result := 'TEMP:' + s
    else
      Result := s;
  end;

begin
  Writeln('COMMANDS');
  Writeln('========');
  for i := 0 to High(commands) do
  begin
    cmd := commands[i];
    Write(i, ': ');
    
    // Impressão dos mnemonics
    case cmd.mnemonic of
      mnADD:    Write('ADD');
      mnSUB:    Write('SUB');
      mnMUL:    Write('MUL');
      mnDIV:    Write('DIV');
      mnMOD:    Write('MOD');
      mnIDIV:   Write('IDIV');
      mnOR:     Write('OR');
      mnAND:    Write('AND');
      mnNOT:    Write('NOT');
      mnEQL:    Write('EQL');
      mnNEQ:    Write('NEQ');
      mnGTR:    Write('GTR');
      mnGEQ:    Write('GEQ');
      mnLSS:    Write('LSS');
      mnLEQ:    Write('LEQ');
      mnASSIGN: Write('ASSIGN');
      mnIF:
        begin
        Write('IF ');
        Write(FormatOperand(cmd.dst.strValue), ', ');
        Write(FormatOperand(cmd.src1.strValue), ', ');
        Write(FormatOperand(cmd.src2.strValue));
      end;
      mnJMP:    Write('JMP');
      mnCALL:   Write('CALL');
      mnLABEL:  Write('LABEL');
    end;
    
    // Tratamento especial para CALL
    if cmd.mnemonic = mnCALL then
    begin
      Write(' ');
      case cmd.dst.callType of
        ctREAD:    Write('READ');
        ctWRITE:   Write('WRITE');
        ctREADLN:  Write('READLN');
        ctWRITELN: Write('WRITELN');
      end;
      
      Write(', ', FormatOperand(cmd.src1.strValue), ', ');
      
      if cmd.src1.writeType = wtSTRING then
        Write('STRING')
      else
        Write('VARIABLE');
    end
    else
    begin
      // Comandos normais
      Write(' ', FormatOperand(cmd.dst.strValue), ', ');
      Write(FormatOperand(cmd.src1.strValue), ', ');
      Write(FormatOperand(cmd.src2.strValue));
    end;
    
    Writeln;
  end;
  Writeln;
end;


class procedure TPrinter.PrintVariableInfoArray(const vars: array of TVariableInfo);
var
  i: Integer;
begin
  Writeln('VARIABLE TYPES');
  Writeln('==============');
  for i := 0 to High(vars) do
    Writeln(vars[i].Name, ': ', VarTypeToString(vars[i].VType));
  Writeln;
end;

class procedure TPrinter.PrintTypeErrors(const typeErrors: array of string);
var
  i: Integer;
begin
  if Length(typeErrors) > 0 then
  begin
    Writeln('TYPE ERRORS DETECTED');
    Writeln('=====================');
    for i := 0 to High(typeErrors) do
      Writeln(typeErrors[i]);
    Writeln;
  end;
end;

class function TPrinter.VarTypeToString(vt: VarType): string;
begin
  case vt of
    vtINTEGER: Result := 'integer';
    vtREAL:    Result := 'real';
    vtSTRING:  Result := 'string';
  else
    Result := 'unknown';
  end;
end;

end.

