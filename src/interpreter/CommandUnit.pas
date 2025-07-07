unit CommandUnit;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

type
  TMnemonic = (
    mnADD, mnSUB, mnMUL, mnDIV, mnMOD, mnIDIV,
    mnOR, mnAND, mnNOT, mnEQL, mnNEQ, mnGTR, mnGEQ, mnLSS, mnLEQ, mnASSIGN,
    mnIF, mnJMP, mnCALL, mnLABEL
  );
  
  TCallType = (ctREAD, ctWRITE, ctREADLN, ctWRITELN);
  TWriteType = (wtSTRING, wtVARIABLE);
  
  // Tipos simplificados para operandos
  TSourceType = (stNone, stInteger, stFloat, stString, stWriteType);
  TTargetType = (ttString, ttCallType);
  
  TSource = record
    sourceType: TSourceType;
    intValue: Int64;
    floatValue: Double;
    strValue: string;   // Armazena apenas o valor essencial (string, número, nome)
    writeType: TWriteType; // Tipo de escrita (STRING ou VARIABLE)
    
    constructor Create(const value: string);
    constructor Create(value: Int64);
    constructor Create(value: Double);
    constructor Create(wt: TWriteType);
  end;
  
  TTarget = record
    targetType: TTargetType;
    strValue: string;     // Para destinos com string (variáveis, temporários, rótulos)
    callType: TCallType;  // Para destinos com tipo de chamada
    
    constructor Create(const value: string);
    constructor Create(ct: TCallType);
  end;
  
  TCommand = record
    mnemonic: TMnemonic;
    dst: TTarget;
    src1: TSource;
    src2: TSource;
  end;

implementation

{ TSource }
constructor TSource.Create(const value: string);
begin
  if value = '' then
    sourceType := stNone
  else begin
    sourceType := stString;
    strValue := value;
  end;
  intValue := 0;
  floatValue := 0.0;
  writeType := wtSTRING;
end;

constructor TSource.Create(value: Int64);
begin
  sourceType := stInteger;
  intValue := value;
  strValue := '';
  floatValue := 0.0;
  writeType := wtSTRING;
end;

constructor TSource.Create(value: Double);
begin
  sourceType := stFloat;
  floatValue := value;
  strValue := '';
  intValue := 0;
  writeType := wtSTRING;
end;

constructor TSource.Create(wt: TWriteType);
begin
  sourceType := stWriteType;
  writeType := wt;
  strValue := '';
  intValue := 0;
  floatValue := 0.0;
end;

{ TTarget }
constructor TTarget.Create(const value: string);
begin
  targetType := ttString;
  strValue := value;
  callType := ctREAD; // Valor padrão
end;

constructor TTarget.Create(ct: TCallType);
begin
  targetType := ttCallType;
  callType := ct;
  strValue := '';
end;

end.
