unit Interpreter;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, CommandUnit;

type
  TVarKind = (vkInteger, vkReal, vkString);

  TVarValue = record
    Kind: TVarKind;
    intValue: Int64;
    realValue: Double;
    strValue: string;
  end;

  TVariableInfoRec = record
    Name: string;
    TypeInfo: VarType;
  end;

  TInterpreter = class
  private
    // Variáveis definidas no programa
    VarNames: array of string;
    VarTypes: array of VarType;
    VarValues: array of TVarValue;

    // Map de labels para índice de comando
    LabelNames: array of string;
    LabelIndices: array of Integer;

    procedure EnsureVariable(const AName: string);
    function  FindVariable(const AName: string): Integer;
    procedure SetVariable(const AName: string; const Value: TVarValue);
    function  GetVariable(const AName: string): TVarValue;
    function  ResolveSource(const S: TSource): TVarValue;
    procedure BuildLabelMap(const cmds: TCommandArray);

    // Execução de cada tipo de comando
    procedure ExecAssign(const cmd: TCommand);
    procedure ExecArith(const cmd: TCommand);
    procedure ExecCompare(const cmd: TCommand);
    procedure ExecLogic(const cmd: TCommand);
    procedure ExecControl(var pc: Integer; const cmds: TCommandArray);
    procedure ExecCall(const cmd: TCommand);

    // Operações auxiliares
    function  ToBool(const V: TVarValue): Boolean;
    function  ToInt(const V: TVarValue): Int64;
    function  ToReal(const V: TVarValue): Double;
    function  ToStr(const V: TVarValue): string;
    function  NewTempValue: TVarValue;
  public
    constructor Create(const VariableInfo: array of TVariableInfoRec);
    procedure Execute(const cmds: TCommandArray);
  end;

implementation

{ TInterpreter }

constructor TInterpreter.Create(const VariableInfo: array of TVariableInfoRec);
var
  i: Integer;
begin
  SetLength(VarNames, Length(VariableInfo));
  SetLength(VarTypes, Length(VariableInfo));
  SetLength(VarValues, Length(VariableInfo));
  for i := 0 to High(VariableInfo) do
  begin
    VarNames[i] := VariableInfo[i].Name;
    VarTypes[i] := VariableInfo[i].TypeInfo;
    // Inicializa valor default
    case VarTypes[i] of
      vtINTEGER: VarValues[i].Kind := vkInteger;
      vtREAL:    VarValues[i].Kind := vkReal;
      vtSTRING:  VarValues[i].Kind := vkString;
    end;
  end;
  SetLength(LabelNames, 0);
  SetLength(LabelIndices, 0);
end;

procedure TInterpreter.EnsureVariable(const AName: string);
begin
  if FindVariable(AName) < 0 then
    raise Exception.Create('Variável não declarada: ' + AName);
end;

function TInterpreter.FindVariable(const AName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(VarNames) do
    if VarNames[i] = AName then
      Exit(i);
end;

procedure TInterpreter.SetVariable(const AName: string; const Value: TVarValue);
var
  idx: Integer;
begin
  idx := FindVariable(AName);
  if idx < 0 then
    raise Exception.Create('Atribuição a variável não declarada: ' + AName);
  // Verifica compatibilidade
  case VarTypes[idx] of
    vtINTEGER: if Value.Kind = vkInteger then
                 VarValues[idx] := Value
               else if Value.Kind = vkReal then
                 VarValues[idx].intValue := Trunc(Value.realValue)
               else raise Exception.Create('Tipo incompatível em atribuição a ' + AName);
    vtREAL:    if Value.Kind in [vkInteger,vkReal] then
                 begin VarValues[idx].Kind := vkReal; VarValues[idx].realValue := ToReal(Value); end
               else raise Exception.Create('Tipo incompatível em atribuição a ' + AName);
    vtSTRING:  if Value.Kind = vkString then
                 VarValues[idx] := Value
               else raise Exception.Create('Tipo incompatível em atribuição a ' + AName);
  end;
end;

function TInterpreter.GetVariable(const AName: string): TVarValue;
var
  idx: Integer;
begin
  idx := FindVariable(AName);
  if idx < 0 then
    raise Exception.Create('Variável não declarada: ' + AName);
  Result := VarValues[idx];
end;

function TInterpreter.ResolveSource(const S: TSource): TVarValue;
begin
  case S.sourceType of
    stInteger: Result := TVarValue(Kind:vkInteger; intValue:S.intValue);
    stFloat:   Result := TVarValue(Kind:vkReal; realValue:S.floatValue);
    stString:
      if FindVariable(S.strValue)>=0 then
        Result := GetVariable(S.strValue)
      else
        Result := TVarValue(Kind:vkString; strValue:S.strValue);
    else
      Result := NewTempValue;
  end;
end;

procedure TInterpreter.BuildLabelMap(const cmds: TCommandArray);
var
  i, n: Integer;
begin
  n := Length(cmds);
  for i := 0 to n-1 do
    if cmds[i].mnemonic = mnLABEL then
    begin
      SetLength(LabelNames, Length(LabelNames)+1);
      SetLength(LabelIndices, Length(LabelIndices)+1);
      LabelNames[High(LabelNames)] := cmds[i].dst.strValue;
      LabelIndices[High(LabelIndices)] := i;
    end;
end;

procedure TInterpreter.ExecAssign(const cmd: TCommand);
var
  val: TVarValue;
begin
  EnsureVariable(cmd.dst.strValue);
  val := ResolveSource(cmd.src1);
  SetVariable(cmd.dst.strValue, val);
end;

procedure TInterpreter.ExecArith(const cmd: TCommand);
var
  left, right, res: TVarValue;
  name: string;
begin
  left  := ResolveSource(cmd.src1);
  right := ResolveSource(cmd.src2);
  case cmd.mnemonic of
    mnADD:  res.Kind := vkInteger; res.intValue := ToInt(left)+ToInt(right);
    mnSUB:  res.Kind := vkInteger; res.intValue := ToInt(left)-ToInt(right);
    mnMUL:  res.Kind := vkInteger; res.intValue := ToInt(left)*ToInt(right);
    mnDIV:  res.Kind := vkInteger; res.intValue := ToInt(left) div ToInt(right);
    mnMOD:  res.Kind := vkInteger; res.intValue := ToInt(left) mod ToInt(right);
    mnIDIV: res.Kind := vkInteger; res.intValue := ToInt(left) div ToInt(right);
  end;
  name := cmd.dst.strValue;
  SetVariable(name, res);
end;

procedure TInterpreter.ExecCompare(const cmd: TCommand);
var
  left, right: TVarValue;
  b: Boolean;
  tmp: TVarValue;
begin
  left  := ResolveSource(cmd.src1);
  right := ResolveSource(cmd.src2);
  case cmd.mnemonic of
    mnEQL: b := (ToStr(left)=ToStr(right));
    mnNEQ: b := (ToStr(left)<>ToStr(right));
    mnLSS: b := (ToStr(left)< ToStr(right));
    mnLEQ: b := (ToStr(left)<=ToStr(right));
    mnGTR: b := (ToStr(left)> ToStr(right));
    mnGEQ: b := (ToStr(left)>=ToStr(right));
  end;
  tmp.Kind := vkInteger;
  tmp.intValue := IfThen(b,1,0);
  SetVariable(cmd.dst.strValue, tmp);
end;

procedure TInterpreter.ExecLogic(const cmd: TCommand);
var
  left, right: TVarValue;
  b: Boolean;
  tmp: TVarValue;
begin
  if cmd.mnemonic=mnNOT then
  begin
    left := ResolveSource(cmd.src1);
    b := not ToBool(left);
  end
  else
  begin
    left  := ResolveSource(cmd.src1);
    right := ResolveSource(cmd.src2);
    if cmd.mnemonic=mnAND then b := ToBool(left) and ToBool(right)
                         else b := ToBool(left) or ToBool(right);
  end;
  tmp.Kind := vkInteger;
  tmp.intValue := IfThen(b,1,0);
  SetVariable(cmd.dst.strValue, tmp);
end;

procedure TInterpreter.ExecControl(var pc: Integer; const cmds: TCommandArray);
var
  lbl: string;
  i: Integer;
  condVal: TVarValue;
  target: string;
begin
  case cmds[pc].mnemonic of
    mnLABEL: Exit;
    mnJMP:
      begin
        lbl := cmds[pc].dst.strValue;
        for i := 0 to High(LabelNames) do
          if LabelNames[i]=lbl then
          begin
            pc := LabelIndices[i]-1;
            Exit;
          end;
      end;
    mnIF:
      begin
        condVal := ResolveSource(cmds[pc].src1);
        if ToBool(condVal) then target := cmds[pc].dst.strValue
                          else target := cmds[pc].src2.strValue;
        for i := 0 to High(LabelNames) do
          if LabelNames[i]=target then
          begin
            pc := LabelIndices[i]-1;
            Exit;
          end;
      end;
  end;
end;

procedure TInterpreter.ExecCall(const cmd: TCommand);
var
  s: TVarValue;
begin
  case cmd.dst.callType of
    ctREAD:
      begin
        ReadLn(s.intValue);
        s.Kind := vkInteger;
        SetVariable(cmd.src1.strValue, s);
      end;
    ctREADLN:
      begin
        ReadLn(s.strValue);
        s.Kind := vkString;
        SetVariable(cmd.src1.strValue, s);
      end;
    ctWRITE:
      begin
        s := ResolveSource(cmd.src1);
        Write(ToStr(s));
      end;
    ctWRITELN:
      begin
        s := ResolveSource(cmd.src1);
        WriteLn(ToStr(s));
      end;
  end;
end;

procedure TInterpreter.Execute(const cmds: TCommandArray);
var
  pc: Integer;
begin
  BuildLabelMap(cmds);
  pc := 0;
  while pc <= High(cmds) do
  begin
    case cmds[pc].mnemonic of
      mnASSIGN: ExecAssign(cmds[pc]);
      mnADD..mnIDIV: ExecArith(cmds[pc]);
      mnEQL..mnGEQ: ExecCompare(cmds[pc]);
      mnAND..mnNOT: ExecLogic(cmds[pc]);
      mnIF, mnJMP, mnLABEL: ExecControl(pc, cmds);
      mnCALL: ExecCall(cmds[pc]);
    end;
    Inc(pc);
  end;
end;

function TInterpreter.ToBool(const V: TVarValue): Boolean;
begin
  case V.Kind of
    vkInteger: Result := V.intValue<>0;
    vkReal:    Result := V.realValue<>0;
    vkString:  Result := V.strValue<>'';
  end;
end;

function TInterpreter.ToInt(const V: TVarValue): Int64;
begin
  case V.Kind of
    vkInteger: Result := V.intValue;
    vkReal:    Result := Trunc(V.realValue);
    vkString:  if not TryStrToInt64(V.strValue, Result) then Result := 0;
  end;
end;

function TInterpreter.ToReal(const V: TVarValue): Double;
begin
  case V.Kind of
    vkInteger: Result := V.intValue;
    vkReal:    Result := V.realValue;
    vkString:  if not TryStrToFloat(V.strValue, Result) then Result := 0.0;
  end;
end;

function TInterpreter.ToStr(const V: TVarValue): string;
begin
  case V.Kind of
    vkInteger: Result := IntToStr(V.intValue);
    vkReal:    Result := FloatToStr(V.realValue);
    vkString:  Result := V.strValue;
  else
    Result := '';
  end;
end;

function TInterpreter.NewTempValue: TVarValue;
begin
  Result.Kind := vkInteger;
  Result.intValue := 0;
end;

end.

