unit Interpreter;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  CommandUnit,
  Parser;

type
  TCommandArray = array of TCommand;
  TVariableInfoArray = Parser.TVariableInfoArray;
  TVariableInfo = Parser.TVariableInfo;

  TVarKind = (vkInteger, vkReal, vkString);

  TVarValue = record
    Kind: TVarKind;
    intValue: Int64;
    realValue: Double;
    strValue: string;
  end;

  TInterpreter = class
  private
    VarNames: array of string;
    VarTypes: array of VarType;
    VarValues: array of TVarValue;
    
    TempValues: array of record
      Name: string;
      Value: TVarValue;
    end;

    LabelNames: array of string;
    LabelIndices: array of Integer;

    procedure EnsureVariable(const AName: string);
    function  FindVariable(const AName: string): Integer;
    function  FindTemp(const AName: string): Integer;
    procedure SetVariable(const AName: string; const Value: TVarValue);
    procedure SetTemp(const AName: string; const Value: TVarValue);
    procedure AssignTo(const AName: string; const Value: TVarValue);
    function  GetVariable(const AName: string): TVarValue;
    function  GetTemp(const AName: string): TVarValue;
    function  ResolveSource(const S: TSource): TVarValue;
    procedure BuildLabelMap(const cmds: TCommandArray);

    procedure ExecAssign(const cmd: TCommand);
    procedure ExecArith(const cmd: TCommand);
    procedure ExecCompare(const cmd: TCommand);
    procedure ExecLogic(const cmd: TCommand);
    procedure ExecControl(var pc: Integer; const cmds: TCommandArray);
    procedure ExecCall(const cmd: TCommand);

    function  ToBool(const V: TVarValue): Boolean;
    function  ToInt(const V: TVarValue): Int64;
    function  ToReal(const V: TVarValue): Double;
    function  ToStr(const V: TVarValue): string;
    function  NewTempValue: TVarValue;

  public
    constructor Create(const VariableInfo: TVariableInfoArray);
    procedure Execute(const cmds: TCommandArray);
    procedure PrintState;
  end;

implementation

uses
  Math;

{ TInterpreter }

constructor TInterpreter.Create(const VariableInfo: TVariableInfoArray);
var
  i: Integer;
begin
  SetLength(VarNames, Length(VariableInfo));
  SetLength(VarTypes, Length(VariableInfo));
  SetLength(VarValues, Length(VariableInfo));
  for i := 0 to High(VariableInfo) do
  begin
    VarNames[i] := VariableInfo[i].Name;
    VarTypes[i] := VariableInfo[i].VType;
    case VarTypes[i] of
      vtINTEGER: VarValues[i].Kind := vkInteger;
      vtREAL:    VarValues[i].Kind := vkReal;
      vtSTRING:  VarValues[i].Kind := vkString;
    end;
  end;
  SetLength(TempValues, 0);
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
  for i := 0 to High(VarNames) do
    if VarNames[i] = AName then
      Exit(i);
  Result := -1;
end;

function TInterpreter.FindTemp(const AName: string): Integer;
var
  i: Integer;
begin
  for i := 0 to High(TempValues) do
    if TempValues[i].Name = AName then
      Exit(i);
  Result := -1;
end;

procedure TInterpreter.SetVariable(const AName: string; const Value: TVarValue);
var
  idx: Integer;
begin
  idx := FindVariable(AName);
  if idx < 0 then
    raise Exception.Create('Atribuição a variável não declarada: ' + AName);
  case VarTypes[idx] of
    vtINTEGER:
      if Value.Kind = vkInteger then
        VarValues[idx] := Value
      else if Value.Kind = vkReal then
      begin
        VarValues[idx].Kind := vkInteger;
        VarValues[idx].intValue := Trunc(Value.realValue);
      end
      else
        raise Exception.Create('Tipo incompatível em atribuição a ' + AName);
    vtREAL:
      if Value.Kind in [vkInteger, vkReal] then
      begin
        VarValues[idx].Kind := vkReal;
        VarValues[idx].realValue := ToReal(Value);
      end
      else
        raise Exception.Create('Tipo incompatível em atribuição a ' + AName);
    vtSTRING:
      if Value.Kind = vkString then
        VarValues[idx] := Value
      else
        raise Exception.Create('Tipo incompatível em atribuição a ' + AName);
  end;
end;

procedure TInterpreter.SetTemp(const AName: string; const Value: TVarValue);
var
  idx: Integer;
begin
  idx := FindTemp(AName);
  if idx >= 0 then
    TempValues[idx].Value := Value
  else
  begin
    SetLength(TempValues, Length(TempValues)+1);
    TempValues[High(TempValues)].Name := AName;
    TempValues[High(TempValues)].Value := Value;
  end;
end;

procedure TInterpreter.AssignTo(const AName: string; const Value: TVarValue);
begin
  if FindVariable(AName) >= 0 then
    SetVariable(AName, Value)
  else
    SetTemp(AName, Value);
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

function TInterpreter.GetTemp(const AName: string): TVarValue;
var
  idx: Integer;
begin
  idx := FindTemp(AName);
  if idx < 0 then
    Result := NewTempValue
  else
    Result := TempValues[idx].Value;
end;

function TInterpreter.ResolveSource(const S: TSource): TVarValue;
var
  idx: Integer;
  iValue: Int64;
  dValue: Double;
begin
  case S.sourceType of
    stInteger:
      begin
        Result.Kind := vkInteger;
        Result.intValue := S.intValue;
      end;
    stFloat:
      begin
        Result.Kind := vkReal;
        Result.realValue := S.floatValue;
      end;
    stString:
      begin
        idx := FindVariable(S.strValue);
        if idx >= 0 then
          Result := VarValues[idx]
        else
        begin
          idx := FindTemp(S.strValue);
          if idx >= 0 then
            Result := TempValues[idx].Value
          else if TryStrToInt64(S.strValue, iValue) then
          begin
            Result.Kind := vkInteger;
            Result.intValue := iValue;
          end
          else if TryStrToFloat(S.strValue, dValue) then
          begin
            Result.Kind := vkReal;
            Result.realValue := dValue;
          end
          else
          begin
            Result.Kind := vkString;
            Result.strValue := S.strValue;
          end;
        end;        
      end; 
  end;
end;

procedure TInterpreter.BuildLabelMap(const cmds: TCommandArray);
var
  i: Integer;
begin
  for i := 0 to High(cmds) do
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
  dest: string;
begin
  dest := cmd.dst.strValue;
  val := ResolveSource(cmd.src1);
  AssignTo(dest, val);
end;

procedure TInterpreter.ExecArith(const cmd: TCommand);
var
  left, right, res: TVarValue;
  dest: string;
begin
  left  := ResolveSource(cmd.src1);
  right := ResolveSource(cmd.src2);
  res := NewTempValue;
  case cmd.mnemonic of
    mnADD:  
      begin 
        res.Kind := vkInteger; 
        res.intValue := ToInt(left) + ToInt(right); 
      end;
    mnSUB:  
      begin 
        res.Kind := vkInteger; 
        res.intValue := ToInt(left) - ToInt(right); 
      end;
    mnMUL:  
      begin 
        res.Kind := vkInteger; 
        res.intValue := ToInt(left) * ToInt(right); 
      end;
    mnDIV:  
      begin 
        res.Kind := vkInteger; 
        res.intValue := ToInt(left) div ToInt(right); 
      end;
    mnMOD:  
      begin 
        res.Kind := vkInteger; 
        res.intValue := ToInt(left) mod ToInt(right); 
      end;
    mnIDIV: 
      begin 
        res.Kind := vkInteger; 
        res.intValue := ToInt(left) div ToInt(right); 
      end;
  end;
  dest := cmd.dst.strValue;
  AssignTo(dest, res);
end;

procedure TInterpreter.ExecCompare(const cmd: TCommand);
var
  left, right: TVarValue;
  b: Boolean;
  res: TVarValue;
  dest: string;
begin
  left  := ResolveSource(cmd.src1);
  right := ResolveSource(cmd.src2);
  case cmd.mnemonic of
    mnEQL: b := ToInt(left) = ToInt(right);
    mnNEQ: b := ToInt(left) <> ToInt(right);
    mnLSS: b := ToInt(left) < ToInt(right);
    mnLEQ: b := ToInt(left) <= ToInt(right);
    mnGTR: b := ToInt(left) > ToInt(right);
    mnGEQ: b := ToInt(left) >= ToInt(right);
  end;
  res.Kind := vkInteger;
  res.intValue := IfThen(b, 1, 0);
  dest := cmd.dst.strValue;
  AssignTo(dest, res);
end;

procedure TInterpreter.ExecLogic(const cmd: TCommand);
var
  left, right: TVarValue;
  b: Boolean;
  res: TVarValue;
  dest: string;
begin
  if cmd.mnemonic = mnNOT then
  begin
    left := ResolveSource(cmd.src1);
    b := not ToBool(left);
  end
  else
  begin
    left  := ResolveSource(cmd.src1);
    right := ResolveSource(cmd.src2);
    if cmd.mnemonic = mnAND then 
      b := ToBool(left) and ToBool(right)
    else 
      b := ToBool(left) or ToBool(right);
  end;
  res.Kind := vkInteger;
  res.intValue := IfThen(b, 1, 0);
  dest := cmd.dst.strValue;
  AssignTo(dest, res);
end;

procedure TInterpreter.ExecControl(var pc: Integer; const cmds: TCommandArray);
var
  target: string;
  i: Integer;
  condVal: TVarValue;
begin
  case cmds[pc].mnemonic of
    mnLABEL:
      Exit;

    mnJMP:
      begin
        target := cmds[pc].dst.strValue;
        for i := 0 to High(LabelNames) do
          if LabelNames[i] = target then
          begin
            pc := LabelIndices[i]; // Correção crucial
            Exit;
          end;
      end;

    mnIF:
      begin
        condVal := ResolveSource(cmds[pc].src1);
        
        if ToBool(condVal) then
          target := cmds[pc].dst.strValue
        else
          target := cmds[pc].src2.strValue;

        for i := 0 to High(LabelNames) do
          if LabelNames[i] = target then
          begin
            pc := LabelIndices[i]; // Correção crucial
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
        AssignTo(cmd.src1.strValue, s);
      end;
    ctREADLN:
      begin 
        ReadLn(s.strValue); 
        s.Kind := vkString;  
        AssignTo(cmd.src1.strValue, s);
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
      mnASSIGN:      ExecAssign(cmds[pc]);
      mnADD..mnIDIV: ExecArith(cmds[pc]);
      mnEQL..mnGEQ:  ExecCompare(cmds[pc]);
      mnAND..mnNOT:  ExecLogic(cmds[pc]);
      mnIF, mnJMP,
      mnLABEL:       ExecControl(pc, cmds);
      mnCALL:        ExecCall(cmds[pc]);
    end;
    Inc(pc);
  end;
end;

procedure TInterpreter.PrintState;
var
  i: Integer;
  v: TVarValue;
begin
  Writeln; Writeln('INTERPRETER STATE'); Writeln('=================');
  for i := 0 to High(VarNames) do
  begin
    Write(VarNames[i], ' = ');
    v := VarValues[i];
    case v.Kind of
      vkInteger: Write(v.intValue);
      vkReal:    Write(v.realValue:0:2);
      vkString:  Write(v.strValue);
    end;
    Writeln;
  end;
  Writeln;
end;

function TInterpreter.ToBool(const V: TVarValue): Boolean;
begin
  case V.Kind of
    vkInteger: Result := V.intValue <> 0;
    vkReal:    Result := V.realValue <> 0.0;
    vkString:  Result := V.strValue <> '';
  else
    Result := False;
  end;
end;

function TInterpreter.ToInt(const V: TVarValue): Int64;
begin
  case V.Kind of
    vkInteger: Result := V.intValue;
    vkReal:    Result := Trunc(V.realValue);
    vkString:  if not TryStrToInt64(V.strValue, Result) then Result := 0;
  else
    Result := 0;
  end;
end;

function TInterpreter.ToReal(const V: TVarValue): Double;
begin
  case V.Kind of
    vkInteger: Result := V.intValue;
    vkReal:    Result := V.realValue;
    vkString:  if not TryStrToFloat(V.strValue, Result) then Result := 0.0;
  else
    Result := 0.0;
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
