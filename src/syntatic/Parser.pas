unit Parser;

interface

uses
  SysUtils,
  LexemeUnit,
  TokenType,
  CommandUnit;

type
  ESyntaticalError = class(Exception);

  VarType = (vtINTEGER, vtREAL, vtSTRING);

  TVariableInfo = record
    VType: VarType;
    Name: string;
  end;
  TVariableInfoArray = array of TVariableInfo;

  TStringArray = array of string;
  TCommandArray = array of TCommand;

  TParser = class
  private
    m_lexemes: TLexemeArray;
    m_pos: Integer;
    m_commands: TCommandArray;
    m_exprStack: TStringArray;
    m_tempCounter: Integer;
    m_labelCounter: Integer;
    m_variableTypes: TVariableInfoArray;

    function current_lexeme: TLexeme;
    procedure consume(expected: TTokenType);
    procedure addCommand(mn: TMnemonic; const aDst: TTarget; const aSrc1: TSource; const aSrc2: TSource);
    procedure pushExpression(const expr: string);
    function popExpression: string;
    function generateTemp: string;
    function generateLabel: string;
    function generateEndLabel: string;
    procedure addVariableType(const name: string; vtype: VarType);
    function getVariableType(const name: string; out vtype: VarType): Boolean;
    function isIntegerLiteral(const s: string): Boolean;
    function isRealLiteral(const s: string): Boolean;
    function isStringLiteral(const s: string): Boolean;
    function getValueType(const s: string): VarType;
    procedure validateAssignment(const varName, value: string);
    function isTypeCompatible(expected, actual: VarType): Boolean;

    procedure proc_function;
    procedure proc_declarations;
    procedure proc_declaration;
    procedure proc_listIdent(var varNames: TStringArray);
    procedure proc_restIdentList(var varNames: TStringArray);
    procedure proc_restDeclaration;
    procedure proc_typeSpec;

    procedure proc_block;
    procedure proc_stmtList;
    procedure proc_stmt;

    procedure proc_forStmt;
    procedure proc_endFor;
    procedure proc_ioStmt;
    procedure proc_outList(const writeType: string);
    procedure proc_restoOutList(const writeType: string);
    procedure proc_out(const writeType: string);
    procedure proc_whileStmt;
    procedure proc_ifStmt(const endLabel: string);
    procedure proc_elsePart(const endLabel: string);

    procedure proc_atrib;
    procedure proc_expr;
    procedure proc_simple_expr;
    procedure proc_term;
    procedure proc_factor;

  public
    constructor Create(lexemes: TLexemeArray);
    procedure Start;
    function GetCommands: TCommandArray;
    function GetVariableTypes: TVariableInfoArray;
  end;

implementation

uses
  TypInfo;

constructor TParser.Create(lexemes: TLexemeArray);
begin
  m_lexemes := lexemes;
  m_pos := 0;
  m_tempCounter := 0;
  m_labelCounter := 0;
  SetLength(m_commands, 0);
  SetLength(m_exprStack, 0);
  SetLength(m_variableTypes, 0);
end;

procedure TParser.Start;
begin
  proc_function;
  if (m_pos < Length(m_lexemes)) and (current_lexeme <> nil) then
    consume(ttEND_OF_FILE);
end;

function TParser.GetCommands: TCommandArray;
begin
  Result := m_commands;
end;

function TParser.GetVariableTypes: TVariableInfoArray;
begin
  Result := m_variableTypes;
end;

function TParser.current_lexeme: TLexeme;
begin
  if (m_pos < 0) or (m_pos >= Length(m_lexemes)) then
    Result := nil
  else
    Result := m_lexemes[m_pos];
end;

procedure TParser.consume(expected: TTokenType);
var
  lex: TLexeme;
begin
  lex := current_lexeme;
  if lex = nil then
    raise ESyntaticalError.CreateFmt('Expected %s, but no more lexemes', [TokenTypeToStr(expected)]);

  if lex.TokenType = expected then
    Inc(m_pos)
  else
    raise ESyntaticalError.CreateFmt('Expected %s, found %s', [TokenTypeToStr(expected), lex.str]);
end;

procedure TParser.addCommand(mn: TMnemonic; const aDst: TTarget; const aSrc1: TSource; const aSrc2: TSource);
begin
  SetLength(m_commands, Length(m_commands) + 1);
  with m_commands[High(m_commands)] do
  begin
    mnemonic := mn;
    dst := aDst;
    src1 := aSrc1;
    src2 := aSrc2;
  end;
end;

procedure TParser.pushExpression(const expr: string);
begin
  SetLength(m_exprStack, Length(m_exprStack) + 1);
  m_exprStack[High(m_exprStack)] := expr;
end;

function TParser.popExpression: string;
begin
  if Length(m_exprStack) = 0 then
  begin
    if current_lexeme <> nil then
      raise ESyntaticalError.Create('Expression stack underflow at token: ' + current_lexeme.str)
    else
      raise ESyntaticalError.Create('Expression stack underflow at end of file');
  end;
  Result := m_exprStack[High(m_exprStack)];
  SetLength(m_exprStack, Length(m_exprStack) - 1);
end;

function TParser.generateTemp: string;
begin
  Result := 'Temp' + IntToStr(m_tempCounter);
  Inc(m_tempCounter);
end;

function TParser.generateLabel: string;
begin
  Result := 'L_' + IntToStr(m_labelCounter);
  Inc(m_labelCounter);
end;

function TParser.generateEndLabel: string;
begin
  Result := 'endLabel' + IntToStr(m_labelCounter);
  Inc(m_labelCounter);
end;

procedure TParser.addVariableType(const name: string; vtype: VarType);
begin
  SetLength(m_variableTypes, Length(m_variableTypes) + 1);
  m_variableTypes[High(m_variableTypes)].Name := name;
  m_variableTypes[High(m_variableTypes)].VType := vtype;
end;

function TParser.getVariableType(const name: string; out vtype: VarType): Boolean;
var
  i: Integer;
begin
  for i := Low(m_variableTypes) to High(m_variableTypes) do
    if m_variableTypes[i].Name = name then
    begin
      vtype := m_variableTypes[i].VType;
      Exit(True);
    end;
  Result := False;
end;

function TParser.isIntegerLiteral(const s: string): Boolean;
var dummy: Integer;
begin
  Result := TryStrToInt(s, dummy);
end;

function TParser.isRealLiteral(const s: string): Boolean;
var dummy: Double;
begin
  Result := TryStrToFloat(s, dummy);
end;

function TParser.isStringLiteral(const s: string): Boolean;
begin
  Result := (Length(s) >= 2) and (s[1] = '"') and (s[Length(s)] = '"');
end;

function TParser.getValueType(const s: string): VarType;
begin
  if isIntegerLiteral(s) then Exit(vtINTEGER)
  else if isRealLiteral(s) then Exit(vtREAL)
  else if isStringLiteral(s) then Exit(vtSTRING)
  else if getVariableType(s, Result) then Exit
  else Exit(vtSTRING);
end;

procedure TParser.validateAssignment(const varName, value: string);
var expected, actual: VarType;
begin
  if getVariableType(varName, expected) then
  begin
    actual := getValueType(value);
    if (Copy(value, 1, 4) <> 'Temp') and not isTypeCompatible(expected, actual) then
      raise ESyntaticalError.CreateFmt('Type error: Cannot assign %s to %s', [GetEnumName(TypeInfo(VarType), Ord(actual)), varName]);
  end;
end;

function TParser.isTypeCompatible(expected, actual: VarType): Boolean;
begin
  Result := (expected = actual) or ((expected = vtREAL) and (actual = vtINTEGER));
end;

// <function> -> program IDENT ; declarations begin stmtList end .
procedure TParser.proc_function;
begin
  consume(ttPROGRAM);
  consume(ttIDENT);
  consume(ttSEMICOLON);
  proc_declarations;
  consume(ttBEGIN);
  proc_stmtList;
  consume(ttEND);
  consume(ttPERIOD);
end;

// declarations -> var declaration restDeclaration
procedure TParser.proc_declarations;
begin
  consume(ttVAR);
  proc_declaration;
  proc_restDeclaration;
end;

// declaration -> listIdent : typeSpec ;
procedure TParser.proc_declaration;
var varNames: TStringArray; i: Integer;
begin
  SetLength(varNames, 0);
  proc_listIdent(varNames);
  consume(ttCOLON);
  case current_lexeme.TokenType of
    ttTYPE_INT:
      begin
        consume(ttTYPE_INT);
        for i := Low(varNames) to High(varNames) do
          addVariableType(varNames[i], vtINTEGER);
      end;
    ttTYPE_REAL:
      begin
        consume(ttTYPE_REAL);
        for i := Low(varNames) to High(varNames) do
          addVariableType(varNames[i], vtREAL);
      end;
    ttTYPE_STR:
      begin
        consume(ttTYPE_STR);
        for i := Low(varNames) to High(varNames) do
          addVariableType(varNames[i], vtSTRING);
      end;
  else
    raise ESyntaticalError.Create('Expected integer, real or string');
  end;
  consume(ttSEMICOLON);
end;

// listIdent -> IDENT restIdentList
procedure TParser.proc_listIdent(var varNames: TStringArray);
begin
  SetLength(varNames, Length(varNames) + 1);
  varNames[High(varNames)] := current_lexeme.token;
  consume(ttIDENT);
  proc_restIdentList(varNames);
end;

// restIdentList -> , IDENT restIdentList | &
procedure TParser.proc_restIdentList(var varNames: TStringArray);
begin
  if current_lexeme.TokenType = ttCOMMA then
  begin
    consume(ttCOMMA);
    SetLength(varNames, Length(varNames) + 1);
    varNames[High(varNames)] := current_lexeme.token;
    consume(ttIDENT);
    proc_restIdentList(varNames);
  end;
end;

// restDeclaration -> declaration restDeclaration | &
procedure TParser.proc_restDeclaration;
begin
  if current_lexeme.TokenType = ttIDENT then
  begin
    proc_declaration;
    proc_restDeclaration;
  end;
end;

// typeSpec -> integer | real | string
procedure TParser.proc_typeSpec;
begin
  case current_lexeme.TokenType of
    ttTYPE_INT: consume(ttTYPE_INT);
    ttTYPE_REAL: consume(ttTYPE_REAL);
    ttTYPE_STR: consume(ttTYPE_STR);
  else
    raise ESyntaticalError.Create('Expected type');
  end;
end;

// block -> begin stmtList end ;
procedure TParser.proc_block;
begin
  consume(ttBEGIN);
  proc_stmtList;
  consume(ttEND);
  consume(ttSEMICOLON);
end;

// stmtList -> stmt stmtList | &
procedure TParser.proc_stmtList;
begin
  if (m_pos < Length(m_lexemes)) and (current_lexeme <> nil) then
    case current_lexeme.TokenType of
      ttFOR, ttREAD, ttWRITE, ttREADLN, ttWRITELN,
      ttWHILE, ttIDENT, ttIF, ttBEGIN, ttBREAK, ttCONTINUE, ttSEMICOLON:
        begin
          proc_stmt;
          proc_stmtList;
        end;
    end;
end;

// stmt -> forStmt | ioStmt | whileStmt | atrib ; | ifStmt | block | break ; | continue ; | ;
procedure TParser.proc_stmt;
begin
  if current_lexeme = nil then Exit;
  case current_lexeme.TokenType of
    ttFOR: proc_forStmt;
    ttREAD, ttWRITE, ttREADLN, ttWRITELN: proc_ioStmt;
    ttWHILE: proc_whileStmt;
    ttIDENT: begin proc_atrib; consume(ttSEMICOLON); end;
    ttIF: proc_ifStmt('');
    ttBEGIN: proc_block;
    ttBREAK: begin consume(ttBREAK); consume(ttSEMICOLON); end;
    ttCONTINUE: begin consume(ttCONTINUE); consume(ttSEMICOLON); end;
    ttSEMICOLON: consume(ttSEMICOLON);
  else
    raise ESyntaticalError.Create('Undefined statement: ' + current_lexeme.str);
  end;
end;

procedure TParser.proc_forStmt;
var
  startLbl, endLbl, bodyLbl: string;
  loopVar, endVal, temp, cond: string;
begin
  // consome 'for'
  consume(ttFOR);

  // captura variável do laço e faz a atribuição inicial (via proc_atrib)
  loopVar := current_lexeme.token;
  proc_atrib;

  // cria os labels
  startLbl := generateLabel;      // rótulo de início
  endLbl   := generateEndLabel;   // rótulo de término
  bodyLbl  := generateLabel;      // rótulo do corpo

  // marca o início do laço
  addCommand(mnLABEL, TTarget.Create(startLbl), TSource.Create(''), TSource.Create(''));

  // consome 'to' e o valor final
  consume(ttTO);
  endVal := current_lexeme.token;
  proc_endFor;

  // compara loopVar <= endVal e guarda em temp
  temp := generateTemp;
  addCommand(
    mnLEQ,
    TTarget.Create(temp),
    TSource.Create(loopVar),
    TSource.Create(endVal)
  );
  // condicional
  cond := temp;
  addCommand(
    mnIF,
    TTarget.Create(bodyLbl),        // se true vai para bodyLbl
    TSource.Create(cond),
    TSource.Create(endLbl)          // se false vai para endLbl
  );

  // corpo do laço
  addCommand(mnLABEL, TTarget.Create(bodyLbl), TSource.Create(''), TSource.Create(''));
  consume(ttDO);
  proc_stmt;

  // incremento: j := j + 1
  temp := generateTemp;
  addCommand(
    mnADD,
    TTarget.Create(temp),
    TSource.Create(loopVar),
    TSource.Create('1')
  );
  addCommand(
    mnASSIGN,
    TTarget.Create(loopVar),
    TSource.Create(temp),
    TSource.Create('')
  );

  // volta ao início
  addCommand(mnJMP, TTarget.Create(startLbl), TSource.Create(''), TSource.Create(''));

  // fim do laço
  addCommand(mnLABEL, TTarget.Create(endLbl), TSource.Create(''), TSource.Create(''));
end;



// endFor -> IDENT | literal
procedure TParser.proc_endFor;
begin
  if current_lexeme <> nil then
  begin
    case current_lexeme.TokenType of
      ttIDENT, ttLITERAL_OCT, ttLITERAL_DEC, ttLITERAL_HEX, ttLITERAL_REAL: 
        consume(current_lexeme.TokenType)
    else 
      raise ESyntaticalError.Create('Expected variable or literal');
    end;
  end;
end;

// ioStmt -> read(...); etc.
procedure TParser.proc_ioStmt;
var 
  identName: string;
begin
  if current_lexeme = nil then Exit;
  
  case current_lexeme.TokenType of
    ttREAD: 
      begin 
        consume(ttREAD); 
        consume(ttLPAREN); 
        identName := current_lexeme.token;
        consume(ttIDENT); 
        consume(ttRPAREN); 
        consume(ttSEMICOLON);
        addCommand(mnCALL, TTarget.Create(ctREAD), TSource.Create(identName), TSource.Create('')); 
      end;
    ttREADLN: 
      begin 
        consume(ttREADLN); 
        consume(ttLPAREN); 
        identName := current_lexeme.token;
        consume(ttIDENT); 
        consume(ttRPAREN); 
        consume(ttSEMICOLON);
        addCommand(mnCALL, TTarget.Create(ctREADLN), TSource.Create(identName), TSource.Create('')); 
      end;
    ttWRITE: 
      begin 
        consume(ttWRITE); 
        consume(ttLPAREN); 
        proc_outList('WRITE'); 
        consume(ttRPAREN); 
        consume(ttSEMICOLON); 
      end;
    ttWRITELN: 
      begin 
        consume(ttWRITELN); 
        consume(ttLPAREN); 
        proc_outList('WRITELN'); 
        consume(ttRPAREN); 
        consume(ttSEMICOLON);
      end;
  else 
    raise ESyntaticalError.Create('Poorly formatted IO');
  end;
end;

// outList -> out restoOutList
procedure TParser.proc_outList(const writeType: string);
begin
  proc_out(writeType);
  proc_restoOutList(writeType);
end;

// restoOutList -> , outList | &
procedure TParser.proc_restoOutList(const writeType: string);
begin
  if (current_lexeme <> nil) and (current_lexeme.TokenType = ttCOMMA) then
  begin
    consume(ttCOMMA);
    proc_outList(writeType);
  end;
end;

procedure TParser.proc_out(const writeType: string);
var
  src: TSource;
  target: TTarget;
  value: string;
begin
  if current_lexeme = nil then Exit;

  if writeType = 'WRITELN' then
    target := TTarget.Create(ctWRITELN)
  else
    target := TTarget.Create(ctWRITE);

  case current_lexeme.TokenType of
    ttLITERAL_STR: 
      begin
        value := Copy(current_lexeme.token, 2, Length(current_lexeme.token) - 2);
        src := TSource.Create(current_lexeme.token);
        src.writeType := wtSTRING;
        consume(ttLITERAL_STR);
      end;
      
    ttIDENT: 
      begin
        value := current_lexeme.token;
        src := TSource.Create(value);
        src.writeType := wtVARIABLE;
        consume(ttIDENT);
      end;
      
    ttLITERAL_OCT, ttLITERAL_DEC, ttLITERAL_HEX, ttLITERAL_REAL: 
      begin
        value := current_lexeme.token;
        src := TSource.Create(value);
        src.writeType := wtSTRING;
        consume(current_lexeme.TokenType);
      end;
  else
    raise ESyntaticalError.Create('Invalid output');
  end;
  
  addCommand(mnCALL, target, src, TSource.Create(''));
end;

procedure TParser.proc_whileStmt;
var
  startLbl, endLbl, cond: string;
begin
  consume(ttWHILE);
  startLbl := generateLabel;
  endLbl := generateEndLabel;

  addCommand(mnLABEL, TTarget.Create(startLbl), TSource.Create(''), TSource.Create(''));

  proc_expr;
  cond := popExpression;

  addCommand(mnIF, TTarget.Create(''), TSource.Create(cond), TSource.Create(''));
  addCommand(mnJMP, TTarget.Create(endLbl), TSource.Create(''), TSource.Create(''));
  
  consume(ttDO);
  proc_stmt;

  addCommand(mnJMP, TTarget.Create(startLbl), TSource.Create(''), TSource.Create(''));
  addCommand(mnLABEL, TTarget.Create(endLbl), TSource.Create(''), TSource.Create(''));
end;

procedure TParser.proc_ifStmt(const endLabel: string);
var
  condition, thenLbl, elseLbl, finalLbl: string;
begin
  consume(ttIF);
  thenLbl := generateLabel;
  elseLbl := generateLabel;
  
  if endLabel <> '' then
    finalLbl := endLabel
  else
    finalLbl := generateEndLabel;

  proc_expr;
  condition := popExpression;

  addCommand(mnIF, TTarget.Create(''), TSource.Create(condition), TSource.Create(''));
  addCommand(mnJMP, TTarget.Create(elseLbl), TSource.Create(''), TSource.Create(''));
  addCommand(mnLABEL, TTarget.Create(thenLbl), TSource.Create(''), TSource.Create(''));

  consume(ttTHEN);
  proc_stmt;
  
  addCommand(mnJMP, TTarget.Create(finalLbl), TSource.Create(''), TSource.Create(''));
  addCommand(mnLABEL, TTarget.Create(elseLbl), TSource.Create(''), TSource.Create(''));
  
  proc_elsePart(finalLbl);

  addCommand(mnLABEL, TTarget.Create(finalLbl), TSource.Create(''), TSource.Create(''));
end;

procedure TParser.proc_elsePart(const endLabel: string);
begin
  if (current_lexeme <> nil) and (current_lexeme.TokenType = ttELSE) then
  begin
    consume(ttELSE);
    proc_stmt;
  end;
end;

// atrib -> IDENT := expr
procedure TParser.proc_atrib;
var
  varName, exprValue: string;
begin
  varName := current_lexeme.token;
  consume(ttIDENT);
  consume(ttASSIGN);
  proc_expr;
  exprValue := popExpression;
  validateAssignment(varName, exprValue);
  addCommand(mnASSIGN, TTarget.Create(varName), TSource.Create(exprValue), TSource.Create(''));
end;

procedure TParser.proc_expr;
var
  left, right, temp: string;
  op: TTokenType;
begin
  proc_simple_expr;
  
  if (current_lexeme <> nil) and (current_lexeme.TokenType in 
      [ttEQL, ttNEQ, ttLSS, ttLEQ, ttGTR, ttGEQ]) then
  begin
    op := current_lexeme.TokenType;
    consume(op);
    proc_simple_expr;
    
    right := popExpression;
    left := popExpression;
    temp := generateTemp;
    
    case op of
      ttEQL: addCommand(mnEQL, TTarget.Create(temp), TSource.Create(left), TSource.Create(right));
      ttNEQ: addCommand(mnNEQ, TTarget.Create(temp), TSource.Create(left), TSource.Create(right));
      ttLSS: addCommand(mnLSS, TTarget.Create(temp), TSource.Create(left), TSource.Create(right));
      ttLEQ: addCommand(mnLEQ, TTarget.Create(temp), TSource.Create(left), TSource.Create(right));
      ttGTR: addCommand(mnGTR, TTarget.Create(temp), TSource.Create(left), TSource.Create(right));
      ttGEQ: addCommand(mnGEQ, TTarget.Create(temp), TSource.Create(left), TSource.Create(right));
    end;
    pushExpression(temp);
  end;
end;

procedure TParser.proc_simple_expr;
var
  left, right, temp: string;
  op: TTokenType;
begin
  proc_term;
  
  while (current_lexeme <> nil) and (current_lexeme.TokenType in [ttADD, ttSUB, ttOR]) do
  begin
    op := current_lexeme.TokenType;
    consume(op);
    proc_term;
    
    right := popExpression;
    left := popExpression;
    temp := generateTemp;
    
    case op of
      ttADD: addCommand(mnADD, TTarget.Create(temp), TSource.Create(left), TSource.Create(right));
      ttSUB: addCommand(mnSUB, TTarget.Create(temp), TSource.Create(left), TSource.Create(right));
      ttOR:  addCommand(mnOR,  TTarget.Create(temp), TSource.Create(left), TSource.Create(right));
    end;
    pushExpression(temp);
  end;
end;

procedure TParser.proc_term;
var
  left, right, temp: string;
  op: TTokenType;
begin
  proc_factor;
  
  while (current_lexeme <> nil) and (current_lexeme.TokenType in [ttMUL, ttDIV, ttAND]) do
  begin
    op := current_lexeme.TokenType;
    consume(op);
    proc_factor;
    
    right := popExpression;
    left := popExpression;
    temp := generateTemp;
    
    case op of
      ttMUL: addCommand(mnMUL, TTarget.Create(temp), TSource.Create(left), TSource.Create(right));
      ttDIV: addCommand(mnDIV, TTarget.Create(temp), TSource.Create(left), TSource.Create(right));
      ttAND: addCommand(mnAND, TTarget.Create(temp), TSource.Create(left), TSource.Create(right));
    end;
    pushExpression(temp);
  end;
end;

procedure TParser.proc_factor;
var
  temp, expr: string;
  op: TTokenType;
begin
  if current_lexeme = nil then Exit;
  
  case current_lexeme.TokenType of
    ttIDENT:
      begin
        pushExpression(current_lexeme.token);
        consume(ttIDENT);
      end;
      
    ttLITERAL_OCT, ttLITERAL_DEC, ttLITERAL_HEX, ttLITERAL_REAL, ttLITERAL_STR:
      begin
        pushExpression(current_lexeme.token);
        consume(current_lexeme.TokenType);
      end;
      
    ttLPAREN:
      begin
        consume(ttLPAREN);
        proc_expr;
        consume(ttRPAREN);
      end;
      
    ttNOT:
      begin
        consume(ttNOT);
        proc_factor;
        expr := popExpression;
        temp := generateTemp;
        addCommand(mnNOT, TTarget.Create(temp), TSource.Create(expr), TSource.Create(''));
        pushExpression(temp);
      end;
      
    ttADD, ttSUB:
      begin
        op := current_lexeme.TokenType;
        consume(op);
        proc_factor;
        expr := popExpression;
        temp := generateTemp;
        
        if op = ttADD then
          addCommand(mnADD, TTarget.Create(temp), TSource.Create('0'), TSource.Create(expr))
        else
          addCommand(mnSUB, TTarget.Create(temp), TSource.Create('0'), TSource.Create(expr));
        
        pushExpression(temp);
      end;
      
  else
    raise ESyntaticalError.Create('Invalid factor: ' + current_lexeme.str);
  end;
end;

end.
