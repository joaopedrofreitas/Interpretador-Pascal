unit TokenType;

interface

type
  TTokenType = (
    ttINVALID, ttEOF, ttEND_OF_FILE,
    ttIDENT,
    ttADD, ttSUB, ttMUL, ttDIV, ttMOD, ttFLOORDIV,
    ttOR, ttAND, ttNOT, ttEQL, ttNEQ, ttGTR, ttGEQ, ttLSS, ttLEQ, ttASSIGN,
    ttPROGRAM, ttVAR, ttTYPE_INT, ttTYPE_REAL, ttTYPE_STR, ttBEGIN, ttEND,
    ttFOR, ttTO, ttWHILE, ttDO, ttBREAK, ttCONTINUE, ttIF, ttELSE, ttTHEN,
    ttWRITE, ttWRITELN, ttREAD, ttREADLN,
    ttSEMICOLON, ttCOMMA, ttPERIOD, ttCOLON, ttLPAREN, ttRPAREN, ttQUOTES,
    ttLITERAL_OCT, ttLITERAL_DEC, ttLITERAL_HEX, ttLITERAL_REAL, ttLITERAL_STR
  );

function TokenTypeToStr(token: TTokenType): string;

implementation

function TokenTypeToStr(token: TTokenType): string;
begin
  case token of
    ttINVALID: Result := 'INVALID_TOKEN';
    ttEOF: Result := 'EOF';
    ttEND_OF_FILE: Result := 'END_OF_FILE';
    ttIDENT: Result := 'IDENTIFIER';
    ttADD: Result := 'ADD';
    ttSUB: Result := 'SUBTRACT';
    ttMUL: Result := 'MULTIPLY';
    ttDIV: Result := 'DIVIDE';
    ttMOD: Result := 'MODULO';
    ttFLOORDIV: Result := 'FLOOR_DIV';
    ttOR: Result := 'OR';
    ttAND: Result := 'AND';
    ttNOT: Result := 'NOT';
    ttEQL: Result := 'EQUAL';
    ttNEQ: Result := 'NOT_EQUAL';
    ttGTR: Result := 'GREATER_THAN';
    ttGEQ: Result := 'GREATER_EQUAL';
    ttLSS: Result := 'LESS_THAN';
    ttLEQ: Result := 'LESS_EQUAL';
    ttASSIGN: Result := 'ASSIGN';
    ttPROGRAM: Result := 'PROGRAM';
    ttVAR: Result := 'VAR';
    ttTYPE_INT: Result := 'INTEGER_TYPE';
    ttTYPE_REAL: Result := 'REAL_TYPE';
    ttTYPE_STR: Result := 'STRING_TYPE';
    ttBEGIN: Result := 'BEGIN';
    ttEND: Result := 'END';
    ttFOR: Result := 'FOR';
    ttTO: Result := 'TO';
    ttWHILE: Result := 'WHILE';
    ttDO: Result := 'DO';
    ttBREAK: Result := 'BREAK';
    ttCONTINUE: Result := 'CONTINUE';
    ttIF: Result := 'IF';
    ttELSE: Result := 'ELSE';
    ttTHEN: Result := 'THEN';
    ttWRITE: Result := 'WRITE';
    ttWRITELN: Result := 'WRITELN';
    ttREAD: Result := 'READ';
    ttREADLN: Result := 'READLN';
    ttSEMICOLON: Result := 'SEMICOLON';
    ttCOMMA: Result := 'COMMA';
    ttPERIOD: Result := 'PERIOD';
    ttCOLON: Result := 'COLON';
    ttLPAREN: Result := 'LEFT_PAREN';
    ttRPAREN: Result := 'RIGHT_PAREN';
    ttQUOTES: Result := 'QUOTES';
    ttLITERAL_OCT: Result := 'OCTAL_LITERAL';
    ttLITERAL_DEC: Result := 'DECIMAL_LITERAL';
    ttLITERAL_HEX: Result := 'HEX_LITERAL';
    ttLITERAL_REAL: Result := 'REAL_LITERAL';
    ttLITERAL_STR: Result := 'STRING_LITERAL';
    else Result := 'UNKNOWN_TOKEN';
  end;
end;

end.
