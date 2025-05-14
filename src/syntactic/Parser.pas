unit Parser;

interface

uses
  SysUtils,
  LexemeUnit,
  TokenType;

type
    TParser = class
    private
        m_lexemes: TArray<TLexeme>;
        m_pos: Integer;
        function current_lexeme: TLexeme;
        procedure consume(expected: TTokenType);

        // ------------------------------------
        //  main function
        // ------------------------------------
        procedure proc_function;
        // ------------------------------------
        //  variable declarations
        // ------------------------------------
        procedure proc_declarations;
        procedure proc_declaration;
        procedure proc_listIdent;
        procedure proc_restIdentList;
        procedure proc_restDeclaration;
        procedure proc_type;
        // ------------------------------------
        //  program statements
        // ------------------------------------
        procedure proc_block;
        procedure proc_stmtList;
        procedure proc_stmt;
        // ---------------------------
        //  statement descriptions
        // ---------------------------
        // for command
        procedure proc_forStmt;
        procedure proc_endFor;
        // IO commands
        procedure proc_ioStmt;
        procedure proc_outList;
        procedure proc_restoOutList;
        procedure proc_out;
        procedure proc_whileStmt;
        procedure proc_ifStmt;
        procedure proc_elsePart;
        // ------------------------------
        //  expressions
        // ------------------------------
        procedure proc_atrib;
        procedure proc_expr;
        procedure proc_or;
        procedure proc_restoOr;
        procedure proc_and;
        procedure proc_restoAnd;
        procedure proc_not;
        procedure proc_rel;
        procedure proc_restoRel;
        procedure proc_add;
        procedure proc_restoAdd;
        procedure proc_mult;
        procedure proc_restoMult;
        procedure proc_uno;
        procedure proc_fator;

    public
        constructor Create(lexemes: TArray<TLexeme>);
        procedure Start;
    end;

implementation

constructor TParser.Create(lexemes: TArray<TLexeme>);
begin
    m_lexemes := lexemes;
    m_pos := 0;
end;

procedure TParser.Start;
begin
    proc_function;
end;

function TParser.current_lexeme: TLexeme;
begin
    Result := m_lexemes[m_pos];
end;

procedure TParser.consume(expected: TTokenType);
begin
    if expected = current_lexeme.type then
        Inc(m_pos)

    else
        raise Exception.Create('Syntactic error -> expected ' + tt2str(expected) +
                            ', found ' + tt2str(current_lexeme.type) +
                            #10 + 'line: ' + IntToStr(current_lexeme.line) +
                            #10 + 'column: ' + IntToStr(current_lexeme.column));
end;

// ------------------------------------
//  main function
// ------------------------------------

// <function*> -> 'program' 'IDENT' ';' <declarations> 'begin' <stmtList> 'end' '.' ;
procedure TParser.proc_function;
begin
    consume(TT_PROGRAMSYM);
    consume(TT_IDENT);
    consume(TT_SEMICOLON);
    proc_declarations;
    consume(TT_BEGINSYM);
    proc_stmtList;
    consume(TT_ENDSYM);
    consume(TT_PERIOD);
end;

// ------------------------------------
//  variable declarations
// ------------------------------------

// <declarations> -> var <declaration> <restDeclaration> ;
procedure TParser.proc_declarations;
begin
    consume(TT_VARSYM);
    proc_declaration;
    proc_restDeclaration;
end;

// <declaration> -> <listIdent> ':' <type> ';' ;
procedure TParser.proc_declaration;
begin
    proc_listIdent;
    consume(TT_COLON);
    proc_type;
    consume(TT_SEMICOLON);
end;

// <listIdent> -> 'IDENT' <restIdentList> ;
procedure TParser.proc_listIdent;
begin
    consume(TT_IDENT);
    proc_restIdentList;
end;

// <restIdentList> -> ',' 'IDENT' <restIdentList> | & ;
procedure TParser.proc_restIdentList;
begin
    if current_lexeme.type = TT_COMMA then
    begin
        consume(TT_COMMA);
        consume(TT_IDENT);
        proc_restIdentList;
    end;
end;

// <restDeclaration> -> <declaration><restDeclaration> | & ;
procedure TParser.proc_restDeclaration;
begin
    if current_lexeme.type = TT_IDENT then
    begin
        proc_declaration;
        proc_restDeclaration;
    end;
end;

// <type> -> 'integer' | 'real' | 'string' ;
procedure TParser.proc_type;
begin
    case current_lexeme.type of
        TT_TYPE_INT: consume(TT_TYPE_INT);
        TT_TYPE_REAL: consume(TT_TYPE_REAL);
        TT_TYPE_STR: consume(TT_TYPE_STR);

    // TODO: jogar um erro namoral aqui
    else
        raise Exception.Create('DEU PAU 1: ' + current_lexeme.str);
    end;
end;

// ------------------------------------
//  program statements
// ------------------------------------

// <block> -> 'begin' <stmtList> 'end' ';' ;
procedure TParser.proc_block;
begin
    consume(TT_BEGINSYM);
    proc_stmtList;
    consume(TT_ENDSYM);
    consume(TT_SEMICOLON);
end;

// <stmtList> -> <stmt> <stmtList> | & ;
procedure TParser.proc_stmtList;
begin
    case current_lexeme.type of
            TT_FORSYM, TT_READSYM, TT_WRITESYM, TT_READLNSYM, TT_WRITELNSYM,
            TT_WHILESYM, TT_IDENT, TT_IFSYM, TT_BEGINSYM, TT_BREAKSYM, TT_CONTINUESYM, TT_SEMICOLON:
    begin
        proc_stmt;
        proc_stmtList;
    end;
end;

// <stmt> -> <forStmt>
//    | <ioStmt>
//    | <whileStmt>
//    | <atrib> ';'
//    | <ifStmt>
//    | <block>
//    | 'break'';'
//    | 'continue'';'
//    | ';' ;
procedure TParser.proc_stmt;
begin
    case current_lexeme.type of
        TT_FORSYM: proc_forStmt;

        TT_READSYM, TT_WRITESYM, TT_READLNSYM, TT_WRITELNSYM: proc_ioStmt;

        TT_WHILESYM: proc_whileStmt;

        TT_IDENT: 
        begin
            proc_atrib;
            consume(TT_SEMICOLON);
        end;

        TT_IFSYM: proc_ifStmt;

        TT_BEGINSYM: proc_block;

        TT_BREAKSYM: 
        begin
            consume(TT_BREAKSYM);
            consume(TT_SEMICOLON);
        end;

        TT_CONTINUESYM: 
        begin
            consume(TT_CONTINUESYM);
            consume(TT_SEMICOLON);
        end;

        TT_SEMICOLON: consume(TT_SEMICOLON);

        // TODO: jogar um erro namoral aqui
        else
            raise Exception.Create('DEU PAU 3: ' + current_lexeme.str);
    end;
end;

// ---------------------------
//  statement descriptions
// ---------------------------

// for command

// <forStmt> -> 'for' <atrib> 'to' <endFor> 'do' <stmt> ;
procedure TParser.proc_forStmt;
begin
    consume(TT_FORSYM);
    proc_atrib;
    consume(TT_TOSYM);
    proc_endFor;
    consume(TT_DOSYM);
    proc_stmt;
end;

// <endFor> -> 'IDENT' | 'NUMint' ;
procedure TParser.proc_endFor;
begin
    case current_lexeme.type of
        TT_IDENT: consume(TT_IDENT);
        TT_LITERAL_OCT: consume(TT_LITERAL_OCT);
        TT_LITERAL_DEC: consume(TT_LITERAL_DEC);
        TT_LITERAL_HEX: consume(TT_LITERAL_HEX);

    // TODO: jogar um erro namoral aqui
    else
        raise Exception.Create('DEU PAU 4: ' + current_lexeme.str);
    end;
end;

// IO commands
// <ioStmt> -> 'read' '(' 'IDENT' ')' ';'
//           | 'write' '(' <outList> ')' ';' ;
//           | 'readln' '(' 'IDENT' ')' ';'
//           | 'writeln' '(' <outList> ')' ';' ;
procedure TParser.proc_ioStmt;
begin
    case current_lexeme.type of
        TT_READSYM: 
        begin
            consume(TT_READSYM);
            consume(TT_LPAREN);
            consume(TT_IDENT);
            consume(TT_RPAREN);
            consume(TT_SEMICOLON);
        end;

        TT_WRITESYM: 
        begin
            consume(TT_WRITESYM);
            consume(TT_LPAREN);
            proc_outList;
            consume(TT_RPAREN);
            consume(TT_SEMICOLON);
        end;

        TT_READLNSYM: 
        begin
            consume(TT_READLNSYM);
            consume(TT_LPAREN);
            consume(TT_IDENT);
            consume(TT_RPAREN);
            consume(TT_SEMICOLON);
        end;

        TT_WRITELNSYM: 
        begin
            consume(TT_WRITELNSYM);
            consume(TT_LPAREN);
            proc_outList;
            consume(TT_RPAREN);
            consume(TT_SEMICOLON);
        end;
    end;
end;

// <outList> -> <out><restoOutList>;
procedure TParser.proc_outList;
begin
    proc_out;
    proc_restoOutList;
end;

// <restoOutList> -> ',' <outList> | &;
procedure TParser.proc_restoOutList;
begin
    if current_lexeme.type = TT_COMMA then
    begin
        consume(TT_COMMA);

        case current_lexeme.type of
                TT_LITERAL_STR, TT_IDENT, TT_LITERAL_OCT, TT_LITERAL_DEC, TT_LITERAL_HEX, TT_LITERAL_REAL:
            proc_outList;
        end;
    end;
end;

// <out> -> 'STR' | 'IDENT' | 'NUMint' | 'NUMfloat' ;
procedure TParser.proc_out;
begin
    case current_lexeme.type of
        TT_LITERAL_STR: consume(TT_LITERAL_STR);
        TT_IDENT: consume(TT_IDENT);
        TT_LITERAL_OCT: consume(TT_LITERAL_OCT);
        TT_LITERAL_DEC: consume(TT_LITERAL_DEC);
        TT_LITERAL_HEX: consume(TT_LITERAL_HEX);
        TT_LITERAL_REAL: consume(TT_LITERAL_REAL);
    end;
end;

// while command

// <whileStmt> -> 'while' <expr> 'do' <stmt> ;
procedure TParser.proc_whileStmt;
begin
    consume(TT_WHILESYM);
    proc_expr;
    consume(TT_DOSYM);
    proc_stmt;
end;

// if command

// <ifStmt> -> 'if' <expr> 'then' <stmt> <elsePart> ;
procedure TParser.proc_ifStmt;
begin
    consume(TT_IFSYM);
    proc_expr;
    consume(TT_THENSYM);
    proc_stmt;
    proc_elsePart;
end;

// <elsePart> -> 'else' <stmt> | & ;
procedure TParser.proc_elsePart;
begin
    if current_lexeme.type = TT_ELSESYM then
    begin
        consume(TT_ELSESYM);

        case current_lexeme.type of
                TT_FORSYM, TT_WHILESYM, TT_IDENT, TT_IFSYM, TT_BEGINSYM, TT_BREAKSYM, TT_CONTINUESYM, TT_SEMICOLON:
            proc_stmt;

        // TODO: jogar um erro namoral aqui
        else
            raise Exception.Create('DEU PAU 5: ' + current_lexeme.str);
        end;
    end;
end;

// ------------------------------
//  expressions
// ------------------------------

// <atrib> -> 'IDENT' ':=' <expr> ;
procedure TParser.proc_atrib;
begin
    consume(TT_IDENT);
    consume(TT_ASSIGN);
    proc_expr;
end;

// <expr> -> <or> ;
procedure TParser.proc_expr;
begin
    proc_or;
end;

// <or> -> <and> <restoOr> ;
procedure TParser.proc_or;
begin
    proc_and;
    proc_restoOr;
end;

// <restoOr> -> 'or' <and> <restoOr> | & ;
procedure TParser.proc_restoOr;
begin
    if current_lexeme.type = TT_OR then
    begin
        consume(TT_OR);
        proc_and;
        proc_restoOr;
    end;
end;

// <and> -> <not> <restoAnd> ;
procedure TParser.proc_and;
begin
    proc_not;
    proc_restoAnd;
end;

// <restoAnd> -> 'and' <not> <restoAnd> | & ;
procedure TParser.proc_restoAnd;
begin
    if current_lexeme.type = TT_AND then
    begin
        consume(TT_AND);
        proc_and;
        proc_restoAnd;
    end;
end;

// <not> -> 'not' <not> | <rel> ;
procedure TParser.proc_not;
begin
    if current_lexeme.type = TT_NOT then
    begin
        consume(TT_NOT);
        proc_not;
    end;

    else
       proc_rel; 
end;

// <rel> -> <add> <restoRel> ;
procedure TParser.proc_rel;
begin
    proc_add;
    proc_restoRel;
end;

// <restoRel> -> '==' <add> | '<>' <add>
//             | '<' <add> | '<=' <add>
//             | '>' <add> | '>=' <add> | & ;
procedure TParser.proc_restoRel;
begin
    case current_lexeme.type of
        TT_EQL:
        begin
            consume(TT_EQL);
            proc_add;
        end;

        TT_NEQ:
        begin
            consume(TT_NEQ);
            proc_add;
        end;

        TT_LSS:
        begin
            consume(TT_LSS);
            proc_add;
        end;

        TT_LEQ:
        begin
            consume(TT_LEQ);
            proc_add;
        end;

        TT_GTR:
        begin
            consume(TT_GTR);
            proc_add;
        end;

        TT_GEQ:
        begin
            consume(TT_GEQ);
            proc_add;
        end;
    end;
end;


// <add> -> <mult> <restoAdd> ;
procedure TParser.proc_add;
begin
    proc_mult;
    proc_restoAdd;
end;

// <restoAdd> -> '+' <mult> <restoAdd>
//             | '-' <mult> <restoAdd> | & ;
procedure TParser.proc_restoAdd;
begin
    case current_lexeme.type of
        TT_ADD:
        begin
            consume(TT_ADD);
            proc_mult;
            proc_restoAdd;
        end;

        TT_SUB:
        begin
            consume(TT_SUB);
            proc_mult;
            proc_restoAdd;
        end;
    end;
end;

// <mult> -> <uno> <restoMult> ;
procedure TParser.proc_mult;
begin
    proc_uno;
    proc_restoMult;
end;

// <restoMult> -> '*' <uno> <restoMult>
//             |  '/' <uno> <restoMult>
//             |  'mod' <uno> <restoMult> | & ;
//             |  'div' <uno> <restoMult> | & ;
procedure TParser.proc_restoMult;
begin
    case current_lexeme.type of
        TT_MUL:
        begin
            consume(TT_MUL);
            proc_uno;
            proc_restoMult;
        end;

        TT_DIV:
        begin
            consume(TT_DIV);
            proc_uno;
            proc_restoMult;
        end;

        TT_MOD:
        begin
            consume(TT_MOD);
            proc_uno;
            proc_restoMult;
        end;

        TT_FLOORDIV:
        begin
            consume(TT_FLOORDIV);
            proc_uno;
            proc_restoMult;
        end;
    end;
end;

// <uno> -> '+' <uno> | '-' <uno> | <fator> ;
procedure TParser.proc_uno;
begin
    case current_lexeme.type of
        TT_ADD:
        begin
            consume(TT_ADD);
            proc_uno;
        end;

        TT_SUB:
        begin
            consume(TT_SUB);
            proc_uno;
        end;
    else
        proc_fator;
    end;
end;

// <fator> -> 'NUMint' | 'NUMfloat'
//          | 'IDENT'  | '(' <expr> ')' | 'STR' ;
procedure TParser.proc_fator;
begin
    case current_lexeme.type of
        TT_LITERAL_OCT: consume(TT_LITERAL_OCT);

        TT_LITERAL_DEC: consume(TT_LITERAL_DEC);

        TT_LITERAL_HEX: consume(TT_LITERAL_HEX);

        TT_IDENT: consume(TT_IDENT);

        TT_LPAREN: 
        begin
            consume(TT_LPAREN);
            proc_expr;
            consume(TT_RPAREN);
        end;

        TT_LITERAL_STR: consume(TT_LITERAL_STR);

    // TODO: jogar um erro namoral aqui
    else
        raise Exception.Create('DEU PAU 6: ' + current_lexeme.str);
    end;
end;

end.
