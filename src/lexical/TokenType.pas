unit TokenType;

interface

type
  TokenT = (
    { Special }
    
    TT_INVALID  = -1,
    TT_EOF = 0,
    TT_END_OF_FILE,

    { Arithmetic operators }
    TT_ADD,                { + }
    TT_SUB,                { - }
    TT_MUL,                { * }
    TT_DIV,                { / }
    TT_MOD,                { mod }
    TT_FLOORDIV,           { div }

    { Logical, relational operators and assignments }
    TT_OR,                 { or }
    TT_AND,                { and }
    TT_NOT,                { not }
    TT_EQL,                { == }
    TT_NEQ,                { <> }
    TT_GTR,                { > }
    TT_GEQ,                { >= }
    TT_LSS,                { < }
    TT_LEQ,                { <= }
    TT_ASSIGN,             { := }

    { Keywords }
    TT_PROGRAM,         { program }
    TT_VAR,             { var }
    TT_TYPE_INT,       { integer }
    TT_TYPE_REAL,          { real }
    TT_TYPE_STR,        { string }
    TT_BEGIN,           { begin }
    TT_END,             { end }
    TT_FOR,             { for }
    TT_TO,              { to }
    TT_WHILE,           { while }
    TT_DO,              { do }
    TT_BREAK,           { break }
    TT_CONTINUE,        { continue }
    TT_IF,              { if }
    TT_ELSE,            { else }
    TT_THEN,            { then }
    TT_WRITE,           { write }
    TT_WRITELN,         { writeln }
    TT_READ,            { read }
    TT_READLN,          { readln }

    { Symbols }
    TT_SEMICOLON,          { ; }
    TT_COMMA,              { , }
    TT_PERIOD,             { . }
    TT_COLON,              { : }
    TT_LPAREN,             { ( }
    TT_RPAREN,             { ) }
    TT_QUOTES,             { " }

    { Others }
    TT_IDENT,
    TT_LITERAL_OCT,
    TT_LITERAL_DEC,
    TT_LITERAL_HEX,
    TT_LITERAL_REAL,
    TT_LITERAL_STR
  );

function tt2str(token: TokenT): string;

implementation

function tt2str(token: TokenT): String;
begin
  case token of
    { Special }
    TT_INVALID:           tt2str := 'INVALID_TOKEN';
    TT_END_OF_FILE:       tt2str := 'END_OF_FILE';

    { Arithmetic operators }
    TT_ADD:               tt2str := 'ADD';
    TT_SUB:               tt2str := 'SUB';
    TT_MUL:               tt2str := 'MUL';
    TT_DIV:               tt2str := 'DIV';
    TT_MOD:               tt2str := 'MOD';
    TT_FLOORDIV:          tt2str := 'FLOORDIV';

    { Logical, relational operators and assignments }
    TT_OR:                tt2str := 'OR';
    TT_AND:               tt2str := 'AND';
    TT_NOT:               tt2str := 'NOT';
    TT_EQL:               tt2str := 'EQL';
    TT_NEQ:               tt2str := 'NEQ';
    TT_GTR:               tt2str := 'GTR';
    TT_GEQ:               tt2str := 'GEQ';
    TT_LSS:               tt2str := 'LSS';
    TT_LEQ:               tt2str := 'LEQ';
    TT_ASSIGN:            tt2str := 'ASSIGN';

    { Keywords }
    TT_PROGRAM:           tt2str := 'PROGRAM';
    TT_VAR:               tt2str := 'VAR';
    TT_TYPE_INT:          tt2str := 'TYPE_INTEGER';
    TT_TYPE_REAL:         tt2str := 'TYPE_REAL';
    TT_TYPE_STR:          tt2str := 'TYPE_STRING';
    TT_BEGIN:             tt2str := 'BEGIN';
    TT_END:               tt2str := 'END';
    TT_FOR:               tt2str := 'FOR';
    TT_TO:                tt2str := 'TO';
    TT_WHILE:             tt2str := 'WHILE';
    TT_DO:                tt2str := 'DO';
    TT_BREAK:             tt2str := 'BREAK';
    TT_CONTINUE:          tt2str := 'CONTINUE';
    TT_IF:                tt2str := 'IF';
    TT_ELSE:              tt2str := 'ELSE';
    TT_THEN:              tt2str := 'THEN';
    TT_WRITE:             tt2str := 'WRITE';
    TT_WRITELN:           tt2str := 'WRITELN';
    TT_READ:              tt2str := 'READ';
    TT_READLN:            tt2str := 'READLN';

    { Symbols }
    TT_SEMICOLON:         tt2str := 'SEMICOLON';
    TT_COMMA:             tt2str := 'COMMA';
    TT_PERIOD:            tt2str := 'PERIOD';
    TT_COLON:             tt2str := 'COLON';
    TT_LPAREN:            tt2str := 'LPAREN';
    TT_RPAREN:            tt2str := 'RPAREN';
    TT_QUOTES:            tt2str := 'QUOTES';

    { Others }
    TT_IDENT:             tt2str := 'IDENT';
    TT_LITERAL_OCT:     tt2str := 'LITERAL_OCTAL';
    TT_LITERAL_DEC:   tt2str := 'LITERAL_DECIMAL';
    TT_LITERAL_HEX:       tt2str := 'LITERAL_HEX';
    TT_LITERAL_REAL:      tt2str := 'LITERAL_REAL';
    TT_LITERAL_STR:    tt2str := 'LITERAL_STRING';

  else
    writeln('Error: Invalid token type');
    halt(1);
  end;
end;

end.
