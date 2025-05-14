unit TokenType;

interface

type
  TokenT = (
    { Special }
    
    TT_INVALID  = -1,
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
    TT_PROGRAMSYM,         { program }
    TT_VARSYM,             { var }
    TT_TYPE_INT,       { integer }
    TT_TYPE_REAL,          { real }
    TT_TYPE_STR,        { string }
    TT_BEGINSYM,           { begin }
    TT_ENDSYM,             { end }
    TT_FORSYM,             { for }
    TT_TOSYM,              { to }
    TT_WHILESYM,           { while }
    TT_DOSYM,              { do }
    TT_BREAKSYM,           { break }
    TT_CONTINUESYM,        { continue }
    TT_IFSYM,              { if }
    TT_ELSESYM,            { else }
    TT_THENSYM,            { then }
    TT_WRITESYM,           { write }
    TT_WRITELNSYM,         { writeln }
    TT_READSYM,            { read }
    TT_READLNSYM,          { readln }

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
    TT_LITERAL_OCTAL,
    TT_LITERAL_DECIMAL,
    TT_LITERAL_HEX,
    TT_LITERAL_REAL,
    TT_LITERAL_STRING
  );

function tt2str(token: TokenT): string;

implementation

function tt2str(token: TokenT): string;
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
    TT_PROGRAMSYM:        tt2str := 'PROGRAMSYM';
    TT_VARSYM:            tt2str := 'VARSYM';
    TT_TYPE_INT:          tt2str := 'TYPE_INTEGER';
    TT_TYPE_REAL:         tt2str := 'TYPE_REAL';
    TT_TYPE_STR:          tt2str := 'TYPE_STRING';
    TT_BEGINSYM:          tt2str := 'BEGINSYM';
    TT_ENDSYM:            tt2str := 'ENDSYM';
    TT_FORSYM:            tt2str := 'FORSYM';
    TT_TOSYM:             tt2str := 'TOSYM';
    TT_WHILESYM:          tt2str := 'WHILESYM';
    TT_DOSYM:             tt2str := 'DOSYM';
    TT_BREAKSYM:          tt2str := 'BREAKSYM';
    TT_CONTINUESYM:       tt2str := 'CONTINUESYM';
    TT_IFSYM:             tt2str := 'IFSYM';
    TT_ELSESYM:           tt2str := 'ELSESYM';
    TT_THENSYM:           tt2str := 'THENSYM';
    TT_WRITESYM:          tt2str := 'WRITESYM';
    TT_WRITELNSYM:        tt2str := 'WRITELNSYM';
    TT_READSYM:           tt2str := 'READSYM';
    TT_READLNSYM:         tt2str := 'READLNSYM';

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
    TT_LITERAL_OCTAL:     tt2str := 'LITERAL_OCTAL';
    TT_LITERAL_DECIMAL:   tt2str := 'LITERAL_DECIMAL';
    TT_LITERAL_HEX:       tt2str := 'LITERAL_HEX';
    TT_LITERAL_REAL:      tt2str := 'LITERAL_REAL';
    TT_LITERAL_STRING:    tt2str := 'LITERAL_STRING';

  else
    writeln('Error: Invalid token type');
    halt(1);
  end;
end;

end.
