unit TOKENTYPE;

interface

type
  TokenT = (
    // Special
    TT_END_OF_FILE,

    // Arithmetic operators
    TT_ADD,                // +
    TT_SUB,                // -
    TT_MUL,                // *
    TT_DIV,                // /
    TT_MOD,                // mod
    TT_DIVINT,             // div

    // Logical, relational operators and assignments
    TT_OR,                 // or
    TT_AND,                // and
    TT_NOT,                // not
    TT_EQUAL,              // ==
    TT_DIFFERENCE,         // <>
    TT_GREATER,            // >
    TT_GREATER_EQUAL,      // >=
    TT_LOWER,              // <
    TT_LOWER_EQUAL,        // <=
    TT_ASSIGN,             // :=

    // Keywords
    TT_PROGRAM,            // program
    TT_VAR,                // var
    TT_TYPE_INTEGER,       // integer
    TT_TYPE_REAL,          // real
    TT_TYPE_STRING,        // string
    TT_BEGIN,              // begin
    TT_END,                // end
    TT_FOR,                // for
    TT_TO,                 // to
    TT_WHILE,              // while
    TT_DO,                 // do
    TT_BREAK,              // break
    TT_CONTINUE,           // continue
    TT_IF,                 // if
    TT_ELSE,               // else
    TT_THEN,               // then
    TT_WRITE,              // write
    TT_WRITELN,            // writeln
    TT_READ,               // read
    TT_READLN,             // readln

    // Symbols
    TT_SEMICOLON,          // ;
    TT_COMMA,              // ,
    TT_PERIOD,             // .
    TT_COLON,              // :
    TT_OPEN_PARENTHESES,   // (
    TT_CLOSE_PARENTHESES,  // )
    TT_QUOTES,             // "

    // Others
    TT_VAR_NAME,
    TT_LITERAL_OCTAL,
    TT_LITERAL_DECIMAL,
    TT_LITERAL_HEX,
    TT_LITERAL_REAL,
    TT_LITERAL_STRING,
  );

const
  // Specials
  TT_UNEXPECTED_EOF = -2;
  TT_INVALID_TOKEN  = -1;

function tt2str(type: TokenT): string;

implementation

uses SysUtils;

function tt2str(type: TokenT): string;
begin
  case type of
    // Specials
    TT_UNEXPECTED_EOF:    Result := 'UNEXPECTED_EOF';
    TT_INVALID_TOKEN:     Result := 'INVALID_TOKEN';
    TT_END_OF_FILE:       Result := 'END_OF_FILE';

    // Arithmetic operators
    TT_ADD:               Result := 'ADD';
    TT_SUB:               Result := 'SUB';
    TT_MUL:               Result := 'MUL';
    TT_DIV:               Result := 'DIV';
    TT_MOD:               Result := 'MOD';
    TT_DIVINT:            Result := 'DIVINT';

    // Logical, relational operators and assignments
    TT_OR:                Result := 'OR';
    TT_AND:               Result := 'AND';
    TT_NOT:               Result := 'NOT';
    TT_EQUAL:             Result := 'EQUAL';
    TT_DIFFERENCE:        Result := 'DIFFERENCE';
    TT_GREATER:           Result := 'GREATER';
    TT_GREATER_EQUAL:     Result := 'GREATER_EQUAL';
    TT_LOWER:             Result := 'LOWER';
    TT_LOWER_EQUAL:       Result := 'LOWER_EQUAL';
    TT_ASSIGN:            Result := 'ASSIGN';

    // Symbols
    TT_SEMICOLON:         Result := 'SEMICOLON';
    TT_COMMA:             Result := 'COMMA';
    TT_PERIOD:            Result := 'PERIOD';
    TT_COLON:             Result := 'COLON';
    TT_OPEN_PARENTHESES:  Result := 'OPEN_PARENTHESES';
    TT_CLOSE_PARENTHESES: Result := 'CLOSE_PARENTHESES';
    TT_QUOTES:            Result := 'QUOTES';

    // Keywords
    TT_PROGRAM:           Result := 'PROGRAM';
    TT_VAR:               Result := 'VAR';
    TT_TYPE_INTEGER:      Result := 'TYPE_INTEGER';
    TT_TYPE_REAL:         Result := 'TYPE_REAL';
    TT_TYPE_STRING:       Result := 'TYPE_STRING';
    TT_BEGIN:             Result := 'BEGIN';
    TT_END:               Result := 'END';
    TT_FOR:               Result := 'FOR';
    TT_TO:                Result := 'TO';
    TT_WHILE:             Result := 'WHILE';
    TT_DO:                Result := 'DO';
    TT_BREAK:             Result := 'BREAK';
    TT_CONTINUE:          Result := 'CONTINUE';
    TT_IF:                Result := 'IF';
    TT_ELSE:              Result := 'ELSE';
    TT_THEN:              Result := 'THEN';
    TT_WRITE:             Result := 'WRITE';
    TT_WRITELN:           Result := 'WRITELN';
    TT_READ:              Result := 'READ';
    TT_READLN:            Result := 'READLN';

    // Others
    TT_VAR_NAME:          Result := 'VAR_NAME';
    TT_LITERAL_OCTAL:     Result := 'LITERAL_OCTAL';
    TT_LITERAL_DECIMAL:   Result := 'LITERAL_DECIMAL';
    TT_LITERAL_HEX:       Result := 'LITERAL_HEX';
    TT_LITERAL_REAL:      Result := 'LITERAL_REAL';
    TT_LITERAL_STRING:    Result := 'LITERAL_STRING';

  else
    // É preciso implementar para caso ache um token invalido
    halt(1);
  end;
end;

end.