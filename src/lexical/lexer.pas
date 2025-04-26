unit Lexer;

interface

uses
  TokenType, LexemeUnit;

type
  AFD_State = (
    STATE_INITIAL,
    STATE_SLASH,
    STATE_COMMENT_LINE,
    STATE_COMMENT_BLOCK,
    STATE_LESS_THAN,
    STATE_GREATER_THAN,
    STATE_ALNUM,
    STATE_DIGIT,
    STATE_REAL,
    STATE_STRING,
    STATE_ZERO,
    STATE_OCTAL,
    STATE_HEX,
    STATE_COLON,
    STATE_EQUAL,
    STATE_FINAL
  );


function nextToken(var f:Text): Lexeme;

implementation

var
  f_line: integer = 1;
  f_column: integer = 1;
  State: AFD_State;

procedure newLine(var lexeme: Lexeme);
begin
  Inc(f_line);
  f_column := 0;
  lexeme.line := f_line;
  lexeme.column := f_column + 1;
end;

procedure ungetChar(c: char; var f: Text);
begin
  if c <> #26 then    
    Seek(f, FilePos(f) - 1);
  Dec(f_column);
end;

function nextToken(var f:Text): Lexeme;
  var 
    c: char;
    lexeme: Lexeme;
  begin
    state := STATE_INITIAL;
    
    lexeme := Lexeme.Create;
    lexeme.line := f_line;
    lexeme.column := f_column;
  end;
    

end.
