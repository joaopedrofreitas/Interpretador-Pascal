unit ExceptionUnit;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  TCompilerError = class(Exception)
  private
    FLine: Integer;
    FColumn: Integer;
  public
    constructor Create(const Msg: string; Line, Column: Integer);
    property Line: Integer read FLine;
    property Column: Integer read FColumn;
  end;

  TLexicalError = class(TCompilerError);
  TSyntaxError = class(TCompilerError);

implementation

constructor TCompilerError.Create(const Msg: string; Line, Column: Integer);
begin
  inherited Create(Msg);
  FLine := Line;
  FColumn := Column;
end;

end.
