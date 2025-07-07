program Main;

{$mode objfpc}{$H+}

uses
  SysUtils,
  Lexer,          // seu módulo de análise léxica
  LexemeUnit,     // define TLexeme e TLexemeArray
  Parser,         // define TParser
  Printer,        // define TPrinter
  ExceptionUnit,  // define ELexicalError e ESyntaticalError
  Interpreter;    // define TInterpreter

var
  lexemes: TLexemeArray;
  commands: TCommandArray;
  varTypes: TVariableInfoArray;
  L: TLexer;
  P: TParser;
  interp: TInterpreter;
  inputFile: string;

begin
  if ParamCount <> 1 then
  begin
    Writeln('Uso: ', ExtractFileName(ParamStr(0)), ' <arquivo_pascal>');
    Halt(1);
  end;
  inputFile := ParamStr(1);

  try
    L := TLexer.Create;
    try
      // 1) Análise léxica
      lexemes := L.ScanFile(inputFile);
      TPrinter.PrintLexemes(lexemes);

      // 2) Parsing + geração de código intermediário
      P := TParser.Create(lexemes);
      try
        P.Start;
        Writeln('Parsing concluído sem erros sintáticos.');
        Writeln;

        // 3) Obtenha os comandos gerados e os tipos
        commands := P.GetCommands;
        varTypes := P.GetVariableTypes;

        // 4) Exiba os tipos de variáveis
        TPrinter.PrintVariableInfoArray(varTypes);
        Writeln;

        // 5) Exiba os comandos
        TPrinter.PrintCommands(commands);

        // 6) Interprete
        interp := TInterpreter.Create(varTypes);
        try
          interp.Execute(commands);
          interp.PrintState;
        finally
          interp.Free;
        end;

      finally
        P.Free;
      end;
    finally
      L.Free;
    end;

  except
    on E: ELexicalError do
      Writeln('Erro Léxico: ', E.Message);
    on E: ESyntaticalError do
      Writeln('Erro Sintático: ', E.Message);
    on E: Exception do
      Writeln('Erro: ', E.ClassName, ': ', E.Message);
  end;
end.

