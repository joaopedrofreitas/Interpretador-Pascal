program SomaSimples;
var
  i, soma, j: integer;
begin
  soma := 0;
  for j := 1 to 5 do
    soma := soma + j;
  writeln("Soma = ", soma);
end.
