program equal;

var
  a, b: integer;
  r: real;

begin
  a := 25 div 03;
  b := a / 0x02;
  r := 0.2;

  if (a = b) then
    write ("that's\tit");

  else if (a == b) then
    write ("ok");

  else if (a = b) then
    write ("arigato");

  else if (a == b) then
    write ("rouje");
end.
