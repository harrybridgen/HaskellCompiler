let var hail
in 
begin
  getint(hail);
  while hail > 1 do
  begin
    printint(hail);
    hail := hail - hail / 2 * 2 == 0 ? hail / 2 : 3*hail + 1
  end;
  printint(hail)
end