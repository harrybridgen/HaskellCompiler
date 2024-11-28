let var x;
    var z := 3
in
begin
  getint(x);
  while z < x do z := z+x/z*7;
  printint(z) 
end 