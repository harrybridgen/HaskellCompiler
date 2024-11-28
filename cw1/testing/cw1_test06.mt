let var x;
    var y;
    var min;
    var max
in
begin
  getint(x);
  getint(y);
  max := x>y ? x : y;
  min := x<=y ? x : y;
  printint(min);
  printint(max) 
end 