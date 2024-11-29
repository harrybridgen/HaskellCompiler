let
  fun power (x : Integer, n : Integer) : Integer = n<=0 ? 1 : x * power(x,n-1);
  var x : Integer;
  var n : Integer
in
begin
  getint(x);
  getint(n);
  printint(power(x,n))
end

