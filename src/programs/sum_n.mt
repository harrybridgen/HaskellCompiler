let
fun sumToN (n : Integer) : Integer = n <= 0 ? 0 : n + sumToN(n - 1);
var n : Integer;
var result : Integer
in
begin
    getint(n);
    result := sumToN(n);
    printint(result)
end
