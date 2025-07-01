let
fun fib (n : Integer) : Integer = n <= 1 ? n : fib(n - 1) + fib(n - 2);
var num : Integer;
var result : Integer
in
begin
    getint(num)
    result := fib(num);
    printint(result)
end
