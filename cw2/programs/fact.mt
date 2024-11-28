let
    fun fact (n: Integer) : Integer = n <= 0 ? 1 : n * fact(n - 1);
    var x: Integer
in

begin
    getint(x);
    printint(fact(x))
end
