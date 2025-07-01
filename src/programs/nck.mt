let 
    fun fact (n: Integer) : Integer = n <= 0 ? 1 : n * fact(n - 1);
    fun nck (n: Integer, k: Integer) : Integer = n < k || n < 0 || k < 0 ? 0 : fact(n) / (fact(n-k) * fact(k));
    var n : Integer;
    var k : Integer;
    var result: Integer
in
begin
    getint(n);
    getint(k);
    result := nck(n, k);
    printint(result)
end
