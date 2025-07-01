let
    fun max (a : Integer, b : Integer) : Integer = a >= b ? a : b;
    fun maxThree (a : Integer, b : Integer, c : Integer) : Integer = max(a, max(b, c));
    var x : Integer;
    var y : Integer;
    var z : Integer;
    var result : Integer
in
begin
    getint(x);
    getint(y);
    getint(z);
    result := maxThree(x, y, z);
    printint(result)
end
