let
    fun square (n : Integer) : Integer = n*n;
    var x : Integer := 2;
    var y : Integer := 2
in
printint(square(x) + square(square (y)))