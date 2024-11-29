let
  fun square (n : Integer) : Integer = n*n;
  var x : Integer := 7;
  var y : Integer := 3
in
printint(square(x) + square(square (y)))
