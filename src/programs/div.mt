let
    fun div(x: Integer, y: Integer): Integer = x/y;
    var x: Integer := 10
in
printint(div(x,div(x,5)))

