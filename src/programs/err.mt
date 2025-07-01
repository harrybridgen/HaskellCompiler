let
    var x : Integer := 10;
    var y : Integer := 5;
    fun addXY (add: Boolean) : Integer =  add ? x+y : 0
in
printint(addXY(true))

