let
    fun and(x: Boolean, y: Boolean): Boolean = x && y;
    fun or(x: Boolean, y: Boolean): Boolean = x || y;
    fun xor(x: Boolean, y: Boolean): Boolean = (x && !y) || (!x && y);

    var carryIn : Boolean := true;

    var a0 : Boolean := true;
    var a1 : Boolean := false;
    var a2 : Boolean := true;
    var a3 : Boolean := false;

    var b0 : Boolean := false;
    var b1 : Boolean := true;
    var b2 : Boolean := false;
    var b3 : Boolean := true;

    var sum0 : Boolean;
    var sum1 : Boolean;
    var sum2 : Boolean;
    var sum3 : Boolean;

    var carryOut : Boolean
in
begin
    sum0 := xor(xor(a0, b0), carryIn);
    sum1 := xor(xor(a1, b1), or(and(a0, b0), and(xor(a0, b0), carryIn)));
    sum2 := xor(xor(a2, b2), or(and(a1, b1), and(xor(a1, b1), or(and(a0, b0), and(xor(a0, b0), carryIn)))));
    sum3 := xor(xor(a3, b3), or(and(a2, b2), and(xor(a2, b2), or(and(a1, b1), and(xor(a1, b1), or(and(a0, b0), and(xor(a0, b0), carryIn)))))));
    carryOut := or(and(a3, b3), and(xor(a3, b3), or(and(a2, b2), and(xor(a2, b2), or(and(a1, b1), and(xor(a1, b1), or(and(a0, b0), and(xor(a0, b0), carryIn))))))))
end