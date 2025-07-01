let
    fun and(x: Boolean, y: Boolean): Boolean = x && y;
    fun or(x: Boolean, y: Boolean): Boolean = x || y;
    fun xor(x: Boolean, y: Boolean): Boolean = (x && !y) || (!x && y);

    fun sum(a:Boolean,b:Boolean,carry:Boolean) : Boolean = xor(xor(a, b), carry);
    fun carry(a:Boolean,b:Boolean,carry:Boolean) : Boolean = or(and(a, b), and(xor(a, b), carry));

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
    sum0 := sum(a0, b0, carryIn);
    carryOut := carry(a0,b0,carryIn);

    sum1 := sum(a1, b1, carryOut);
    carryOut := carry(a1,b1,carryOut);

    sum2 := sum(a2, b2, carryOut);
    carryOut := carry(a2,b2,carryOut);

    sum3 := sum(a3, b3, carryOut);
    carryOut := carry(a3,b3,carryOut)
end
