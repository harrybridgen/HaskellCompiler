let
    fun double(x: Integer): Integer = x * 2;
    fun quad(x: Integer): Integer = double(x) * 2;
    fun times10(x: Integer, y: Boolean): Integer = y ? x * 10 : x;
    fun add1(x: Integer): Integer = x + 1;
    fun times100(x: Integer): Integer = x * 100;
    fun complex(x: Integer): Integer = times100(add1(quad(double(x))))
in
    printint(times10(complex(quad(2)), true))
