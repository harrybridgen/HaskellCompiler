let var n;
    var a;
    var b;
    var temp;
    var i
in
begin
    getint (n);
    a := 0;
    b := 1;
    i := 1;
    while i <= n do
        begin
            printint (a);
            temp := b;
            b := a + b;
            a := temp;
            i := i + 1
        end
end
