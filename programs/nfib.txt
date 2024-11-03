let var n;
    var a;
    var b;
    var temp
in
begin
    getint (n);
    if n <= 1 then a := n else
    begin
        a := 0;
        b := 1;
        while n > 1 do
        begin
            temp := a + b;
            a := b;
            b := temp;
            n := n - 1
        end
    end;
    printint (b)
end
