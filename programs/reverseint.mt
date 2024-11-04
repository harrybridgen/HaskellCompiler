let var n;
    var reversed
in
begin
    getint (n);
    reversed := 0;
    while n != 0 do
        begin
            reversed := reversed * 10 + (n % 10);
            n := n / 10
        end;
    printint (reversed)
end
