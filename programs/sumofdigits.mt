let var n;
    var sum
in
begin
    getint (n);
    sum := 0;
    while n != 0 do
        begin
            sum := sum + (n % 10);
            n := n / 10 
        end;
    printint (sum)
end
