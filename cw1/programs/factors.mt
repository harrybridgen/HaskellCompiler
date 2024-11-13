let var n;
    var factor := 2;
    var temp
in
begin
    getint (n);
    while (n > 1) do
    begin
        temp := n - (factor * (n / factor));

        if (temp == 0) then
        begin
            printint (factor);
            n := n / factor
        end
        else
            factor := factor + 1
    end
end