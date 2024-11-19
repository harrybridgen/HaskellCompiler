let var n;
    var result
in
begin
    getint (n);
    if n <= 1 then result := 1 else
    begin
        result := 1;
        while n > 1 do
        begin
            result := result * n;
            n := n - 1
        end
    end;
    printint (result)
end
