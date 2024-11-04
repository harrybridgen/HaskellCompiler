let var x
in
begin
    x := 5;
    while x > 0 do
        begin
            printint(x);
            x := x - 1
        end;
    printint(x)
end
