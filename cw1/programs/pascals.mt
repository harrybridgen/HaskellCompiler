let var n;
    var row;
    var col;
    var current;
    var previous;
    var numerator;
    var denominator
in
begin
    getint (n); row := 0; while row < n do
        begin col := 0; current := 1; while col <= row do
                begin
                    printint (current);  
                    previous := current;
                    numerator := previous * (row - col);  
                    denominator := col + 1;               
                    current := numerator / denominator;   
                    col := col + 1
                end;
            row := row + 1
        end
end
