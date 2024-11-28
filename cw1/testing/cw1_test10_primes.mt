let var x;
    var y := 2;
    var k;
    var prime := 1
in 
begin
  getint (k);
  k := k>=0 ? k : -k;
  while k>0 do
    begin
      while !prime do
        begin
          y:=y+1;
          x := 2;
          prime := 1;
          while x*x <= y do
            begin
              prime := prime && y/x*x != y;
              if x==2 then x:=x+1 else x:=x+2
            end
        end;
      printint (y);
      k := k-1;
      prime:=0
    end
end