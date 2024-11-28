let var x0 := 1;
    var x1 := 1;
    var transfer;
    var distance := 10
in 
begin
  while distance > 0 do
   begin
     printint(x0);
     transfer := x0+x1;
     x0 := x1;
     x1 := transfer;
     distance := distance - 1
   end
end