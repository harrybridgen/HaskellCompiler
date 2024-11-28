let var x :=35
in
  printint( 10 <= x && x < 20 || !(x < 30 || x >= 40) ?
            x*x-5-x :
            x/17*x )