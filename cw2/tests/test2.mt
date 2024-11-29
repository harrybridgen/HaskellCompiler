let var x : Integer := 6;
    fun f (y:Boolean, z:Integer) : Boolean = !y ? z-1 : z+1
in 
  printint(f (x<10,x*x))

