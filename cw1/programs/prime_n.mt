let 




var 




n 

; 






var 

i 

; 

var 

j 

; 

var 

c 




:= 




2 

; 

var 

prime 


in 





begin 





    




getint 

(    

n     

)   

; 




while 

(        

c        

<=                  

n 

) 

do 

begin 

j                

:=             

2 

; 

prime             

:=          

1 

; 

while 

( 

prime 

&& 

j 

< 

c 

) 

do 

begin 

i         

:= 

c 

/ 

j 

; 

prime 

:= 

prime 

&& 

c  

!=       

j 

*    

i    

; 

j 

:= 

j    

+    

1 

end 

; 

if 

(      

prime      

) 

then 

printint 

(       

c 

) 

else 

prime 

:=            

prime      

; 

c     

:= 

c       

+  

1 

end 




end
















