boolean(input1, I1*i1, L, X) :- 
   member(I2*i2, L),
   member(Out*o, L),
   Out is I1/\I2 -> X=1.

   

