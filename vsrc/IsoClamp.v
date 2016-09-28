module IsoClamp (
                 input  in,
                 input  iso, 
                 output out);
   
   assign out  = in & ~iso;
   
endmodule // AsyncResetReg

