import "stdlib.spec";
import "../rules/llvm.logic";

procedure (i32 %x) := pointer_arith(i32 %y):
   {} ()
   {/i32 %x/ i32 %x = i32 %y} [i32 %y]

procedure (i32 %x) := main:
  {}
  {/i32 %x/ i32 %x = i32 0}
