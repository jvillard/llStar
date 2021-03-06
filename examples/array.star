import "llvm.logic";
import "stdlib.spec";

assert_in_bounds:
 {pointer(@parameter0:,integer_type(numeric_const("64")),_n) * !bvule(_n, @parameter1:)}
 {pointer(@parameter0:,integer_type(numeric_const("64")),_m) * !bvule(_m, @parameter1:)}

const_iter: {}{}

const_access_begin: {}{}
const_access_end: {}{}
const_access_middle: {}{}
const_accesses: {}{}

access:
  {pointer(@parameter0:,array_type(_n,integer_type("32")),_v) * !bvuge(@parameter1:,bvconst("64","0")) * !bvult(@parameter1:,_n)}
  {pointer(@parameter0:,array_type(_n,integer_type("32")),_w)}

access_i32:
  {pointer(@parameter0:,array_type(_n,integer_type("32")),_v) * !bvuge(@parameter1:,bvconst("32","0")) * !bvult(sign_extend.32("32",@parameter1:),_n)}
  {pointer(@parameter0:,array_type(_n,integer_type("32")),_w)}

multi_accesses: {}{$ret_v1 = bv_const("32", "0")}
const_array_iter: {}{}
heap_array_iter: {!bvugt(@parameter0:,NULL())}{}
stack_array_iter: {}{}
