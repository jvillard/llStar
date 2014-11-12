import "llvm.logic";

test_ret:
  {}{i32 $ret_v1 = i32 42}

test_unreachable: {False}{False}

test_load:
  {x |-> i32 _v}
  {x |-> i32 _v * i32 $ret_v1 = i32 _v}

test_store:
  {x |-> i32 _v}
  {x |-> i32 y}

test_alloca: {}{i32 $ret_v1 = i32 1}

test_switch:
  {}{i1 $ret_v1 = True}

trunc_and_zext:
  {}{i32 $ret_v1 = i32 @parameter0:}

trunc_and_sext:
  {}{i32 $ret_v1 = i32 @parameter0:}

ptr_to_int_to_ptr:
  {}{i32 $ret_v1 = concat(i24 0, i8 @parameter0:)}
