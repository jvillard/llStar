malloc:
  {}
  {(i64 $ret_v1 != NULL()
    * malloced(i64 $ret_v1, i64 @parameter0:)
    * $ret_v1 |-> [ i64 @parameter0: x i8 ] _v)
  || i64 $ret_v1 = NULL()}

__safe_malloc:
  {}
  {malloced(i64 $ret_v1, i64 @parameter0:)
   * $ret_v1 |-> [ i64 @parameter0: x i8 ] _v}

/*
free:
  {malloced(@parameter0:,_s) * pointer(@parameter0:,array_type(_s,integer_type("8")),_v)}
  {}
*/

free:
  {malloced(i64 @parameter0:, i64 ?s) * @parameter0: |-> [ i64 ?s x i8] _v}
  {}

__VERIFIER_assert:
   {i32 @parameter0: != i32 0 }
   {i32 @parameter0: != i32 0 }

__VERIFIER_assume:
   {}
   {i32 @parameter0: != i32 0 }
