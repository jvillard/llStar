malloc:
  {}
  {($ret_v1 != NULL()
    * !bvult(bvadd.64($ret_v1,@parameter0:),bv_const("64", "18446744073709551615")) /* no overflow (2^64 - 1) */
    * malloced($ret_v1,@parameter0:)
    * pointer($ret_v1,array_type(@parameter0:,integer_type(numeric_const("8"))),_v))
  || $ret_v1 = NULL()}

__safe_malloc:
  {}
  {malloced($ret_v1,@parameter0:) * pointer($ret_v1,@parameter0:,_v)}

/*
free:
  {malloced(@parameter0:,_s) * pointer(@parameter0:,array_type(_s,integer_type(numeric_const("8"))),_v)}
  {}
*/

free:
  {malloced(@parameter0:,sizeof(?t)) * pointer(@parameter0:,?t,_v)}
  {}

__VERIFIER_assert:
   {@parameter0: != bv_const("32", "0")}
   {@parameter0: != bv_const("32", "0")}

__VERIFIER_assume:
   {}
   {@parameter0: != bv_const("32", "0")}
