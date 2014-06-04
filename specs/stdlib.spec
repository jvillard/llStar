procedure malloc(i64 %s) returns (i64 %x)
  {emp}
  {(i64 %x != NULL()
    * malloced(i64 %x, i64 %s)
    * pointer(i64 %x, lltype [ i64 %s x lltype i8 ], llmem _v))
  || i64 %x = NULL()}
;
/*
free:
  {malloced(@parameter0:,_s) * pointer(@parameter0:,array_type(_s,integer_type("8")),_v)}
  {emp}
*/

procedure free(i64 %x)
  {malloced(i64 %x, i64 ?s) * pointer(i64 %x, lltype [ i64 ?s x lltype i8], llmem _v)}
  {emp}
;
/*
__VERIFIER_assert:
   {i32 @parameter0: != i32 0 }
   {i32 @parameter0: != i32 0 }

__VERIFIER_assume:
   {emp}
   {i32 @parameter0: != i32 0 }
*/
