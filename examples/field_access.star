predicate pointer[s](i64, s); /* location, value (of sort s) */
predicate pad(i64, i64); /* location, size in bytes */
predicate array[s](i64, i64, i64 -> s); /* location, number of elements, value */
predicate malloced(i64, i64); /* location, size in bytes */


import "../specs/stdlib.spec";
import "../rules/llvm.logic";

procedure malloc_free {emp}{emp};
procedure malloc_use_struct_free {emp}{emp};
procedure malloc_use_field0_free {emp}{emp};
procedure malloc_use_field1_free {emp}{emp};
procedure malloc_use_field2_free {emp}{emp};

procedure access_fields_of_struct(i64 %s)
  {pointer(i64 %s, named("astruct") _v)}
  {pointer(i64 %s, named("astruct") _v)};

procedure access_fields_of_structs(i64 %s1, i64 %s2, i64 %s3)
  {pointer(i64 %s1,named("astruct") _v1)
  *pointer(i64 %s2,named("astruct") _v2)
  *pointer(i64 %s3,named("astruct") _v3)}
  {pointer(i64 %s1,named("astruct") _v1)
  *pointer(i64 %s2,named("astruct") _v2)
  *pointer(i64 %s3,named("astruct") _v3)};

procedure unfold_set_fold(i64 %s)
  {pointer(i64 %s,named("astruct") _v)}
  {pointer(i64 %s,named("astruct") { i12 32, i32 52, i64 0})};

/*
procedure field_value_extraction(i64 %s) returns (i12 %x)
 {astruct_fld0(_v) = _x * astruct_fld1(_v) = i32 _y * astruct_fld2(_v) = i64 _z
  * pointer(i64 %s, named("astruct") _v)}
 {pointer(i64 %s, named("astruct") v) * i12 %x = i12 _x};
*/
