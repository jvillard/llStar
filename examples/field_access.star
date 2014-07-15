import "../specs/stdlib.spec";
import "../rules/llvm.logic";

procedure malloc_free {emp}{emp};
procedure malloc_use_struct_free {emp}{emp};
procedure malloc_use_field0_free {emp}{emp};
procedure malloc_use_field1_free {emp}{emp};
procedure malloc_use_field2_free {emp}{emp};

procedure access_fields_of_struct(i64 %s)
  {pointer(i64 %s, lltype named("astruct"), llmem _v)}
  {pointer(i64 %s, lltype named("astruct"), llmem _v)};

procedure access_fields_of_structs(i64 %s1, i64 %s2, i64 %s3)
  {pointer(i64 %s1,lltype named("astruct"),llmem _v1)
  *pointer(i64 %s2,lltype named("astruct"),llmem _v2)
  *pointer(i64 %s3,lltype named("astruct"),llmem _v3)}
  {pointer(i64 %s1,lltype named("astruct"),llmem _v1)
  *pointer(i64 %s2,lltype named("astruct"),llmem _v2)
  *pointer(i64 %s3,lltype named("astruct"),llmem _v3)};

procedure unfold_set_fold(i64 %s)
  {pointer(i64 %s,lltype named("astruct"),llmem _v)}
  {pointer(i64 %s,lltype named("astruct"), as(named("astruct") { i12 32, i32 52, i64 0}))};

/*
procedure field_value_extraction(i64 %s) returns (i12 %x)
 {astruct_fld0(_v) = _x * astruct_fld1(_v) = i32 _y * astruct_fld2(_v) = i64 _z
  * pointer(i64 %s, lltype named("astruct"), llmem _v)}
 {pointer(i64 %s, lltype named("astruct"), v) * i12 %x = i12 _x};
*/
