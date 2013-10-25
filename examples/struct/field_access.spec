import "stdlib.spec";

malloc_free: {}{}
malloc_use_struct_free: {}{}
malloc_use_field0_free: {}{}
malloc_use_field1_free: {}{}
malloc_use_field2_free: {}{}

access_fields_of_struct:
  {pointer(@parameter0:,named_type("astruct"),_v)}
  {pointer(@parameter0:,named_type("astruct"),_v)}

access_fields_of_structs:
  {pointer(@parameter0:,named_type("astruct"),_v1)*pointer(@parameter1:,named_type("astruct"),_v2)*pointer(@parameter2:,named_type("astruct"),_v3)}
  {pointer(@parameter0:,named_type("astruct"),_v1)*pointer(@parameter1:,named_type("astruct"),_v2)*pointer(@parameter2:,named_type("astruct"),_v3)}

unfold_set_fold:
  {pointer(@parameter0:,named_type("astruct"),_v)}
  {pointer(@parameter0:,named_type("astruct"),mk_astruct(bv_const("12", "32"), bv_const("32", "52"), NULL()))}

field_value_extraction:
 {astruct_fld0(v) = x * astruct_fld1(v) = y * astruct_fld2(v) = z * pointer(@parameter0:, named_type("astruct"), v)}
 {$ret_v1 = x * pointer(@parameter0:, named_type("astruct"), v)}
