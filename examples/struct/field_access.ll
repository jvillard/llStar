target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%astruct = type { i12, i32, i32* } ; a fairly typical struct maybe

define void @access_fields_of_struct(%astruct* %s) {
  %1 = getelementptr inbounds %astruct* %s, i32 0, i32 0
  %2 = load i12* %1, align 4
  %3 = getelementptr inbounds %astruct* %s, i32 0, i32 1
  %4 = load i32* %3, align 4
  %5 = getelementptr inbounds %astruct* %s, i32 0, i32 2
  %6 = load i32** %5, align 4
  ret void
}

; access field i of %si
define void @access_fields_of_structs(%astruct* %s1, %astruct* %s2, %astruct* %s3) {
  %1 = getelementptr inbounds %astruct* %s1, i32 0, i32 0
  %2 = load i12* %1, align 4
  %3 = getelementptr inbounds %astruct* %s2, i32 0, i32 1
  %4 = load i32* %3, align 4
  %5 = getelementptr inbounds %astruct* %s3, i32 0, i32 2
  %6 = load i32** %5, align 4
  ret void
}

define void @unfold_set_fold(%astruct* %s) {
  %1 = getelementptr inbounds %astruct* %s, i32 0, i32 0
  store i12 32, i12* %1, align 4
  %2 = getelementptr inbounds %astruct* %s, i32 0, i32 1
  store i32 52, i32* %2, align 4
  %3 = getelementptr inbounds %astruct* %s, i32 0, i32 2
  store i32* null, i32** %3, align 4
  call void @access_fields_of_struct(%astruct* %s)
  ret void
}

define i12 @field_value_extraction(%astruct* %s) {
  %pf0 = getelementptr inbounds %astruct* %s, i32 0, i32 0
  %f0 = load i12* %pf0
  ret i12 %f0
}
