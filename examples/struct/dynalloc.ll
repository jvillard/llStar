%astruct = type { i12, i32, i32* } ; a fairly typical struct maybe

declare noalias i8* @malloc(i64)
declare void @free(i8*)

define void @malloc_free() {
  %1 = call noalias i8* @malloc(i64 16)
  %2 = bitcast i8* %1 to %astruct*
  %3 = icmp ne %astruct* %2, null
  br i1 %3, label %4, label %6

; <label>:4                                       ; preds = %0
  %5 = bitcast %astruct* %2 to i8*
  call void @free(i8* %5)
  br label %6

; <label>:6                                       ; preds = %0, %4
  ret void
}

declare void @access_fields_of_struct(%astruct*)

define void @malloc_use_struct_free() {
  %1 = call noalias i8* @malloc(i64 16)
  %2 = bitcast i8* %1 to %astruct*
  %3 = icmp ne %astruct* %2, null
  br i1 %3, label %4, label %6

; <label>:4                                       ; preds = %0
  call void @access_fields_of_struct(%astruct* %2)
  %5 = bitcast %astruct* %2 to i8*
  call void @free(i8* %5)
  br label %6

; <label>:6                                       ; preds = %0, %4
  ret void
}

define void @malloc_use_field0_free() {
  %1 = call noalias i8* @malloc(i64 16)
  %2 = bitcast i8* %1 to %astruct*
  %3 = icmp ne %astruct* %2, null
  br i1 %3, label %4, label %8

; <label>:4                                       ; preds = %0
  %5 = getelementptr inbounds %astruct* %2, i32 0, i32 0
  %6 = load i12* %5, align 4
  %7 = bitcast %astruct* %2 to i8*
  call void @free(i8* %7)
  br label %8

; <label>:8                                       ; preds = %0, %4
  ret void
}

define void @malloc_use_field1_free() {
  %1 = call noalias i8* @malloc(i64 16)
  %2 = bitcast i8* %1 to %astruct*
  %3 = icmp ne %astruct* %2, null
  br i1 %3, label %4, label %8

; <label>:4                                       ; preds = %0
  %5 = getelementptr inbounds %astruct* %2, i32 0, i32 1
  %6 = load i32* %5, align 4
  %7 = bitcast %astruct* %2 to i8*
  call void @free(i8* %7)
  br label %8

; <label>:8                                       ; preds = %0, %4
  ret void
}

define void @malloc_use_field2_free() {
  %1 = call noalias i8* @malloc(i64 16)
  %2 = bitcast i8* %1 to %astruct*
  %3 = icmp ne %astruct* %2, null
  br i1 %3, label %4, label %8

; <label>:4                                       ; preds = %0
  %5 = getelementptr inbounds %astruct* %2, i32 0, i32 2
  %6 = load i32** %5, align 4
  %7 = bitcast %astruct* %2 to i8*
  call void @free(i8* %7)
  br label %8

; <label>:8                                       ; preds = %0, %4
  ret void
}
