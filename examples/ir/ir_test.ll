target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

define i32 @test_ret() {
  ret i32 42
}

define void @test_unreachable() {
  unreachable
}

define i32 @test_load(i32* %x) {
  %1 = load i32* %x
  ret i32 %1
}

define void @test_store(i32* %x, i32 %y) {
  store i32 %y, i32* %x
  ret void
}

define i32 @test_alloca() {
  %1 = alloca i32
  store i32 1, i32* %1
  %2 = load i32* %1
  ret i32 %2
}

define i1 @test_switch(i32 %rand) {
  %1 = alloca i32, align 4
  %x = alloca i32, align 4
  store i32 %rand, i32* %1, align 4
  store i32 1, i32* %x, align 4
  %2 = load i32* %1, align 4
  switch i32 %2, label %4 [
    i32 0, label %3
    i32 1, label %5
  ]

; <label>:3
  store i32 0, i32* %x, align 4
  br label %5

; <label>:4
  store i32 1, i32* %1, align 4
  br label %5

; <label>:5
  %6 = load i32* %x, align 4
  %7 = load i32* %1, align 4
  %8 = icmp eq i32 %6, %7
  ret i1 %8
}

define i32 @trunc_and_zext(i32 %x) nounwind uwtable {
  %1 = alloca i32, align 4
  store i32 %x, i32* %1, align 4
  %2 = load i32* %1, align 4
  %3 = zext i32 %2 to i64
  %4 = trunc i64 %3 to i32
  ret i32 %4
}

define i32 @trunc_and_sext(i32 %x) nounwind uwtable {
  %1 = alloca i32, align 4
  store i32 %x, i32* %1, align 4
  %2 = load i32* %1, align 4
  %3 = sext i32 %2 to i64
  %4 = trunc i64 %3 to i32
  ret i32 %4
}

define i32 @ptr_to_int_to_ptr(i8 zeroext %x) nounwind uwtable {
  %1 = alloca i8, align 1
  store i8 %x, i8* %1, align 1
  %2 = load i8* %1, align 1
  %3 = zext i8 %2 to i64
  %4 = inttoptr i64 %3 to i8*
  %5 = ptrtoint i8* %4 to i32
  ret i32 %5
}
