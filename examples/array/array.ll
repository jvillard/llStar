target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@const_accesses.a = private unnamed_addr constant [10 x i32] [i32 -1, i32 -2, i32 -3, i32 -4, i32 -5, i32 -6, i32 -7, i32 -8, i32 -9, i32 -10], align 16

define i32 @const_access_begin() nounwind uwtable {
  %a = alloca [10 x i32], align 16
  %1 = getelementptr inbounds [10 x i32]* %a, i32 0, i64 0
  %2 = load i32* %1, align 4
  ret i32 %2
}

define i32 @const_access_middle() nounwind uwtable {
  %a = alloca [10 x i32], align 16
  %1 = getelementptr inbounds [10 x i32]* %a, i32 0, i64 4
  %2 = load i32* %1, align 4
  ret i32 %2
}

define i32 @const_access_end() nounwind uwtable {
  %a = alloca [10 x i32], align 16
  %1 = getelementptr inbounds [10 x i32]* %a, i32 0, i64 9
  %2 = load i32* %1, align 4
  ret i32 %2
}


; accesses a[i]
define i32 @access([0 x i32]* %a, i64 %i) nounwind uwtable {
  %1 = getelementptr inbounds [0 x i32]* %a, i32 0, i64 %i
  %2 = load i32* %1, align 4
  ret i32 %2
}

; accesses a[i]
define i32 @access_i32([0 x i32]* %a, i32 %i) nounwind uwtable {
  %1 = sext i32 %i to i64
  %2 = getelementptr inbounds [0 x i32]* %a, i64 0, i64 %1
  %3 = load i32* %2, align 4
  ret i32 %3
}
