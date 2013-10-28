; ModuleID = 'list_reverse.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.node = type { i32, %struct.node* }

; Function Attrs: nounwind uwtable
define %struct.node* @list_reverse(%struct.node* %l) #0 {
  %1 = alloca %struct.node*, align 8
  %a = alloca %struct.node*, align 8
  %b = alloca %struct.node*, align 8
  %c = alloca %struct.node*, align 8
  store %struct.node* %l, %struct.node** %1, align 8
  store %struct.node* null, %struct.node** %a, align 8
  store %struct.node* null, %struct.node** %b, align 8
  store %struct.node* null, %struct.node** %c, align 8
  %2 = load %struct.node** %1, align 8
  store %struct.node* %2, %struct.node** %a, align 8
  store %struct.node* null, %struct.node** %b, align 8
  br label %3

; <label>:3                                       ; preds = %6, %0
  %4 = load %struct.node** %a, align 8
  %5 = icmp ne %struct.node* %4, null
  br i1 %5, label %6, label %15

; <label>:6                                       ; preds = %3
  %7 = load %struct.node** %b, align 8
  store %struct.node* %7, %struct.node** %c, align 8
  %8 = load %struct.node** %a, align 8
  store %struct.node* %8, %struct.node** %b, align 8
  %9 = load %struct.node** %a, align 8
  %10 = getelementptr inbounds %struct.node* %9, i32 0, i32 1
  %11 = load %struct.node** %10, align 8
  store %struct.node* %11, %struct.node** %a, align 8
  %12 = load %struct.node** %c, align 8
  %13 = load %struct.node** %b, align 8
  %14 = getelementptr inbounds %struct.node* %13, i32 0, i32 1
  store %struct.node* %12, %struct.node** %14, align 8
  br label %3

; <label>:15                                      ; preds = %3
  %16 = load %struct.node** %b, align 8
  ret %struct.node* %16
}

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf"="true" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "unsafe-fp-math"="false" "use-soft-float"="false" }
