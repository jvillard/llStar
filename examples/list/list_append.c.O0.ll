; ModuleID = 'list_append.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.node = type { i32, %struct.node* }

; Function Attrs: nounwind uwtable
define %struct.node* @list_append(%struct.node* %l1, %struct.node* %l2) #0 {
  %1 = alloca %struct.node*, align 8
  %2 = alloca %struct.node*, align 8
  %cur = alloca %struct.node*, align 8
  %prev = alloca %struct.node*, align 8
  store %struct.node* %l1, %struct.node** %1, align 8
  store %struct.node* %l2, %struct.node** %2, align 8
  %3 = load %struct.node** %1, align 8
  store %struct.node* %3, %struct.node** %prev, align 8
  %4 = load %struct.node** %1, align 8
  %5 = getelementptr inbounds %struct.node* %4, i32 0, i32 1
  %6 = load %struct.node** %5, align 8
  store %struct.node* %6, %struct.node** %cur, align 8
  br label %7

; <label>:7                                       ; preds = %10, %0
  %8 = load %struct.node** %cur, align 8
  %9 = icmp ne %struct.node* %8, null
  br i1 %9, label %10, label %15

; <label>:10                                      ; preds = %7
  %11 = load %struct.node** %cur, align 8
  store %struct.node* %11, %struct.node** %prev, align 8
  %12 = load %struct.node** %cur, align 8
  %13 = getelementptr inbounds %struct.node* %12, i32 0, i32 1
  %14 = load %struct.node** %13, align 8
  store %struct.node* %14, %struct.node** %cur, align 8
  br label %7

; <label>:15                                      ; preds = %7
  %16 = load %struct.node** %2, align 8
  %17 = load %struct.node** %prev, align 8
  %18 = getelementptr inbounds %struct.node* %17, i32 0, i32 1
  store %struct.node* %16, %struct.node** %18, align 8
  %19 = load %struct.node** %1, align 8
  ret %struct.node* %19
}

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf"="true" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "unsafe-fp-math"="false" "use-soft-float"="false" }
