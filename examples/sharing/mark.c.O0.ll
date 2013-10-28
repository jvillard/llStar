; ModuleID = 'mark.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.node = type { i32, %struct.node*, %struct.node* }

; Function Attrs: nounwind uwtable
define void @mark(%struct.node* %x) #0 {
  %1 = alloca %struct.node*, align 8
  store %struct.node* %x, %struct.node** %1, align 8
  %2 = load %struct.node** %1, align 8
  %3 = icmp ne %struct.node* %2, null
  br i1 %3, label %4, label %9

; <label>:4                                       ; preds = %0
  %5 = load %struct.node** %1, align 8
  %6 = getelementptr inbounds %struct.node* %5, i32 0, i32 0
  %7 = load i32* %6, align 4
  %8 = icmp ne i32 %7, 0
  br i1 %8, label %9, label %10

; <label>:9                                       ; preds = %4, %0
  br label %19

; <label>:10                                      ; preds = %4
  %11 = load %struct.node** %1, align 8
  %12 = getelementptr inbounds %struct.node* %11, i32 0, i32 0
  store i32 1, i32* %12, align 4
  %13 = load %struct.node** %1, align 8
  %14 = getelementptr inbounds %struct.node* %13, i32 0, i32 1
  %15 = load %struct.node** %14, align 8
  call void @mark(%struct.node* %15)
  %16 = load %struct.node** %1, align 8
  %17 = getelementptr inbounds %struct.node* %16, i32 0, i32 2
  %18 = load %struct.node** %17, align 8
  call void @mark(%struct.node* %18)
  br label %19

; <label>:19                                      ; preds = %10, %9
  ret void
}

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf"="true" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "unsafe-fp-math"="false" "use-soft-float"="false" }
