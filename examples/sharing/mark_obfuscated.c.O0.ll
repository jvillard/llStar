; ModuleID = 'mark_obfuscated.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.node = type { i32, %struct.node*, %struct.node* }

; Function Attrs: nounwind uwtable
define void @mark_trickier(%struct.node* %x) #0 {
  %1 = alloca %struct.node*, align 8
  %tmp = alloca %struct.node*, align 8
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
  br label %41

; <label>:10                                      ; preds = %4
  %11 = load %struct.node** %1, align 8
  %12 = getelementptr inbounds %struct.node* %11, i32 0, i32 0
  store i32 1, i32* %12, align 4
  %13 = load %struct.node** %1, align 8
  %14 = getelementptr inbounds %struct.node* %13, i32 0, i32 1
  %15 = load %struct.node** %14, align 8
  store %struct.node* %15, %struct.node** %tmp, align 8
  %16 = load %struct.node** %1, align 8
  %17 = getelementptr inbounds %struct.node* %16, i32 0, i32 2
  %18 = load %struct.node** %17, align 8
  %19 = load %struct.node** %1, align 8
  %20 = getelementptr inbounds %struct.node* %19, i32 0, i32 1
  store %struct.node* %18, %struct.node** %20, align 8
  %21 = load %struct.node** %tmp, align 8
  %22 = load %struct.node** %1, align 8
  %23 = getelementptr inbounds %struct.node* %22, i32 0, i32 2
  store %struct.node* %21, %struct.node** %23, align 8
  %24 = load %struct.node** %1, align 8
  %25 = getelementptr inbounds %struct.node* %24, i32 0, i32 1
  %26 = load %struct.node** %25, align 8
  call void @mark_trickier(%struct.node* %26)
  %27 = load %struct.node** %1, align 8
  %28 = getelementptr inbounds %struct.node* %27, i32 0, i32 1
  %29 = load %struct.node** %28, align 8
  store %struct.node* %29, %struct.node** %tmp, align 8
  %30 = load %struct.node** %1, align 8
  %31 = getelementptr inbounds %struct.node* %30, i32 0, i32 2
  %32 = load %struct.node** %31, align 8
  %33 = load %struct.node** %1, align 8
  %34 = getelementptr inbounds %struct.node* %33, i32 0, i32 1
  store %struct.node* %32, %struct.node** %34, align 8
  %35 = load %struct.node** %1, align 8
  %36 = getelementptr inbounds %struct.node* %35, i32 0, i32 2
  %37 = load %struct.node** %36, align 8
  call void @mark_trickier(%struct.node* %37)
  %38 = load %struct.node** %tmp, align 8
  %39 = load %struct.node** %1, align 8
  %40 = getelementptr inbounds %struct.node* %39, i32 0, i32 2
  store %struct.node* %38, %struct.node** %40, align 8
  br label %41

; <label>:41                                      ; preds = %10, %9
  ret void
}

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf"="true" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "unsafe-fp-math"="false" "use-soft-float"="false" }
