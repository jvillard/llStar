; ModuleID = 'list_dispose.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.node = type { i32, %struct.node* }

; Function Attrs: nounwind uwtable
define void @list_dispose(%struct.node* %l) #0 {
  %1 = alloca %struct.node*, align 8
  %prev = alloca %struct.node*, align 8
  store %struct.node* %l, %struct.node** %1, align 8
  %2 = load %struct.node** %1, align 8
  store %struct.node* %2, %struct.node** %prev, align 8
  br label %3

; <label>:3                                       ; preds = %6, %0
  %4 = load %struct.node** %1, align 8
  %5 = icmp ne %struct.node* %4, null
  br i1 %5, label %6, label %13

; <label>:6                                       ; preds = %3
  %7 = load %struct.node** %1, align 8
  store %struct.node* %7, %struct.node** %prev, align 8
  %8 = load %struct.node** %1, align 8
  %9 = getelementptr inbounds %struct.node* %8, i32 0, i32 1
  %10 = load %struct.node** %9, align 8
  store %struct.node* %10, %struct.node** %1, align 8
  %11 = load %struct.node** %prev, align 8
  %12 = bitcast %struct.node* %11 to i8*
  call void @free(i8* %12) #2
  br label %3

; <label>:13                                      ; preds = %3
  ret void
}

; Function Attrs: nounwind
declare void @free(i8*) #1

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf"="true" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf"="true" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind }
