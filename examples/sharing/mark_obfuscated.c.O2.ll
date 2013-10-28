; ModuleID = 'mark_obfuscated.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.node = type { i32, %struct.node*, %struct.node* }

; Function Attrs: nounwind uwtable
define void @mark_trickier(%struct.node* %x) #0 {
  %1 = icmp eq %struct.node* %x, null
  br i1 %1, label %13, label %2

; <label>:2                                       ; preds = %0
  %3 = getelementptr inbounds %struct.node* %x, i64 0, i32 0
  %4 = load i32* %3, align 4, !tbaa !0
  %5 = icmp eq i32 %4, 0
  br i1 %5, label %6, label %13

; <label>:6                                       ; preds = %2
  store i32 1, i32* %3, align 4, !tbaa !0
  %7 = getelementptr inbounds %struct.node* %x, i64 0, i32 1
  %8 = load %struct.node** %7, align 8, !tbaa !3
  %9 = getelementptr inbounds %struct.node* %x, i64 0, i32 2
  %10 = load %struct.node** %9, align 8, !tbaa !3
  store %struct.node* %10, %struct.node** %7, align 8, !tbaa !3
  store %struct.node* %8, %struct.node** %9, align 8, !tbaa !3
  tail call void @mark_trickier(%struct.node* %10)
  %11 = load %struct.node** %7, align 8, !tbaa !3
  %12 = load %struct.node** %9, align 8, !tbaa !3
  store %struct.node* %12, %struct.node** %7, align 8, !tbaa !3
  tail call void @mark_trickier(%struct.node* %12)
  store %struct.node* %11, %struct.node** %9, align 8, !tbaa !3
  ret void

; <label>:13                                      ; preds = %2, %0
  ret void
}

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-frame-pointer-elim-non-leaf"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "unsafe-fp-math"="false" "use-soft-float"="false" }

!0 = metadata !{metadata !"int", metadata !1}
!1 = metadata !{metadata !"omnipotent char", metadata !2}
!2 = metadata !{metadata !"Simple C/C++ TBAA"}
!3 = metadata !{metadata !"any pointer", metadata !1}
