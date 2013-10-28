; ModuleID = 'list_append.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.node = type { i32, %struct.node* }

; Function Attrs: nounwind uwtable
define %struct.node* @list_append(%struct.node* %l1, %struct.node* %l2) #0 {
  br label %1

; <label>:1                                       ; preds = %1, %0
  %l1.pn = phi %struct.node* [ %l1, %0 ], [ %cur.0, %1 ]
  %cur.0.in = getelementptr inbounds %struct.node* %l1.pn, i64 0, i32 1
  %cur.0 = load %struct.node** %cur.0.in, align 8
  %2 = icmp eq %struct.node* %cur.0, null
  br i1 %2, label %3, label %1

; <label>:3                                       ; preds = %1
  store %struct.node* %l2, %struct.node** %cur.0.in, align 8, !tbaa !0
  ret %struct.node* %l1
}

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-frame-pointer-elim-non-leaf"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "unsafe-fp-math"="false" "use-soft-float"="false" }

!0 = metadata !{metadata !"any pointer", metadata !1}
!1 = metadata !{metadata !"omnipotent char", metadata !2}
!2 = metadata !{metadata !"Simple C/C++ TBAA"}
