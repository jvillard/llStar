; ModuleID = 'list.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%node = type { i32, %node* }

define void @smaug(%node* %l) nounwind uwtable readonly {
  %1 = icmp eq %node* %l, null
  br i1 %1, label %._crit_edge, label %.lr.ph

.lr.ph:                                           ; preds = %0, %.lr.ph
  %cur.01 = phi %node* [ %3, %.lr.ph ], [ %l, %0 ]
  %2 = getelementptr inbounds %node* %cur.01, i64 0, i32 1
  %3 = load %node** %2, align 8, !tbaa !0
  %4 = icmp eq %node* %3, null
  br i1 %4, label %._crit_edge, label %.lr.ph

._crit_edge:                                      ; preds = %.lr.ph, %0
  ret void
}

!0 = metadata !{metadata !"any pointer", metadata !1}
!1 = metadata !{metadata !"omnipotent char", metadata !2}
!2 = metadata !{metadata !"Simple C/C++ TBAA", null}
