; ModuleID = 'list.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%listitem = type { i640, i32*, i64*, i1, %listitem*, i1, i1}

define void @smaug(%listitem* %l) nounwind uwtable readonly {
  %1 = icmp eq %listitem* %l, null
  br i1 %1, label %._crit_edge, label %.lr.ph

.lr.ph:                                           ; preds = %0, %.lr.ph
  %cur.01 = phi %listitem* [ %3, %.lr.ph ], [ %l, %0 ]
  %2 = getelementptr inbounds %listitem* %cur.01, i64 0, i32 4
  %3 = load %listitem** %2, align 8, !tbaa !0
  %4 = icmp eq %listitem* %3, null
  br i1 %4, label %._crit_edge, label %.lr.ph

._crit_edge:                                      ; preds = %.lr.ph, %0
  ret void
}

!0 = metadata !{metadata !"any pointer", metadata !1}
!1 = metadata !{metadata !"omnipotent char", metadata !2}
!2 = metadata !{metadata !"Simple C/C++ TBAA", null}
