; ModuleID = '/home/jvillard/science/tools/llstar/bitbucket/examples/list/_llstar/list_traverse.c.bc'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.node = type { i32, %struct.node* }

; Function Attrs: nounwind readonly uwtable
define void @list_traverse(%struct.node* nocapture %l) #0 {
  tail call void @llvm.dbg.value(metadata !{%struct.node* %l}, i64 0, metadata !17), !dbg !20
  tail call void @llvm.dbg.value(metadata !{%struct.node* %l}, i64 0, metadata !18), !dbg !21
  %1 = icmp eq %struct.node* %l, null, !dbg !22
  br i1 %1, label %._crit_edge, label %.lr.ph, !dbg !22

.lr.ph:                                           ; preds = %.lr.ph, %0
  %cur.02 = phi %struct.node* [ %3, %.lr.ph ], [ %l, %0 ]
  %2 = getelementptr inbounds %struct.node* %cur.02, i64 0, i32 1, !dbg !23
  %3 = load %struct.node** %2, align 8, !dbg !23, !tbaa !24
  tail call void @llvm.dbg.value(metadata !{%struct.node* %3}, i64 0, metadata !18), !dbg !23
  %4 = icmp eq %struct.node* %3, null, !dbg !22
  br i1 %4, label %._crit_edge, label %.lr.ph, !dbg !22

._crit_edge:                                      ; preds = %.lr.ph, %0
  ret void, !dbg !27
}

; Function Attrs: nounwind readnone
declare void @llvm.dbg.value(metadata, i64, metadata) #1

attributes #0 = { nounwind readonly uwtable }
attributes #1 = { nounwind readnone }

!llvm.dbg.cu = !{!0}

!0 = metadata !{i32 786449, i32 0, i32 12, metadata !"list_traverse.c", metadata !"/home/jvillard/science/tools/llstar/bitbucket/examples/list", metadata !"Debian clang version 3.2-11 (tags/RELEASE_32/final) (based on LLVM 3.2)", i1 true, i1 true, metadata !"", i32 0, metadata !1, metadata !1, metadata !3, metadata !1}
!1 = metadata !{metadata !2}
!2 = metadata !{i32 0}
!3 = metadata !{metadata !4}
!4 = metadata !{metadata !5}
!5 = metadata !{i32 786478, i32 0, metadata !6, metadata !"list_traverse", metadata !"list_traverse", metadata !"", metadata !6, i32 6, metadata !7, i1 false, i1 true, i32 0, i32 0, null, i32 256, i1 true, void (%struct.node*)* @list_traverse, null, null, metadata !15, i32 6}
!6 = metadata !{i32 786473, metadata !"list_traverse.c", metadata !"/home/jvillard/science/tools/llstar/bitbucket/examples/list", null}
!7 = metadata !{i32 786453, i32 0, metadata !"", i32 0, i32 0, i64 0, i64 0, i64 0, i32 0, null, metadata !8, i32 0, i32 0} ; [ DW_TAG_subroutine_type ] [line 0, size 0, align 0, offset 0] [from ]
!8 = metadata !{null, metadata !9}
!9 = metadata !{i32 786447, null, metadata !"", null, i32 0, i64 64, i64 64, i64 0, i32 0, metadata !10} ; [ DW_TAG_pointer_type ] [line 0, size 64, align 64, offset 0] [from ]
!10 = metadata !{i32 786451, null, metadata !"node", metadata !6, i32 1, i64 128, i64 64, i32 0, i32 0, null, metadata !11, i32 0, i32 0, i32 0} ; [ DW_TAG_structure_type ] [line 1, size 128, align 64, offset 0] [from ]
!11 = metadata !{metadata !12, metadata !14}
!12 = metadata !{i32 786445, metadata !10, metadata !"data", metadata !6, i32 2, i64 32, i64 32, i64 0, i32 0, metadata !13} ; [ DW_TAG_member ] [line 2, size 32, align 32, offset 0] [from ]
!13 = metadata !{i32 786468, null, metadata !"int", null, i32 0, i64 32, i64 32, i64 0, i32 0, i32 5} ; [ DW_TAG_base_type ] [line 0, size 32, align 32, offset 0, enc DW_ATE_signed]
!14 = metadata !{i32 786445, metadata !10, metadata !"next", metadata !6, i32 3, i64 64, i64 64, i64 64, i32 0, metadata !9} ; [ DW_TAG_member ] [line 3, size 64, align 64, offset 64] [from ]
!15 = metadata !{metadata !16}
!16 = metadata !{metadata !17, metadata !18}
!17 = metadata !{i32 786689, metadata !5, metadata !"l", metadata !6, i32 16777222, metadata !9, i32 0, i32 0}
!18 = metadata !{i32 786688, metadata !19, metadata !"cur", metadata !6, i32 7, metadata !9, i32 0, i32 0} ; [ DW_TAG_auto_variable ] [cur] [line 7]
!19 = metadata !{i32 786443, metadata !5, i32 6, i32 0, metadata !6, i32 0} ; [ DW_TAG_lexical_block ] [/]
!20 = metadata !{i32 6, i32 0, metadata !5, null}
!21 = metadata !{i32 9, i32 0, metadata !19, null}
!22 = metadata !{i32 11, i32 0, metadata !19, null}
!23 = metadata !{i32 12, i32 0, metadata !19, null}
!24 = metadata !{metadata !"any pointer", metadata !25}
!25 = metadata !{metadata !"omnipotent char", metadata !26}
!26 = metadata !{metadata !"Simple C/C++ TBAA"}
!27 = metadata !{i32 13, i32 0, metadata !19, null}
