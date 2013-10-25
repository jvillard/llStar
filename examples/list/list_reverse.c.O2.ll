; ModuleID = '/home/jvillard/science/tools/llstar/bitbucket/examples/list/_llstar/list_reverse.c.bc'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.node = type { i32, %struct.node* }

; Function Attrs: nounwind uwtable
define %struct.node* @list_reverse(%struct.node* %l) #0 {
  tail call void @llvm.dbg.value(metadata !{%struct.node* %l}, i64 0, metadata !17), !dbg !22
  tail call void @llvm.dbg.value(metadata !23, i64 0, metadata !18), !dbg !24
  tail call void @llvm.dbg.value(metadata !23, i64 0, metadata !20), !dbg !25
  tail call void @llvm.dbg.value(metadata !23, i64 0, metadata !21), !dbg !26
  tail call void @llvm.dbg.value(metadata !{%struct.node* %l}, i64 0, metadata !18), !dbg !27
  tail call void @llvm.dbg.value(metadata !23, i64 0, metadata !20), !dbg !27
  %1 = icmp eq %struct.node* %l, null, !dbg !28
  br i1 %1, label %._crit_edge, label %.lr.ph, !dbg !28

.lr.ph:                                           ; preds = %.lr.ph, %0
  %b.05 = phi %struct.node* [ %a.04, %.lr.ph ], [ null, %0 ]
  %a.04 = phi %struct.node* [ %3, %.lr.ph ], [ %l, %0 ]
  tail call void @llvm.dbg.value(metadata !{%struct.node* %b.0.lcssa}, i64 0, metadata !21), !dbg !29
  tail call void @llvm.dbg.value(metadata !{%struct.node* %a.04}, i64 0, metadata !20), !dbg !29
  %2 = getelementptr inbounds %struct.node* %a.04, i64 0, i32 1, !dbg !29
  %3 = load %struct.node** %2, align 8, !dbg !29, !tbaa !31
  tail call void @llvm.dbg.value(metadata !{%struct.node* %3}, i64 0, metadata !18), !dbg !29
  store %struct.node* %b.05, %struct.node** %2, align 8, !dbg !34, !tbaa !31
  %4 = icmp eq %struct.node* %3, null, !dbg !28
  br i1 %4, label %._crit_edge, label %.lr.ph, !dbg !28

._crit_edge:                                      ; preds = %.lr.ph, %0
  %b.0.lcssa = phi %struct.node* [ null, %0 ], [ %a.04, %.lr.ph ]
  ret %struct.node* %b.0.lcssa, !dbg !35
}

; Function Attrs: nounwind readnone
declare void @llvm.dbg.value(metadata, i64, metadata) #1

attributes #0 = { nounwind uwtable }
attributes #1 = { nounwind readnone }

!llvm.dbg.cu = !{!0}

!0 = metadata !{i32 786449, i32 0, i32 12, metadata !"list_reverse.c", metadata !"/home/jvillard/science/tools/llstar/bitbucket/examples/list", metadata !"Debian clang version 3.2-11 (tags/RELEASE_32/final) (based on LLVM 3.2)", i1 true, i1 true, metadata !"", i32 0, metadata !1, metadata !1, metadata !3, metadata !1}
!1 = metadata !{metadata !2}
!2 = metadata !{i32 0}
!3 = metadata !{metadata !4}
!4 = metadata !{metadata !5}
!5 = metadata !{i32 786478, i32 0, metadata !6, metadata !"list_reverse", metadata !"list_reverse", metadata !"", metadata !6, i32 8, metadata !7, i1 false, i1 true, i32 0, i32 0, null, i32 256, i1 true, %struct.node* (%struct.node*)* @list_reverse, null, null, metadata !15, i32 8}
!6 = metadata !{i32 786473, metadata !"list_reverse.c", metadata !"/home/jvillard/science/tools/llstar/bitbucket/examples/list", null}
!7 = metadata !{i32 786453, i32 0, metadata !"", i32 0, i32 0, i64 0, i64 0, i64 0, i32 0, null, metadata !8, i32 0, i32 0} ; [ DW_TAG_subroutine_type ] [line 0, size 0, align 0, offset 0] [from ]
!8 = metadata !{metadata !9, metadata !9}
!9 = metadata !{i32 786447, null, metadata !"", null, i32 0, i64 64, i64 64, i64 0, i32 0, metadata !10} ; [ DW_TAG_pointer_type ] [line 0, size 64, align 64, offset 0] [from ]
!10 = metadata !{i32 786451, null, metadata !"node", metadata !6, i32 3, i64 128, i64 64, i32 0, i32 0, null, metadata !11, i32 0, i32 0, i32 0} ; [ DW_TAG_structure_type ] [line 3, size 128, align 64, offset 0] [from ]
!11 = metadata !{metadata !12, metadata !14}
!12 = metadata !{i32 786445, metadata !10, metadata !"data", metadata !6, i32 4, i64 32, i64 32, i64 0, i32 0, metadata !13} ; [ DW_TAG_member ] [line 4, size 32, align 32, offset 0] [from ]
!13 = metadata !{i32 786468, null, metadata !"int", null, i32 0, i64 32, i64 32, i64 0, i32 0, i32 5} ; [ DW_TAG_base_type ] [line 0, size 32, align 32, offset 0, enc DW_ATE_signed]
!14 = metadata !{i32 786445, metadata !10, metadata !"next", metadata !6, i32 5, i64 64, i64 64, i64 64, i32 0, metadata !9} ; [ DW_TAG_member ] [line 5, size 64, align 64, offset 64] [from ]
!15 = metadata !{metadata !16}
!16 = metadata !{metadata !17, metadata !18, metadata !20, metadata !21}
!17 = metadata !{i32 786689, metadata !5, metadata !"l", metadata !6, i32 16777224, metadata !9, i32 0, i32 0}
!18 = metadata !{i32 786688, metadata !19, metadata !"a", metadata !6, i32 9, metadata !9, i32 0, i32 0} ; [ DW_TAG_auto_variable ] [a] [line 9]
!19 = metadata !{i32 786443, metadata !5, i32 8, i32 0, metadata !6, i32 0} ; [ DW_TAG_lexical_block ] [/]
!20 = metadata !{i32 786688, metadata !19, metadata !"b", metadata !6, i32 10, metadata !9, i32 0, i32 0} ; [ DW_TAG_auto_variable ] [b] [line 10]
!21 = metadata !{i32 786688, metadata !19, metadata !"c", metadata !6, i32 11, metadata !9, i32 0, i32 0} ; [ DW_TAG_auto_variable ] [c] [line 11]
!22 = metadata !{i32 8, i32 0, metadata !5, null}
!23 = metadata !{%struct.node* null}
!24 = metadata !{i32 9, i32 0, metadata !19, null}
!25 = metadata !{i32 10, i32 0, metadata !19, null}
!26 = metadata !{i32 11, i32 0, metadata !19, null}
!27 = metadata !{i32 12, i32 0, metadata !19, null}
!28 = metadata !{i32 14, i32 0, metadata !19, null}
!29 = metadata !{i32 15, i32 0, metadata !30, null}
!30 = metadata !{i32 786443, metadata !19, i32 14, i32 0, metadata !6, i32 1} ; [ DW_TAG_lexical_block ] [/]
!31 = metadata !{metadata !"any pointer", metadata !32}
!32 = metadata !{metadata !"omnipotent char", metadata !33}
!33 = metadata !{metadata !"Simple C/C++ TBAA"}
!34 = metadata !{i32 16, i32 0, metadata !30, null}
!35 = metadata !{i32 19, i32 0, metadata !19, null}
