; ModuleID = '/home/jvillard/science/tools/llstar/bitbucket/examples/list/_llstar/list_append.c.bc'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.node = type { i32, %struct.node* }

; Function Attrs: nounwind uwtable
define %struct.node* @list_append(%struct.node* %l1, %struct.node* %l2) #0 {
  tail call void @llvm.dbg.value(metadata !{%struct.node* %l1}, i64 0, metadata !17), !dbg !22
  tail call void @llvm.dbg.value(metadata !{%struct.node* %l2}, i64 0, metadata !18), !dbg !22
  tail call void @llvm.dbg.value(metadata !{%struct.node* %l1}, i64 0, metadata !21), !dbg !23
  br label %1, !dbg !24

; <label>:1                                       ; preds = %1, %0
  %l1.pn = phi %struct.node* [ %l1, %0 ], [ %cur.0, %1 ]
  %cur.0.in = getelementptr inbounds %struct.node* %l1.pn, i64 0, i32 1, !dbg !25
  %cur.0 = load %struct.node** %cur.0.in, align 8, !dbg !25
  %2 = icmp eq %struct.node* %cur.0, null, !dbg !24
  br i1 %2, label %3, label %1, !dbg !24

; <label>:3                                       ; preds = %1
  store %struct.node* %l2, %struct.node** %cur.0.in, align 8, !dbg !26, !tbaa !27
  ret %struct.node* %l1, !dbg !30
}

; Function Attrs: nounwind readnone
declare void @llvm.dbg.value(metadata, i64, metadata) #1

attributes #0 = { nounwind uwtable }
attributes #1 = { nounwind readnone }

!llvm.dbg.cu = !{!0}

!0 = metadata !{i32 786449, i32 0, i32 12, metadata !"list_append.c", metadata !"/home/jvillard/science/tools/llstar/bitbucket/examples/list", metadata !"Debian clang version 3.2-11 (tags/RELEASE_32/final) (based on LLVM 3.2)", i1 true, i1 true, metadata !"", i32 0, metadata !1, metadata !1, metadata !3, metadata !1}
!1 = metadata !{metadata !2}
!2 = metadata !{i32 0}
!3 = metadata !{metadata !4}
!4 = metadata !{metadata !5}
!5 = metadata !{i32 786478, i32 0, metadata !6, metadata !"list_append", metadata !"list_append", metadata !"", metadata !6, i32 7, metadata !7, i1 false, i1 true, i32 0, i32 0, null, i32 256, i1 true, %struct.node* (%struct.node*, %struct.node*)* @list_append, null, null, metadata !15, i32 7}
!6 = metadata !{i32 786473, metadata !"list_append.c", metadata !"/home/jvillard/science/tools/llstar/bitbucket/examples/list", null}
!7 = metadata !{i32 786453, i32 0, metadata !"", i32 0, i32 0, i64 0, i64 0, i64 0, i32 0, null, metadata !8, i32 0, i32 0} ; [ DW_TAG_subroutine_type ] [line 0, size 0, align 0, offset 0] [from ]
!8 = metadata !{metadata !9, metadata !9, metadata !9}
!9 = metadata !{i32 786447, null, metadata !"", null, i32 0, i64 64, i64 64, i64 0, i32 0, metadata !10} ; [ DW_TAG_pointer_type ] [line 0, size 64, align 64, offset 0] [from ]
!10 = metadata !{i32 786451, null, metadata !"node", metadata !6, i32 1, i64 128, i64 64, i32 0, i32 0, null, metadata !11, i32 0, i32 0, i32 0} ; [ DW_TAG_structure_type ] [line 1, size 128, align 64, offset 0] [from ]
!11 = metadata !{metadata !12, metadata !14}
!12 = metadata !{i32 786445, metadata !10, metadata !"data", metadata !6, i32 2, i64 32, i64 32, i64 0, i32 0, metadata !13} ; [ DW_TAG_member ] [line 2, size 32, align 32, offset 0] [from ]
!13 = metadata !{i32 786468, null, metadata !"int", null, i32 0, i64 32, i64 32, i64 0, i32 0, i32 5} ; [ DW_TAG_base_type ] [line 0, size 32, align 32, offset 0, enc DW_ATE_signed]
!14 = metadata !{i32 786445, metadata !10, metadata !"next", metadata !6, i32 3, i64 64, i64 64, i64 64, i32 0, metadata !9} ; [ DW_TAG_member ] [line 3, size 64, align 64, offset 64] [from ]
!15 = metadata !{metadata !16}
!16 = metadata !{metadata !17, metadata !18, metadata !19, metadata !21}
!17 = metadata !{i32 786689, metadata !5, metadata !"l1", metadata !6, i32 16777223, metadata !9, i32 0, i32 0}
!18 = metadata !{i32 786689, metadata !5, metadata !"l2", metadata !6, i32 33554439, metadata !9, i32 0, i32 0}
!19 = metadata !{i32 786688, metadata !20, metadata !"cur", metadata !6, i32 8, metadata !9, i32 0, i32 0} ; [ DW_TAG_auto_variable ] [cur] [line 8]
!20 = metadata !{i32 786443, metadata !5, i32 7, i32 0, metadata !6, i32 0} ; [ DW_TAG_lexical_block ] [/]
!21 = metadata !{i32 786688, metadata !20, metadata !"prev", metadata !6, i32 8, metadata !9, i32 0, i32 0} ; [ DW_TAG_auto_variable ] [prev] [line 8]
!22 = metadata !{i32 7, i32 0, metadata !5, null}
!23 = metadata !{i32 10, i32 0, metadata !20, null}
!24 = metadata !{i32 13, i32 0, metadata !20, null}
!25 = metadata !{i32 11, i32 0, metadata !20, null}
!26 = metadata !{i32 18, i32 0, metadata !20, null}
!27 = metadata !{metadata !"any pointer", metadata !28}
!28 = metadata !{metadata !"omnipotent char", metadata !29}
!29 = metadata !{metadata !"Simple C/C++ TBAA"}
!30 = metadata !{i32 19, i32 0, metadata !20, null}
