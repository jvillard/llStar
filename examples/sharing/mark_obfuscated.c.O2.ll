; ModuleID = '/home/jvillard/science/tools/llstar/bitbucket/examples/sharing/_llstar/mark_obfuscated.c.bc'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.node = type { i32, %struct.node*, %struct.node* }

; Function Attrs: nounwind uwtable
define void @mark_trickier(%struct.node* %x) #0 {
  tail call void @llvm.dbg.value(metadata !{%struct.node* %x}, i64 0, metadata !18), !dbg !21
  %1 = icmp eq %struct.node* %x, null, !dbg !22
  br i1 %1, label %13, label %2, !dbg !22

; <label>:2                                       ; preds = %0
  %3 = getelementptr inbounds %struct.node* %x, i64 0, i32 0, !dbg !22
  %4 = load i32* %3, align 4, !dbg !22, !tbaa !23
  %5 = icmp eq i32 %4, 0, !dbg !22
  br i1 %5, label %6, label %13, !dbg !22

; <label>:6                                       ; preds = %2
  store i32 1, i32* %3, align 4, !dbg !26, !tbaa !23
  %7 = getelementptr inbounds %struct.node* %x, i64 0, i32 1, !dbg !27
  %8 = load %struct.node** %7, align 8, !dbg !27, !tbaa !28
  tail call void @llvm.dbg.value(metadata !{%struct.node* %8}, i64 0, metadata !19), !dbg !27
  %9 = getelementptr inbounds %struct.node* %x, i64 0, i32 2, !dbg !29
  %10 = load %struct.node** %9, align 8, !dbg !29, !tbaa !28
  store %struct.node* %10, %struct.node** %7, align 8, !dbg !29, !tbaa !28
  store %struct.node* %8, %struct.node** %9, align 8, !dbg !30, !tbaa !28
  tail call void @mark_trickier(%struct.node* %10), !dbg !31
  %11 = load %struct.node** %7, align 8, !dbg !32, !tbaa !28
  tail call void @llvm.dbg.value(metadata !{%struct.node* %11}, i64 0, metadata !19), !dbg !32
  %12 = load %struct.node** %9, align 8, !dbg !33, !tbaa !28
  store %struct.node* %12, %struct.node** %7, align 8, !dbg !33, !tbaa !28
  tail call void @mark_trickier(%struct.node* %12), !dbg !34
  store %struct.node* %11, %struct.node** %9, align 8, !dbg !35, !tbaa !28
  ret void, !dbg !36

; <label>:13                                      ; preds = %2, %0
  ret void, !dbg !36
}

; Function Attrs: nounwind readnone
declare void @llvm.dbg.value(metadata, i64, metadata) #1

attributes #0 = { nounwind uwtable }
attributes #1 = { nounwind readnone }

!llvm.dbg.cu = !{!0}

!0 = metadata !{i32 786449, i32 0, i32 12, metadata !"mark_obfuscated.c", metadata !"/home/jvillard/science/tools/llstar/bitbucket/examples/sharing", metadata !"Debian clang version 3.2-11 (tags/RELEASE_32/final) (based on LLVM 3.2)", i1 true, i1 true, metadata !"", i32 0, metadata !1, metadata !1, metadata !3, metadata !1}
!1 = metadata !{metadata !2}
!2 = metadata !{i32 0}
!3 = metadata !{metadata !4}
!4 = metadata !{metadata !5}
!5 = metadata !{i32 786478, i32 0, metadata !6, metadata !"mark_trickier", metadata !"mark_trickier", metadata !"", metadata !6, i32 7, metadata !7, i1 false, i1 true, i32 0, i32 0, null, i32 256, i1 true, void (%struct.node*)* @mark_trickier, null, null, metadata !16, i32 7}
!6 = metadata !{i32 786473, metadata !"mark_obfuscated.c", metadata !"/home/jvillard/science/tools/llstar/bitbucket/examples/sharing", null}
!7 = metadata !{i32 786453, i32 0, metadata !"", i32 0, i32 0, i64 0, i64 0, i64 0, i32 0, null, metadata !8, i32 0, i32 0} ; [ DW_TAG_subroutine_type ] [line 0, size 0, align 0, offset 0] [from ]
!8 = metadata !{null, metadata !9}
!9 = metadata !{i32 786447, null, metadata !"", null, i32 0, i64 64, i64 64, i64 0, i32 0, metadata !10} ; [ DW_TAG_pointer_type ] [line 0, size 64, align 64, offset 0] [from ]
!10 = metadata !{i32 786451, null, metadata !"node", metadata !6, i32 1, i64 192, i64 64, i32 0, i32 0, null, metadata !11, i32 0, i32 0, i32 0} ; [ DW_TAG_structure_type ] [line 1, size 192, align 64, offset 0] [from ]
!11 = metadata !{metadata !12, metadata !14, metadata !15}
!12 = metadata !{i32 786445, metadata !10, metadata !"color", metadata !6, i32 2, i64 32, i64 32, i64 0, i32 0, metadata !13} ; [ DW_TAG_member ] [line 2, size 32, align 32, offset 0] [from ]
!13 = metadata !{i32 786468, null, metadata !"int", null, i32 0, i64 32, i64 32, i64 0, i32 0, i32 5} ; [ DW_TAG_base_type ] [line 0, size 32, align 32, offset 0, enc DW_ATE_signed]
!14 = metadata !{i32 786445, metadata !10, metadata !"left", metadata !6, i32 3, i64 64, i64 64, i64 64, i32 0, metadata !9} ; [ DW_TAG_member ] [line 3, size 64, align 64, offset 64] [from ]
!15 = metadata !{i32 786445, metadata !10, metadata !"right", metadata !6, i32 4, i64 64, i64 64, i64 128, i32 0, metadata !9} ; [ DW_TAG_member ] [line 4, size 64, align 64, offset 128] [from ]
!16 = metadata !{metadata !17}
!17 = metadata !{metadata !18, metadata !19}
!18 = metadata !{i32 786689, metadata !5, metadata !"x", metadata !6, i32 16777223, metadata !9, i32 0, i32 0}
!19 = metadata !{i32 786688, metadata !20, metadata !"tmp", metadata !6, i32 10, metadata !9, i32 0, i32 0} ; [ DW_TAG_auto_variable ] [tmp] [line 10]
!20 = metadata !{i32 786443, metadata !5, i32 7, i32 0, metadata !6, i32 0} ; [ DW_TAG_lexical_block ] [/]
!21 = metadata !{i32 7, i32 0, metadata !5, null}
!22 = metadata !{i32 8, i32 0, metadata !20, null}
!23 = metadata !{metadata !"int", metadata !24}
!24 = metadata !{metadata !"omnipotent char", metadata !25}
!25 = metadata !{metadata !"Simple C/C++ TBAA"}
!26 = metadata !{i32 9, i32 0, metadata !20, null}
!27 = metadata !{i32 10, i32 0, metadata !20, null}
!28 = metadata !{metadata !"any pointer", metadata !24}
!29 = metadata !{i32 11, i32 0, metadata !20, null}
!30 = metadata !{i32 12, i32 0, metadata !20, null}
!31 = metadata !{i32 13, i32 0, metadata !20, null}
!32 = metadata !{i32 14, i32 0, metadata !20, null}
!33 = metadata !{i32 15, i32 0, metadata !20, null}
!34 = metadata !{i32 16, i32 0, metadata !20, null}
!35 = metadata !{i32 17, i32 0, metadata !20, null}
!36 = metadata !{i32 18, i32 0, metadata !20, null}
