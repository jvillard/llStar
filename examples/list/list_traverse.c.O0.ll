; ModuleID = '/home/jvillard/science/tools/llstar/bitbucket/examples/list/_llstar/list_traverse.c.bc'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.node = type { i32, %struct.node* }

; Function Attrs: nounwind uwtable
define void @list_traverse(%struct.node* %l) #0 {
  %1 = alloca %struct.node*, align 8
  %cur = alloca %struct.node*, align 8
  store %struct.node* %l, %struct.node** %1, align 8
  call void @llvm.dbg.declare(metadata !{%struct.node** %1}, metadata !15), !dbg !16
  call void @llvm.dbg.declare(metadata !{%struct.node** %cur}, metadata !17), !dbg !19
  %2 = load %struct.node** %1, align 8, !dbg !20
  store %struct.node* %2, %struct.node** %cur, align 8, !dbg !20
  br label %3, !dbg !21

; <label>:3                                       ; preds = %6, %0
  %4 = load %struct.node** %cur, align 8, !dbg !21
  %5 = icmp ne %struct.node* %4, null, !dbg !21
  br i1 %5, label %6, label %10, !dbg !21

; <label>:6                                       ; preds = %3
  %7 = load %struct.node** %cur, align 8, !dbg !22
  %8 = getelementptr inbounds %struct.node* %7, i32 0, i32 1, !dbg !22
  %9 = load %struct.node** %8, align 8, !dbg !22
  store %struct.node* %9, %struct.node** %cur, align 8, !dbg !22
  br label %3, !dbg !22

; <label>:10                                      ; preds = %3
  ret void, !dbg !23
}

; Function Attrs: nounwind readnone
declare void @llvm.dbg.declare(metadata, metadata) #1

attributes #0 = { nounwind uwtable }
attributes #1 = { nounwind readnone }

!llvm.dbg.cu = !{!0}

!0 = metadata !{i32 786449, i32 0, i32 12, metadata !"list_traverse.c", metadata !"/home/jvillard/science/tools/llstar/bitbucket/examples/list", metadata !"Debian clang version 3.2-11 (tags/RELEASE_32/final) (based on LLVM 3.2)", i1 true, i1 false, metadata !"", i32 0, metadata !1, metadata !1, metadata !3, metadata !1}
!1 = metadata !{metadata !2}
!2 = metadata !{i32 0}
!3 = metadata !{metadata !4}
!4 = metadata !{metadata !5}
!5 = metadata !{i32 786478, i32 0, metadata !6, metadata !"list_traverse", metadata !"list_traverse", metadata !"", metadata !6, i32 6, metadata !7, i1 false, i1 true, i32 0, i32 0, null, i32 256, i1 false, void (%struct.node*)* @list_traverse, null, null, metadata !1, i32 6}
!6 = metadata !{i32 786473, metadata !"list_traverse.c", metadata !"/home/jvillard/science/tools/llstar/bitbucket/examples/list", null}
!7 = metadata !{i32 786453, i32 0, metadata !"", i32 0, i32 0, i64 0, i64 0, i64 0, i32 0, null, metadata !8, i32 0, i32 0} ; [ DW_TAG_subroutine_type ] [line 0, size 0, align 0, offset 0] [from ]
!8 = metadata !{null, metadata !9}
!9 = metadata !{i32 786447, null, metadata !"", null, i32 0, i64 64, i64 64, i64 0, i32 0, metadata !10} ; [ DW_TAG_pointer_type ] [line 0, size 64, align 64, offset 0] [from ]
!10 = metadata !{i32 786451, null, metadata !"node", metadata !6, i32 1, i64 128, i64 64, i32 0, i32 0, null, metadata !11, i32 0, i32 0, i32 0} ; [ DW_TAG_structure_type ] [line 1, size 128, align 64, offset 0] [from ]
!11 = metadata !{metadata !12, metadata !14}
!12 = metadata !{i32 786445, metadata !10, metadata !"data", metadata !6, i32 2, i64 32, i64 32, i64 0, i32 0, metadata !13} ; [ DW_TAG_member ] [line 2, size 32, align 32, offset 0] [from ]
!13 = metadata !{i32 786468, null, metadata !"int", null, i32 0, i64 32, i64 32, i64 0, i32 0, i32 5} ; [ DW_TAG_base_type ] [line 0, size 32, align 32, offset 0, enc DW_ATE_signed]
!14 = metadata !{i32 786445, metadata !10, metadata !"next", metadata !6, i32 3, i64 64, i64 64, i64 64, i32 0, metadata !9} ; [ DW_TAG_member ] [line 3, size 64, align 64, offset 64] [from ]
!15 = metadata !{i32 786689, metadata !5, metadata !"l", metadata !6, i32 16777222, metadata !9, i32 0, i32 0}
!16 = metadata !{i32 6, i32 0, metadata !5, null}
!17 = metadata !{i32 786688, metadata !18, metadata !"cur", metadata !6, i32 7, metadata !9, i32 0, i32 0} ; [ DW_TAG_auto_variable ] [cur] [line 7]
!18 = metadata !{i32 786443, metadata !5, i32 6, i32 0, metadata !6, i32 0} ; [ DW_TAG_lexical_block ] [/]
!19 = metadata !{i32 7, i32 0, metadata !18, null}
!20 = metadata !{i32 9, i32 0, metadata !18, null}
!21 = metadata !{i32 11, i32 0, metadata !18, null}
!22 = metadata !{i32 12, i32 0, metadata !18, null}
!23 = metadata !{i32 13, i32 0, metadata !18, null}
