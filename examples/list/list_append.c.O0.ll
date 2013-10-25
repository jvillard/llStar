; ModuleID = '/home/jvillard/science/tools/llstar/bitbucket/examples/list/_llstar/list_append.c.bc'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.node = type { i32, %struct.node* }

; Function Attrs: nounwind uwtable
define %struct.node* @list_append(%struct.node* %l1, %struct.node* %l2) #0 {
  %1 = alloca %struct.node*, align 8
  %2 = alloca %struct.node*, align 8
  %cur = alloca %struct.node*, align 8
  %prev = alloca %struct.node*, align 8
  store %struct.node* %l1, %struct.node** %1, align 8
  call void @llvm.dbg.declare(metadata !{%struct.node** %1}, metadata !15), !dbg !16
  store %struct.node* %l2, %struct.node** %2, align 8
  call void @llvm.dbg.declare(metadata !{%struct.node** %2}, metadata !17), !dbg !16
  call void @llvm.dbg.declare(metadata !{%struct.node** %cur}, metadata !18), !dbg !20
  call void @llvm.dbg.declare(metadata !{%struct.node** %prev}, metadata !21), !dbg !20
  %3 = load %struct.node** %1, align 8, !dbg !22
  store %struct.node* %3, %struct.node** %prev, align 8, !dbg !22
  %4 = load %struct.node** %1, align 8, !dbg !23
  %5 = getelementptr inbounds %struct.node* %4, i32 0, i32 1, !dbg !23
  %6 = load %struct.node** %5, align 8, !dbg !23
  store %struct.node* %6, %struct.node** %cur, align 8, !dbg !23
  br label %7, !dbg !24

; <label>:7                                       ; preds = %10, %0
  %8 = load %struct.node** %cur, align 8, !dbg !24
  %9 = icmp ne %struct.node* %8, null, !dbg !24
  br i1 %9, label %10, label %15, !dbg !24

; <label>:10                                      ; preds = %7
  %11 = load %struct.node** %cur, align 8, !dbg !25
  store %struct.node* %11, %struct.node** %prev, align 8, !dbg !25
  %12 = load %struct.node** %cur, align 8, !dbg !27
  %13 = getelementptr inbounds %struct.node* %12, i32 0, i32 1, !dbg !27
  %14 = load %struct.node** %13, align 8, !dbg !27
  store %struct.node* %14, %struct.node** %cur, align 8, !dbg !27
  br label %7, !dbg !28

; <label>:15                                      ; preds = %7
  %16 = load %struct.node** %2, align 8, !dbg !29
  %17 = load %struct.node** %prev, align 8, !dbg !29
  %18 = getelementptr inbounds %struct.node* %17, i32 0, i32 1, !dbg !29
  store %struct.node* %16, %struct.node** %18, align 8, !dbg !29
  %19 = load %struct.node** %1, align 8, !dbg !30
  ret %struct.node* %19, !dbg !30
}

; Function Attrs: nounwind readnone
declare void @llvm.dbg.declare(metadata, metadata) #1

attributes #0 = { nounwind uwtable }
attributes #1 = { nounwind readnone }

!llvm.dbg.cu = !{!0}

!0 = metadata !{i32 786449, i32 0, i32 12, metadata !"list_append.c", metadata !"/home/jvillard/science/tools/llstar/bitbucket/examples/list", metadata !"Debian clang version 3.2-11 (tags/RELEASE_32/final) (based on LLVM 3.2)", i1 true, i1 false, metadata !"", i32 0, metadata !1, metadata !1, metadata !3, metadata !1}
!1 = metadata !{metadata !2}
!2 = metadata !{i32 0}
!3 = metadata !{metadata !4}
!4 = metadata !{metadata !5}
!5 = metadata !{i32 786478, i32 0, metadata !6, metadata !"list_append", metadata !"list_append", metadata !"", metadata !6, i32 7, metadata !7, i1 false, i1 true, i32 0, i32 0, null, i32 256, i1 false, %struct.node* (%struct.node*, %struct.node*)* @list_append, null, null, metadata !1, i32 7}
!6 = metadata !{i32 786473, metadata !"list_append.c", metadata !"/home/jvillard/science/tools/llstar/bitbucket/examples/list", null}
!7 = metadata !{i32 786453, i32 0, metadata !"", i32 0, i32 0, i64 0, i64 0, i64 0, i32 0, null, metadata !8, i32 0, i32 0} ; [ DW_TAG_subroutine_type ] [line 0, size 0, align 0, offset 0] [from ]
!8 = metadata !{metadata !9, metadata !9, metadata !9}
!9 = metadata !{i32 786447, null, metadata !"", null, i32 0, i64 64, i64 64, i64 0, i32 0, metadata !10} ; [ DW_TAG_pointer_type ] [line 0, size 64, align 64, offset 0] [from ]
!10 = metadata !{i32 786451, null, metadata !"node", metadata !6, i32 1, i64 128, i64 64, i32 0, i32 0, null, metadata !11, i32 0, i32 0, i32 0} ; [ DW_TAG_structure_type ] [line 1, size 128, align 64, offset 0] [from ]
!11 = metadata !{metadata !12, metadata !14}
!12 = metadata !{i32 786445, metadata !10, metadata !"data", metadata !6, i32 2, i64 32, i64 32, i64 0, i32 0, metadata !13} ; [ DW_TAG_member ] [line 2, size 32, align 32, offset 0] [from ]
!13 = metadata !{i32 786468, null, metadata !"int", null, i32 0, i64 32, i64 32, i64 0, i32 0, i32 5} ; [ DW_TAG_base_type ] [line 0, size 32, align 32, offset 0, enc DW_ATE_signed]
!14 = metadata !{i32 786445, metadata !10, metadata !"next", metadata !6, i32 3, i64 64, i64 64, i64 64, i32 0, metadata !9} ; [ DW_TAG_member ] [line 3, size 64, align 64, offset 64] [from ]
!15 = metadata !{i32 786689, metadata !5, metadata !"l1", metadata !6, i32 16777223, metadata !9, i32 0, i32 0}
!16 = metadata !{i32 7, i32 0, metadata !5, null}
!17 = metadata !{i32 786689, metadata !5, metadata !"l2", metadata !6, i32 33554439, metadata !9, i32 0, i32 0}
!18 = metadata !{i32 786688, metadata !19, metadata !"cur", metadata !6, i32 8, metadata !9, i32 0, i32 0} ; [ DW_TAG_auto_variable ] [cur] [line 8]
!19 = metadata !{i32 786443, metadata !5, i32 7, i32 0, metadata !6, i32 0} ; [ DW_TAG_lexical_block ] [/]
!20 = metadata !{i32 8, i32 0, metadata !19, null}
!21 = metadata !{i32 786688, metadata !19, metadata !"prev", metadata !6, i32 8, metadata !9, i32 0, i32 0} ; [ DW_TAG_auto_variable ] [prev] [line 8]
!22 = metadata !{i32 10, i32 0, metadata !19, null}
!23 = metadata !{i32 11, i32 0, metadata !19, null}
!24 = metadata !{i32 13, i32 0, metadata !19, null}
!25 = metadata !{i32 14, i32 0, metadata !26, null}
!26 = metadata !{i32 786443, metadata !19, i32 13, i32 0, metadata !6, i32 1} ; [ DW_TAG_lexical_block ] [/]
!27 = metadata !{i32 15, i32 0, metadata !26, null}
!28 = metadata !{i32 16, i32 0, metadata !26, null}
!29 = metadata !{i32 18, i32 0, metadata !19, null}
!30 = metadata !{i32 19, i32 0, metadata !19, null}
