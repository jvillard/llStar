; ModuleID = '/home/jvillard/science/tools/llstar/bitbucket/examples/list/_llstar/list_reverse.c.bc'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.node = type { i32, %struct.node* }

; Function Attrs: nounwind uwtable
define %struct.node* @list_reverse(%struct.node* %l) #0 {
  %1 = alloca %struct.node*, align 8
  %a = alloca %struct.node*, align 8
  %b = alloca %struct.node*, align 8
  %c = alloca %struct.node*, align 8
  store %struct.node* %l, %struct.node** %1, align 8
  call void @llvm.dbg.declare(metadata !{%struct.node** %1}, metadata !15), !dbg !16
  call void @llvm.dbg.declare(metadata !{%struct.node** %a}, metadata !17), !dbg !19
  store %struct.node* null, %struct.node** %a, align 8, !dbg !19
  call void @llvm.dbg.declare(metadata !{%struct.node** %b}, metadata !20), !dbg !21
  store %struct.node* null, %struct.node** %b, align 8, !dbg !21
  call void @llvm.dbg.declare(metadata !{%struct.node** %c}, metadata !22), !dbg !23
  store %struct.node* null, %struct.node** %c, align 8, !dbg !23
  %2 = load %struct.node** %1, align 8, !dbg !24
  store %struct.node* %2, %struct.node** %a, align 8, !dbg !24
  store %struct.node* null, %struct.node** %b, align 8, !dbg !24
  br label %3, !dbg !25

; <label>:3                                       ; preds = %6, %0
  %4 = load %struct.node** %a, align 8, !dbg !25
  %5 = icmp ne %struct.node* %4, null, !dbg !25
  br i1 %5, label %6, label %15, !dbg !25

; <label>:6                                       ; preds = %3
  %7 = load %struct.node** %b, align 8, !dbg !26
  store %struct.node* %7, %struct.node** %c, align 8, !dbg !26
  %8 = load %struct.node** %a, align 8, !dbg !26
  store %struct.node* %8, %struct.node** %b, align 8, !dbg !26
  %9 = load %struct.node** %a, align 8, !dbg !26
  %10 = getelementptr inbounds %struct.node* %9, i32 0, i32 1, !dbg !26
  %11 = load %struct.node** %10, align 8, !dbg !26
  store %struct.node* %11, %struct.node** %a, align 8, !dbg !26
  %12 = load %struct.node** %c, align 8, !dbg !28
  %13 = load %struct.node** %b, align 8, !dbg !28
  %14 = getelementptr inbounds %struct.node* %13, i32 0, i32 1, !dbg !28
  store %struct.node* %12, %struct.node** %14, align 8, !dbg !28
  br label %3, !dbg !29

; <label>:15                                      ; preds = %3
  %16 = load %struct.node** %b, align 8, !dbg !30
  ret %struct.node* %16, !dbg !30
}

; Function Attrs: nounwind readnone
declare void @llvm.dbg.declare(metadata, metadata) #1

attributes #0 = { nounwind uwtable }
attributes #1 = { nounwind readnone }

!llvm.dbg.cu = !{!0}

!0 = metadata !{i32 786449, i32 0, i32 12, metadata !"list_reverse.c", metadata !"/home/jvillard/science/tools/llstar/bitbucket/examples/list", metadata !"Debian clang version 3.2-11 (tags/RELEASE_32/final) (based on LLVM 3.2)", i1 true, i1 false, metadata !"", i32 0, metadata !1, metadata !1, metadata !3, metadata !1}
!1 = metadata !{metadata !2}
!2 = metadata !{i32 0}
!3 = metadata !{metadata !4}
!4 = metadata !{metadata !5}
!5 = metadata !{i32 786478, i32 0, metadata !6, metadata !"list_reverse", metadata !"list_reverse", metadata !"", metadata !6, i32 8, metadata !7, i1 false, i1 true, i32 0, i32 0, null, i32 256, i1 false, %struct.node* (%struct.node*)* @list_reverse, null, null, metadata !1, i32 8}
!6 = metadata !{i32 786473, metadata !"list_reverse.c", metadata !"/home/jvillard/science/tools/llstar/bitbucket/examples/list", null}
!7 = metadata !{i32 786453, i32 0, metadata !"", i32 0, i32 0, i64 0, i64 0, i64 0, i32 0, null, metadata !8, i32 0, i32 0} ; [ DW_TAG_subroutine_type ] [line 0, size 0, align 0, offset 0] [from ]
!8 = metadata !{metadata !9, metadata !9}
!9 = metadata !{i32 786447, null, metadata !"", null, i32 0, i64 64, i64 64, i64 0, i32 0, metadata !10} ; [ DW_TAG_pointer_type ] [line 0, size 64, align 64, offset 0] [from ]
!10 = metadata !{i32 786451, null, metadata !"node", metadata !6, i32 3, i64 128, i64 64, i32 0, i32 0, null, metadata !11, i32 0, i32 0, i32 0} ; [ DW_TAG_structure_type ] [line 3, size 128, align 64, offset 0] [from ]
!11 = metadata !{metadata !12, metadata !14}
!12 = metadata !{i32 786445, metadata !10, metadata !"data", metadata !6, i32 4, i64 32, i64 32, i64 0, i32 0, metadata !13} ; [ DW_TAG_member ] [line 4, size 32, align 32, offset 0] [from ]
!13 = metadata !{i32 786468, null, metadata !"int", null, i32 0, i64 32, i64 32, i64 0, i32 0, i32 5} ; [ DW_TAG_base_type ] [line 0, size 32, align 32, offset 0, enc DW_ATE_signed]
!14 = metadata !{i32 786445, metadata !10, metadata !"next", metadata !6, i32 5, i64 64, i64 64, i64 64, i32 0, metadata !9} ; [ DW_TAG_member ] [line 5, size 64, align 64, offset 64] [from ]
!15 = metadata !{i32 786689, metadata !5, metadata !"l", metadata !6, i32 16777224, metadata !9, i32 0, i32 0}
!16 = metadata !{i32 8, i32 0, metadata !5, null}
!17 = metadata !{i32 786688, metadata !18, metadata !"a", metadata !6, i32 9, metadata !9, i32 0, i32 0} ; [ DW_TAG_auto_variable ] [a] [line 9]
!18 = metadata !{i32 786443, metadata !5, i32 8, i32 0, metadata !6, i32 0} ; [ DW_TAG_lexical_block ] [/]
!19 = metadata !{i32 9, i32 0, metadata !18, null}
!20 = metadata !{i32 786688, metadata !18, metadata !"b", metadata !6, i32 10, metadata !9, i32 0, i32 0} ; [ DW_TAG_auto_variable ] [b] [line 10]
!21 = metadata !{i32 10, i32 0, metadata !18, null}
!22 = metadata !{i32 786688, metadata !18, metadata !"c", metadata !6, i32 11, metadata !9, i32 0, i32 0} ; [ DW_TAG_auto_variable ] [c] [line 11]
!23 = metadata !{i32 11, i32 0, metadata !18, null}
!24 = metadata !{i32 12, i32 0, metadata !18, null}
!25 = metadata !{i32 14, i32 0, metadata !18, null}
!26 = metadata !{i32 15, i32 0, metadata !27, null}
!27 = metadata !{i32 786443, metadata !18, i32 14, i32 0, metadata !6, i32 1} ; [ DW_TAG_lexical_block ] [/]
!28 = metadata !{i32 16, i32 0, metadata !27, null}
!29 = metadata !{i32 17, i32 0, metadata !27, null}
!30 = metadata !{i32 19, i32 0, metadata !18, null}
