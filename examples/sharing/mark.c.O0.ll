; ModuleID = '/home/jvillard/science/tools/llstar/bitbucket/examples/sharing/_llstar/mark.c.bc'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.node = type { i32, %struct.node*, %struct.node* }

; Function Attrs: nounwind uwtable
define void @mark(%struct.node* %x) #0 {
  %1 = alloca %struct.node*, align 8
  store %struct.node* %x, %struct.node** %1, align 8
  call void @llvm.dbg.declare(metadata !{%struct.node** %1}, metadata !16), !dbg !17
  %2 = load %struct.node** %1, align 8, !dbg !18
  %3 = icmp ne %struct.node* %2, null, !dbg !18
  br i1 %3, label %4, label %9, !dbg !18

; <label>:4                                       ; preds = %0
  %5 = load %struct.node** %1, align 8, !dbg !18
  %6 = getelementptr inbounds %struct.node* %5, i32 0, i32 0, !dbg !18
  %7 = load i32* %6, align 4, !dbg !18
  %8 = icmp ne i32 %7, 0, !dbg !18
  br i1 %8, label %9, label %10, !dbg !18

; <label>:9                                       ; preds = %4, %0
  br label %19, !dbg !18

; <label>:10                                      ; preds = %4
  %11 = load %struct.node** %1, align 8, !dbg !20
  %12 = getelementptr inbounds %struct.node* %11, i32 0, i32 0, !dbg !20
  store i32 1, i32* %12, align 4, !dbg !20
  %13 = load %struct.node** %1, align 8, !dbg !21
  %14 = getelementptr inbounds %struct.node* %13, i32 0, i32 1, !dbg !21
  %15 = load %struct.node** %14, align 8, !dbg !21
  call void @mark(%struct.node* %15), !dbg !21
  %16 = load %struct.node** %1, align 8, !dbg !22
  %17 = getelementptr inbounds %struct.node* %16, i32 0, i32 2, !dbg !22
  %18 = load %struct.node** %17, align 8, !dbg !22
  call void @mark(%struct.node* %18), !dbg !22
  br label %19, !dbg !23

; <label>:19                                      ; preds = %10, %9
  ret void, !dbg !23
}

; Function Attrs: nounwind readnone
declare void @llvm.dbg.declare(metadata, metadata) #1

attributes #0 = { nounwind uwtable }
attributes #1 = { nounwind readnone }

!llvm.dbg.cu = !{!0}

!0 = metadata !{i32 786449, i32 0, i32 12, metadata !"mark.c", metadata !"/home/jvillard/science/tools/llstar/bitbucket/examples/sharing", metadata !"Debian clang version 3.2-11 (tags/RELEASE_32/final) (based on LLVM 3.2)", i1 true, i1 false, metadata !"", i32 0, metadata !1, metadata !1, metadata !3, metadata !1}
!1 = metadata !{metadata !2}
!2 = metadata !{i32 0}
!3 = metadata !{metadata !4}
!4 = metadata !{metadata !5}
!5 = metadata !{i32 786478, i32 0, metadata !6, metadata !"mark", metadata !"mark", metadata !"", metadata !6, i32 7, metadata !7, i1 false, i1 true, i32 0, i32 0, null, i32 256, i1 false, void (%struct.node*)* @mark, null, null, metadata !1, i32 7}
!6 = metadata !{i32 786473, metadata !"mark.c", metadata !"/home/jvillard/science/tools/llstar/bitbucket/examples/sharing", null}
!7 = metadata !{i32 786453, i32 0, metadata !"", i32 0, i32 0, i64 0, i64 0, i64 0, i32 0, null, metadata !8, i32 0, i32 0} ; [ DW_TAG_subroutine_type ] [line 0, size 0, align 0, offset 0] [from ]
!8 = metadata !{null, metadata !9}
!9 = metadata !{i32 786447, null, metadata !"", null, i32 0, i64 64, i64 64, i64 0, i32 0, metadata !10} ; [ DW_TAG_pointer_type ] [line 0, size 64, align 64, offset 0] [from ]
!10 = metadata !{i32 786451, null, metadata !"node", metadata !6, i32 1, i64 192, i64 64, i32 0, i32 0, null, metadata !11, i32 0, i32 0, i32 0} ; [ DW_TAG_structure_type ] [line 1, size 192, align 64, offset 0] [from ]
!11 = metadata !{metadata !12, metadata !14, metadata !15}
!12 = metadata !{i32 786445, metadata !10, metadata !"color", metadata !6, i32 2, i64 32, i64 32, i64 0, i32 0, metadata !13} ; [ DW_TAG_member ] [line 2, size 32, align 32, offset 0] [from ]
!13 = metadata !{i32 786468, null, metadata !"int", null, i32 0, i64 32, i64 32, i64 0, i32 0, i32 5} ; [ DW_TAG_base_type ] [line 0, size 32, align 32, offset 0, enc DW_ATE_signed]
!14 = metadata !{i32 786445, metadata !10, metadata !"left", metadata !6, i32 3, i64 64, i64 64, i64 64, i32 0, metadata !9} ; [ DW_TAG_member ] [line 3, size 64, align 64, offset 64] [from ]
!15 = metadata !{i32 786445, metadata !10, metadata !"right", metadata !6, i32 4, i64 64, i64 64, i64 128, i32 0, metadata !9} ; [ DW_TAG_member ] [line 4, size 64, align 64, offset 128] [from ]
!16 = metadata !{i32 786689, metadata !5, metadata !"x", metadata !6, i32 16777223, metadata !9, i32 0, i32 0}
!17 = metadata !{i32 7, i32 0, metadata !5, null}
!18 = metadata !{i32 8, i32 0, metadata !19, null}
!19 = metadata !{i32 786443, metadata !5, i32 7, i32 0, metadata !6, i32 0} ; [ DW_TAG_lexical_block ] [/]
!20 = metadata !{i32 9, i32 0, metadata !19, null}
!21 = metadata !{i32 10, i32 0, metadata !19, null}
!22 = metadata !{i32 11, i32 0, metadata !19, null}
!23 = metadata !{i32 12, i32 0, metadata !19, null}
