; ModuleID = '/home/jvillard/science/tools/llstar/bitbucket/examples/sharing/_llstar/mark_obfuscated.c.bc'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.node = type { i32, %struct.node*, %struct.node* }

; Function Attrs: nounwind uwtable
define void @mark_trickier(%struct.node* %x) #0 {
  %1 = alloca %struct.node*, align 8
  %tmp = alloca %struct.node*, align 8
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
  br label %41, !dbg !18

; <label>:10                                      ; preds = %4
  %11 = load %struct.node** %1, align 8, !dbg !20
  %12 = getelementptr inbounds %struct.node* %11, i32 0, i32 0, !dbg !20
  store i32 1, i32* %12, align 4, !dbg !20
  call void @llvm.dbg.declare(metadata !{%struct.node** %tmp}, metadata !21), !dbg !22
  %13 = load %struct.node** %1, align 8, !dbg !22
  %14 = getelementptr inbounds %struct.node* %13, i32 0, i32 1, !dbg !22
  %15 = load %struct.node** %14, align 8, !dbg !22
  store %struct.node* %15, %struct.node** %tmp, align 8, !dbg !22
  %16 = load %struct.node** %1, align 8, !dbg !23
  %17 = getelementptr inbounds %struct.node* %16, i32 0, i32 2, !dbg !23
  %18 = load %struct.node** %17, align 8, !dbg !23
  %19 = load %struct.node** %1, align 8, !dbg !23
  %20 = getelementptr inbounds %struct.node* %19, i32 0, i32 1, !dbg !23
  store %struct.node* %18, %struct.node** %20, align 8, !dbg !23
  %21 = load %struct.node** %tmp, align 8, !dbg !24
  %22 = load %struct.node** %1, align 8, !dbg !24
  %23 = getelementptr inbounds %struct.node* %22, i32 0, i32 2, !dbg !24
  store %struct.node* %21, %struct.node** %23, align 8, !dbg !24
  %24 = load %struct.node** %1, align 8, !dbg !25
  %25 = getelementptr inbounds %struct.node* %24, i32 0, i32 1, !dbg !25
  %26 = load %struct.node** %25, align 8, !dbg !25
  call void @mark_trickier(%struct.node* %26), !dbg !25
  %27 = load %struct.node** %1, align 8, !dbg !26
  %28 = getelementptr inbounds %struct.node* %27, i32 0, i32 1, !dbg !26
  %29 = load %struct.node** %28, align 8, !dbg !26
  store %struct.node* %29, %struct.node** %tmp, align 8, !dbg !26
  %30 = load %struct.node** %1, align 8, !dbg !27
  %31 = getelementptr inbounds %struct.node* %30, i32 0, i32 2, !dbg !27
  %32 = load %struct.node** %31, align 8, !dbg !27
  %33 = load %struct.node** %1, align 8, !dbg !27
  %34 = getelementptr inbounds %struct.node* %33, i32 0, i32 1, !dbg !27
  store %struct.node* %32, %struct.node** %34, align 8, !dbg !27
  %35 = load %struct.node** %1, align 8, !dbg !28
  %36 = getelementptr inbounds %struct.node* %35, i32 0, i32 2, !dbg !28
  %37 = load %struct.node** %36, align 8, !dbg !28
  call void @mark_trickier(%struct.node* %37), !dbg !28
  %38 = load %struct.node** %tmp, align 8, !dbg !29
  %39 = load %struct.node** %1, align 8, !dbg !29
  %40 = getelementptr inbounds %struct.node* %39, i32 0, i32 2, !dbg !29
  store %struct.node* %38, %struct.node** %40, align 8, !dbg !29
  br label %41, !dbg !30

; <label>:41                                      ; preds = %10, %9
  ret void, !dbg !30
}

; Function Attrs: nounwind readnone
declare void @llvm.dbg.declare(metadata, metadata) #1

attributes #0 = { nounwind uwtable }
attributes #1 = { nounwind readnone }

!llvm.dbg.cu = !{!0}

!0 = metadata !{i32 786449, i32 0, i32 12, metadata !"mark_obfuscated.c", metadata !"/home/jvillard/science/tools/llstar/bitbucket/examples/sharing", metadata !"Debian clang version 3.2-11 (tags/RELEASE_32/final) (based on LLVM 3.2)", i1 true, i1 false, metadata !"", i32 0, metadata !1, metadata !1, metadata !3, metadata !1}
!1 = metadata !{metadata !2}
!2 = metadata !{i32 0}
!3 = metadata !{metadata !4}
!4 = metadata !{metadata !5}
!5 = metadata !{i32 786478, i32 0, metadata !6, metadata !"mark_trickier", metadata !"mark_trickier", metadata !"", metadata !6, i32 7, metadata !7, i1 false, i1 true, i32 0, i32 0, null, i32 256, i1 false, void (%struct.node*)* @mark_trickier, null, null, metadata !1, i32 7}
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
!16 = metadata !{i32 786689, metadata !5, metadata !"x", metadata !6, i32 16777223, metadata !9, i32 0, i32 0}
!17 = metadata !{i32 7, i32 0, metadata !5, null}
!18 = metadata !{i32 8, i32 0, metadata !19, null}
!19 = metadata !{i32 786443, metadata !5, i32 7, i32 0, metadata !6, i32 0} ; [ DW_TAG_lexical_block ] [/]
!20 = metadata !{i32 9, i32 0, metadata !19, null}
!21 = metadata !{i32 786688, metadata !19, metadata !"tmp", metadata !6, i32 10, metadata !9, i32 0, i32 0} ; [ DW_TAG_auto_variable ] [tmp] [line 10]
!22 = metadata !{i32 10, i32 0, metadata !19, null}
!23 = metadata !{i32 11, i32 0, metadata !19, null}
!24 = metadata !{i32 12, i32 0, metadata !19, null}
!25 = metadata !{i32 13, i32 0, metadata !19, null}
!26 = metadata !{i32 14, i32 0, metadata !19, null}
!27 = metadata !{i32 15, i32 0, metadata !19, null}
!28 = metadata !{i32 16, i32 0, metadata !19, null}
!29 = metadata !{i32 17, i32 0, metadata !19, null}
!30 = metadata !{i32 18, i32 0, metadata !19, null}
