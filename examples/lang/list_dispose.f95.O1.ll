; ModuleID = 'list_dispose.f95'
target datalayout = "e-p:64:64:64-S128-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f16:16:16-f32:32:32-f64:64:64-f128:128:128-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64--linux-gnu"

module asm "\09.ident\09\22GCC: (Debian 4.6.4-4) 4.6.4 LLVM: 3.3\22"

%struct.node = type { i32, %struct.node* }

; Function Attrs: nounwind uwtable
define i32 @__linkedlist_MOD_list_dispose(%struct.node** nocapture %list) unnamed_addr #0 {
entry:
  %0 = load %struct.node** %list, align 8
  %1 = icmp eq %struct.node* %0, null
  br i1 %1, label %return, label %"4"

"4":                                              ; preds = %entry, %"4"
  %2 = phi %struct.node* [ %4, %"4" ], [ %0, %entry ]
  %3 = getelementptr inbounds %struct.node* %2, i64 0, i32 1
  %4 = load %struct.node** %3, align 8
  %5 = bitcast %struct.node* %2 to i8*
  tail call void @free(i8* %5) #1
  %6 = icmp eq %struct.node* %4, null
  br i1 %6, label %return, label %"4"

return:                                           ; preds = %"4", %entry
  ret i32 undef
}

; Function Attrs: nounwind
declare void @free(i8* nocapture) #1

attributes #0 = { nounwind uwtable }
attributes #1 = { nounwind }
