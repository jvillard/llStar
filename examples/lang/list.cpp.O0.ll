; ModuleID = '/home/jvillard/science/tools/llstar/bitbucket/examples/lang/_llstar/list.cpp.bc'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.node = type { i32, %struct.node* }

; Function Attrs: nounwind uwtable
define void @_Z8traverseP4node(%struct.node* %l) #0 {
  %1 = alloca %struct.node*, align 8
  %cur = alloca %struct.node*, align 8
  store %struct.node* %l, %struct.node** %1, align 8
  %2 = load %struct.node** %1, align 8
  store %struct.node* %2, %struct.node** %cur, align 8
  br label %3

; <label>:3                                       ; preds = %6, %0
  %4 = load %struct.node** %cur, align 8
  %5 = icmp ne %struct.node* %4, null
  br i1 %5, label %6, label %10

; <label>:6                                       ; preds = %3
  %7 = load %struct.node** %cur, align 8
  %8 = getelementptr inbounds %struct.node* %7, i32 0, i32 1
  %9 = load %struct.node** %8, align 8
  store %struct.node* %9, %struct.node** %cur, align 8
  br label %3

; <label>:10                                      ; preds = %3
  ret void
}

attributes #0 = { nounwind uwtable }
