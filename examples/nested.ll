target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%dstruct = type <{ i6, i6, i6, i6, i8, i32 }>
%cstruct = type { %dstruct }
%bstruct = type { i32*, %cstruct }
%astruct = type { i32, i12, %bstruct, %bstruct, %dstruct, i32* } ; a nested struct

define i6 @access_nested_struct(%astruct* %s) {
  %1 = getelementptr inbounds %astruct* %s, i32 0, i32 3, i32 1, i32 0, i32 2
  %2 = load i6* %1
  ret i6 %2
}
