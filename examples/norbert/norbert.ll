%node = type { i32, %node* }

define %node* @norbert(i32 %a, %node* %b) {
  %p = call noalias i8* @malloc(i64 16)
  %r = bitcast i8* %p to %node*
  %br = icmp eq i8* %p, null
  br i1 %br, label %.success, label %.exit
.success:
  %i = bitcast i8* %p to i32*
  store i32 %a, i32* %i
  %n = getelementptr %node* %r, i32 0, i32 1
  store %node* %b, %node** %n
  br label %.exit
.exit:
  ret %node* %r
}

declare noalias i8* @malloc(i64) nounwind
