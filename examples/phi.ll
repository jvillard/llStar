define i32 @test(i1 %b1, i1 %b2, i1 %b3) {
bb0:
  br i1 %b1, label %bb1, label %bbphi

bb1:
  br i1 %b2, label %bb2, label %bbphi

bb2:
  br label %bbphi

bbphi:
  %x = phi i32 [0, %bb0], [1, %bb1], [2, %bb2]
  %y = phi i32 [3, %bb0], [4, %bb1], [5, %bb2]
  %z = phi i32 [3, %bb0], [5, %bb1], [7, %bb2]
  %tmp = add i32 %x, %y
  %r = sub i32 %z, %tmp
  ret i32 %r
}
