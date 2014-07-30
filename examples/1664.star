import "../rules/llvm.logic";

procedure main returns (i32 %x)
   {emp}
   {i32 %x = i32 1664 * sizeof(lltype i32) = i64 4};

procedure f(i32 %b) returns (i32 %x)
   {i32 %b = i32 _x}
   {i32 %x = i32 _x};
