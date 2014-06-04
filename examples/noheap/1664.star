import "../../rules/llvm.logic";

procedure main returns (i32 %x)
   {emp}
   {i32 %x = i32 1664 * sizeof(lltype i32) = i64 4};

procedure f(i32 %a) returns (i32 %x)
   {i32 %a = i32 _x}
   {i32 %x = i32 _x };
