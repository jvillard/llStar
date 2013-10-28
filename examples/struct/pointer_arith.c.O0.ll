; ModuleID = 'pointer_arith.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.ole = type { i32*, %struct.ole* }

; Function Attrs: nounwind uwtable
define i32 @pointer_arith(i32 %a) #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %p = alloca %struct.ole*, align 8
  %b = alloca i32, align 4
  store i32 %a, i32* %2, align 4
  %3 = call noalias i8* @malloc(i64 16) #2
  %4 = bitcast i8* %3 to %struct.ole*
  store %struct.ole* %4, %struct.ole** %p, align 8
  %5 = load %struct.ole** %p, align 8
  %6 = icmp ne %struct.ole* %5, null
  br i1 %6, label %9, label %7

; <label>:7                                       ; preds = %0
  %8 = load i32* %2, align 4
  store i32 %8, i32* %1
  br label %49

; <label>:9                                       ; preds = %0
  %10 = call noalias i8* @malloc(i64 4) #2
  %11 = bitcast i8* %10 to i32*
  %12 = load %struct.ole** %p, align 8
  %13 = getelementptr inbounds %struct.ole* %12, i32 0, i32 0
  store i32* %11, i32** %13, align 8
  %14 = load %struct.ole** %p, align 8
  %15 = getelementptr inbounds %struct.ole* %14, i32 0, i32 0
  %16 = load i32** %15, align 8
  %17 = icmp ne i32* %16, null
  br i1 %17, label %22, label %18

; <label>:18                                      ; preds = %9
  %19 = load %struct.ole** %p, align 8
  %20 = bitcast %struct.ole* %19 to i8*
  call void @free(i8* %20) #2
  %21 = load i32* %2, align 4
  store i32 %21, i32* %1
  br label %49

; <label>:22                                      ; preds = %9
  %23 = load %struct.ole** %p, align 8
  %24 = load %struct.ole** %p, align 8
  %25 = bitcast %struct.ole* %24 to i8*
  %26 = getelementptr i8* %25, i64 8
  %27 = bitcast i8* %26 to %struct.ole**
  store %struct.ole* %23, %struct.ole** %27, align 8
  %28 = load i32* %2, align 4
  %29 = load %struct.ole** %p, align 8
  %30 = getelementptr inbounds %struct.ole* %29, i32 0, i32 1
  %31 = load %struct.ole** %30, align 8
  %32 = getelementptr inbounds %struct.ole* %31, i32 0, i32 1
  %33 = load %struct.ole** %32, align 8
  %34 = getelementptr inbounds %struct.ole* %33, i32 0, i32 1
  %35 = load %struct.ole** %34, align 8
  %36 = getelementptr inbounds %struct.ole* %35, i32 0, i32 0
  %37 = load i32** %36, align 8
  store i32 %28, i32* %37, align 4
  %38 = load %struct.ole** %p, align 8
  %39 = getelementptr inbounds %struct.ole* %38, i32 0, i32 0
  %40 = load i32** %39, align 8
  %41 = load i32* %40, align 4
  store i32 %41, i32* %b, align 4
  %42 = load %struct.ole** %p, align 8
  %43 = getelementptr inbounds %struct.ole* %42, i32 0, i32 0
  %44 = load i32** %43, align 8
  %45 = bitcast i32* %44 to i8*
  call void @free(i8* %45) #2
  %46 = load %struct.ole** %p, align 8
  %47 = bitcast %struct.ole* %46 to i8*
  call void @free(i8* %47) #2
  %48 = load i32* %b, align 4
  store i32 %48, i32* %1
  br label %49

; <label>:49                                      ; preds = %22, %18, %7
  %50 = load i32* %1
  ret i32 %50
}

; Function Attrs: nounwind
declare noalias i8* @malloc(i64) #1

; Function Attrs: nounwind
declare void @free(i8*) #1

; Function Attrs: nounwind uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %n = alloca i32, align 4
  store i32 0, i32* %1
  %2 = load i32* %n, align 4
  %3 = call i32 @pointer_arith(i32 %2)
  %4 = load i32* %n, align 4
  %5 = icmp eq i32 %3, %4
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  store i32 0, i32* %1
  br label %9

; <label>:7                                       ; preds = %0
  br label %8

; <label>:8                                       ; preds = %7
  store i32 1, i32* %1
  br label %9

; <label>:9                                       ; preds = %8, %6
  %10 = load i32* %1
  ret i32 %10
}

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf"="true" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf"="true" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind }
