%single = type { i64 } ; pathological case 1: a single-member struct w/o padding
%singlepadded = type { i6 } ; pathological case 2: a single-member struct w/ padding
%empty = type { } ; pathological case 3: an empty struct
%packed = type <{ i12, i20 }> ; a packed struct

