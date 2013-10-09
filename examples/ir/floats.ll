; mainly a parsing test I guess...
define float @addf(float %f1, float %f2) {
  %a = fadd float %f1, %f2
  %b = fsub float %a, %f1
  %c = fpext float %a to double
  %d = fpext double %c to ppc_fp128
  %e = fptrunc ppc_fp128 %d to float
  %f = fptoui float %e to i64
  %g = sitofp i64 %f to float
  ret float %g
}
