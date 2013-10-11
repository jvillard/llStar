type funspec =
    Funspec of string * Spec.spec

let mkEmptySpec =
  Spec.mk_spec Psyntax.mkEmpty Psyntax.mkEmpty Spec.ClassMap.empty

(** returns the spec of function id [fid] if it is known *)
let rec spec_of_fun_id specs fid = match specs with
  | Funspec(i, spec)::ss when i = fid -> spec
  | _::ss -> spec_of_fun_id ss fid
  | [] ->
    if not !Llstar_config.abduction_flag then
      Llutils.warn ("no spec found for "^fid);
    mkEmptySpec
