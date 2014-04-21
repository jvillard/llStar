open Core

type funspec = Funspec of string * spec

let mkEmptySpec =
  let t = { pre = Syntax.mk_emp;
	    post = Syntax.mk_emp;
	    in_vars = []; out_vars = []; modifies = [] } in
  TripleSet.singleton t

(** returns the spec of function id [fid] if it is known *)
let rec spec_of_fun_id specs fid = match specs with
  | Funspec(i, spec)::ss -> if i = fid then spec else spec_of_fun_id ss fid
  | [] ->
    if not !Llstar_config.abduction_flag then
      Llutils.warn ("no spec found for "^fid);
    mkEmptySpec
