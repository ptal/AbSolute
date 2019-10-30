open Domains.Abstract_domain
open Lang.Ast

module Logical_wrapper(A: Abstract_domain) =
struct

  module B = A.B
  module I = A.I

  (** The type of the abstract domain. *)
  type t = {
    a: A.t;
    r: I.t;
  }

  let init a r = {a;r}
  let unwrap l = l.a
  let interpretation l = l.r

  let rec qinterpret lw approx = function
    | QFFormula f ->
      begin
        match I.interpret lw.r approx f with
        | [] -> None
        | cs -> Some {lw with a=List.fold_left A.weak_incremental_closure lw.a cs}
      end
    | Exists (v, ty, qf) ->
        try
          ignore(I.to_abstract_var lw.r v);
          qinterpret lw approx qf
        (* We add the variable only if it is not already in `lw.r`. *)
        with Not_found ->
          let a, v_id, aty = A.extend ~ty lw.a in
          let r = I.extend lw.r (v, v_id, aty) in
          qinterpret {a;r} approx qf

  let print' fmt lw = A.print lw.r fmt lw.a

  (* The following function essentially delegates the work to `A`. *)

  let wrap l a = {l with a}

  let empty uid = {a = A.empty uid; r = I.empty}
  let uid lw = A.uid lw.a
  let extend ?ty lw =
    let (a, id, aty) = A.extend ?ty lw.a in
    wrap lw a, id, aty
  let project lw v = A.project lw.a v
  let lazy_copy lw n = List.map (wrap lw) (A.lazy_copy lw.a n)
  let copy lw = wrap lw (A.copy lw.a)
  let closure lw = wrap lw (A.closure lw.a)
  let weak_incremental_closure lw c = wrap lw (A.weak_incremental_closure lw.a c)
  let entailment lw c = A.entailment lw.a c
  let split lw = List.map (wrap lw) (A.split lw.a)
  let volume lw = A.volume lw.a
  let state_decomposition lw = A.state_decomposition lw.a
  let print _ fmt lw = print' fmt lw
end
