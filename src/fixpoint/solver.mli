(** This module provide an branch and bound algorithm for abstract domain. *)
open Domains.Abstract_domain
open Transformer

module Make(Domain: Abstract_domain):
sig
  module T: module type of(Transformer(Domain))

  val solve: T.t -> T.t
end
