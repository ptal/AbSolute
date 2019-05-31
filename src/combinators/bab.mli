(** This module provide an branch and bound algorithm for abstract domain. *)
open Abstract_domain
open State

module type BAB_sig = functor(Domain: Abstract_domain) ->
sig
  type t

  (** `print_node status depth domain` is called in each node. *)
  val init : ?print_node:(string -> int -> Domain.t -> unit) -> ?print_bound:(Domain.t -> unit) -> Domain.R.t -> Domain.t -> t
  val minimize : t -> Mtime.span -> Domain.R.var_id -> (Domain.t option * global_statistics)
  val maximize : t -> Mtime.span -> Domain.R.var_id -> (Domain.t option * global_statistics)
end

module Make : BAB_sig
