open Domains.Abstract_domain
open Domains.Logical_abstract_domain

module Logical_wrapper(A: Abstract_domain) :
sig
  include Logical_abstract_domain
  val init: A.t -> A.I.t -> t
  val unwrap: t -> A.t
end
