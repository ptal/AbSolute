open Core
open Domains.Abstract_domain
open Transformer

module Make(Domain: Abstract_domain) =
struct
  module T=Transformer(Domain)

  let rec solve t =
    try
      let (gs,bs) = T.on_node t in
      let (gs,bs) = T.wrap_exception (gs,bs) (fun (gs,bs) ->
        (gs,{ bs with domain=Domain.closure bs.domain})) in
      match Domain.state_decomposition bs.domain with
      | Kleene.False -> T.on_fail (gs,bs)
      | Kleene.True -> T.on_solution (gs,bs)
      | Kleene.Unknown ->
          let (gs,bs) = T.on_unknown (gs,bs) in
          let branches = Domain.split bs.domain in
          let bss = List.map (fun domain -> {bs with domain}) branches in
          List.fold_left (fun (gs,_) bs -> solve (gs,bs)) (gs,bs) bss
    with
    | T.Backjump (0, t) -> t
    | T.Backjump (n, t) -> raise (T.Backjump ((n-1), t))
end
