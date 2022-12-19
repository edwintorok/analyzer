open Prelude.Ana
open Analyses
open Cilint

module DomainLock =
struct
  include Printable.Std

  type t = Acquired | Released [@@deriving eq, ord, hash, to_yojson, show]
  let name () = "domainlock"

  include Printable.SimpleShow (struct
      type nonrec t = t
      let show = show
    end)
end

(* Now we turn this into a lattice by adding Top and Bottom elements.
 * We then lift the above operations to the lattice. *)
module SL =
struct
  include Lattice.Flat (DomainLock) (Printable.DefaultNames)

  let acquired = `Lifted DomainLock.Acquired
  let released = `Lifted DomainLock.Released
end

module Spec : Analyses.MCPSpec =
struct
  let name () = "domainlock"

  module D = SL
  module C = D

  let startstate v = D.bot ()
  let exitstate = startstate

  include Analyses.IdentitySpec

  let is_value_ptr = function
  	| TPtr (TNamed({tname = "value"; _}, _), _) -> true
  	| _ -> false

	(* TODO: use visitor class *)
  let rec has_ocaml_value : exp -> bool = function
  	| Lval(Mem e, _) -> has_ocaml_value e
  	| CastE (t, e) ->
  		is_value_ptr t || has_ocaml_value e
    | BinOp(_, a, b, _) ->
    		has_ocaml_value a || has_ocaml_value b
  	| e ->
  		ignore (Pretty.printf "has_ocaml_value? %a\n" Cil.d_exp e);
  		false

  let special (ctx:(D.t, G.t, C.t,V.t) ctx) (lval: lval option) (f:varinfo) (arglist:exp list) =
  	match f.vname with
  	| "caml_enter_blocking_section" -> C.released
  	| "caml_leave_blocking_section" -> C.acquired
  	| _ ->
  		let () = arglist |> List.iter @@ fun arg ->
			if has_ocaml_value arg then
				Messages.warn ~category:Messages.Category.Race "MYMESSAGE Call using
				OCaml value after domain lock as been released: %s(... %a ...)"
				f.vname Cil.d_exp arg
        in
  		(* TODO: better logging *)
  		ignore (Pretty.printf "special(%s) : %a\n" f.vname SL.pretty ctx.local);
  		ctx.local

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
