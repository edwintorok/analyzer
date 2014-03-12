open Cil
open Pretty
open Analyses

module LV = Lval.CilLval
module LS = Queries.LS
module LM = MapDomain.MapBot_LiftTop (LV) (LS)

module Spec =
struct
  include Analyses.DefaultSpec

  let name = "impvar"
  module D = Lattice.Unit
  module G = LS
  module C = Lattice.Unit
  
  let get_deps ctx (v,os) = 
    match ctx.ask (Queries.VariableDeps (Var v, os)) with
      | `Bot -> LS.bot ()
      | `LvalSet ls -> ls
      | _ -> LS.top ()

  let add_var ctx (v,os) = 
    (* Printf.printf "%s is important too!\n" v.vname; *)
    let ls = LS.add (v,LV.of_ciloffs os) (get_deps ctx (v,os)) in
    LS.iter (fun (v,os) -> ctx.sideg v (LS.singleton (v,os))) ls
    
  let os_leq (_,o1) (_,o2) = 
    let rec leq o1 o2 = 
      match o1, o2 with
        | _, `NoOffset -> true
        | `Index (i,os), `Index (i',os') -> Expcompare.compareExp   i i' && leq os os'
        | `Field (f,os), `Field (f',os') -> Basetype.CilField.equal f f' && leq os os'
        | _ -> false 
    in
    leq o1 o2
  
  let is_important ctx (v,os) =
    LS.exists (os_leq (v,os)) (ctx.global v)
    
  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t = ()
  let branch ctx (exp:exp) (tv:bool) : D.t = ()
  let body ctx (f:fundec) : D.t = ()
  let return ctx (exp:exp option) (f:fundec) : D.t = ()
  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list = [(),()]
  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t = ()
  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    match f.vname, List.map stripCasts arglist with
      | "important_var", [Lval (Var v, os)] -> add_var ctx (v,os)
      | _ -> ()

  let startstate v = D.bot ()
  let otherstate v = D.bot ()
  let exitstate  v = D.bot ()
  
  let query ctx = function 
    | Queries.IsImportant (Var v, os) -> `Bool (is_important ctx (v, LV.of_ciloffs os))
    | Queries.SetImportant e -> 
        let ls = VarDep.Spec.eval_rval_shallow e in
        LS.iter (fun (v,os) -> add_var ctx (v, LV.to_ciloffs os)) ls; 
        `Bot
    | _ -> Queries.Result.top ()
  
end

let _ = 
  MCP.register_analysis (module Spec : Spec)
