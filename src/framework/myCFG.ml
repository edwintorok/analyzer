(* 
 * Copyright (c) 2005-2007,
 *     * University of Tartu
 *     * Vesal Vojdani <vesal.vojdani@gmail.com>
 *     * Kalmer Apinis <kalmera@ut.ee>
 *     * Jaak Randmets <jaak.ra@gmail.com>
 *     * Toomas Römer <toomasr@gmail.com>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 * 
 *     * Redistributions of source code must retain the above copyright notice,
 *       this list of conditions and the following disclaimer.
 * 
 *     * Redistributions in binary form must reproduce the above copyright notice,
 *       this list of conditions and the following disclaimer in the documentation
 *       and/or other materials provided with the distribution.
 * 
 *     * Neither the name of the University of Tartu nor the names of its
 *       contributors may be used to endorse or promote products derived from
 *       this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

module GU = Goblintutil
module CF = Cilfacade
open Cil

type node = 
  | Statement of stmt 
  | Function of varinfo 

module Node : Hashtbl.HashedType with type t = node =
struct
  type t = node
  let equal x y = 
    match x,y with
      | Statement s1, Statement s2 -> s1.sid = s2.sid
      | Function f1, Function f2 -> f1.vid = f2.vid
      | _ -> false
  let hash x = 
    match x with 
      | Statement s -> Hashtbl.hash (s.sid, 0)
      | Function f -> Hashtbl.hash (f.vid, 1)
end

type asm_out = (string option * string * lval) list
type asm_in  = (string option * string * exp ) list

type edge = 
  | Assign of lval * exp
  | Proc of lval option * exp * exp list
  | Entry of fundec
  | Ret of exp option * fundec
  | Test of exp * bool
  | ASM of string list * asm_out * asm_in
  | Skip

type cfg = node -> (edge * node) list

module H = Hashtbl.Make(Node)

(* Dumps a statement to the standard output *)
let pstmt stmt = dumpStmt defaultCilPrinter stdout 0 stmt; print_newline ()

let stmt_index_hack = Hashtbl.create 113

let do_the_params_and_bajs (fd: fundec) =
  (* This function used to create extra variables, but now it just sets the
   * vdecl to -3, lovely... *)
  let create_extra_var (p: varinfo): unit = 
    match p.vtype with
      | TPtr (t,_) -> p.vdecl <- {p.vdecl with line = -3 }
      | _ -> p.vdecl <- {p.vdecl with line = -3 }
  in
    List.iter create_extra_var fd.sformals

let createCFG (file: file) =
  let cfg = H.create 113 in
  (* Utility function to add stmt edges to the cfg *)
  let mkEdge fromNode edge toNode = H.add cfg (Statement toNode) (edge, Statement fromNode) in
  (* Function for finding the next real successor of a statement. CIL tends to
   * put a lot of junk between stuff: *)
  let realnode ie stmt = 
    let rec realnode ie visited stmt = 
      if List.mem stmt.sid visited then stmt  else
      let sid = stmt.sid in
      try 
        match stmt.skind with
          | Block _ -> realnode ie (sid::visited) (List.hd stmt.succs)
          | Goto _ -> realnode ie (sid::visited) (List.hd stmt.succs)
          | Instr [] -> begin
              let next = List.hd stmt.succs in
                if next.sid == stmt.sid
                  then stmt
                  else realnode ie (sid::visited) next 
            end
          | Loop _ -> realnode ie (sid::visited) (List.hd stmt.succs)
          | If (exp,_,_,_) -> if isZero exp then realnode ie (sid::visited) (List.hd stmt.succs) else stmt
          | _ -> stmt
      with
        | Failure "hd" -> if ie then stmt else raise (Failure "hd")
    in realnode ie [] stmt
  in
  (* We iterate over all globals looking for functions: *)
  iterGlobals file (fun glob -> 
    match glob with
      | GFun (fd,loc) ->
          (* Walk through the parameters and pre-process them a bit... *)
          do_the_params_and_bajs fd;
          (* Find the first statement in the function *)
          let entrynode = realnode true (CF.getFirstStmt fd) in
          (* I just add the entry edge to that node (pointing to itself).
           * Strange, but it works fine. *)
          let _ = mkEdge entrynode (Entry fd) entrynode in
          (* So for each statement in the function body, we do the following: *)
          let handle stmt = 
            (* Please ignore the next line. It creates an index of statements
             * so the Eclipse plug-in can know what function a given result
             * belongs to. *)
            Hashtbl.add stmt_index_hack stmt fd;
            match stmt.skind with 
              (* Normal instructions are easy. They should be a list of a single
               * instruction, either Set, Call or ASM: *)
              | Instr x ->
                  (* We need to add an edge to each of the successors of the
                   * current statement *)
                  let foreach succ = 
                    match x with 
                      | [Set (lval,exp,loc)] -> mkEdge stmt (Assign (lval, exp)) succ
                      | [Call (lval,func,args,loc)] -> mkEdge stmt (Proc (lval,func,args)) succ
                      | [Asm (attr,tmpl,out,inp,regs,loc)] -> mkEdge stmt (ASM (tmpl,out,inp)) succ
                      | [] -> ()
                      | _ -> print_endline "block escaped:"; pstmt stmt; 
                  in begin
                    (* Sometimes a statement might not have a successor, but we
                     * still need to do something about him, this can happen if
                     * the last statement of a function is a call to exit. *)
                    match stmt.succs with
                      | [] -> begin
                          match x with
                            | [Call (lval,Lval (Var v,NoOffset),args,loc)] when v.vname = "exit" ->
                                H.add cfg (Function fd.svar) (Ret (None, fd), Statement stmt)
                            | _ -> () (* H.add cfg (Function fd.svar) (Ret (None, fd), Statement stmt) *)
                        end
                      | _ -> List.iter foreach (List.map (realnode true) stmt.succs)
                  end
              (* If expressions are a bit more interesting, but CIL has done
               * it's job well and we just pick out the right successors *)
              | If (exp, true_block, false_block, loc) -> begin
                  if isZero exp then ()
                  else
                    let false_stmt = realnode true (List.nth stmt.succs 0) in
                    let true_stmt = try  
                      realnode true (List.nth stmt.succs 1)
                    with Failure _ -> realnode true (List.hd stmt.succs) in
                      mkEdge stmt (Test (exp, true )) true_stmt; 
                      mkEdge stmt (Test (exp, false)) false_stmt
                end
              (* Loops can generally be ignored because CIL creats gotos for us,
               * except constant conditions are eliminated, so non-terminating
               * loops are not connected to the rest of the code. This is a
               * problem for side-effecting demand driven solvers. I add one
               * extra edge that is always false to the exit of the loop. *)
              | Loop (bl,loc,Some cont, Some brk) -> begin 
                  try
                    mkEdge (realnode true stmt) (Test (one, false)) (realnode false brk); 
                  with
                    (* The [realnode brk] fails when the break label is at the end
                     * of the function. In that case, we need to connect it to
                     * the [Call] node. *)
                    | Failure "hd" -> 
                        let newst = mkStmt (Return (None, locUnknown)) in
                          newst.sid <- new_sid ();
                          mkEdge (realnode true stmt) (Test (one, false)) newst;
                          H.add cfg (Function fd.svar) (Ret (None,fd), Statement newst);
                end
              (* The return edges are connected to the function *)
              | Return (exp,loc) -> H.add cfg (Function fd.svar) (Ret (exp,fd), Statement stmt)
              | _ -> ()
          in
            List.iter handle fd.sallstmts
      | _ -> ()
  );
  cfg


let print cfg  =
  let out = open_out "cfg.dot" in
  let printf fmt = Printf.fprintf out fmt in
  let pc arg = Pretty.sprint 80 arg in
  let _ = printf "digraph cfg {\n" in
  let p_node node = 
    match node.skind with
      | Instr [Set(lval,exp,loc)] -> pc (Pretty.dprintf "%a = %a" d_lval lval d_exp exp)
      | Instr [Call _] -> pc (Pretty.dprintf "%a" d_stmt node)
      | Return (Some exp,loc) -> pc (Pretty.dprintf "return %a" d_exp exp)
      | Return (None,loc) -> "return"
      | If (exp,_,_,_) -> pc (Pretty.dprintf "%a" d_exp exp)
      | _ -> string_of_int node.sid
  in
  let p_edge edge = 
    match edge with
      | Test (exp,true)  when isZero exp -> "extra"
      | Test (exp,false) when isConstant  exp -> "extra"
      | Test (exp,tv) -> string_of_bool tv
      | Skip -> "skip"
      | _  -> ""
  in
  let printNode (toNode: node) ((edge:edge), (fromNode: node)) = 
    match toNode, edge, fromNode with 
      | Statement toNode, Entry {svar=fv}, _ -> begin
          printf "\tfun%d -> %d [label = \"%s\"] ;\n" fv.vid toNode.sid (p_edge edge);
          printf "\tfun%d [label=\"%s()\", shape=box]\n" fv.vid fv.vname
        end
      | _, Ret _, Statement stmt ->
          printf "\t%d [label=\"%s\", shape=box]\n" stmt.sid (p_node stmt)
      | Statement toNode, _, Statement fromNode -> begin
          printf "\t%d -> %d [label = \"%s\"] ;\n" fromNode.sid toNode.sid (p_edge edge);
          match edge with
            | Test (_,true) -> printf "\t%d [label=\"%s\", shape=diamond]\n" fromNode.sid (p_node fromNode)
            | Test (_,false) -> ()
            | _ -> printf "\t%d [label=\"%s\"]\n" fromNode.sid (p_node fromNode)
        end
      | _ -> ()
  in
    H.iter printNode cfg;
    printf "}\n";
    flush out

let initfun = emptyFunction "goblin_initfun"
let getGlobalInits (file: file) : (edge * location) list  =
  let vars = ref [] in
  let inits = ref [] in
  let rec doInit lval loc init = 
    let rec initoffs offs init typ lval = 
      doInit (addOffsetLval offs lval) loc init;
      lval 
    in
    match init with
      | SingleInit exp -> begin
          inits := (Assign (lval, exp), loc) :: !inits
        end
      | CompoundInit (typ, lst) ->
          ignore (foldLeftCompound ~implicit:true ~doinit:initoffs ~ct:typ ~initl:lst ~acc:lval)
  in
  let f glob = 
    match glob with 
      | GVar ({vtype=vtype} as v, init, loc) -> begin
          vars := v :: !vars;
          let init = match init.init with
            | None -> makeZeroInit vtype
            | Some x -> x
          in
            doInit (var v) loc init
        end
      | _ -> ()
  in 
  iterGlobals file f;
  initfun.slocals <- List.rev !vars;
  (Entry initfun, {line = 0; file="nofile"; byte= 0} ) :: List.rev !inits

  
let getCFG (file: file): cfg = 
  let cfg = createCFG file in
    if !GU.cfg_print then print cfg;
    H.find_all cfg

let getLoc (node: node) = 
  match node with
    | Statement stmt -> get_stmtLoc stmt.skind
    | Function fv -> fv.vdecl

let get_containing_function (stmt: stmt): fundec = Hashtbl.find stmt_index_hack stmt

let getFun (node: node) = 
  match node with
    | Statement stmt -> get_containing_function stmt
    | Function fv -> CF.getdec fv
