open Cil

val single: name:string -> (unit -> varinfo)

module type VarinfoMap =
sig
  type t
  type marshal
  val to_varinfo : t -> varinfo
  val from_varinfo: varinfo -> t option
  val is_contained_varinfo: varinfo -> bool
  val describe_varinfo: varinfo -> t -> string
  val unmarshal: marshal -> unit
  val marshal: unit -> marshal
end

module VarinfoMapCollection:
sig
  val mappings : (module VarinfoMap) list ref
  val is_rich_varinfo : varinfo -> bool
  val describe_varinfo : varinfo -> string
  val register_mapping : (module VarinfoMap) -> unit
end

module type S =
sig
  type t
  type marshal
  val map: ?size:int -> ?describe_varinfo:(varinfo -> t -> string) -> name:(t -> string) -> unit -> (module VarinfoMap with type t = t and type marshal = marshal)
end


module Make:
  functor (X: Hashtbl.HashedType) ->
    S with type t = X.t
