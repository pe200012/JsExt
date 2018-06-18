open Yojson

module Hide : sig
  type 'a typ =
    | Assoc : (string * Safe.json) list typ
    | Bool : bool typ
    | Float : float typ
    | Int : int typ
    | List : Safe.json list typ
    | Null : unit typ
    | String : string typ
    | Tuple: Safe.json list typ
    | Variant : (string * (Safe.json option)) typ
end
open Hide

val extract : Safe.json -> 'a typ -> 'a option

val pack : 'a -> 'a typ -> Safe.json

val find_assoc : Safe.json -> string -> Safe.json

val find_index : Safe.json -> int -> Safe.json

val add_assoc : Safe.json -> string * Safe.json -> Safe.json

module Operator : sig
    val (>=>) : Safe.json -> 'a typ -> 'a 
    
    val (<=<) : 'a -> 'a typ -> Safe.json

    val (>|>) : Safe.json -> string -> Safe.json

    val (>->) : Safe.json -> int -> Safe.json

    val (>+>) : Safe.json -> string * Safe.json -> Safe.json
end