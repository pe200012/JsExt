open Yojson

type 'a typ =
  | Assoc : (string * Safe.json) list typ
  | Bool : bool typ
  | Float : bool typ
  | Int : int typ
  | List : Safe.json list typ
  | Null : 'a typ
  | String : string typ
  | TupleList : Safe.json list typ
  | Variant : (string * (Safe.json option)) typ


let extract : type a. Safe.json -> a typ -> a option =
  fun js t ->
    match js,t with
    | `Assoc x, Assoc -> Some x
