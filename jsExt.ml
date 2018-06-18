open Yojson

module Hide = struct
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
    let (<.>) f g x = f (g x)
    let (>>.) g f x = f (g x)
end
open Hide

let extract : type a. Safe.json -> a typ -> a option =
  fun js t ->
    match js,t with
    | `Assoc x, Assoc -> Some x
    | `Bool x, Bool -> Some x
    | `Float x, Float -> Some x
    | `Int x, Int -> Some x
    | `List x, List -> Some x
    | `Null, Null -> Some ()
    | `String x, String -> Some x
    | `Tuple x, Tuple -> Some x
    | `Variant x, Variant -> Some x
    | _,_ -> None

let pack : type a. a -> a typ -> Safe.json =
  fun v t ->
    match t with
    | Assoc -> `Assoc v 
    | Bool -> `Bool v 
    | Float -> `Float v 
    | Int -> `Int v 
    | List -> `List v 
    | Null -> `Null
    | String -> `String v 
    | Tuple -> `Tuple v 
    | Variant -> `Variant v 

let find_assoc : Safe.json -> string -> Safe.json =
  fun js k ->
    match js with
    | `Assoc x -> List.assoc k x
    | _ -> raise (Invalid_argument "Unexpected JSON type")

let find_index : Safe.json -> int -> Safe.json =
  fun js i ->
    match js with 
    | `List x -> List.nth x i
    | _ -> raise (Invalid_argument "Unexpected JSON type")

let add_assoc : Safe.json -> string * Safe.json -> Safe.json =
  fun js (k,js') ->
    match js with
    | `Assoc x ->
      if List.exists (fst >>. ((=)k)) x
      then raise (Invalid_argument "Duplicated key")
      else `Assoc ((k,js') :: x)
    | _ -> raise (Invalid_argument "Unexpected JSON type")

let lift e x =
  match x with
  | Some x -> x
  | None -> raise e

module Operator = struct
  let (>=>) x t = lift (Invalid_argument "") (extract x t)
  let (>|>) = find_assoc
  let (>->) = find_index
  let (>+>) = add_assoc
  let (<=<) = pack
end
