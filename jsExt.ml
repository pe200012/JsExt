(**
 *    Copyright 2018 pe200012
 * 
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 * 
 *        http://www.apache.org/licenses/LICENSE-2.0
 * 
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 *)

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

  type ('a,'b) either = 
    | Left : 'a -> ('a,'b) either
    | Right : 'b -> ('a,'b) either

    let (<.>) f g x = f (g x)
    let (>>.) g f x = f (g x)

end
open Hide

module Utils = struct
    let return x = Right x

    let (>>=) e f =
      match e with
      | Left e -> Left e
      | Right x -> f x

    let revert = function
    | Left x -> Right x
    | Right x -> Left x

    let concat : (('a,'b) either,('a,'b) either) either -> ('a,'b) either = function
    | Left x -> x
    | Right x -> x
end
open Utils

let extract : type a. Safe.json -> a typ -> (exn,a) either =
  fun js t ->
    match js,t with
    | `Assoc x, Assoc -> Right x
    | `Bool x, Bool -> Right x
    | `Float x, Float -> Right x
    | `Int x, Int -> Right x
    | `List x, List -> Right x
    | `Null, Null -> Right ()
    | `String x, String -> Right x
    | `Tuple x, Tuple -> Right x
    | `Variant x, Variant -> Right x
    | _,_ -> Left (Invalid_argument "Non-corresponding Type")

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

let find_assoc : Safe.json -> string -> (exn,Safe.json) either =
  fun js k ->
    match js with
    | `Assoc x -> Right (List.assoc k x)
    | _ -> Left (Invalid_argument "Unexpected JSON type")

let find_index : Safe.json -> int -> (exn,Safe.json) either =
  fun js i ->
    match js with 
    | `List x -> Right (List.nth x i)
    | _ -> Left (Invalid_argument "Unexpected JSON type")

let add_assoc : Safe.json -> string * Safe.json -> (exn,Safe.json) either =
  fun js (k,js') ->
    match js with
    | `Assoc x ->
      if List.exists (fst >>. ((=)k)) x
      then Left (Invalid_argument "Duplicated key")
      else Right (`Assoc ((k,js') :: x))
    | _ -> Left (Invalid_argument "Unexpected JSON type")

module Operator = struct
  let (>=>) x t = x >>= fun x -> extract x t
  let (>|>) x t = x >>= fun x -> find_assoc x t
  let (>->) x t = x >>= fun x -> find_index x t
  let (>+>) x t = x >>= fun x -> add_assoc x t
  let (<=<) x t = x >>= fun x -> Right (pack x t)
end
include Operator
