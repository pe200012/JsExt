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

  type ('a,'b) either = 
    | Left : 'a -> ('a,'b) either
    | Right : 'b -> ('a,'b) either

end
open Hide

val extract : Safe.json -> 'a typ -> (exn,'a) either

val pack : 'a -> 'a typ -> Safe.json

val find_assoc : Safe.json -> string -> (exn,Safe.json) either

val find_index : Safe.json -> int -> (exn,Safe.json) either

val add_assoc : Safe.json -> string * Safe.json -> (exn,Safe.json) either

module Operator : sig
    val (>=>) : Safe.json -> 'a typ -> 'a 
    
    val (<=<) : 'a -> 'a typ -> Safe.json

    val (>|>) : Safe.json -> string -> Safe.json

    val (>->) : Safe.json -> int -> Safe.json

    val (>+>) : Safe.json -> string * Safe.json -> Safe.json
end