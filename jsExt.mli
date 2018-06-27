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

open Yojson.Safe

module Hide :
sig
  type 'a typ =
      Assoc : (string * json) list typ
    | Bool : bool typ
    | Float : float typ
    | Int : int typ
    | List : json list typ
    | Null : unit typ
    | String : string typ
    | Tuple : json list typ
    | Variant : (string * json option) typ
  type ('a, 'b) either =
      Left : 'a -> ('a, 'b) either
    | Right : 'b -> ('a, 'b) either
end
open Hide
module Utils : sig
  val return : 'a -> ('b,'a) either
  val revert : ('a,'b) either -> ('b,'a) either
  val concat : (('a,'b) either,('a,'b) either) either -> ('a,'b) either
end
val extract : json -> 'a typ -> (exn, 'a) either
val pack : 'a -> 'a typ -> json
val find_assoc : json -> string -> (exn, json) either
val find_index : json -> int -> (exn, json) either
val add_assoc : json -> string * json -> (exn, json) either
module Operator :
sig
  val ( >=> ) : (exn, json) either -> 'a typ -> (exn, 'a) either
  val ( >|> ) : (exn, json) either -> string -> (exn, json) either
  val ( >-> ) : (exn, json) either -> int -> (exn, json) either
  val ( >+> ) : (exn, json) either -> string * json -> (exn, json) either
  val ( <=< ) : ('a, 'b) either -> 'b typ -> ('a, json) either
end
val ( >=> ) : (exn, json) either -> 'a typ -> (exn, 'a) either
val ( >|> ) : (exn, json) either -> string -> (exn, json) either
val ( >-> ) : (exn, json) either -> int -> (exn, json) either
val ( >+> ) : (exn, json) either -> string * json -> (exn, json) either
val ( <=< ) : ('a, 'b) either -> 'b typ -> ('a, json) either

