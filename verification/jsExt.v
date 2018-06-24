Require Import List.
Require Import String.
Require Import QArith.
Require Import ZArith.
Open Scope string_scope.
Set Implicit Arguments.

Definition compose (A B C : Type) (f:B -> C) (g:A -> B) (x:A) := f (g x).
Definition rcompose (A B C : Type) (g:A -> B) (f:B -> C) (x:A) := f (g x).

Notation "x <.> y" := (compose x y)
                    (at level 50, left associativity).
Notation "x >>. y" := (rcompose x y)
                    (at level 50, left associativity).


Inductive json : Type :=
  | Assoc : list (prod string json) -> json
  | Bool : bool -> json
  | Float : Q -> json
  | Int : Z -> json
  | List : list json -> json
  | Null : unit -> json
  | String : string -> json
  | Tuple : list json -> json
  | Variant : prod string (option json) -> json.

Inductive typ : Type -> Type :=
  | TAssoc : typ (list (prod string json))
  | TBool : typ bool 
  | TFloat : typ Q 
  | TInt : typ Z 
  | TList : typ (list json)
  | TNull : typ unit 
  | TString : typ string 
  | TTuple : typ (list json) 
  | TVariant : typ (prod string (option json)).

Inductive either (A:Type) (B:Type) : Type :=
  | Left : A -> either A B
  | Right : B -> either A B.

Inductive exn (A:Type) : Type := Exn : string * A -> exn A.

Definition extract (A:Type) (j:json) (t:typ A) : either (exn string) A :=
  match j,t with
  | Assoc x, TAssoc => Right _ x
  | Bool x, TBool => Right _ x
  | Float x, TFloat => Right _ x
  | Int x, TInt => Right _ x
  | List x, TList => Right _ x
  | String x, TString => Right _ x
  | Variant x, TVariant => Right _ x
  | _, _ => Left _ (Exn ("Invalid_argument","Non-corresponding Type"))
  end.
