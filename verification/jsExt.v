Require Import List.
Require Import String.
Require Import QArith.
Require Import ZArith.
Require Import Classical.
Open Scope string_scope.
Open Scope list_scope.
Import ListNotations.
Set Implicit Arguments.

Definition compose (A B C : Type) (f:B -> C) (g:A -> B) (x:A) := f (g x).
Definition rcompose (A B C : Type) (g:A -> B) (f:B -> C) (x:A) := f (g x).
Definition eq_str (A B:string) :bool :=
  match string_dec A B with
  | left eq_refl => true
  | right _ => false
  end.

Notation "x <.> y" := (compose x y)
                    (at level 50, left associativity).
Notation "x >>. y" := (rcompose x y)
                    (at level 50, left associativity).

Inductive either (A:Type) (B:Type) : Type :=
  | Left : A -> either A B
  | Right : B -> either A B.

Inductive exn (A:Type) : Type := Exn : string * A -> exn A.

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

Definition find_assoc (j:json) (s:string) : either (exn string) json := 
  match j with
  | Assoc x => 
    (fix f' (l:list (string * json)) : either (exn string) json :=
      match l with
      | [] => Left _ (Exn ("Not_found",""))
      | (k,y) :: l' => if eq_str k s then Right _ y else f' l'
      end) x
  | _ => Left _ (Exn ("Invalid_argument","Non-corresponding Type"))
  end.

Definition find_index (j:json) (i:nat) : either (exn string) json :=
  match j with
  | List x =>
    match nth_error x i with
    | Some y => Right _ y
    | None => Left _ (Exn ("Not_found",""))
    end
  | _ => Left _ (Exn ("Invalid_argument","Non-corresponding Type"))
  end.

Definition add_assoc (j:json) (k:string*json) : either (exn string) json :=
  match j with
  | Assoc x =>
    if existsb (fun (p:string*json) => eq_str (fst p) (fst k)) x then Left _ (Exn ("Invalid_argument","Duplicated argument"))
    else Right _ (Assoc (k :: x))
  | _ => Left _ (Exn ("Invalid_argument","Non-corresponding Type"))
  end.

Lemma json_eq : forall (T:Type) (a b:json) (t:typ T), a = b -> extract a t = extract b t.
Proof.
  intros.
  rewrite -> H.
  reflexivity.
Qed.

Lemma empty_assoc_left : forall (k:string), find_assoc (Assoc []) k = Left _ (Exn ("Not_found","")).
Proof.
  auto.
Qed.


