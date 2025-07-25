(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Auxiliary type for reporting syntax errors *)

type invalid_package_type =
  | Parameterized_types
  | Constrained_types
  | Private_types
  | Not_with_type
  | Neither_identifier_nor_with_type
  | Misplaced_attribute

type error =
    Unclosed of Location.t * string * Location.t * string
  | Expecting of Location.t * string
  | Not_expecting of Location.t * string
  | Applicative_path of Location.t
  | Variable_in_scope of Location.t * string
  | Other of Location.t
  | Ill_formed_ast of Location.t * string
  | Invalid_package_type of Location.t * invalid_package_type
  | Removed_string_set of Location.t
  | Missing_unboxed_literal_suffix of Location.t
  | Malformed_instance_identifier of Location.t
  | Let_mutable_not_allowed_at_structure_level of Location.t
  | Let_mutable_not_allowed_in_class_definition of Location.t
  | Let_mutable_not_allowed_with_function_bindings of Location.t

exception Error of error
exception Escape_error

let location_of_error = function
  | Unclosed(l,_,_,_)
  | Applicative_path l
  | Variable_in_scope(l,_)
  | Other l
  | Not_expecting (l, _)
  | Ill_formed_ast (l, _)
  | Invalid_package_type (l, _)
  | Expecting (l, _)
  | Removed_string_set l -> l
  | Missing_unboxed_literal_suffix l -> l
  | Malformed_instance_identifier l -> l
  | Let_mutable_not_allowed_at_structure_level l -> l
  | Let_mutable_not_allowed_in_class_definition l -> l
  | Let_mutable_not_allowed_with_function_bindings l -> l


let ill_formed_ast loc s =
  raise (Error (Ill_formed_ast (loc, s)))
