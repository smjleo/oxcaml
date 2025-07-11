open Stdlib

(* !!!

Should be kept in sync with basic256.ml.
CR-someday mslater: with layout polymorphism, the tests could be functorized.

!!! *)

[@@@ocaml.warning "-unused-type-declaration"]

external box_float : float# -> float = "%box_float"

external box_int64x4 : int64x4# -> int64x4 = "%box_vec256"
external unbox_int64x4 : int64x4 -> int64x4# = "%unbox_vec256"

external int64x4_of_int64s : int64 -> int64 -> int64 -> int64 -> int64x4 = "" "vec256_of_int64s" [@@noalloc] [@@unboxed]
external int64x4_first_int64 : int64x4 -> int64 = "" "vec256_first_int64" [@@noalloc] [@@unboxed]
external int64x4_second_int64 : int64x4 -> int64 = "" "vec256_second_int64" [@@noalloc] [@@unboxed]
external int64x4_third_int64 : int64x4 -> int64 = "" "vec256_third_int64" [@@noalloc] [@@unboxed]
external int64x4_fourth_int64 : int64x4 -> int64 = "" "vec256_fourth_int64" [@@noalloc] [@@unboxed]

let eq l r = if l <> r then Printf.printf "%Ld <> %Ld\n" l r

let[@inline never] check v a b c d =
  let v1, v2, v3, v4 = int64x4_first_int64 v, int64x4_second_int64 v, int64x4_third_int64 v, int64x4_fourth_int64 v in
  eq v1 a;
  eq v2 b;
  eq v3 c;
  eq v4 d
;;

(* Unbox/Box *)
let () =
  let[@inline never] opaque_identity v = v in
  let v = unbox_int64x4 (int64x4_of_int64s 1L 2L 3L 4L) in
  let v = opaque_identity v in
  let v = box_int64x4 v in
  check v 1L 2L 3L 4L
;;

(* Unboxed *)

type nonrec int8x32 = int8x32#
type nonrec int16x16 = int16x16#
type nonrec int32x8 = int32x8#
type nonrec int64x4 = int64x4#
type nonrec float32x8 = float32x8#
type nonrec float64x4 = float64x4#

external int64x4_of_int64s : int64 -> int64 -> int64 -> int64 -> int64x4 = "" "vec256_of_int64s" [@@noalloc] [@@unboxed]
external int64x4_first_int64 : int64x4 -> int64 = "" "vec256_first_int64" [@@noalloc] [@@unboxed]
external int64x4_second_int64 : int64x4 -> int64 = "" "vec256_second_int64" [@@noalloc] [@@unboxed]
external int64x4_third_int64 : int64x4 -> int64 = "" "vec256_third_int64" [@@noalloc] [@@unboxed]
external int64x4_fourth_int64 : int64x4 -> int64 = "" "vec256_fourth_int64" [@@noalloc] [@@unboxed]

let eq l r = if l <> r then Printf.printf "%Ld <> %Ld\n" l r

let[@inline never] check v a b c d =
  let v1, v2, v3, v4 = int64x4_first_int64 v, int64x4_second_int64 v, int64x4_third_int64 v, int64x4_fourth_int64 v in
  eq v1 a;
  eq v2 b;
  eq v3 c;
  eq v4 d
;;

(* Box/Unbox *)
let () =
  let v = box_int64x4 (int64x4_of_int64s 1L 2L 3L 4L) in
  let v = Sys.opaque_identity v in
  let v = unbox_int64x4 v in
  check v 1L 2L 3L 4L
;;

let[@inline never] combine v0 v1 =
  let a0, b0, c0, d0 = int64x4_first_int64 v0, int64x4_second_int64 v0, int64x4_third_int64 v0, int64x4_fourth_int64 v0 in
  let a1, b1, c1, d1 = int64x4_first_int64 v1, int64x4_second_int64 v1, int64x4_third_int64 v1, int64x4_fourth_int64 v1 in
  int64x4_of_int64s (Int64.add a0 a1) (Int64.add b0 b1) (Int64.add c0 c1) (Int64.add d0 d1)
;;

let[@inline never] combine_with_floats v0 f0 v1 f1 =
  let a0, b0, c0, d0 = int64x4_first_int64 v0, int64x4_second_int64 v0, int64x4_third_int64 v0, int64x4_fourth_int64 v0 in
  let a1, b1, c1, d1 = int64x4_first_int64 v1, int64x4_second_int64 v1, int64x4_third_int64 v1, int64x4_fourth_int64 v1 in
  let a, b, c, d = Int64.add a0 a1, Int64.add b0 b1, Int64.add c0 c1, Int64.add d0 d1 in
  let a = Int64.add (Int64.of_float f0) a in
  let b = Int64.add (Int64.of_float f1) b in
  int64x4_of_int64s a b c d
;;

(* Identity *)
let () =
  let v = int64x4_of_int64s 1L 2L 3L 4L in
  let v = Sys.opaque_identity v in
  check v 1L 2L 3L 4L
;;

(* Identity fn *)
let () =
  let v = int64x4_of_int64s 1L 2L 3L 4L in
  let[@inline never] id v = v in
  let v = id v in
  check v 1L 2L 3L 4L
;;

(* Pass to function *)
let () =
  let v0 = int64x4_of_int64s 1L 2L 3L 4L in
  let v1 = int64x4_of_int64s 5L 6L 7L 8L in
  let v = combine v0 v1 in
  check v 6L 8L 10L 12L
;;

(* Pass to function (inlined) *)
let () =
  let v0 = int64x4_of_int64s 1L 2L 3L 4L in
  let v1 = int64x4_of_int64s 5L 6L 7L 8L in
  let v = (combine[@inlined hint]) v0 v1 in
  check v 6L 8L 10L 12L
;;

(* Pass to function with floats *)
let () =
  let v0 = int64x4_of_int64s 1L 2L 3L 4L in
  let v1 = int64x4_of_int64s 5L 6L 7L 8L in
  let f0 = Sys.opaque_identity 9. in
  let v = combine_with_floats v0 f0 v1 10. in
  check v 15L 18L 10L 12L
;;

(* Pass to function with floats (inlined) *)
let () =
  let v0 = int64x4_of_int64s 1L 2L 3L 4L in
  let v1 = int64x4_of_int64s 5L 6L 7L 8L in
  let v = (combine_with_floats[@inlined hint]) v0 9. v1 10. in
  check v 15L 18L 10L 12L
;;

(* Capture in closure *)
let () =
  let v0 = int64x4_of_int64s 1L 2L 3L 4L in
  let v1 = int64x4_of_int64s 5L 6L 7L 8L in
  let f = combine v0 in
  let f = Sys.opaque_identity f in
  let v = f v1 in
  check v 6L 8L 10L 12L
;;

(* Capture vectors and floats in a closure *)
let () =
  let[@inline never] f v0 v1 f0 v2 f1 v3 =
    combine (combine_with_floats v0 f0 v1 f1) (combine v2 v3)
  in
  let v0 = int64x4_of_int64s 1L 2L 3L 4L in
  let v1 = int64x4_of_int64s 5L 6L 7L 8L in
  let v2 = int64x4_of_int64s 9L 10L 11L 12L in
  let v3 = int64x4_of_int64s 13L 14L 15L 16L in
  let f = f v0 v1 17. v2  in
  let f = Sys.opaque_identity f in
  let v = f 18. v3 in
  check v 45L 50L 36L 40L
;;

(* Capture vectors and floats in a closure (inlined) *)
let () =
  let[@inline always] f v0 v1 f0 v2 f1 v3 =
    (combine[@inlined hint])
      ((combine_with_floats[@inlined hint]) v0 f0 v1 f1)
      ((combine[@inlined hint]) v2 v3)
  in
  let v0 = int64x4_of_int64s 1L 2L 3L 4L in
  let v1 = int64x4_of_int64s 5L 6L 7L 8L in
  let v2 = int64x4_of_int64s 9L 10L 11L 12L in
  let v3 = int64x4_of_int64s 13L 14L 15L 16L in
  let f = f v0 v1 17. v2 in
  let v = f 18. v3 in
  check v 45L 50L 36L 40L
;;

(* Store in record *)
type record = { prefix : string
              ; a : int64x4
              ; mutable b : int64x4
              ; c : float# }

let () =
  let record = { prefix = "prefix"; a = int64x4_of_int64s 1L 2L 3L 4L; b = int64x4_of_int64s 5L 6L 7L 8L; c = #9. } in
  check record.a 1L 2L 3L 4L;
  check record.b 5L 6L 7L 8L;
  assert (record.prefix = "prefix");
  let record = Sys.opaque_identity record in
  record.b <- int64x4_of_int64s 10L 11L 12L 13L;
  check record.a 1L 2L 3L 4L;
  check record.b 10L 11L 12L 13L;
  assert (record.prefix = "prefix");
  let v = combine_with_floats record.a (box_float record.c) record.b 14. in
  check v 20L 27L 15L 17L
;;

type record2 = { imm0 : int; str1 : string; a : int64x4 }

let copy_via_weak x =
  let weak = Weak.create 1 in
  Weak.set weak 0 (Some x);
  Weak.get_copy weak 0 |> Option.get
;;

let copy_via_tag x =
  let obj = Obj.repr x in
  Obj.with_tag (Obj.tag obj) obj |> Obj.obj;;
;;

let () =
  let record = { imm0 = 0; str1 = ""; a = int64x4_of_int64s 1L 2L 3L 4L } in
  check record.a 1L 2L 3L 4L;
  assert (record.str1 = "" && record.imm0 = 0);
  let record = { record with imm0 = 5 } in
  check record.a 1L 2L 3L 4L;
  assert (record.str1 = "" && record.imm0 = 5);
  let record = copy_via_weak record in
  check record.a 1L 2L 3L 4L;
  assert (record.str1 = "" && record.imm0 = 5);
  let record = copy_via_tag record in
  check record.a 1L 2L 3L 4L;
  assert (record.str1 = "" && record.imm0 = 5);
;;

(* Store in variant *)
type variant = A of int64x4 | B of int64x4 | C of float

let () =
  let variant = A (int64x4_of_int64s 1L 2L 3L 4L) in
  let variant = Sys.opaque_identity variant in
  match variant with
  | A v -> check v 1L 2L 3L 4L
  | _ -> print_endline "fail";
  let variant = ref (C 5.) in
  let variant = Sys.opaque_identity variant in
  variant := B (int64x4_of_int64s 6L 7L 8L 9L);
  match !variant with
  | B v -> check v 6L 7L 8L 9L
  | _ -> print_endline "fail"
;;

(* Pass lots of vectors to an external *)
external lots_of_vectors :
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 = "" "lots_of_vectors256"
  [@@noalloc] [@@unboxed]

let () =
  let v0 = int64x4_of_int64s 1L 2L 3L 4L in
  let v1 = int64x4_of_int64s 5L 6L 7L 8L in
  let v2 = int64x4_of_int64s 9L 10L 11L 12L in
  let v3 = int64x4_of_int64s 13L 14L 15L 16L in
  let v4 = int64x4_of_int64s 17L 18L 19L 20L in
  let v5 = int64x4_of_int64s 21L 22L 23L 24L in
  let v6 = int64x4_of_int64s 25L 26L 27L 28L in
  let v7 = int64x4_of_int64s 29L 30L 31L 32L in
  let v8 = int64x4_of_int64s 33L 34L 35L 36L in
  let v9 = int64x4_of_int64s 37L 38L 39L 40L in
  let v10 = int64x4_of_int64s 41L 42L 43L 44L in
  let v11 = int64x4_of_int64s 45L 46L 47L 48L in
  let v12 = int64x4_of_int64s 49L 50L 51L 52L in
  let v13 = int64x4_of_int64s 53L 54L 55L 56L in
  let v14 = int64x4_of_int64s 57L 58L 59L 60L in
  let v15 = int64x4_of_int64s 61L 62L 63L 64L in
  let v =
    lots_of_vectors v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15
  in
  check v 496L 512L 528L 544L

(* Pass mixed floats/vectors to an external *)
external vectors_and_floats :
  int64x4 ->
  float ->
  int64x4 ->
  float ->
  int64x4 ->
  float ->
  int64x4 ->
  float ->
  float ->
  int64x4 ->
  int64x4 ->
  float ->
  float ->
  int64x4 ->
  int64x4 ->
  float ->
  float ->
  float ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  float ->
  float ->
  float ->
  int64x4 = "" "vectors_and_floats256"
  [@@noalloc] [@@unboxed]

let () =
  let v0 = int64x4_of_int64s 1L 2L 3L 4L in
  let v1 = int64x4_of_int64s 5L 6L 7L 8L in
  let v2 = int64x4_of_int64s 9L 10L 11L 12L in
  let v3 = int64x4_of_int64s 13L 14L 15L 16L in
  let v4 = int64x4_of_int64s 17L 18L 19L 20L in
  let v5 = int64x4_of_int64s 21L 22L 23L 24L in
  let v6 = int64x4_of_int64s 25L 26L 27L 28L in
  let v7 = int64x4_of_int64s 29L 30L 31L 32L in
  let v8 = int64x4_of_int64s 33L 34L 35L 36L in
  let v9 = int64x4_of_int64s 37L 38L 39L 40L in
  let v10 = int64x4_of_int64s 41L 42L 43L 44L in
  let v =
    vectors_and_floats v0 23. v1 24. v2 25. v3 26. 27. v4 v5 28. 29. v6 v7 30.
      31. 32. v8 v9 v10 33. 34. 35.
  in
  check v 377L 473L 253L 264L

(* Pass mixed ints/floats/vectors to an external *)
external vectors_and_floats_and_ints :
  int64x4 ->
  float ->
  int64x4 ->
  int64 ->
  int64x4 ->
  float ->
  int64x4 ->
  int64 ->
  int64 ->
  int64x4 ->
  int64x4 ->
  float ->
  float ->
  int64x4 ->
  int64x4 ->
  int64 ->
  int64 ->
  float ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64 ->
  int64 ->
  float ->
  int64x4 = "" "vectors_and_floats_and_ints256"
  [@@noalloc] [@@unboxed]

let () =
  let v0 = int64x4_of_int64s 1L 2L 3L 4L in
  let v1 = int64x4_of_int64s 5L 6L 7L 8L in
  let v2 = int64x4_of_int64s 9L 10L 11L 12L in
  let v3 = int64x4_of_int64s 13L 14L 15L 16L in
  let v4 = int64x4_of_int64s 17L 18L 19L 20L in
  let v5 = int64x4_of_int64s 21L 22L 23L 24L in
  let v6 = int64x4_of_int64s 25L 26L 27L 28L in
  let v7 = int64x4_of_int64s 29L 30L 31L 32L in
  let v8 = int64x4_of_int64s 33L 34L 35L 36L in
  let v9 = int64x4_of_int64s 37L 38L 39L 40L in
  let v10 = int64x4_of_int64s 41L 42L 43L 44L in
  let v =
    vectors_and_floats_and_ints v0 23. v1 24L v2 25. v3 26L 27L v4 v5 28. 29. v6
      v7 30L 31L 32. v8 v9 v10 33L 34L 35.
  in
  check v 377L 473L 253L 264L

(* Pass vector reg and then stack floats *)
external vector_and_then_stack_floats :
  int64x4 ->
  float ->
  float ->
  float ->
  float ->
  float ->
  float ->
  float ->
  float ->
  float = "" "vector_and_then_stack_floats"
  [@@noalloc] [@@unboxed]

let () =
  let v0 = int64x4_of_int64s 1L 2L 3L 4L in
  let v = vector_and_then_stack_floats v0 1. 2. 3. 4. 5. 6. 7. 8. in
  assert (v = 36.)
