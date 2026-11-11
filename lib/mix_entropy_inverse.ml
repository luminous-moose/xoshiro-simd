module Int64 = struct
  include Int64

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( lxor ) = logxor
  let ( lsr ) = shift_right_logical
end

(* Entropy-mixer constants copied from xs256_common.c. *)
let mix_hashmix_init = 0x9e3779b97f4a7c15L
let mix_hashmix_mult = 0x00000100000001b3L
let mix_mult_l = 0xbf58476d1ce4e5b9L
let mix_mult_r = 0x94d049bb133111ebL

(* This function is its own inverse. *)
let xorshift32 y =
  let open Int64 in
  let hi = y lsr 32 in
  y lxor hi

let inv_xorshift32 = xorshift32

(* Modular multiplicative inverse mod 2^64 of an odd value, computed via
   Newton's method. We can use it to get the reciprocal of an odd number and
   then use that to reverse a multiplication. *)
let modinv64 a =
  let rec loop x n =
    if n = 0 then x else loop Int64.(x * (2L - (a * x))) (n - 1)
  in
  loop a 5

let hashmix_forward value mix_state =
  let open Int64 in
  let value = value lxor mix_state in
  let mix_state' = mix_state * mix_hashmix_mult in
  let value = value * mix_state' in
  xorshift32 value

(* Given result = mix(x, y) and y, solves for x. *)
let inv_mix_in_x result y =
  let open Int64 in
  let pre = inv_xorshift32 result in
  let rhs = pre + (mix_mult_r * y) in
  rhs * modinv64 mix_mult_l

(* Inverts hashmix(value, mix_state_before) -> result, given mix_state_before
   (the mix_state value as it was before this particular call). *)
let inv_hashmix result mix_state_before =
  let open Int64 in
  let mix_state_after = mix_state_before * mix_hashmix_mult in
  let v2 = inv_xorshift32 result in
  let v1 = v2 * modinv64 mix_state_after in
  v1 lxor mix_state_before

(* Exactly reverse mix_entropy in xs256_common.c for a 4-word seed: every state
   has exactly one such seed. Pass 1 hashes each seed word into its own slot.
   Pass 2 mixes every slot into every other slot. *)
let compute (pool_out : int64 array) : int64 array =
  let mix_state = ref mix_hashmix_init in
  (* Pass 2 has to be undone first, and in reverse order, but each step needs
     the mix_state it had going forward. mix_state is easy to compute because it
     never depends on the input. So we replay it forward through pass 1 and then
     pass 2 in the next block, recording the values each step used. *)
  let pass1_mix_states =
    Array.init 4 (fun _ ->
        let h = !mix_state in
        (mix_state := Int64.(!mix_state * mix_hashmix_mult));
        h)
  in
  (* Create pass 2's steps in forward order, consed so the list comes out
     reversed and can be iterated over in the next block. *)
  let pass2_steps_reversed = ref [] in
  for i_src = 0 to 3 do
    for i_dst = 0 to 3 do
      if i_src <> i_dst then (
        pass2_steps_reversed :=
          (i_src, i_dst, !mix_state) :: !pass2_steps_reversed;
        mix_state := Int64.(!mix_state * mix_hashmix_mult))
    done
  done;
  let pool = Array.copy pool_out in
  (* Undo pass 2 by walking its 12 steps in exact reverse order. Each forward
     step writes to pool[i_dst], and i_src <> i_dst, so pool[i_src] still holds
     the value that step used. So we can recompute h with hashmix_forward, then
     invert the mix with h and pool[i_dst]. After each undo, pool is in the
     state it was before that step. *)
  List.iter
    (fun (i_src, i_dst, mix_state_before) ->
      let h = hashmix_forward pool.(i_src) mix_state_before in
      pool.(i_dst) <- inv_mix_in_x pool.(i_dst) h)
    !pass2_steps_reversed;
  (* In pass 1, each slot depends only on its own seed word and mix_state, so
     each slot is undone independently. *)
  Array.init 4 (fun i -> inv_hashmix pool.(i) pass1_mix_states.(i))
