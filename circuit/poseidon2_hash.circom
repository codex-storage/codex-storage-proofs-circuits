pragma circom 2.0.0;

include "poseidon2_sponge.circom";

//------------------------------------------------------------------------------
// Hash `n` field elements into 1, with approximately 127 bits of preimage security
// (assuming bn128 scalar field. We use capacity=1, rate=2, t=3).

template Poseidon2_hash_rate2(n) {
  signal input  inp[n];
  signal output out;

  component sponge = PoseidonSponge(3,1,n,1);
  sponge.inp    <== inp;
  sponge.out[0] ==> out;
}

//------------------------------------------------------------------------------
