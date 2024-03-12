pragma circom 2.0.0;

include "poseidon2_perm.circom";

//
// The Poseidon2 compression function (used when constructing binary Merkle trees)
//

//------------------------------------------------------------------------------
// the "compression function" takes 2 field elements as input and produces
// 1 field element as output. It is a trivial application of the permutation.

template Compression() {
  signal input  inp[2];
  signal output out;

  component perm = Permutation();
  perm.inp[0] <== inp[0];
  perm.inp[1] <== inp[1];
  perm.inp[2] <== 0;

  perm.out[0] ==> out;
}

//------------------------------------------------------------------------------
// the "keyed compression function" additionally takes a key parameter, resulting
// in a keyed family of compression functions. In practice we use 4 different
// keys (0,1,2, and 3).

template KeyedCompression() {
  signal input  key;
  signal input  inp[2];
  signal output out;

  component perm = Permutation();
  perm.inp[0] <== inp[0];
  perm.inp[1] <== inp[1];
  perm.inp[2] <== key;

  perm.out[0] ==> out;
}

//------------------------------------------------------------------------------

