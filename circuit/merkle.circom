pragma circom 2.0.0;

include "poseidon2_perm.circom";

include "misc.circom";

//------------------------------------------------------------------------------

//
// reconstruct the Merkle root using a Merkle inclusion proof
//
// parameters:
//  - depth: the depth of the Merkle tree = log2( numberOfLeaves )
//
// inputs and outputs:
//  - leaf:        the leaf hash
//  - pathBits:    the linear index of the leaf, in binary decomposition (little-endian)
//  - lastBits:    the index of the last leaf (= nLeaves-1), in binary decomposition
//  - maskBits:    the bits of the the mask `2^ceilingLog2(size) - 1`
//  - merklePath:  the Merkle inclusion proof (required hashes, starting from the leaf and ending near the root)
//  - recRoot:     the reconstructod Merkle root
//
// NOTE: we don't check whether the bits are really bits, that's the
//       responsability of the caller!
//

template RootFromMerklePath( maxDepth ) {

  signal input  leaf;
  signal input  pathBits[ maxDepth ];       // bits of the linear index
  signal input  lastBits[ maxDepth ];       // bits of the last linear index `= size-1`
  signal input  maskBits[ maxDepth+1 ];     // bit mask for `2^ceilingLog(size) - 1`
  signal input  merklePath[ maxDepth ];
  signal output recRoot;

  // the sequence of reconstructed hashes along the path
  signal aux[ maxDepth+1 ];
  aux[0] <== leaf;

  // compute which prefixes (in big-endian) of the index is
  // the same as the corresponding prefix of the last index
  component eq[ maxDepth ];
  signal isLast[ maxDepth+1 ];
  isLast[ maxDepth ] <== 1;
  for(var i=maxDepth-1; i>=0; i--) {
    eq[i] = IsEqual();
    eq[i].A <== pathBits[i];
    eq[i].B <== lastBits[i];
    isLast[i] <== isLast[i+1] * eq[i].out;
  }

  // compute the sequence of hashes
  signal switch[ maxDepth ];
  component comp[ maxDepth ];
  for(var i=0; i<maxDepth; i++) {

    var bottom = (i==0) ? 1 : 0;                // is it the bottom layer?
    var odd    = isLast[i] * (1-pathBits[i]);   // is it an odd node?

    comp[i] = KeyedCompression();

    var L = aux[i];
    var R = merklePath[i];

    // based on pathBits[i], we switch or not

    switch[i]      <== (R-L) * pathBits[i];
    comp[i].key    <== bottom + 2*odd;
    comp[i].inp[0] <== L + switch[i];
    comp[i].inp[1] <== R - switch[i];

    comp[i].out ==> aux[i+1];
  }

  // now we need to select the right layer from the sequence of hashes
  var sum = 0;
  signal prods[maxDepth];
  for(var i=0; i<maxDepth; i++) {
    prods[i] <== (maskBits[i] - maskBits[i+1]) * aux[i+1];
    sum += prods[i];
  }
  recRoot <== sum;
}

//------------------------------------------------------------------------------

