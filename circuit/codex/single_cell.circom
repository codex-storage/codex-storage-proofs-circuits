pragma circom 2.0.0;

include "poseidon2_hash.circom";

include "merkle.circom";

//------------------------------------------------------------------------------

//
// calculates a single cell's hash and reconstructs the Merkle root, 
// checking whether it matches the given slot root
//
// parameters:
//  - nFieldElemsPerCell: how many field elements a cell consists of (= 2048/31 = 67)
//  - botDepth: the depth of the per-block minitree (= 5)
//  - maxDepth: the maximum depth of slot subtree (= 32)
//
// inputs and outputs:
//  - indexBits:      the linear index of the cell, within the slot subtree, in binary
//  - lastBits:       the index of the last cell (size - 1), in binary (required for odd-even node key)
//  - maskBits:       the binary mask of the size rounded up to a power of two
//  - data:           the cell data (already encoded as field elements)
//  - merklePath:     the Merkle inclusion proof
//  - slotRoot:       the expected slot root
//
// NOTE: we don't check whether the bits are really bits, that's the
//       responsability of the caller!
//

template ProveSingleCell( nFieldElemsPerCell, botDepth, maxDepth ) {

  signal input slotRoot;
  signal input data[ nFieldElemsPerCell ];
  signal input lastBits  [ maxDepth ];
  signal input indexBits [ maxDepth ];
  signal input maskBits  [ maxDepth + 1 ];
  signal input merklePath[ maxDepth ];

  // these will reconstruct the Merkle path up to the slot root
  // in two steps: first the block-level ("bottom"), then the slot-level ("middle")
  component pbot = RootFromMerklePath( botDepth            );
  component pmid = RootFromMerklePath( maxDepth - botDepth );

  for(var i=0; i<maxDepth; i++) {
    if (i<botDepth) {
      pbot.pathBits[i]     <== indexBits[i];
      pbot.maskBits[i]     <== maskBits[i];
      pbot.lastBits[i]     <== lastBits[i];
      pbot.merklePath[i]   <== merklePath[i];
    }
    else {
      var j = i - botDepth;
      pmid.pathBits[j]     <== indexBits[i];
      pmid.maskBits[j]     <== maskBits[i];
      pmid.lastBits[j]     <== lastBits[i];
      pmid.merklePath[j]   <== merklePath[i];
    }
  }
  pbot.maskBits[ botDepth            ] <== 0;
  pmid.maskBits[ maxDepth - botDepth ] <== 0;

  // compute the cell hash
  component hash = Poseidon2_hash_rate2( nFieldElemsPerCell );
  hash.inp  <== data;
  hash.out  ==> pbot.leaf;
  pmid.leaf <== pbot.recRoot;

log("middle bottom root check = ", pmid.recRoot == slotRoot);

  // check if the reconstructed root matches the actual slot root
  pmid.recRoot === slotRoot;

}

//------------------------------------------------------------------------------

