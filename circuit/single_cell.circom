pragma circom 2.0.0;

include "poseidon2_perm.circom";
include "poseidon2_hash.circom";

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
//  - merklePath:  the Merkle inclusion proof (required hashes, starting from the leaf and ending near the root)
//  - recRoot:     the reconstructod Merkle root
//
// NOTE: we don't check whether the bits are really bits, that's the
// responsability of the caller!
//

template MerklePathBits( depth ) {
  
  signal input  leaf;
  signal input  pathBits[ depth ];
  signal input  merklePath[ depth ];
  signal output recRoot;

  // the sequence of reconstructed hashes along the path
  signal aux[ depth+1 ];
  aux[0] <== leaf;

  signal switch[ depth];
  
  component comp[ depth ];
  for(var i=0; i<depth; i++) {
    comp[i] = Compression();

    var L = aux[i];
    var R = merklePath[i];

    // based on pathBits[i], we switch or not
    
    switch[i]      <== (R-L) * pathBits[i];
    comp[i].inp[0] <== L + switch[i];
    comp[i].inp[1] <== R - switch[i];

    comp[i].out ==> aux[i+1];
  }

  aux[depth] ==> recRoot;
}

//--------------------------------------

//
// a version of the above where the leaf index is given as an integer
// instead of a sequence of bits
//

template MerklePathIndex( depth ) {
  
  signal input  leaf;
  signal input  linearIndex;
  signal input  merklePath[ depth ];
  signal output recRoot;

  // decompose the linear cell index into bits (0 = left, 1 = right)
  component tb   = ToBits( depth );
  component path = MerklePathBits( depth );

  tb.inp <== linearIndex;
  tb.out ==> path.pathBits;

  path.leaf       <== leaf;
  path.merklePath <== merklePath;
  path.recRoot    ==> recRoot;
}

//------------------------------------------------------------------------------

//
// calculates a single cell's hash and reconstructs the Merkle root, 
// checking whether it matches the given slot root
//
// parameters:
//  - nFieldElemsPerCell: how many field elements a cell consists of
//  - merkleDepth: the depth of slot subtree = log2(nCellsPerSlot)
//
// inputs and outputs:
//  - indexBits:  the linear index of the cell, within the slot subtree, in binary
//  - data:       the cell data (already encoded as field elements)
//  - merklePath: the Merkle inclusion proof
//  - slotRoot:   the expected slot root
//
// NOTE: we don't check whether the bits are really bits, that's the
// responsability of the caller!
//

template ProveSingleCell( nFieldElemsPerCell, merkleDepth ) {

  signal input slotRoot;
  signal input data[ nFieldElemsPerCell ];

  signal input indexBits [ merkleDepth ];
  signal input merklePath[ merkleDepth ];

  // this will reconstruct the Merkle path up to the slot root
  component path = MerklePathBits( merkleDepth );
  path.pathBits   <== indexBits;
  path.merklePath <== merklePath;

  // compute the cell hash
  component hash = Poseidon2_hash_rate2( nFieldElemsPerCell );
  hash.inp <== data;
  hash.out ==> path.leaf;

  // check if the reconstructed root matches the actual slot root
  path.recRoot === slotRoot;

}

//------------------------------------------------------------------------------

