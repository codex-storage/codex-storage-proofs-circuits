pragma circom 2.0.0;

include "single_cell.circom";
include "poseidon2_hash.circom";
include "extract_bits.circom";
include "log2.circom";
include "misc.circom";

//------------------------------------------------------------------------------

//
// calculate the linear index of the k-th cell we want to sample.
// this version return the binary decomposition of the index
// (we need that for the Merkle proof anyway, it's cheaper this way)
//
// the formula for this is:
//
//     idx = H( entropy | slotRoot | counter ) `mod` nCells
//
// NOTE: we assume `nCells` is a power of two.
//

template CalculateCellIndexBits( maxLog2N ) {

  signal input  entropy;
  signal input  slotRoot;
  signal input  counter;
  signal input  cellIndexBitMask[maxLog2N];      // bit mask for the cell index range

  signal output indexBits[maxLog2N];

  // calculate the hash
  component pos = Poseidon2_hash_rate2( 3 );     // input is 3 field elements
  signal hash;
  pos.inp[0] <== entropy;
  pos.inp[1] <== slotRoot;
  pos.inp[2] <== counter;
  pos.out    ==> hash;

  // extract the lowest `maxLog2N = 32` bits
  component md = ExtractLowerBits(maxLog2N);
  md.inp <== hash;

  for(var i=0; i<maxLog2N; i++ ) {
    indexBits[i] <== cellIndexBitMask[i] * md.out[i];
  }

}

//------------------------------------------------------------------------------

//
// sample `nSamples` number of cells; calculate their hashes;
// reconstruct the slot root using the Merkle paths, then
// the dataset root too, and checks if everything is consistent.
//

template SampleAndProve( maxDepth, maxLog2NSlots, blockTreeDepth, nFieldElemsPerCell, nSamples ) {

  // var maxDepth       = 32;    // maximum depth of the slot Merkle tree (so max `2^maxDepth` cells in a slot)
  // var maxLog2NSlots  = 8;     // maximum depth of the dataset-level Merkle tree (so max 2^8 slots per dataset)
  // var blockTreeDepth = 5;     // depth of the "network block tree" (= log2(64k / 2k))

  signal input entropy;                                  // public input
  signal input dataSetRoot;                              // public input
  signal input slotIndex;                                // must be public, otherwise we could prove a different slot

  signal input slotRoot;                                 // can be private input
  signal input nCellsPerSlot;                            // can be private input (Merkle tree is safe)
  signal input nSlotsPerDataSet;                         // can be private input (Merkle tree is safe)

  signal input slotProof[maxLog2NSlots];                 // path from the slot root the the dataset root (private input)

  signal input cellData[nSamples][nFieldElemsPerCell];   // private input
  signal input merklePaths[nSamples][maxDepth];          // private input

  // -------------------------------------------------------
  //
  // first we prove the inclusion of the slot root in the dataset-level
  // (small) Merkle tree

  component tbtp = ToBits( maxLog2NSlots );
  component clog = CeilingLog2( maxLog2NSlots );
  component mtop = RootFromMerklePath( maxLog2NSlots );

  tbtp.inp        <== slotIndex;
  clog.inp        <== nSlotsPerDataSet;
  mtop.leaf       <== slotRoot;
  mtop.pathBits   <== tbtp.out;
  mtop.lastBits   <== clog.bits;
  mtop.maskBits   <== clog.mask;
  mtop.merklePath <== slotProof;

log("top root check = ", mtop.recRoot == dataSetRoot);

  mtop.recRoot === dataSetRoot;

  // -------------------------------------------------------
  //
  // then we prove the individual sampled cells

  component lg = Log2(maxDepth);                          // we allow at most 2^32 cells per slot
  lg.inp <== nCellsPerSlot;

  // NOTE: in general we need for the Merkle prover the binary decomposition
  // of `nLeaves - 1`. But currently this is in fact a power of two, so we
  // can reuse the binary mask for this. Later we may change this?
  //
  signal lastBits[maxDepth];
  for(var i=0; i<maxDepth; i++) { lastBits[i] <== lg.mask[i]; }

  component calci[nSamples];
  component prove[nSamples];

  for(var cnt=0; cnt<nSamples; cnt++) {

    calci[cnt] = CalculateCellIndexBits( maxDepth );
    prove[cnt] = ProveSingleCell( nFieldElemsPerCell, blockTreeDepth, maxDepth );

    // calci[cnt].cellIndexBitMask <== lg.mask;
    for(var i=0; i<maxDepth; i++) { calci[cnt].cellIndexBitMask[i] <== lg.mask[i]; }

    calci[cnt].entropy          <== entropy;
    calci[cnt].slotRoot         <== slotRoot;
    calci[cnt].counter          <== cnt + 1;
    calci[cnt].indexBits        ==> prove[cnt].indexBits;

    prove[cnt].slotRoot   <== slotRoot;
    prove[cnt].lastBits   <== lastBits;
    prove[cnt].maskBits   <== lg.mask;
    prove[cnt].data       <== cellData[cnt];
    prove[cnt].merklePath <== merklePaths[cnt];

  }
}

//------------------------------------------------------------------------------

