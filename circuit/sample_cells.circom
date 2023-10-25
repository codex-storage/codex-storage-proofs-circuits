pragma circom 2.0.0;

include "single_cell.circom";
include "poseidon2_hash.circom";
include "extract_bits.circom";
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

template CalculateCellIndexBits( nCells ) {
  
  var log2N = CeilLog2(nCells);
  assert( nCells == (1<<log2N) );

  signal input  entropy;
  signal input  slotRoot;
  signal input  counter;
  signal output indexBits[log2N];

  // calculate the hash
  component pos = Poseidon2_hash_rate2(3);
  signal hash;
  pos.inp[0] <== entropy;
  pos.inp[1] <== slotRoot;
  pos.inp[2] <== counter;
  pos.out    ==> hash;

  // extract the lowest `log2(nCells)` bits
  component md = ExtractLowerBits(log2N);
  md.inp <== hash;
  md.out ==> indexBits;

}

//------------------------------------------------------------------------------

//
// same as above, but returns an integer index instead of its binary decomposition.
//

template CalculateCellIndexInteger( nCells ) {

  var log2N = CeilLog2(nCells);
  assert( nCells == (1<<log2N) );

  signal input  entropy;
  signal input  slotRoot;
  signal input  counter;
  signal output linearIndex;

  component calc = CalculateCellIndexBits( nCells );
  calc.entropy  <== entropy;
  calc.slotRoot <== slotRoot;
  calc.counter  <== counter;

  var sum = 0;
  for(var i=0; i<log2N; i++) {
    sum += (1<<i) * calc.indexBits[i];
  }

  linearIndex <== sum;
}

//------------------------------------------------------------------------------

// 
// sample `nSamples` number of cells; calculate their hashes;
// reconstruct the slot root using the Merkle pathes, and 
// checks if it matches the externally given slot root.
//
// NOTE: difference between V1 and V2 is whether we use the slot root
//       or the dataset root.
//

template SampleAndProveV1( nCells, nFieldElemsPerCell, nSamples ) {

  var log2N = CeilLog2(nCells);
  assert( nCells == (1<<log2N) );

  signal input  entropy;                                  // public input
  signal input  slotRoot;                                 // public input

  signal input  cellData[nSamples][nFieldElemsPerCell];   // private input
  signal input  merklePaths[nSamples][log2N];             // private input

  component calci[nSamples];
  component prove[nSamples];

  for(var cnt=0; cnt<nSamples; cnt++) {

    calci[cnt] = CalculateCellIndexBits( nCells );
    prove[cnt] = ProveSingleCell( nFieldElemsPerCell, log2N );

    calci[cnt].entropy    <== entropy;
    calci[cnt].slotRoot   <== slotRoot;
    calci[cnt].counter    <== cnt + 1;
    calci[cnt].indexBits  ==> prove[cnt].indexBits;

    prove[cnt].slotRoot   <== slotRoot;
    prove[cnt].data       <== cellData[cnt];
    prove[cnt].merklePath <== merklePaths[cnt];

  }
}

//------------------------------------------------------------------------------

