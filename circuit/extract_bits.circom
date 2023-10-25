pragma circom 2.0.0;

include "binary_compare.circom";
include "misc.circom";

//------------------------------------------------------------------------------

// 
// extract the lowest `n` bits from a field element.
//
// NOTE: this is rather nontrivial, as everything is computed modulo `r`, 
// so naive bit decomposition does not work (there are multiple solutions).
// 

template ExtractLowerBits(n) {

  signal input  inp;
  signal output out[n];

  // we may have 2 solutions for this
  component tb = ToBits(254);      // note: 2^253 < r < 2^254
  tb.inp <== inp;

  // bits of field prime `r` in little-endian order
  var primeBits[254] = [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,1,0,0,1,1,0,1,0,1,1,1,1,1,0,0,0,0,1,1,1,1,1,0,0,0,0,1,0,1,0,0,0,1,0,0,1,0,0,0,0,1,1,1,0,1,0,0,1,1,1,0,1,1,0,0,1,1,1,1,0,0,0,0,1,0,0,1,0,0,0,0,1,0,1,1,1,1,1,0,0,1,1,0,0,0,0,0,1,0,1,0,0,1,0,1,1,1,0,1,0,0,0,0,1,1,0,1,0,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,0,1,1,0,1,1,0,1,1,0,1,0,0,0,1,0,0,0,0,0,1,0,1,0,0,0,0,1,1,1,0,1,1,0,0,1,0,1,0,0,0,0,0,0,0,1,0,1,1,0,0,0,1,1,0,0,1,0,0,0,0,1,1,1,0,1,0,0,1,1,1,0,0,1,1,1,0,0,1,0,0,0,1,0,0,1,1,0,0,0,0,0,1,1];

  // enforce that the binary representation is < r
  component le = BinaryCompare(254);
  le.A <== tb.out;
  le.B <== primeBits;
  le.out === -1;          // enforce `A < B`, that is, `bits < prime`

  // extract the lowest `n` bits
  for(var i=0; i<n; i++) { 
    tb.out[i] ==> out[i];
  }

}

//------------------------------------------------------------------------------

//
// a version of the above specialized to the test field `p = 65537`
//
// this is used only for testing using the `r1cs-solver` tool
//

template ExtractLowerBits_testfield65537(n) {

  signal input  inp;
  signal output out[n];

  // we may have up to 4 solutions for this
  component tb = ToBits(18);      // note: 2^16 < r < 2^18
  tb.inp <== inp;

  // bits of field prime `r` in little-endian order
  var primeBits[18] = [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0];

  // enforce that the binary representation is < r
  component le = BinaryCompare(18);
  le.A <== tb.out;
  le.B <== primeBits;
  le.out === -1;          // enforce `A < B`, that is, `bits < prime`

  // extract the lowest `n` bits
  for(var i=0; i<n; i++) { 
    tb.out[i] ==> out[i];
  }

}

//------------------------------------------------------------------------------
