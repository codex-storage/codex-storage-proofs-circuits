pragma circom 2.0.0;

//------------------------------------------------------------------------------

//
// given two numbers in `n`-bit binary decomposition (little-endian), we compute
//
//             /  -1   if   A <  B
//   out  :=  {    0   if   A == B
//             \  +1   if   A >  B
//
// NOTE: we don't check that the digits are indeed binary;
//       that's the responsibility of the caller!
//
// This version uses `(3*n-1)` nonlinear constraints.
// Question: can we do better? (note that this has to work with n >= 254 digits too!)
//

template BinaryCompare(n) {
  signal input  A[n];
  signal input  B[n];
  signal output out;

  signal eq[n];
  signal aux[n];

  signal jump[n+1];
  jump[n] <== 1;

  var sum = 0;
  for(var k=n-1; k>=0; k--) {
    var y = A[k] - B[k];
    eq[k]   <== 1 - y*y;                      // (A[k] == B[k]) ? 1 : 0
    jump[k] <== eq[k] * jump[k+1];            // this jumps from 1 to 0 at the highest inequal digit
    aux[k]  <== (jump[k+1] - jump[k]) * y;    // where we store whether A was greater or less
    sum += aux[k];
  }

  out <== sum;
}

//------------------------------------------------------------------------------

//
// given two numbers in `n`-bit binary decomposition (little-endian), we compute
//
//   out  :=  (A <= B) ? 1 : 0
//
// NOTE: we don't check that the digits are indeed binary;
//       that's the responsibility of the caller!
//

template BinaryLessOrEqual(n) {
  signal input  A[n];
  signal input  B[n];
  signal output out;

  var phalf = 1/2;       // +1/2 as field element
  var mhalf = -phalf;    // -1/2 as field element

  component cmp = BinaryCompare(n);
  cmp.A <== A;
  cmp.B <== B;

  var x = cmp.out;
  out <== mhalf * (x-1) * (x+2);
}

//------------------------------------------------------------------------------

//
// given two numbers in `n`-bit binary decomposition (little-endian), we compute
//
//   out  :=  (A >= B) ? 1 : 0
//
// NOTE: we don't check that the digits are indeed binary;
//       that's the responsibility of the caller!
//

template BinaryGreaterOrEqual(n) {
  signal input  A[n];
  signal input  B[n];
  signal output out;

  var phalf = 1/2;       // +1/2 as field element
  var mhalf = -phalf;    // -1/2 as field element

  component cmp = BinaryCompare(n);
  cmp.A <== A;
  cmp.B <== B;

  var x = cmp.out;
  out <== mhalf * (x+1) * (x-2);
}

//------------------------------------------------------------------------------
