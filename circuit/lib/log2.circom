pragma circom 2.0.0;

include "misc.circom";

//------------------------------------------------------------------------------
//
// given an input `inp`, this template checks that inp == 2^out
// with 0 < out <= n
//
// we also output a mask vector which is 1 for i=0..out-1, and 0 elsewhere
//

template Log2(n) {

  signal input  inp;
  signal output out;
  signal output mask[n+1];

  // mask will be a vector [1,1,1,...1,0,...,0,0]
  // which can change only where index == out

  var log2 = -1;
  for(var i=0; i<=n; i++) {
    mask[i] <-- ((2**i) < inp) ? 1 : 0;
    if (2**i == inp) { log2 = i; }
  }

  out <-- log2;

  mask[0] === 1;
  mask[n] === 0;

  var sum = 0;
  for(var i=0; i<n; i++) {
    sum += (2**(i+1)) * (mask[i] - mask[i+1]);
    0 === (mask[i] - mask[i+1]) * (i + 1 - out);
  }

  inp === sum;
}

//------------------------------------------------------------------------------
//
// a version of `Log2(n)` which works with `circom-witnesscalc`
//
// yeah this is very ugly. Direct your complaints to the iden3 people. ¯\_(ツ)_/¯
//

template Log2_CircomWitnessCalc_Hack(n) {

  signal input  inp;
  signal output out;
  signal output mask[n+1];

  // compute the logarithm
  // as `circom-witnesscalc` does not support any kind of dynamic computation
  // and support for functions, and other things also seems to be limited
  // we have to implement this here as a static computation instead
  // but even that's not enough for `circom-witnesscalc`! hence this uglyness 
  var x    = inp;
  var cnt  = -1;
  signal done[257]; done[0] <--  0;     // we need *signals* here because `circom-witnesscalc` doesn't work otherwise (seriously...)
  signal log2[257]; log2[0] <-- -1;     // but hopefully circom will optimize these away...
  for(var i=0; i<256; i++) {
    var not_done  = (done[i] == 0);
    var do_switch = (x==0) && not_done;
    log2[i+1] <-- not_done  ? cnt : log2[i];
    done[i+1] <-- do_switch ? 1   : done[i];
    cnt   = cnt + 1;
    x     = x >> 1;
  }

  // mask will be a vector [1,1,1,...1,0,...,0,0]
  // which can change only where index == out

  for(var i=0; i<=n; i++) {
    mask[i] <-- ((2**i) < inp) ? 1 : 0;
  }

  out <-- log2[256];

  log("Log2 hack: input = ",inp," | logarithm = ",out);

  mask[0] === 1;
  mask[n] === 0;

  var sum = 0;
  for(var i=0; i<n; i++) {
    sum += (2**(i+1)) * (mask[i] - mask[i+1]);
    0 === (mask[i] - mask[i+1]) * (i + 1 - out);
  }

  inp === sum;

}

//------------------------------------------------------------------------------
//
// given an input `inp`, this template computes `out := k` such that 2^k <= inp < 2^{k+1}
// it also returns the binary decomposition of `inp-1`, and the binary decomposition
// of the mask `(2^k-1)`
//
// we also output a mask vector which is 1 for i=0..k-1, and 0 elsewhere
//
// we require `k <= n`, otherwise this will fail.
//

template CeilingLog2(n) {

  signal input  inp;
  signal output out;
  signal output bits[n];
  signal output mask[n+1];

  component tb = ToBits(n);
  tb.inp <== inp - 1;
  tb.out ==> bits;

  signal aux[n+1];
  aux[n] <== 1;
  var sum = 0;
  for(var i=n-1; i>=0; i--) {
    aux[i]  <== aux[i+1] * (1 - bits[i]);
    mask[i] <== 1 - aux[i];
    sum = sum + (aux[i+1] - aux[i]) * (i+1);
  }
  mask[n] <== 0;

  out <== sum;
}

//------------------------------------------------------------------------------
