pragma circom 2.0.0;

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
  // which can change only where inp == 2^out

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
