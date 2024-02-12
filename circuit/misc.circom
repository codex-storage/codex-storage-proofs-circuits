pragma circom 2.0.0;

//------------------------------------------------------------------------------
// decompose an n-bit number into bits (least significant bit first)

template ToBits(n) {
  signal input  inp;
  signal output out[n];

  var sum = 0;
  for(var i=0; i<n; i++) {
    out[i] <-- (inp >> i) & 1;
    out[i] * (1-out[i]) === 0;
    sum += (1<<i) * out[i];
  }

  inp === sum;
}

//------------------------------------------------------------------------------
// check equality to zero; that is, compute `(inp==0) ? 1 : 0`

template IsZero() {
  signal input  inp;
  signal output out;

  // guess the inverse
  signal inv;
  inv <-- (inp != 0) ? (1/inp) : 0 ;

  // if `inp==0`, then by definition `out==1`
  // if `out==0`, then the inverse must must exist, so `inp!=0`
  out <== 1 - inp * inv;

  // enfore that either `inp` or `out` must be zero
  inp*out === 0;
}

//------------------------------------------------------------------------------
// check equality of two field elements; that is, computes `(A==B) ? 1 : 0`

template IsEqual() {
  signal input  A,B;
  signal output out;

  component isz = IsZero();
  isz.inp <== A - B;
  isz.out ==> out;
}

//------------------------------------------------------------------------------
