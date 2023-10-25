
Codex Storage Proofs for the MVP
================================

This document describes the storage proof system for the Codex 2023 Q4 MVP.


Setup
-----

We assume that a user dataset is split into `nSlots` number of uniformly sized
"slots" of size `slotSize`, for example 100 GB (for the MVP we may chose
a smaller size). The slots of the same dataset are spread over different 
storage nodes, but a single storage node can hold several slots (belonging to
different datasets). The slots themselves can be optionally erasure coded,
but this does not change the proof system, only the robustness of it.

We assume the slots are split into `nCells` number of uniformly sized
"cells" of size `cellSize`, for example 512 bytes. We don't in general assume 
that these numbers of powers of two, though in practice `nCells` will be probably
a power of two, and in fact the initial implementation assumes this. 

Note that we can simply calculate:

    nCells = slotSize / cellSize

We then hash each cell (using the sponge construction with Poseidon2; see below
for details), and build a binary Merkle tree over this hashes. This has depth
`d = ceil[ log2(nCells) ]`. Note: if `nCells` is not a power of two, then we
have to add dummy hashes. The exact conventions for doing this is to be determined 
later.

The Merkle root of the cells of a single slot is called the "slot root", and
is denoted by `slotRoot`.

Then for a given dataset, we can build another binary Merkle tree on the top of
its slot roots, resulting in the "dataset root". Grafting these Merkle trees
together we get a big dataset Merkle tree; however one should be careful 
about the padding conventions (it makes sense to construct the dataset-level
Merkle tree separately, as `nSlots` may not be a power of two).

The dataset root is a commitment to the whole dataset, and the user will post
it on-chain, to ensure that the nodes really store its data and not something else.
Optionally, the slot roots can be posted on-chain, but this seems to be somewhat
wasteful.


Hash function
-------------

For the MVP we will use the Poseidon2 hash function, specialized to state size
`t = 3` field elements, and to the alt-bn128 elliptic curve's scalar field, which 
has size

  r = 21888242871839275222246405745257275088548364400416034343698204186575808495617

For more details about this curve, see [BN254 For The Rest Of Us](https://hackmd.io/@jpw/bn254).
Remark: we have `2^253 < r < 2^254`.

For more details about the Poseidon2 hash, see

- [the Poseidon2 paper](https://eprint.iacr.org/2023/323)
- [the reference implementation](https://github.com/HorizenLabs/poseidon2)

### Data encoding convention

Poseidon2 hash works not over sequence of bytes, but field elements. Hence we
need to convert the data to be hashed to a sequence of field elements.

Note that the field size is approximately 254 bits, so a field element can store 
31 bytes of data but cannot store 32 bytes. Hence we have to split up our data
into 31 bytes "hash chunks", then encode these as field elements. We will use
little-endian byte convention to get from 31 bytes to the _standard form_ of 
a field element, that is, the 31 little-endian bytes encode the number `a`
where `0 <= a < 2^248 < r < 2^254` is the standard representation of a field element.
For padding the last field element just use zero bytes.

It's probably better to choose the standard form, because most standard tools
like circom use that, even though choosing the Montgomery form would be somewhat
more efficient (but the difference is very small, the hashing will dominate).

### Compression and sponge

Poseidon2 offers essentially two API functions: a so called _compression function_
which takes 2 field elements and returns 1; this can be used to build binary
Merkle trees.

The more complex _sponge_ can hash an arbitrary sequence of field elements into
a single (or several) field element(s).

While we can always use a Merkle tree instead of the sponge, if the data to be
hashed does not correspond to a power of two number of field elements, then
the sponge could be more efficient (up to a factor of two). Also we can use
the sponge construction with `rate=2`, which in practice means twice as fast.

Questions: 

- should we use the [SAFE sponge](https://hackmd.io/bHgsH6mMStCVibM_wYvb2w) 
  or the original sponge?
- should we use rate 1 or 2? It seems that even `rate=2` gives an approximately
  128 bit of collision and preimage security, per standard cryptographic
  assumptions; and higher rate means faster hashing.

I propose to use the SAFE convention and `rate=2`. However the current 
implementation uses Poseidon1-style sponge.


Parameters
----------

Parameters should be set based on bechmarks and other external constraints,
but already we can have some preliminary targets. If we use a slot Merkle tree
of depth between 16-32, and sponge hash with a rate of 2, then the Merkle 
inclusion proof cost will be approximately in balance with the cell hashing
cost if the cell size is 1024-2048 bytes (34-67 field elements).

If we use 2048 byte cells, then `nCells = 8192*8192 = 2^26` gives a slot size
of 128 Gb, which looks like a good compromise target slot size (we don't want
it to be too small, because then the number of slot proofs per node will be
too big; but we don't want it to be too big either, because that's inflexible)

### 2D erasure coding

We need to use 2D erasure coding inside a slot, because while 1D would be better,
the required K,N parameters of the EC would be too big. Furthermore, we don't
want too big storage overhead, so the idea is to use `K ~= 2/3 * N`
(with `N` being a power of two). With such a setting, we can store the
original `K x K` matrix and the small `(N-K) x (N-K)` matrix, and reconstruct
the rest real-time. This particular parameter setting gives us a storage overhead 
of `25%` (of course we can tune the parameters, this is just an example).

Note that the EC rate does not affect the proof system itself, only the required
number of samples to achieve a given target detection probability.

With the above setting (EC rate = 2/3), we need 117 samples to achieve 6 nines
of detection probability. Sampling a single 2048 cell with sponge rate 2
means approx 34 Poseidon2 permutation calls, while computing the root of
a depth 26 Merkle path is another 26 such calls. Finally computing the index
needs 2 calls and an extra cost similar to approx 3 calls, so we have about
65 calls per sample, each about 250 constraints. So in total we have about
`117 * 65 * 250 ~= 1.9 million` constraints, or equivalent to hashing about
`235 kb` of data with a binary Merkle tree or with a sponge of rate 1.


Sampling
--------

Given a slot of a dataset, we want to check `nSamples` randomly selected
cells, to see whether they are still stored by the node. We expect `nSamples`
to be in the range 20-200.

To be able to do this, we need to compute:

- the indices of the selected cells (or Merkle leaves)
- the hashes of the selected cells
- the Merkle paths from these hashes up to the slot root (Merkle proofs)
- the (single) Merkle path from the slot root to the dataset root.

To be able to sample randomly, we need some external source of entropy; for
this, presumably a block hash of some public blockchain (eg. Ethereum) will
be used. It can be convenient to have the entropy in the form a field element;
for this we can simply take the block hash `modulo r`. While this gives a
somewhat non-uniform probability distribution, in practice this should not
cause any issue. Alternatively, we could just keep 31 bytes of the block hash and
interpret it as a field element, giving 248 bits of uniform entropy.

We propose the following function to compute the indices of the selected cells:

    idx = H( entropy | slotRoot | counter ) `mod` nCells

where `counter` iterates over the range `[1..nSamples]`, `H` is our hash
function, and `|` denotes concatenation.


Circuit
-------

For the MVP, we build a monolithic circuit / R1CS instance for proving all
the samples in single slot; then use Groth16 to prove it.

Public inputs:

- dataset root
- entropy
- slot index: which slot of the dataset we are talking about; `[1..nSlots]`

Private inputs:

- the slot root
- the underlying data of the cells, as sequences of field elements
- the Merkle paths from the leaves (the cell hashes) to the slot root
- the Merkle path from the slot root to the dataset root

Computation:

- compute the cell indices from the slot root and the entropy
- compute the cell hashes from the cell data
- from the cell hashes, using the provided Merkle paths, recompute the slot root
- compare the reconstructed slot root to the slot root given as private input
- from the slot root, using the provided Merkle path, recompute and check the
  dataset root

Note that given the index of a leaf, we can compute the left-right zig-zag
of the corresponding Merkle path, simply by looking at the binary decomposition.
So the Merkle paths will only consist lists of hashes.

