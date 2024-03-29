
Storage proof `circom` circuit
------------------------------

See the [README in the parent dir](../README.md) for the (draft) specification.

### Organization of the circuit code

- `codex` - the storage proof circuit
- `poseidon2` - Poseidon2 hash function
- `lib` - general purpose, reusable circom templates ("circuit library")

In `codex`:

- `sample_cells.circom` - compute cell indices to sample, and prove those cells
- `single_cell.circom` - prove a single cell
- `merkle.circom` - Merkle inclusion proof (using our custom Merkle tree convention)

In `poseidon2`

- `poseidon2_hash.circom` - compute Poseidon2 hash with sponge construction
- `poseidon2_sponge.circom` - generic sponge construction
- `poseidon2_perm.circom` - the Poseidon2 permutation
- `poseidon2_compr.circom` - the compression function

In `lib`:

- `extract_bits.circom` - extract lower bits of the *standard representation* of a field element
- `binary_compare.circom` - compare numbers given in binary representation (the point is that they can be bigger than the field size!)
- `log2.circom` - circom code for computing base 2 logarithm
- `misc.circom` - miscellaneous helper funtions


### Main component

Note: the main component is not included in the above, as it depends on the
parameters. You can use one of the reference input generators to create one;
if you want to do manually, it should look like this:

    pragma circom 2.0.0;
    include "sample_cells.circom";
    
    // argument conventions:
    // SampleAndProven( maxDepth, maxLog2NSlots, blockTreeDepth, nFieldElemsPerCell, nSamples )
    component main {public [entropy,dataSetRoot,slotIndex]} = SampleAndProve(32, 8, 5, 67, 100);


