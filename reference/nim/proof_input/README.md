
Proof circuit input generator
-----------------------------

This is a Nim program to generate inputs for the proof circuit, and also the
main component of the circuit (it needs some global parameters).

Note: this is only for testing purposes, it's not optimized, and does not
use real data.

### Usage

    $ ./cli [options] --output=proof_input.json --circom=proof_main.circom
    
    available options:
     -h, --help                         : print this help
     -v, --verbose                      : verbose output (print the actual parameters)
     -d, --depth      = <maxdepth>      : maximum depth of the slot tree (eg. 32)
     -N, --maxslots   = <maxslots>      : maximum number of slots (eg. 256)
     -c, --cellsize   = <cellSize>      : cell size in bytes (eg. 2048)
     -b, --blocksize  = <blockSize>     : block size in bytes (eg. 65536)
     -s, --nslots     = <nslots>        : number of slots in the dataset (eg. 13)
     -n, --nsamples   = <nsamples>      : number of samples we prove (eg. 100)
     -e, --entropy    = <entropy>       : external randomness (eg. 1234567)
     -S, --seed       = <seed>          : seed to generate the fake data (eg. 12345)
     -f, --file       = <datafile>      : slot data file, base name (eg. "slotdata" would mean "slotdata5.dat" for slot index = 5)
     -i, --index      = <slotIndex>     : index of the slot (within the dataset) we prove
     -k, --log2ncells = <log2(ncells)>  : log2 of the number of cells inside this slot (eg. 10)
     -K, --ncells     = <ncells>        : number of cells inside this slot (eg. 1024; must be a power of two)
     -o, --output     = <input.json>    : the JSON file into which we write the proof input
     -C, --circom     = <main.circom>   : the circom main component to create with these parameters
     -F, --field      = <field>         : the underlying field: "bn254" or "goldilocks"
     -H, --hash       = <hash>          : the hash function to use: "poseidon2" or "monolith"

You can generate only the `.json` or only the `.circom` file, or both at the same 
time.

There are some default values for all the rest of the parameters.

NOTE: When using actual files for the slot data (which is untested :), you will 
need _all_ slots to be able to compute the dataset root. The convention used is 
that you specify the base name of the file, say `"slotdata"`; and the individual 
slots file names are derived from this to be for example `"slotdata5.dat"` for 
the slot with index 5 (note: indexing starts from 0).
