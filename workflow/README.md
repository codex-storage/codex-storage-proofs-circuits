
Guide though the whole proof workflow
-------------------------------------

The workflow described below is implemented with shell scripts in this directory.
So the below is more like an explanation.

To run the full workflow:

- set the parameters by editing `params.sh`
- run `setup.sh` to do the circuit-specific setup
- run `prove.sh` to generate input, compute witness and create (and verify) the proof

NOTE: the examples below assume `bash`. In particular, it won't work with `zsh` 
(which is the dafault on newer macOS)! Because, you know, reasons...

### Preliminaries

- install `circom`, `snarkjs`, `rapidsnark`: <https://docs.circom.io/getting-started/installation>
- install Nim: <https://nim-lang.org/>

Build the Nim cli proof input generator:

    $ cd ../reference/nim/proof_input/
    $ nimble build -d:release cli
    $ cd ../../../workflow

### Powers of tau setup

Either download a ready-to-use "powers of tau" setup file (section 7), or generate one
youself using `snarkjs` (sections 1..7), see the README here: <https://github.com/iden3/snarkjs>

Size `2^21` (file size about 2GB) should be big enough:

    $ cd ..
    $ mkdir -p ceremony
    $ cd ceremony
    $ wget https://storage.googleapis.com/zkevm/ptau/powersOfTau28_hez_final_21.ptau
    $ cd ../workflow

Note: generating this yourself will probably take quite a long time.

### Set the parameters

There are quite a few parameters (run `cli --help` too see them), it's probably
best to collect them into a parameter file. Check out `params.sh` and `cli_args.sh` 
to see one way to do that.

You can edit `params.sh` to your taste before running the workflow scripts.

### Compile the circuit

First create the main component:

    $ mkdir -p build
    $ cd build
    $ source ./cli_args.sh && ../../reference/nim/proof_input/cli $CLI_ARGS -v --circom="proof_main.circom"

Then compile the circuit:

    $ circom --r1cs --wasm --O2 -l../../circuit proof_main.circom

### Do the circuit-specific setup

See the [`snarkjs` README](https://github.com/iden3/snarkjs) for an overview of
the whole process.

    $ snarkjs groth16 setup proof_main.r1cs ../../ceremony/powersOfTau28_hez_final_21.ptau proof_main_0000.zkey
    $ snarkjs zkey contribute proof_main_0000.zkey proof_main_0001.zkey --name="1st Contributor Name"

You can add more contributors here if you want.

Finally rename the last contributions result and export the verification key:

    $ rm proof_main_0000.zkey
    $ mv proof_main_0001.zkey proof_main.zkey
    
    $ snarkjs zkey export verificationkey proof_main.zkey proof_main_verification_key.json

NOTE: You have redo all the above if you change any of the five parameters the circuit 
depends on (these are: maxdepth, maxslots, cellsize, blocksize, nsamples).

### Generate an input to the circuit

    $ source ../cli_args.sh && ../../reference/nim/proof_input/cli $CLI_ARGS -v --output=input.json

### Generate the witness

    $ cd proof_main_js
    $ time node generate_witness.js proof_main.wasm ../input.json ../witness.wtns
    $ cd ..

### Create the proof

Using `snarkjs` (very slow, but more portable):

    $ snarkjs groth16 prove proof_main.zkey witness.wtns proof.json public.json

Or using `rapidsnark` (fast, but not very portable):

    $ rapidsnark proof_main.zkey witness.wtns proof.json public.json

The output of this step will consist of:

- `proof.json` containing the proof itself
- `public.json` containing the public inputs

### Verify the proof (on CPU)

    $ snarkjs groth16 verify proof_main_verification_key.json public.json proof.json

### Generate solidity verifier contract

    $ snarkjs zkey export solidityverifier proof_main.zkey verifier.sol

