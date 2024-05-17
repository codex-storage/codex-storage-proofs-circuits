
The cogs of a ZK proof
======================

The goal of this document is to describe all the different moving parts 
(circuit, witness, public input, etc) of a ZK proof through a simple example,
hopefully giving some intuition about how these cogs fit together.


The example: Fibonacci sequence
-------------------------------

ZK proofs (or more precisely: "succint arguments") are all about convicing
another party (the "verifier"), about the truth of some statement, without
telling them _everything_.

In our toy example, the statement will be the 1000th element of a Fibonacci
sequence.

A Fibonacci sequence is generated deterministically from its first two elements,
let's call them `U` and `V`. These are numbers. The sequence is then defined as

    a[0] = U
    a[1] = V
    a[n] = a[n-1] + a[n-2]

In the standard Fibonacci sequence, we have `U=0` and `V=1` (or sometimes `U=V=1`,
which results in the same sequence but shifted by one). But we can consider 
this generalized setting with `U` and `V` being arbitrary numbers.

The statement I want to convince you, is that for a given `U` (which I tell you,
say `U=53`), I know a value for `V` (which I don't tell you), such that `a[1000]`
is some given value (of course, I tell you this value too, otherwise there is
nothing to be convinced about).

Pretty simple so far.


The statement
-------------

A proof always start with a statement. Usually, the statement is descibed by
a computer program (in our case, the program is computing the Fibonacci sequence).
A program normally has some inputs and some ouputs. Some of the inputs are
public (I tell you what they are), some others can be secret (I don't tell you what 
they are). And the statement is _usually_ that if you run this program with the given 
inputs, then the program runs normally (it doesn't throw an error) and you get the 
given output.

Note: you can freely move things from outputs to inputs. In our case, the output
would be `a[1000]`, but equivalently, I can make another program where there is
a third input `W`, and the program just throws an error at the end if `a[1000] != W`.
Because of this, we often just say IO for input/output, and "public IO" for the
part of it I tell you, and "private IO" or "secret IO" for the part I don't tell 
you.

So far we have:

- the statement (a computer program)
- the public IO
- and the private IO

The computer program is often called a "circuit", because for technical reasons
it often needs to be described as an "arithmetic circuit", which is something
similar to digital electronics circuits (but in math). This gets a bit more
complicated with zkVM-s, where the circuit itself is something like a CPU, and your
statement is an actual program running on that (virtual) CPU.


The witness
-----------

How essentially all proof system works is to translate the statement you want
the prove into some math equations, and then prove that I know a solution for
those equations. Because of this, the prover needs to know not only the input
and the output, but all the intermediate results too - essentially a very detailed
log of all individual operations happening in the computer program describing 
the statement. This huge logfile is called "the witness", and in practice it's
usually just a large array of numbers (more precisely, finite field elements).

Unfortunately, the word "witness" is a bit overloaded: Sometimes it refers only
to the private IO, and sometimes to the whole log. While these are kind of the same
from a holistic point of view, it still makes sense to distinguish them, and
I will call the whole execution log the witness.

Creating a proof always start by running the program which descibes statement,
and creating this very detailed log, the witness (sometimes also called a 
"transcript"). This step is usually called the "witness generation".


Trusted setup
-------------

Some, but not all, proof systems requires something called a "trusted setup".
This is some structured piece of information produced by an external party, 
which has some secret encoded in it. It's important that the secret is
deleted (hence the word "trusted") after generating this setup, because anybody 
in possesion of the secret can cheat and create fake proofs. 

For example in the popular scheme called "KZG commitments", the secret is
a number (a field element) usually called `tau`, and the result of the 
trusted setup is a sequence

    [ g , tau*g , tau^2*g , ... , tau^N*g ]

where `g` is a fixed group element. Because of this, the procedure generating
this sequence is called a "powers-of-tau ceremony". Assuming the discrete 
logarithm problem is hard in the group, it's not feasible to compute `tau` 
itself given that sequence, but it's easy to compute `f(tau)*g` for a polynomial
`f` with degree at most `N`.

In practice such a trusted setup can be accomplished using multiparty computation
(MPC), so that if there is at least _one_ honest participant, then the resulting
sequence is safe to use. These events are called "ceremonies" (in early days
people actually had to do this in person, and at the end they ceremonially destroyed 
the computer which contained the random secret `tau`).

There are two kinds of trusted setups: universal and circuit-specific. An universal
one can be used to prove many different statements, while a circuit-specific one
can be used to prove a single fixed statement, or circuit (but with different inputs).

For example Groth16 needs a circuit-specific trusted setup, Plonk+KZG only needs 
a universal trusted setup, while Plonk+FRI does not need any such setup at all. 
However usually systems with trusted setups are more efficient (for example Groth16 has
the smallest proofs among all known proof system). In case of Groth16, the circuit-specific 
setup is generated from an already existing universal setup and the circuit.


The files
---------

Using the popular [`circom`](https://docs.circom.io/) + [`snarkjs`](https://github.com/iden3/snarkjs) 
ecosystem, all the above parts are in different files, which are produced by
different commands.

These are:

- `.circom` files contain the source code of your circuit (both the equations and 
  the actual program, interleaved together)
- `input.json` contains the circuit inputs (both public and private inputs)
- `.r1cs` file contains the circuit, that is, the statement you want to prove
  (R1CS is short for Rank-1 Constraint System, a specific form of a "circuit").
  This is one output of the `circom` compiler, which reads the above source code and produces `.r1cs` files
- `.wtns` files contain the witness. This is generated by the so-called "witness generator", which is 
  another output of the `circom` compiler. Essentially the compiler separates the equations (goes into `.r1cs`)
  and running the program (goes into the witness generator, which in this case can be either a WASM or a C++ program).
  When you run the witness generator, it takes the `input.json` and produces the `.wtns` file
- `.ptau` files are containing universal trusted setups
- `.zkey` files are called a "prover key" (it contains everything the prover needs), and 
  in case of Groth16 it also corresponds to a circuit-specific trusted setup.
- the prover takes the `.zkey` and the `.wtns` files, and produces two outputs: a `proof.json` 
  containing the proof itself, and a `public.json` containing only the public input (so this
  is copied from `input.json`, but the latter also contains the private inputs)
- from the `.zkey` file, a "verification key" can be extracted (this is again `.json` file),
  which contains everything the verifier needs (in particular, it contains something like a 
  hash of the circuit. At least once this has to be checked against a source code, trusted setup
  and resulting `.zkey`, so that you actually know which statement you verify!
  However you don't want to do this agin and again, because the statement - the circuit - can be very big).
  In practice the verifier key is often hardcoded in the on-chain verifier contract.
- then finally the verifier takes this verification key, the proof file, and the public input file
  (all `.json` files here), and outputs either "valid" or "invalid"

The command line workflow of doing all this is described in the file
[README.md](README.md).
