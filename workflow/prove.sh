#!/bin/bash

source ./paths.sh
source ./cli_args.sh

# --- setup build directory ---

mkdir -p build 
cd build

# --- export the witness computation graph ---

if command -v build-circuit
then
  CIRCUIT_INCLUDES="-l${CIRCUIT_LIB_DIR} -l${CIRCUIT_POS_DIR} -l${CIRCUIT_PRF_DIR}"
  build-circuit ${CIRCUIT_MAIN}.circom ${CIRCUIT_MAIN}_graph.bin ${CIRCUIT_INCLUDES}
else
  echo " "
  echo "\`circom-witnesscalc\` not found; skipping graph extraction"
fi

# --- generate input for the circuit ---

echo ""
echo "generating the input for the proof circuit..."
${NIMCLI_DIR}/cli $CLI_ARGS -v --output=input.json

# --- generate the witness ---

start=`date +%s`
echo ""
echo "generating the witness..."
cd ${CIRCUIT_MAIN}_js
time node generate_witness.js ${CIRCUIT_MAIN}.wasm ../input.json ../witness.wtns
cd ${ORIG}/build
end=`date +%s`
echo "Generating the witness took `expr $end - $start` seconds."

# --- create the proof ---

PROVER="snarkjs"

RS=`which rapidsnark`
if [[ ! -z "$RS" ]]
then
  PROVER="rapidsnark"
fi

# PROVER="zikkurat"
PROVER="nim"

echo ""
echo "creating the proof... using prover: \`$PROVER\`"

start=`date +%s`
case $PROVER in
  snarkjs)
    time snarkjs groth16 prove ${CIRCUIT_MAIN}.zkey witness.wtns proof.json public.json
    ;;
  rapidsnark)
    time rapidsnark ${CIRCUIT_MAIN}.zkey witness.wtns proof.json public.json
    ;;
  nim)
    time nim-groth16 -tpv --zkey=${CIRCUIT_MAIN}.zkey --wtns=witness.wtns -o=proof.json -i=public.json
    ;;
  zikkurat)
    time zikkurat-groth16 -tpv --zkey=${CIRCUIT_MAIN}.zkey --wtns=witness.wtns # -o=proof.json -i=public.json
    ;;
  *)
    echo "unknown prover \`$PROVER\`"
    exit 99
    ;;
esac
end=`date +%s`
echo "Creating the proof took `expr $end - $start` seconds."

# --- verify the proof ---

echo ""
echo "verifying the proof:"
snarkjs groth16 verify ${CIRCUIT_MAIN}_verification_key.json public.json proof.json

# --- create solidity verifier contract ---

echo ""
echo "creating solidity verifier contract:"
snarkjs zkey export solidityverifier ${CIRCUIT_MAIN}.zkey verifier.sol

# --- finish  ---

cd $ORIG
