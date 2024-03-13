#!/bin/bash

source ./paths.sh
source ./cli_args.sh

# --- setup build directory ---

mkdir -p build
cd build

# --- generate the main component ---

${NIMCLI_DIR}/cli $CLI_ARGS -v --circom=${CIRCUIT_MAIN}.circom

# --- compile the circuit ---

CIRCUIT_INCLUDES="-l${CIRCUIT_LIB_DIR} -l${CIRCUIT_POS_DIR} -l${CIRCUIT_PRF_DIR}"
time circom --r1cs --wasm --O2 ${CIRCUIT_INCLUDES} ${CIRCUIT_MAIN}.circom

# --- circuit specific setup ---

start=`date +%s`

NODE_OPTIONS="--max-old-space-size=8192" snarkjs groth16 setup ${CIRCUIT_MAIN}.r1cs $PTAU_PATH ${CIRCUIT_MAIN}_0000.zkey
echo "some_entropy_75289v3b7rcawcsyiur" | \
NODE_OPTIONS="--max-old-space-size=8192" snarkjs zkey contribute ${CIRCUIT_MAIN}_0000.zkey ${CIRCUIT_MAIN}_0001.zkey --name="1st Contributor Name"

rm ${CIRCUIT_MAIN}_0000.zkey
mv ${CIRCUIT_MAIN}_0001.zkey ${CIRCUIT_MAIN}.zkey
snarkjs zkey export verificationkey ${CIRCUIT_MAIN}.zkey ${CIRCUIT_MAIN}_verification_key.json

end=`date +%s`
echo "The circuit specific setup took `expr $end - $start` seconds."

# --- finish the setup ---

cd $ORIG