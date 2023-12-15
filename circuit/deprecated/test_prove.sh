#!/bin/bash

ORIG=`pwd`

PTAU_DIR="/Users/bkomuves/zk/ptau/"
PTAU_FILE="${PTAU_DIR}/powersOfTau28_hez_final_20.ptau"

NAME="slot_main"

# --- setup ---

cd $ORIG/build
echo "circuit-specific ceremony..."
snarkjs groth16 setup ${NAME}.r1cs ${PTAU_FILE} ${NAME}_0000.zkey
echo "some_entropy_xxx" | snarkjs zkey contribute ${NAME}_0000.zkey ${NAME}_0001.zkey --name="1st Contributor Name"
rm ${NAME}_0000.zkey
mv ${NAME}_0001.zkey ${NAME}.zkey
snarkjs zkey export verificationkey ${NAME}.zkey ${NAME}_verification_key.json

# --- prove ---

echo ""
echo "trying to prove... (with snarkjs)"

cd $ORIG/build
time snarkjs groth16 prove ${NAME}.zkey ${NAME}.wtns ${NAME}_proof.json ${NAME}_public.json

# --- verify ---

echo ""
echo "verifyng proof..."
snarkjs groth16 verify ${NAME}_verification_key.json ${NAME}_public.json ${NAME}_proof.json

cd $ORIG
cd $ORIG
