#!/bin/bash

ORIG=`pwd`
mkdir -p build

cd ../reference/haskell
mkdir -p json
cabal v1-run cli/testMain.hs || { echo "ghc failed"; cd $ORIG; exit 101; }

mv json/input_example.json ${ORIG}/build/
mv json/slot_main.circom   ${ORIG}

cd ${ORIG}/build

NAME="slot_main"
circom ../${NAME}.circom --r1cs --wasm || { echo "circom failed"; cd $ORIG; exit 102; }

echo "generating witness... (WASM)"
cd ${NAME}_js
node generate_witness.js ${NAME}.wasm ../input_example.json ../${NAME}.wtns || { echo "witness gen failed"; cd $ORIG; exit 101; }
cd ..

echo "witness generation succeeded"

cd $ORIG
