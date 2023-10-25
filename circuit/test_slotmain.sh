#!/bin/bash

ORIG=`pwd`

cd ../reference/haskell
runghc testMain.hs || { echo "ghc failed"; exit 101; }

mv input_example.json ${ORIG}/build/
mv slot_main.circom   ${ORIG}

cd ${ORIG}/build

NAME="slot_main"
circom ../${NAME}.circom --r1cs --wasm || { echo "circom failed"; exit 102; }

echo "generating witness... (WASM)"
cd ${NAME}_js
node generate_witness.js ${NAME}.wasm ../input_example.json ../${NAME}_witness.wtns || { echo "witness gen failed"; exit 101; }
cd ..

cd $ORIG
