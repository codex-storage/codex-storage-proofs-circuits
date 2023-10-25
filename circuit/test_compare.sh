#!/bin/bash

NAME="main_test_compare"
INPUT="input_test_compare"

cd build
circom ../${NAME}.circom --r1cs --wasm || { echo "circom failed"; exit 102; }

echo "{ \"A\":[1,1,1,0,1,1,0,1], \"B\":[1,1,1,0,1,1,0,1] }" >${INPUT}.json

echo "generating witness... (WASM)"
cd ${NAME}_js
node generate_witness.js ${NAME}.wasm ../${INPUT}.json ../${NAME}_witness.wtns || { echo "witness gen failed"; exit 101; }
cd ..
