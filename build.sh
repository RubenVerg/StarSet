#!/usr/bin/env bash

set -e

rm -rf dist
mkdir dist

out_path=$(wasm32-wasi-cabal list-bin starset-client | tail -n1)

wasm32-wasi-cabal build starset-client
cp $out_path dist/starset.wasm
"$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs --input "$out_path" --output dist/ghc_wasm_jsffi.js
cp static/* dist
