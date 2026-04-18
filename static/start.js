import { WASI, OpenFile, File, ConsoleStdout } from 'https://unpkg.com/@bjorn3/browser_wasi_shim@0.4.1/dist/index.js';
import ghc_wasm_jsffi from './ghc_wasm_jsffi.js';

const files = [
	new OpenFile(new File([], {})), // stdin
	ConsoleStdout.lineBuffered(msg => console.log(`[WASI] ${msg}`)), // stdout
	ConsoleStdout.lineBuffered(msg => console.warn(`[WASI] ${msg}`)), // stderr
];
const options = { debug: false };
const args = [];
const env = ["GHCRTS=-H64m"];
const wasi = new WASI(args, env, files, options);

const instanceExports = {};
const url = 'resolve' in import.meta ? import.meta.resolve('./starset.wasm') : './starset.wasm';
const { instance } = await WebAssembly.instantiateStreaming(fetch(url), {
	wasi_snapshot_preview1: wasi.wasiImport,
	ghc_wasm_jsffi: ghc_wasm_jsffi(instanceExports),
});
Object.assign(instanceExports, instance.exports);

wasi.initialize(instance);
const exports = instance.exports;
await exports.hs_start();