#!/usr/bin/env node
// Copies the Rizzo C runtime headers from src/runtime into the extension's
// bundled runtime/ directory so they are included in the packaged VSIX.
const fs = require("fs");
const path = require("path");

const srcRuntime = path.resolve(__dirname, "../../src/runtime");
const destRuntime = path.resolve(__dirname, "../runtime");

if (!fs.existsSync(srcRuntime)) {
    console.error(`copy-runtime: source not found at ${srcRuntime}`);
    process.exit(1);
}

if (!fs.existsSync(destRuntime)) {
    fs.mkdirSync(destRuntime, { recursive: true });
}

const headers = fs.readdirSync(srcRuntime).filter(f => f.endsWith(".h"));

// Remove stale headers that no longer exist in the source.
for (const existing of fs.readdirSync(destRuntime)) {
    if (!headers.includes(existing)) {
        fs.unlinkSync(path.join(destRuntime, existing));
        console.log(`copy-runtime: removed stale ${existing}`);
    }
}

for (const header of headers) {
    const src = path.join(srcRuntime, header);
    const dest = path.join(destRuntime, header);
    fs.copyFileSync(src, dest);
    console.log(`copy-runtime: copied ${header}`);
}

console.log(`copy-runtime: done (${headers.length} headers)`);
