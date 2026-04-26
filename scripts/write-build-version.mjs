import { readFileSync, writeFileSync } from "node:fs";

const path = "src/lib/build_version.ml";
let version = process.env.RIZZO_VERSION ?? "Issue caused by missing RIZZO_VERSION environment variable.\nPlease contact the maintainer.";

const source = readFileSync(path, "utf8");

const escaped = JSON.stringify(version).slice(1, -1);

writeFileSync(
  path,
  source.replace("##Build_version##", escaped),
  "utf8",
);