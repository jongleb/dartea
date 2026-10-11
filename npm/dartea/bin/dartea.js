#!/usr/bin/env node
import { spawnSync } from "node:child_process";
import { createRequire } from "node:module";

const platform = `${process.platform}-${process.arch}`;
const name = `@dartea/${platform}`;

const locate = () => {
  try {
    return createRequire(import.meta.url).resolve(`${name}/bin/dartea`);
  } catch {
    console.error(
      `dartea has no binary for ${platform}. The package ${name} is not installed: ` +
        "either this platform is not supported yet, or optional dependencies were skipped " +
        "(npm --omit=optional, --no-optional). Supported: darwin-arm64, darwin-x64, " +
        "linux-x64, linux-arm64 (glibc).",
    );
    process.exit(1);
  }
};

const run = spawnSync(locate(), process.argv.slice(2), { stdio: "inherit" });

if (run.error) throw run.error;
if (run.signal) process.kill(process.pid, run.signal);
process.exit(run.status ?? 1);
