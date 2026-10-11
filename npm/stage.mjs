import { chmod, copyFile, mkdir, readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

const [version, binaries, out] = process.argv.slice(2);

if (!version || !binaries || !out) {
  console.error("usage: node npm/stage.mjs VERSION BINARIES_DIR OUT_DIR");
  process.exit(2);
}

const platforms = [
  { name: "darwin-arm64", os: "darwin", cpu: "arm64" },
  { name: "darwin-x64", os: "darwin", cpu: "x64" },
  { name: "linux-x64", os: "linux", cpu: "x64", libc: "glibc" },
  { name: "linux-arm64", os: "linux", cpu: "arm64", libc: "glibc" },
];

const here = new URL(".", import.meta.url).pathname;
const main = JSON.parse(await readFile(join(here, "dartea/package.json"), "utf8"));

const copyLicences = (folder) =>
  Promise.all(
    ["LICENSE", "NOTICE"].map((file) => copyFile(join(here, "..", file), join(folder, file))),
  );

const stagePlatform = async ({ name, os, cpu, libc }) => {
  const folder = join(out, name);
  await mkdir(join(folder, "bin"), { recursive: true });
  const binary = join(folder, "bin/dartea");
  await copyFile(join(binaries, `dartea-${name}`), binary);
  await chmod(binary, 0o755);
  await copyLicences(folder);
  const manifest = {
    name: `@dartea/${name}`,
    version,
    description: `The dartea compiler binary for ${name}.`,
    homepage: main.homepage,
    repository: main.repository,
    license: main.license,
    os: [os],
    cpu: [cpu],
    ...(libc ? { libc: [libc] } : {}),
    files: ["bin", "NOTICE"],
  };
  await writeFile(join(folder, "package.json"), `${JSON.stringify(manifest, null, 2)}\n`);
  return folder;
};

const stageMain = async () => {
  const folder = join(out, "dartea");
  await mkdir(join(folder, "bin"), { recursive: true });
  await copyFile(join(here, "dartea/bin/dartea.js"), join(folder, "bin/dartea.js"));
  await chmod(join(folder, "bin/dartea.js"), 0o755);
  await copyLicences(folder);
  await copyFile(join(here, "..", "README.md"), join(folder, "README.md"));
  const manifest = {
    ...main,
    version,
    optionalDependencies: Object.fromEntries(
      platforms.map(({ name }) => [`@dartea/${name}`, version]),
    ),
  };
  await writeFile(join(folder, "package.json"), `${JSON.stringify(manifest, null, 2)}\n`);
  return folder;
};

const staged = [...(await Promise.all(platforms.map(stagePlatform))), await stageMain()];
console.log(staged.join("\n"));
