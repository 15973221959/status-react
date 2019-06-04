{ stdenv, target-os, fetchurl,
  nodeProjectName, nodejs }:

let
  # need to keep this value in sync with node_modules/realm/dependencies.list
  realm-version = "2.28.0";

  node-abi-version = assert ((stdenv.lib.versions.major (stdenv.lib.getVersion nodejs)) == "10"); "v64";
  platform = if builtins.currentSystem == "x86_64-darwin" then "darwin" else "linux";

  realm-module-path = "$out/lib/node_modules/${nodeProjectName}/node_modules/realm";
  # We download ${realm-pre-gyp-archive} in order to avoid having node-pre-gyp try to download these files on its own (which is disallowed by Nix)
  realm-pre-gyp = {
    url = "https://static.realm.io/node-pre-gyp/${realm-version}/realm-v${realm-version}-node-${node-abi-version}-${platform}-x64.tar.gz";
    sha256 = if builtins.currentSystem == "x86_64-darwin" then "12x7pnyc827814gfqww2ablsi38dyladqg7bqqdgba9q2nxky6jr" else "10mmxk0cvky82q915kcgqg7xsad7hy6750imljjfmibxlckdlsyd";
  };
  realm-pre-gyp-archive = fetchurl (realm-pre-gyp);

  # Path from node_modules/realm/package.json/binary.module_path
  realm-dest-dir = "${realm-module-path}/compiled/node-${node-abi-version}_${platform}_x64/realm.node";

in oldAttrs: if target-os == "ios" then {} else { # for pure builds, patch the sources so that we don't try any downloads
  reconstructLock = true;
  preRebuild = ''
    # Do not attempt to do any http calls!
    substituteInPlace ${realm-module-path}/scripts/download-realm.js \
      --replace "!shouldSkipAcquire(realmDir, requirements, options.force)" "false"

    mkdir -p ${realm-dest-dir}
    tar -xzf ${realm-pre-gyp-archive} -C ${realm-dest-dir}
 '';
}