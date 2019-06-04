{ stdenvNoCC,
  bash, status-go, zlib,
  src, nodeProjectName, projectNodePackage, developmentNodePackages }:

let
  deps = stdenvNoCC.mkDerivation {
    name = "patched-npm-modules";
    inherit src;
    nativeBuildInputs = builtins.attrValues developmentNodePackages;
    buildInputs = [ bash zlib ] ++ status-go.buildInputs-ios;
    buildPhases = [ "unpackPhase" "patchPhase" "installPhase" ];
    unpackPhase = ''
      runHook preUnpack

      cp -a $src/. .
      chmod u+w .

      # Copy fresh node_modules
      rm -rf ./node_modules
      mkdir -p ./node_modules
      cp -a ${projectNodePackage}/lib/node_modules/${nodeProjectName}/node_modules .

      # Adjust permissions
      chmod -R u+w .

      # Set up symlinks to mobile enviroment in project root 
      ln -sf ./mobile_files/package.json.orig package.json
      ln -sf ./mobile_files/metro.config.js

      runHook postUnpack
    '';
    patchPhase = ''
      runHook prePatch

      patchShebangs .

      # HACK: Run what would get executed in the `prepare` script (though index.js.flow will be missing)
      # Ideally we'd invoke `npm run prepare` instead, but that requires quite a few additional dependencies
      (cd ./node_modules/react-native-firebase && \
       chmod u+w -R . && \
       mkdir ./dist && \
       genversion ./src/version.js && \
       cp -R ./src/* ./dist && \
       chmod u-w -R .) || exit

      runHook postPatch
    '';
    installPhase = ''
      rm -rf $out
      mkdir -p $out
      cp -R node_modules/ $out
    '';

    # The ELF types are incompatible with the host platform, so let's not even try
    dontPatchELF = true;
    dontStripHost = true;
    noAuditTmpdir = true;

    # Take whole sources into consideration when calculating sha
    outputHashMode = "recursive";
    outputHashAlgo = "sha256";
  };

in {
  inherit deps;

  shellHook = ''
    ln -sf $STATUS_REACT_HOME/mobile_files/package.json.orig $STATUS_REACT_HOME/package.json
    ln -sf $STATUS_REACT_HOME/mobile_files/metro.config.js $STATUS_REACT_HOME/metro.config.js

    export PATH="$STATUS_REACT_HOME/node_modules/.bin:$PATH"
  '';
}
