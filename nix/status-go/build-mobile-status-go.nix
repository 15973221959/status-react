
{ stdenv, utils, callPackage,
  buildGoPackage, go, gomobile, openjdk, xcodeWrapper, unzip, zip, androidPkgs }:

{ owner, repo, rev, version, goPackagePath, src, sha256, host,

  # mobile-only arguments
  goBuildFlags, goBuildLdFlags,
  config } @ args':

with stdenv;

let
  targetConfig = config;
  buildStatusGo = callPackage ./build-status-go.nix { inherit buildGoPackage go xcodeWrapper utils; };

  remove-go-references-to = callPackage ./remove-go-references-to.nix { };
  hostllvm = "${androidPkgs.ndk-bundle}/libexec/android-sdk/ndk-bundle/toolchains/llvm/prebuilt/${if isDarwin then "darwin" else "linux"}-x86_64";

  args = removeAttrs args' [ "config" "goBuildFlags" "goBuildLdFlags" ]; # Remove mobile-only arguments from args
  buildStatusGoMobileLib = buildStatusGo (args // {
    nativeBuildInputs = [ gomobile ] ++ lib.optional (targetConfig.name == "android") [ openjdk remove-go-references-to unzip zip ];

    buildMessage = "Building mobile library for ${targetConfig.name}";
    # Build mobile libraries
    # TODO: Manage to pass "-s -w -Wl,--build-id=none" to -ldflags. Seems to only accept a single flag
    buildPhase = ''
      GOPATH=${gomobile.dev}:$GOPATH \
      PATH=${lib.makeBinPath [ gomobile.bin ]}:$PATH \
      ${lib.concatStringsSep " " targetConfig.envVars} \
      gomobile bind ${goBuildFlags} -target=${targetConfig.name} ${lib.concatStringsSep " " targetConfig.gomobileExtraFlags} \
                    -o ${targetConfig.outputFileName} \
                    ${goBuildLdFlags} \
                    ${goPackagePath}/mobile
    '';

    installPhase = ''
      mkdir -p $out/lib
      mv ${targetConfig.outputFileName} $out/lib/
    '';

    # replace .note.go.buildid section and $TMPDIR/gomobile-work-xxxxxxxxx paths, since they make the build non-reproducible
    preFixup =
      let jni = "$out/lib/tmp/jni";
      in lib.optionalString (targetConfig.name == "android") ''
      aar=$(ls $out/lib/*.aar)
      if [ -f "$aar" ]; then
        mkdir -p $out/lib/tmp
        unzip $aar -d $out/lib/tmp

        # remove the references to build paths
        find $out -type f -exec ${remove-go-references-to}/bin/remove-go-references-to '{}' + || true

        # remove the ".note.go.buildid" and ".gnu.version_d" sections from libgojni.so files to help achieve reproducible builds 
        ${hostllvm}/aarch64-linux-android/bin/objcopy --remove-section ".note.go.buildid" --remove-section ".gnu.version_d" ${jni}/arm64-v8a/libgojni.so
        ${hostllvm}/arm-linux-androideabi/bin/objcopy --remove-section ".note.go.buildid" --remove-section ".gnu.version_d" ${jni}/armeabi-v7a/libgojni.so
        ${hostllvm}/i686-linux-android/bin/objcopy --remove-section ".note.go.buildid" --remove-section ".gnu.version_d" ${jni}/x86/libgojni.so
        ${hostllvm}/x86_64-linux-android/bin/objcopy --remove-section ".note.go.buildid" --remove-section ".gnu.version_d" ${jni}/x86_64/libgojni.so

        # reset the timestamps
        find $out/lib/tmp -exec touch --reference=$out/lib/tmp/classes.jar -h '{}' +

        pushd $out/lib/tmp
        zip -fr $aar
        popd
        rm -rf $out/lib/tmp
      else
        echo ".aar file not found!"
        exit 1
      fi
    '';

    outputs = [ "out" ];

    meta = {
      platforms = with lib.platforms; linux ++ darwin;
    };
  });

in buildStatusGoMobileLib