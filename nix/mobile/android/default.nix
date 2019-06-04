{ config, stdenv, stdenvNoCC, callPackage,
  pkgs, mkFilter, androidenv, fetchurl, openjdk, nodejs, bash, gradle, perl, zlib,
  status-go, nodeProjectName, projectNodePackage, developmentNodePackages }:

with stdenv;

let
  androidComposition = androidenv.composeAndroidPackages {
    toolsVersion = "26.1.1";
    platformToolsVersion = "28.0.2";
    buildToolsVersions = [ "28.0.3" ];
    includeEmulator = false;
    platformVersions = [ "28" ];
    includeSources = false;
    includeDocs = false;
    includeSystemImages = false;
    systemImageTypes = [ "default" ];
    abiVersions = [ "armeabi-v7a" ];
    lldbVersions = [ "2.0.2558144" ];
    cmakeVersions = [ "3.6.4111459" ];
    includeNDK = true;
    ndkVersion = "19.2.5345600";
    useGoogleAPIs = false;
    useGoogleTVAddOns = false;
    includeExtras = [ "extras;android;m2repository" "extras;google;m2repository" ];
  };
  licensedAndroidEnv = callPackage ./licensed-android-sdk.nix { inherit androidComposition; };
  src =
    let path = ./../../..; # Import the root /android and /mobile_files folders clean of any build artifacts
    in builtins.path { # We use builtins.path so that we can name the resulting derivation, otherwise the name would be taken from the checkout directory, which is outside of our control
      inherit path;
      name = "status-react";
      filter =
        mkFilter {
          dirsToInclude = [ "android" "mobile_files" "packager" "resources" ];
          dirsToExclude = [ ".git" ".svn" "CVS" ".hg" ".gradle" "build" "intermediates" "libs" "obj" ];
          filesToInclude = [ ".babelrc" ];
          root = path;
        };
    };

  mavenAndNodeDeps = callPackage ./maven-and-npm-deps.nix { inherit stdenvNoCC gradle bash perl zlib src nodeProjectName androidEnvShellHook projectNodePackage developmentNodePackages status-go; };

  androidEnvShellHook = ''
    export JAVA_HOME="${openjdk}"
    export ANDROID_HOME="${licensedAndroidEnv}"
    export ANDROID_SDK_ROOT="$ANDROID_HOME"
    export ANDROID_NDK_ROOT="${androidComposition.androidsdk}/libexec/android-sdk/ndk-bundle"
    export ANDROID_NDK_HOME="$ANDROID_NDK_ROOT"
    export ANDROID_NDK="$ANDROID_NDK_ROOT"
    export PATH="$ANDROID_HOME/bin:$ANDROID_HOME/tools:$ANDROID_HOME/tools/bin:$ANDROID_HOME/platform-tools:$ANDROID_HOME/build-tools:$PATH"
  '';

in
  {
    inherit androidComposition;

    buildInputs = [ mavenAndNodeDeps.deps openjdk gradle ];
    shellHook =
      androidEnvShellHook + 
      (mavenAndNodeDeps.shellHook mavenAndNodeDeps)+ ''
      $STATUS_REACT_HOME/scripts/generate-keystore.sh

      $STATUS_REACT_HOME/nix/mobile/reset-node_modules.sh "${mavenAndNodeDeps.deps}" && \
      $STATUS_REACT_HOME/nix/mobile/android/fix-node_modules-permissions.sh

      if [ $? -ne 0 ]; then
        # HACK: Allow CI to clean node_modules, will need to rethink this later
        [ -n "$JENKINS_URL" ] && chmod -R u+w "$STATUS_REACT_HOME/node_modules"
        exit 1
      fi
    '';
  }
