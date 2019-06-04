{ config, stdenv, stdenvNoCC, callPackage,
  pkgs, xcodeWrapper, mkFilter, fetchurl, nodejs, bash, zlib,
  status-go, nodeProjectName, projectNodePackage, developmentNodePackages }:

with stdenv;

let
  src =
    let path = ./../../..;
    in builtins.path { # We use builtins.path so that we can name the resulting derivation, otherwise the name would be taken from the checkout directory, which is outside of our control
      inherit path;
      name = "status-react";
      filter =
        mkFilter {
          dirsToInclude = [ "mobile_files" ];
          dirsToExclude = [ ".git" ".svn" "CVS" ".hg" ];
          filesToInclude = [ ".babelrc" ];
          root = path;
        };
    };

  npmDeps = callPackage ./npm-deps.nix { inherit stdenvNoCC bash zlib src nodeProjectName projectNodePackage developmentNodePackages status-go; };

in {
  buildInputs = [ npmDeps.deps xcodeWrapper ];
  shellHook =
    npmDeps.shellHook + ''
    $STATUS_REACT_HOME/nix/mobile/reset-node_modules.sh "${npmDeps.deps}" && \
    $STATUS_REACT_HOME/nix/mobile/ios/fix-node_modules-permissions.sh && \
    $STATUS_REACT_HOME/nix/mobile/ios/install-pods-and-status-go.sh

    if [ $? -ne 0 ]; then
      # HACK: Allow CI to clean node_modules, will need to rethink this later
      [ -n "$JENKINS_URL" ] && chmod -R u+w "$STATUS_REACT_HOME/node_modules"
      exit 1
    fi
  '';
}
