{ lib }:

# This Nix expression allows filtering a local directory by specifying dirsToInclude, dirsToExclude and filesToInclude.
# It also filters out symlinks to result folders created by nix-build, as well as backup/swap/generated files

let
  mkFilter = { dirsToInclude, dirsToExclude, filesToInclude, root }: path: type:
    let
      inherit (lib) elem elemAt splitString;
      baseName = baseNameOf (toString path);
      subpath = elemAt (splitString "${toString root}/" path) 1;
      spdir = elemAt (splitString "/" subpath) 0;

    in lib.cleanSourceFilter path type && ((type != "directory" && (elem spdir filesToInclude)) || ((elem spdir dirsToInclude) && ! (
      # Filter out version control software files/directories
      (type == "directory" && (elem baseName dirsToExclude)) ||
      # Filter out editor backup / swap files.
      lib.hasSuffix "~" baseName ||
      builtins.match "^\\.sw[a-z]$" baseName != null ||
      builtins.match "^\\..*\\.sw[a-z]$" baseName != null ||

      # Filter out generated files.
      lib.hasSuffix ".o" baseName ||
      lib.hasSuffix ".so" baseName ||
      # Filter out nix-build result symlinks
      (type == "symlink" && lib.hasPrefix "result" baseName)
    )));

in mkFilter
