# The program `remove-go-references-to' created by this derivation replaces all
# references to Nix build paths in the specified files by a
# non-existent path (/build/eeee...).  This is useful for getting rid of
# paths that you know are not actually needed at runtime.

{ stdenv, writeScriptBin }:

writeScriptBin "remove-go-references-to" ''
#! ${stdenv.shell} -e

# Files to remove the references from
regions=()
for i in "$@"; do
    test ! -L "$i" -a -f "$i" && regions+=("$i")
done

sed -i -E "s|$TMPDIR/go-build[0-9]{9}|$TMPDIR/go-buildeeeeeeeee|g" "''${regions[@]}"
sed -i -E "s|$TMPDIR/gomobile-work-[0-9]{9}|$TMPDIR/gomobile-work-eeeeeeeee|g" "''${regions[@]}"
''
