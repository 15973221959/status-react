#!/usr/bin/env bash

#
# Run this file to regenerate the Nix files in ./node2nix.
# Prerequisites: Node, npm, and node2nix (installed with npm i -g https://github.com/svanderburg/node2nix)
#

GIT_ROOT=$(cd "${BASH_SOURCE%/*}" && git rev-parse --show-toplevel)
SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"
toolversion="${GIT_ROOT}/scripts/toolversion"
supplement_json=$SCRIPTPATH/StatusIm/package.json
input_json=$SCRIPTPATH/../../../mobile_files/package.json
node_env=$SCRIPTPATH/StatusIm/node-env.nix

mv $input_json.orig $input_json

node_required_version=$($toolversion node)
node_major_version=$(echo $node_required_version | cut -d. -f1,1)

cat << EOF > $supplement_json
[
  "node-pre-gyp"
]
EOF

node2nix --nodejs-${node_major_version} --bypass-cache \
         --input             $input_json \
         --supplement-input  $supplement_json \
         --supplement-output $SCRIPTPATH/StatusIm/supplement.nix \
         --output            $SCRIPTPATH/StatusIm/node-packages.nix \
         --composition       $SCRIPTPATH/StatusIm/default.nix \
         --node-env          $node_env

mv $input_json $input_json.orig
rm -f $supplement_json

development_json=$SCRIPTPATH/development/package.json
cat << EOF > $development_json
[
  {
    "flow-bin": "^0.80.0",
    "flow-copy-source": "^2.0.2",
    "genversion": "^2.1.0"
  }
]
EOF

node2nix --nodejs-${node_major_version} --development --bypass-cache \
         --input        $development_json \
         --output       $SCRIPTPATH/development/node-packages.nix \
         --composition  $SCRIPTPATH/development/default.nix \
         --node-env     $SCRIPTPATH/development/node-env.nix

rm -f $development_json