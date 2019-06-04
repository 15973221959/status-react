#!/usr/bin/env bash

#
# This script takes a maven-inputs.txt file and builds a Nix expression that can be used by maven-repo-builder.nix to produce a path to a local Maven repository
#

function getSHA() {
  nix-prefetch-url "$1" 2> /dev/null
}

_tmp=$(mktemp)
sort $1 | uniq > $_tmp
jcenterUrl='https://jcenter.bintray.com'
googleUrl='https://dl.google.com/dl/android/maven2'
fabricUrl='https://maven.fabric.io/public'
echo "{}:

let
  jcenter = \"$jcenterUrl\";
  google = \"$googleUrl\";
  fabric-io = \"$fabricUrl\";

in {"
while read depurl
do
    if [ -n "$depurl" ]; then
        host='jcenter'
        prefix=$jcenterUrl
        if [[ $depurl = 'https://dl.google.com/dl/android/maven2'* ]]; then
            host='google'
            prefix=$googleUrl
        elif [[ $depurl = 'https://maven.fabric.io/public'* ]]; then
            host='fabric-io'
            prefix=$fabricUrl
        fi
        deppath="${depurl/$prefix\//}"
        pom_sha256=$(getSHA "$depurl.pom")
        jar_sha256=$(getSHA "$depurl.jar")
        type='jar'
        if [ -z "$jar_sha256" ]; then
            jar_sha256=$(getSHA "$depurl.aar")
            [ -n "$jar_sha256" ] && type='aar'
        fi

        if [ -z "$pom_sha256" ] && [ -z "$jar_sha256" ] && [ -z "$aar_sha256" ]; then
            echo "Warning: failed to download $depurl" > /dev/stderr
            echo "Exiting." > /dev/stderr
        fi

        echo "  \"$depurl\" = {
    host = $host;
    path = \"$deppath\";
    type = \"$type\";
    pom-sha256 = \"$pom_sha256\";
    jar-sha256 = \"$jar_sha256\";
  };"
    fi
done < $_tmp

rm $_tmp

echo "}"
