{}:

let
  jcenter = "https://jcenter.bintray.com";
  google = "https://dl.google.com/dl/android/maven2";
  fabric-io = "https://maven.fabric.io/public";

in {
  "https://jcenter.bintray.com/com/android/tools/annotations/24.3.1/annotations-24.3.1" = {
    host = jcenter;
    path = "com/android/tools/annotations/24.3.1/annotations-24.3.1";
    type = "jar";
    pom-sha256 = "063k7zqjs4zsjra611gck9l6j1xb6vz3mw4xdz2j3mdlwycwn23y";
    jar-sha256 = "123qw6v6w8cmznyf8lbfkn56209wzd7p24ddgs64wsp4dw47q16a";
  };
  "https://jcenter.bintray.com/com/android/tools/build/builder-model/1.3.1/builder-model-1.3.1" = {
    host = jcenter;
    path = "com/android/tools/build/builder-model/1.3.1/builder-model-1.3.1";
    type = "jar";
    pom-sha256 = "1ag6ngamvvz8z32796l3j2kaqcnx2lyap0339fzl540063iy3vvb";
    jar-sha256 = "0v29x4pvgyksqg6fsaxdz46846snm1va73cd2d85pjll4vncjrh1";
  };
  "https://jcenter.bintray.com/com/android/tools/build/builder-test-api/1.3.1/builder-test-api-1.3.1" = {
    host = jcenter;
    path = "com/android/tools/build/builder-test-api/1.3.1/builder-test-api-1.3.1";
    type = "jar";
    pom-sha256 = "005wia2cqs9p1bya48pyvlhag6sfmh19amdlld3bs02z64rf1z49";
    jar-sha256 = "065z0hh2cy2n0m25gx6hd7hvkf1c02kf3zin4mvk1idigl6c80xc";
  };
  "https://jcenter.bintray.com/com/android/tools/build/builder/1.3.1/builder-1.3.1" = {
    host = jcenter;
    path = "com/android/tools/build/builder/1.3.1/builder-1.3.1";
    type = "jar";
    pom-sha256 = "0d3h8z83a8pr8kcj0r4ar0w49s1gyg8s6k1k6j5sfghza4s7amkp";
    jar-sha256 = "1h1dc7nx2hfz6rw0si0k3hfjcwf26j7016d6vfxdrabavn27hwx7";
  };
  "https://jcenter.bintray.com/com/android/tools/build/gradle-core/1.3.1/gradle-core-1.3.1" = {
    host = jcenter;
    path = "com/android/tools/build/gradle-core/1.3.1/gradle-core-1.3.1";
    type = "jar";
    pom-sha256 = "03bmm01wyh05zpxanam9f9fnvkwj8q7jr2idz1vj62fazwv9n7da";
    jar-sha256 = "1jnqk68w67pypljqfda1b3l1nwdk6kf85vc71svkc0c81qmscq69";
  };
  "https://jcenter.bintray.com/com/android/tools/build/gradle/1.3.1/gradle-1.3.1" = {
    host = jcenter;
    path = "com/android/tools/build/gradle/1.3.1/gradle-1.3.1";
    type = "jar";
    pom-sha256 = "0s3kgm6q0n67n9ji3gxjr91vd1ssjj70vq03sjf0pb3vpwialamn";
    jar-sha256 = "0p3z1yl397hm9va0i049k09cvkpkp1hwqngpicwdm3j3a6cfbbbm";
  };
  "https://jcenter.bintray.com/com/android/tools/build/manifest-merger/24.3.1/manifest-merger-24.3.1" = {
    host = jcenter;
    path = "com/android/tools/build/manifest-merger/24.3.1/manifest-merger-24.3.1";
    type = "jar";
    pom-sha256 = "0rsi5yg6zkmkag9gzk4ygk1qlbs1fma9qqwy1x5ryjqnrhjlgk05";
    jar-sha256 = "1qy78vvpdz13ayrfv594gi276m48s14rj5rq42lpyfzjd9n81yl7";
  };
  "https://jcenter.bintray.com/com/android/tools/common/24.3.1/common-24.3.1" = {
    host = jcenter;
    path = "com/android/tools/common/24.3.1/common-24.3.1";
    type = "jar";
    pom-sha256 = "03gp1i0awzy0s327qkycma9v231id4fyz28izzjwsyyfbdxvwfka";
    jar-sha256 = "0yh7c9jpy8ry54s02xwy23dscfqcca17gj7vdzj9iyd5ixkzk7pl";
  };
  "https://jcenter.bintray.com/com/android/tools/ddms/ddmlib/24.3.1/ddmlib-24.3.1" = {
    host = jcenter;
    path = "com/android/tools/ddms/ddmlib/24.3.1/ddmlib-24.3.1";
    type = "jar";
    pom-sha256 = "0ghs0942dgyprfzmyx7h24c2sgrsrjhqjkjiyfxad6v1mnsclqy8";
    jar-sha256 = "0lmay6v7svqn2l97iwqhcc3sgs87pnhwc2p7293ykbr7gba9m5gz";
  };
  "https://jcenter.bintray.com/com/android/tools/dvlib/24.3.1/dvlib-24.3.1" = {
    host = jcenter;
    path = "com/android/tools/dvlib/24.3.1/dvlib-24.3.1";
    type = "jar";
    pom-sha256 = "1v97xla81338arh6nrc9bmbv8n35dcqkkfyqnb5yi09dqg9k16wr";
    jar-sha256 = "15r51gii0hx1fif8v3nwffh6fdjrc7vzzl8kp32mh3k2fjl8san4";
  };
  "https://jcenter.bintray.com/com/android/tools/external/lombok/lombok-ast/0.2.3/lombok-ast-0.2.3" = {
    host = jcenter;
    path = "com/android/tools/external/lombok/lombok-ast/0.2.3/lombok-ast-0.2.3";
    type = "jar";
    pom-sha256 = "1gbljyqw5r2vc31dl1flclsvdias3049pdrc6dicizcbvph92qcc";
    jar-sha256 = "0vg8w757n8a8v6lafrg4xb9nl7dqw3x56p1qzfsp20j395p0vss5";
  };
  "https://jcenter.bintray.com/com/android/tools/jack/jack-api/0.9.0/jack-api-0.9.0" = {
    host = jcenter;
    path = "com/android/tools/jack/jack-api/0.9.0/jack-api-0.9.0";
    type = "jar";
    pom-sha256 = "11cx90mikr26cvylgz0d1sdvcmc5dpp5g6alm8agv022wx7jf34j";
    jar-sha256 = "0ahl804jlvxr2h4ls6mbq3ym4k6mr5kx62hdigsfil7iglmj80mq";
  };
  "https://jcenter.bintray.com/com/android/tools/jill/jill-api/0.9.0/jill-api-0.9.0" = {
    host = jcenter;
    path = "com/android/tools/jill/jill-api/0.9.0/jill-api-0.9.0";
    type = "jar";
    pom-sha256 = "0ffa7a7dhm2dqckk3w0vllknsq5i2q6qng7srlisdl8g7na5svx3";
    jar-sha256 = "072gj0rkzxa77jhjs1bkzdfsmxpv57h5809yznrvqsav82nyy75q";
  };
  "https://jcenter.bintray.com/com/android/tools/layoutlib/layoutlib-api/24.3.1/layoutlib-api-24.3.1" = {
    host = jcenter;
    path = "com/android/tools/layoutlib/layoutlib-api/24.3.1/layoutlib-api-24.3.1";
    type = "jar";
    pom-sha256 = "1klb099wp98j55k0fwkncnlqr7fp7sy8jl0ibhxp2dsh9p0gp45b";
    jar-sha256 = "16aw3kkkhmn055dxnmhfvf31fi1x1zh3ksj3kb990rcjb23yc6vr";
  };
  "https://jcenter.bintray.com/com/android/tools/lint/lint-api/24.3.1/lint-api-24.3.1" = {
    host = jcenter;
    path = "com/android/tools/lint/lint-api/24.3.1/lint-api-24.3.1";
    type = "jar";
    pom-sha256 = "09czl4whxcdrgyk0k6q43j9lwzvrqbsxr6j46293hdglyayyypcw";
    jar-sha256 = "1j6x1ccnb53ihv0n1xn20ba5plnsywqwrvi17cj3aznmz97gzhf4";
  };
  "https://jcenter.bintray.com/com/android/tools/lint/lint-checks/24.3.1/lint-checks-24.3.1" = {
    host = jcenter;
    path = "com/android/tools/lint/lint-checks/24.3.1/lint-checks-24.3.1";
    type = "jar";
    pom-sha256 = "0r10z3ya4rc0c25prplxgmvpk0fpv5kfv09ajng9sdd61w8rkns6";
    jar-sha256 = "0w92dm31sxr3findkp58ar774vjrfn64j1bdg54f0j1j7acrxsz4";
  };
  "https://jcenter.bintray.com/com/android/tools/lint/lint/24.3.1/lint-24.3.1" = {
    host = jcenter;
    path = "com/android/tools/lint/lint/24.3.1/lint-24.3.1";
    type = "jar";
    pom-sha256 = "114mjyx7kabw4xnhn4gfps8gxiyk7nmhigdnfg3kl03lbg8qf9nj";
    jar-sha256 = "1pd71803sj1zi9nji2n1s9vhx6fkwbiqp3h5wr4g0kg80fw2ifvd";
  };
  "https://jcenter.bintray.com/com/android/tools/sdk-common/24.3.1/sdk-common-24.3.1" = {
    host = jcenter;
    path = "com/android/tools/sdk-common/24.3.1/sdk-common-24.3.1";
    type = "jar";
    pom-sha256 = "0w77vwybipx1g2q4xdbh3f3vn5kk0y1r4zmr9ywyzda62xhanfsw";
    jar-sha256 = "1lzykp6lqknxmlgxj4zq83dgj4m1x0cnsq0jw7mklwhmbghcc0n8";
  };
  "https://jcenter.bintray.com/com/android/tools/sdklib/24.3.1/sdklib-24.3.1" = {
    host = jcenter;
    path = "com/android/tools/sdklib/24.3.1/sdklib-24.3.1";
    type = "jar";
    pom-sha256 = "0lcsi0hqc697rmby7416v4glcq6b5biqrcran697ijdn71zqng2z";
    jar-sha256 = "0piy4k3ybxswl8y2d0mi4x8w6f2624mbn9nzd06vcj88vb8hmjsa";
  };
  "https://jcenter.bintray.com/com/google/code/gson/gson/2.2.4/gson-2.2.4" = {
    host = jcenter;
    path = "com/google/code/gson/gson/2.2.4/gson-2.2.4";
    type = "jar";
    pom-sha256 = "106k9ynbhls8nkihxrhkj5033z7q2am6x1l98vffck4935flv65f";
    jar-sha256 = "1yw7qszcw1dsh54q6wyksr5mbkz8mzs1q36hmjjn7qx9gk88qcn0";
  };
  "https://jcenter.bintray.com/com/google/guava/guava-parent/17.0/guava-parent-17.0" = {
    host = jcenter;
    path = "com/google/guava/guava-parent/17.0/guava-parent-17.0";
    type = "jar";
    pom-sha256 = "0ajka1xsh28sammy3zbx82mvxi3k19v5ks4x2xswglsiamc57flz";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/com/google/guava/guava/17.0/guava-17.0" = {
    host = jcenter;
    path = "com/google/guava/guava/17.0/guava-17.0";
    type = "jar";
    pom-sha256 = "0nxpdkb27m770j0c4xlg7l34aj87h7qja7dbmqrcf99q1l0ic39a";
    jar-sha256 = "1g7bhyvzsx61lrca01hvpivkdgjvgj1wy5qa0jwbdl0klq7ahdlc";
  };
  "https://jcenter.bintray.com/com/intellij/annotations/12.0/annotations-12.0" = {
    host = jcenter;
    path = "com/intellij/annotations/12.0/annotations-12.0";
    type = "jar";
    pom-sha256 = "11lhm19lbcscmfrq1qpwacp6njamfbrm7mkw6apc1q02vkh2vy7s";
    jar-sha256 = "0vgdfmihsggnbmcmrspf9ldll3knk5ayb43zc4pzx0709fqi7azq";
  };
  "https://jcenter.bintray.com/com/squareup/javawriter/2.5.0/javawriter-2.5.0" = {
    host = jcenter;
    path = "com/squareup/javawriter/2.5.0/javawriter-2.5.0";
    type = "jar";
    pom-sha256 = "1c57k79i2dcyjm63k8xjjr1ck3i0i4hkwsa7k72y1xbc27qxgaz1";
    jar-sha256 = "1w4p04j3z05k7ihb69pid3j4h4q8k0ipksp7rz9rgam01vxhkyzw";
  };
  "https://jcenter.bintray.com/commons-codec/commons-codec/1.4/commons-codec-1.4" = {
    host = jcenter;
    path = "commons-codec/commons-codec/1.4/commons-codec-1.4";
    type = "jar";
    pom-sha256 = "13s0k9nv1zbmppf5b5fwc4j0506cgm061rzcdgki780v89lh1wzm";
    jar-sha256 = "06v3ccc5srj9w2v459i5f5rc7j37b1a24n52a5bh78gkfi62793a";
  };
  "https://jcenter.bintray.com/commons-logging/commons-logging/1.1.1/commons-logging-1.1.1" = {
    host = jcenter;
    path = "commons-logging/commons-logging/1.1.1/commons-logging-1.1.1";
    type = "jar";
    pom-sha256 = "1s2yqfrwxa3xg0l2mkczh29gcii3xcjnb8lwvmxbk2sf0mny3wnh";
    jar-sha256 = "0brngjgq24kk2c5qxzp3k6dcrzy7bdfdd1h1symb638zmly92vyf";
  };
  "https://jcenter.bintray.com/net/sf/kxml/kxml2/2.3.0/kxml2-2.3.0" = {
    host = jcenter;
    path = "net/sf/kxml/kxml2/2.3.0/kxml2-2.3.0";
    type = "jar";
    pom-sha256 = "11d66hzaang99y3wkrqxzn2y2qgsg34jf3dvk5i9664m9rpn1kii";
    jar-sha256 = "1qixcdxqmw7ja9yhp0qw6g4y8jzjxwhk5igcwl6f3zd1g6gxsr7j";
  };
  "https://jcenter.bintray.com/net/sf/proguard/proguard-base/5.2.1/proguard-base-5.2.1" = {
    host = jcenter;
    path = "net/sf/proguard/proguard-base/5.2.1/proguard-base-5.2.1";
    type = "jar";
    pom-sha256 = "1hrsv9fkjyn11sbrizqzlyppckr2hfc271j5vzlbx10ddrsfr0bg";
    jar-sha256 = "1gzkcgg9aamxnda5vjzahaylv6dl0bbga55bi6vwkjsq7kbvsqzi";
  };
  "https://jcenter.bintray.com/net/sf/proguard/proguard-gradle/5.2.1/proguard-gradle-5.2.1" = {
    host = jcenter;
    path = "net/sf/proguard/proguard-gradle/5.2.1/proguard-gradle-5.2.1";
    type = "jar";
    pom-sha256 = "0q9d1ayqzcp1g7ypylk3xqnxii2q32dzq4km743f8cpaax7kspbg";
    jar-sha256 = "1fy7i6ddbd1qjlkz9yahgnymx6ja6lg84sihiqagsrgk0m54rc7g";
  };
  "https://jcenter.bintray.com/net/sf/proguard/proguard-parent/5.2.1/proguard-parent-5.2.1" = {
    host = jcenter;
    path = "net/sf/proguard/proguard-parent/5.2.1/proguard-parent-5.2.1";
    type = "jar";
    pom-sha256 = "01l30ijq5r8slw6jd9ya6v0lpyr09r3al1nd1a2wixfpmpd0499f";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/apache/apache/13/apache-13" = {
    host = jcenter;
    path = "org/apache/apache/13/apache-13";
    type = "jar";
    pom-sha256 = "07c4yg52q1qiz2b982pcsiwf9ahmpil4jy7lpqvi5m0z6sq3slgz";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/apache/apache/4/apache-4" = {
    host = jcenter;
    path = "org/apache/apache/4/apache-4";
    type = "jar";
    pom-sha256 = "152iri0kbrxir93w23b31045gpvh3g9rc37gxya27sx8dfi274wy";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/apache/commons/commons-compress/1.8.1/commons-compress-1.8.1" = {
    host = jcenter;
    path = "org/apache/commons/commons-compress/1.8.1/commons-compress-1.8.1";
    type = "jar";
    pom-sha256 = "1f4z7dc667b0d07s1yfiqhvbhbj37q00ihyxphm0nlp82kdpl4qh";
    jar-sha256 = "1gbzd7aang7xp3k98m3sbjs5d7nm6yqigg8znrncqvpq0dji7jjz";
  };
  "https://jcenter.bintray.com/org/apache/commons/commons-parent/11/commons-parent-11" = {
    host = jcenter;
    path = "org/apache/commons/commons-parent/11/commons-parent-11";
    type = "jar";
    pom-sha256 = "0am014qph77cw2n596d9z8g1naf5a191199zi9di0q1l75pk1q5r";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/apache/commons/commons-parent/33/commons-parent-33" = {
    host = jcenter;
    path = "org/apache/commons/commons-parent/33/commons-parent-33";
    type = "jar";
    pom-sha256 = "1yzqvnngasa9bqqc0hw2yzqcckkmqzdcrbwx6xpi1rg2a89h3l2k";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/apache/commons/commons-parent/5/commons-parent-5" = {
    host = jcenter;
    path = "org/apache/commons/commons-parent/5/commons-parent-5";
    type = "jar";
    pom-sha256 = "0am0vbkyp6b6srwhvw52xkc4ghaj4kqn0ay26vgag06z1g035mlb";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/apache/httpcomponents/httpclient/4.1.1/httpclient-4.1.1" = {
    host = jcenter;
    path = "org/apache/httpcomponents/httpclient/4.1.1/httpclient-4.1.1";
    type = "jar";
    pom-sha256 = "19z21zvaypyi1mg6vc3clmhmq6fq1ybng0pj0p7d95wa6kcigfmd";
    jar-sha256 = "0la7iqy72w09h6z8h9s09bnac2xj0l05mm1ql5nbyyb6ib82drga";
  };
  "https://jcenter.bintray.com/org/apache/httpcomponents/httpcomponents-client/4.1.1/httpcomponents-client-4.1.1" = {
    host = jcenter;
    path = "org/apache/httpcomponents/httpcomponents-client/4.1.1/httpcomponents-client-4.1.1";
    type = "jar";
    pom-sha256 = "0lra0a51ir2x7ww44qq4hmcjvrrc843ll4qmfxz6sllmx7jq8lj7";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/apache/httpcomponents/httpcomponents-client/4.1/httpcomponents-client-4.1" = {
    host = jcenter;
    path = "org/apache/httpcomponents/httpcomponents-client/4.1/httpcomponents-client-4.1";
    type = "jar";
    pom-sha256 = "09wq25fxxrsj8c01313w9f4wr95mpz9l4dnykjdqsv4cg54bc5aj";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/apache/httpcomponents/httpcomponents-core/4.1/httpcomponents-core-4.1" = {
    host = jcenter;
    path = "org/apache/httpcomponents/httpcomponents-core/4.1/httpcomponents-core-4.1";
    type = "jar";
    pom-sha256 = "1r2dyq3bgz1xamv3a167jd55in4z7vkd27zcvgh6s59xkkypyyag";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/apache/httpcomponents/httpcore/4.1/httpcore-4.1" = {
    host = jcenter;
    path = "org/apache/httpcomponents/httpcore/4.1/httpcore-4.1";
    type = "jar";
    pom-sha256 = "09vk7if9fwc6zsmr0912mgk57m23qwmmrx6ck6qgmjg973x6mj2g";
    jar-sha256 = "1kr95c4q7w32yk81klyj2plgjhc5s2l5fh0qdn66c92f3zjqvqrw";
  };
  "https://jcenter.bintray.com/org/apache/httpcomponents/httpmime/4.1/httpmime-4.1" = {
    host = jcenter;
    path = "org/apache/httpcomponents/httpmime/4.1/httpmime-4.1";
    type = "jar";
    pom-sha256 = "1lbkfv2qdkhsijfgswmn5gx15rs2hlz89wjk2rg6008hxidxjg6v";
    jar-sha256 = "1ahmkd6g4jw202ybwcsvs4z7ik9yph544fz4i9l4g2lf2ik9aqii";
  };
  "https://jcenter.bintray.com/org/apache/httpcomponents/project/4.1.1/project-4.1.1" = {
    host = jcenter;
    path = "org/apache/httpcomponents/project/4.1.1/project-4.1.1";
    type = "jar";
    pom-sha256 = "1v7qi1k2g9zbadaap6ln02p04hkika2nkbqigjik6kpmvx24vfr1";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/bouncycastle/bcpkix-jdk15on/1.48/bcpkix-jdk15on-1.48" = {
    host = jcenter;
    path = "org/bouncycastle/bcpkix-jdk15on/1.48/bcpkix-jdk15on-1.48";
    type = "jar";
    pom-sha256 = "0z91vn29rq51r1p14kndr1p47kb7xc01smd1p2p44ba8mjdgyz5z";
    jar-sha256 = "0dxss10nkpwyg5w1hyp5yd1vrk8m9p2id2x1d1wxgap5nhv36isk";
  };
  "https://jcenter.bintray.com/org/bouncycastle/bcprov-jdk15on/1.48/bcprov-jdk15on-1.48" = {
    host = jcenter;
    path = "org/bouncycastle/bcprov-jdk15on/1.48/bcprov-jdk15on-1.48";
    type = "jar";
    pom-sha256 = "10krh8bnjf2rypqqpfh4ik1jnqpbifcs4g8yakyb9hcf9bkjn4i9";
    jar-sha256 = "12129q8rmqwlvj6z4j0gc3w0hq5ccrkf2gdlsggp3iws7cp7wjw0";
  };
  "https://jcenter.bintray.com/org/eclipse/jdt/core/compiler/ecj/4.4.2/ecj-4.4.2" = {
    host = jcenter;
    path = "org/eclipse/jdt/core/compiler/ecj/4.4.2/ecj-4.4.2";
    type = "jar";
    pom-sha256 = "14aman9k2n7zpgab8y40f3rnqwwn822za8yhfndi4hbmjcia1a0g";
    jar-sha256 = "1lg6lfgh6izblc81rr0404vkbyl7cmpbx0q3dcmh3fmvahay4vid";
  };
  "https://jcenter.bintray.com/org/ow2/asm/asm-analysis/5.0.3/asm-analysis-5.0.3" = {
    host = jcenter;
    path = "org/ow2/asm/asm-analysis/5.0.3/asm-analysis-5.0.3";
    type = "jar";
    pom-sha256 = "1l570la07zr6ay1c9i5caglj029dyjwrv24zmn9n511xpjhh5z4z";
    jar-sha256 = "17c4j2r94xgbjh5wylgqdwv7fjr6w4jmbhinrmymb5ic8rijmyp8";
  };
  "https://jcenter.bintray.com/org/ow2/asm/asm-parent/5.0.3/asm-parent-5.0.3" = {
    host = jcenter;
    path = "org/ow2/asm/asm-parent/5.0.3/asm-parent-5.0.3";
    type = "jar";
    pom-sha256 = "0clf3252nhw52k34vhjsm4mj8kz9casvkc5lp3s4slwa2bsapvf2";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/ow2/asm/asm-tree/5.0.3/asm-tree-5.0.3" = {
    host = jcenter;
    path = "org/ow2/asm/asm-tree/5.0.3/asm-tree-5.0.3";
    type = "jar";
    pom-sha256 = "0djjrxa10ny1mv0sq5yh6zqq371174xjzrzbz14zy652qnkrr0b5";
    jar-sha256 = "0755acl6d70q49znzcy39c18vp7bivj80f8xr63lx5pr02a7lyil";
  };
  "https://jcenter.bintray.com/org/ow2/asm/asm/5.0.3/asm-5.0.3" = {
    host = jcenter;
    path = "org/ow2/asm/asm/5.0.3/asm-5.0.3";
    type = "jar";
    pom-sha256 = "18565g4264fv0d7hq9fq34dyrybxl5h9d5pis8a7ggk2zqznad3x";
    jar-sha256 = "1k265g2nq810yg7sb9h5f1kyp2f0m2z2mz8drkcxr3vv8f7ggi3i";
  };
  "https://jcenter.bintray.com/org/ow2/ow2/1.3/ow2-1.3" = {
    host = jcenter;
    path = "org/ow2/ow2/1.3/ow2-1.3";
    type = "jar";
    pom-sha256 = "1yr8hfx8gffpppa4ii6cvrsq029a6x8hzy7nsavxhs60s9kmq8ai";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/sonatype/oss/oss-parent/7/oss-parent-7" = {
    host = jcenter;
    path = "org/sonatype/oss/oss-parent/7/oss-parent-7";
    type = "jar";
    pom-sha256 = "0m4lallnlhyfj3z24ispxzwvsxzaznhw6zsmk4j74sibr5kqh7xm";
    jar-sha256 = "";
  };
}
