{}:

let
  jcenter = "https://jcenter.bintray.com";
  google = "https://dl.google.com/dl/android/maven2";
  fabric-io = "https://maven.fabric.io/public";

in {
  "https://jcenter.bintray.com/com/android/databinding/baseLibrary/1.0-rc5/baseLibrary-1.0-rc5" = {
    host = jcenter;
    path = "com/android/databinding/baseLibrary/1.0-rc5/baseLibrary-1.0-rc5";
    type = "jar";
    pom-sha256 = "0w80l57rlyjbdadwav8vwacanymvhimxzwirmgpbhyd1073s0hha";
    jar-sha256 = "18fkkkdy3qp7ixa10az5jmqf4x9vk2gqg6r6xqp8lid2c07w6hzc";
  };
  "https://jcenter.bintray.com/com/android/databinding/compilerCommon/1.0-rc5/compilerCommon-1.0-rc5" = {
    host = jcenter;
    path = "com/android/databinding/compilerCommon/1.0-rc5/compilerCommon-1.0-rc5";
    type = "jar";
    pom-sha256 = "1bxfnqb8pvc9hlk7mxnqy8am6lin9wxqn86irz1k47fc9j3gjjpp";
    jar-sha256 = "0ks2a7dff0wfihmcvircr9lkfc7j7g5cgprp021hqgk2hgij5iic";
  };
  "https://jcenter.bintray.com/com/android/tools/annotations/24.5.0/annotations-24.5.0" = {
    host = jcenter;
    path = "com/android/tools/annotations/24.5.0/annotations-24.5.0";
    type = "jar";
    pom-sha256 = "0m2wj96mar7cij4yzvcx1da9ab7ffcaa1s8mcgl9z242h7srpjf3";
    jar-sha256 = "08nrgm8j7q49ds9r0k1cbn1jsxdv0if9bhi37szxpjz7ks7azyr9";
  };
  "https://jcenter.bintray.com/com/android/tools/build/builder-model/1.5.0/builder-model-1.5.0" = {
    host = jcenter;
    path = "com/android/tools/build/builder-model/1.5.0/builder-model-1.5.0";
    type = "jar";
    pom-sha256 = "0mz3dq6in417r7rlads3pzkk0427yz9fpvw3j2l1vcgsm0mgy1yg";
    jar-sha256 = "0mnl8znv1m6mfp67mlyjypvnsad2zpn18rj1wkccd9964kgk6jj0";
  };
  "https://jcenter.bintray.com/com/android/tools/build/builder-test-api/1.5.0/builder-test-api-1.5.0" = {
    host = jcenter;
    path = "com/android/tools/build/builder-test-api/1.5.0/builder-test-api-1.5.0";
    type = "jar";
    pom-sha256 = "0a40zglgq1pddacrch3w2sqmdrm3ykq32n3ifk17nr1z7xph9ycd";
    jar-sha256 = "0zvlzsdkj6h897124mv7x8ckf9wh51gfkl2y3i33r4pa6yjahxlh";
  };
  "https://jcenter.bintray.com/com/android/tools/build/builder/1.5.0/builder-1.5.0" = {
    host = jcenter;
    path = "com/android/tools/build/builder/1.5.0/builder-1.5.0";
    type = "jar";
    pom-sha256 = "1q7ad0rraq0k3n3n1ac037qbwl6i62x51zvr2jhp369h9wh8br5j";
    jar-sha256 = "03kcavz74wk8wvk2g4a4l42sj7nifg17nsacsrkgpnr82fz573dp";
  };
  "https://jcenter.bintray.com/com/android/tools/build/gradle-core/1.5.0/gradle-core-1.5.0" = {
    host = jcenter;
    path = "com/android/tools/build/gradle-core/1.5.0/gradle-core-1.5.0";
    type = "jar";
    pom-sha256 = "0dbfh9zk4si34i7s9zjz1dd0q8fqvaj335kbh27s17wj7mi6h87l";
    jar-sha256 = "0x4n6kpswqdaf4dw0w3iv1ahg6dmydvmnb07rl1gax4a8lpx1v68";
  };
  "https://jcenter.bintray.com/com/android/tools/build/gradle/1.5.0/gradle-1.5.0" = {
    host = jcenter;
    path = "com/android/tools/build/gradle/1.5.0/gradle-1.5.0";
    type = "jar";
    pom-sha256 = "1lfyn5mpv1iyfb0kff869hp2dvpf45y28v1lylva6im1nk9wja1s";
    jar-sha256 = "0yq6kw511hrcdr0383ar0lxjr0r247m10j1902qs3y5k15kmp8fd";
  };
  "https://jcenter.bintray.com/com/android/tools/build/manifest-merger/24.5.0/manifest-merger-24.5.0" = {
    host = jcenter;
    path = "com/android/tools/build/manifest-merger/24.5.0/manifest-merger-24.5.0";
    type = "jar";
    pom-sha256 = "0xmzf5s7y22h8qbdk9jfr41yyf166v50d60dmcj2ns2xkz485lnk";
    jar-sha256 = "0smk0lrn6cl4rknhhbv8vj67rlqwrvbn5c07prgdnlbxv9r4qkwr";
  };
  "https://jcenter.bintray.com/com/android/tools/build/transform-api/1.5.0/transform-api-1.5.0" = {
    host = jcenter;
    path = "com/android/tools/build/transform-api/1.5.0/transform-api-1.5.0";
    type = "jar";
    pom-sha256 = "1mclffhshq5llxv7d9ri4667rm6xsgc7qb6js4w8zfvydraisy31";
    jar-sha256 = "09knq0fpnal7a98x2bmv4ip5r486vnbi4npxy38p582rqa3dgd5f";
  };
  "https://jcenter.bintray.com/com/android/tools/common/24.5.0/common-24.5.0" = {
    host = jcenter;
    path = "com/android/tools/common/24.5.0/common-24.5.0";
    type = "jar";
    pom-sha256 = "1g3dw2vv5jpsw6hzdczq777hxcf93svqhg3h1dk203v9fsq0jld4";
    jar-sha256 = "1csmfwm5f02bdx9nmcr5pngf7si22608bymsm10raham3gdw540a";
  };
  "https://jcenter.bintray.com/com/android/tools/ddms/ddmlib/24.5.0/ddmlib-24.5.0" = {
    host = jcenter;
    path = "com/android/tools/ddms/ddmlib/24.5.0/ddmlib-24.5.0";
    type = "jar";
    pom-sha256 = "1qap223ph5mpndck080q2kp7r74w78pal9hsmka6al8yx2c4pys9";
    jar-sha256 = "05r4fsjafhsqln5lblgzya9s6a5a9h28lb24w2ajs9ikr4qa73gm";
  };
  "https://jcenter.bintray.com/com/android/tools/dvlib/24.5.0/dvlib-24.5.0" = {
    host = jcenter;
    path = "com/android/tools/dvlib/24.5.0/dvlib-24.5.0";
    type = "jar";
    pom-sha256 = "05ljif0lfvviz7c36m9xw2v5h16ylc7hz0v0wi8h01rxh5ajlz36";
    jar-sha256 = "1zdnbx8mwxxh9g5894ly1sz4wfdnl63bppx0f4naq0ip7ahgcwvi";
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
  "https://jcenter.bintray.com/com/android/tools/layoutlib/layoutlib-api/24.5.0/layoutlib-api-24.5.0" = {
    host = jcenter;
    path = "com/android/tools/layoutlib/layoutlib-api/24.5.0/layoutlib-api-24.5.0";
    type = "jar";
    pom-sha256 = "1hagbs722dgnwagxgwazggzp5y8wzgiz89qgw14yd3b1zsvwzv7c";
    jar-sha256 = "03dmpkz64xb2fmca8gw67vbvlpjvdbv0rs5hsymk90rj2cqz66mv";
  };
  "https://jcenter.bintray.com/com/android/tools/lint/lint-api/24.5.0/lint-api-24.5.0" = {
    host = jcenter;
    path = "com/android/tools/lint/lint-api/24.5.0/lint-api-24.5.0";
    type = "jar";
    pom-sha256 = "0yqmk5n9mljw2781r9mihfnm3322wfn22hyhc8pvik25qlyhy9kj";
    jar-sha256 = "150h0x0yrf4gw325qlml4nalxm85ci2cr3gjini8sjljdhiaffp6";
  };
  "https://jcenter.bintray.com/com/android/tools/lint/lint-checks/24.5.0/lint-checks-24.5.0" = {
    host = jcenter;
    path = "com/android/tools/lint/lint-checks/24.5.0/lint-checks-24.5.0";
    type = "jar";
    pom-sha256 = "02kfbr9hh29gx4brc7zlcv2x6pvbh5di5dfm4ir3dn4bi328h7dg";
    jar-sha256 = "0iny9gdiw3l8x909g670ph4hl30crl1mmrqbsiacm7qb861v5msr";
  };
  "https://jcenter.bintray.com/com/android/tools/lint/lint/24.5.0/lint-24.5.0" = {
    host = jcenter;
    path = "com/android/tools/lint/lint/24.5.0/lint-24.5.0";
    type = "jar";
    pom-sha256 = "0qgbh875q595zc72ka74p3dmbkrgcgcn8yc63qmvqxb4p9y7j9yb";
    jar-sha256 = "09ql9nn4l4v7rvl7qdww9klwaml2dazfixisf31lvs58vmd3yw1p";
  };
  "https://jcenter.bintray.com/com/android/tools/sdk-common/24.5.0/sdk-common-24.5.0" = {
    host = jcenter;
    path = "com/android/tools/sdk-common/24.5.0/sdk-common-24.5.0";
    type = "jar";
    pom-sha256 = "0qm1q8yfs2m5iply1wkamszgqap989m9hsq10x1rz1xav7wf5iq9";
    jar-sha256 = "09p26l9i7qmzhvk6jy2lk7z0v08p9ak1n2bc0902jbns3av2c9n7";
  };
  "https://jcenter.bintray.com/com/android/tools/sdklib/24.5.0/sdklib-24.5.0" = {
    host = jcenter;
    path = "com/android/tools/sdklib/24.5.0/sdklib-24.5.0";
    type = "jar";
    pom-sha256 = "1sgcrp7ahzz9rn5f64zbjf75jfwajranzjinnq9qjch5ibpdap1g";
    jar-sha256 = "1w95kzmspw692crs1w8pc4bpn35jpmjdwkbbvw772kjs3k6simhn";
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
  "https://jcenter.bintray.com/com/googlecode/juniversalchardet/juniversalchardet/1.0.3/juniversalchardet-1.0.3" = {
    host = jcenter;
    path = "com/googlecode/juniversalchardet/juniversalchardet/1.0.3/juniversalchardet-1.0.3";
    type = "jar";
    pom-sha256 = "0sq85z68pcf87j9vcis5garlxl325lz0q01skcm69kf76ndkjikq";
    jar-sha256 = "0xjakmvlyl4i2lwb1prc19vx0mbbgpb6rhlxwx8vdf4kc68gwyvm";
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
  "https://jcenter.bintray.com/com/tunnelvisionlabs/antlr4-annotations/4.5/antlr4-annotations-4.5" = {
    host = jcenter;
    path = "com/tunnelvisionlabs/antlr4-annotations/4.5/antlr4-annotations-4.5";
    type = "jar";
    pom-sha256 = "1agr98zh9c7yjji5a5i99bcbdjx2zs7jj2vzj8lk8ms7rxia2l8s";
    jar-sha256 = "1h49j3bdmlrzfznwqg20bf1xx94ba602wnd94hk40lcxfs0v8b8b";
  };
  "https://jcenter.bintray.com/com/tunnelvisionlabs/antlr4-master/4.5/antlr4-master-4.5" = {
    host = jcenter;
    path = "com/tunnelvisionlabs/antlr4-master/4.5/antlr4-master-4.5";
    type = "jar";
    pom-sha256 = "0cyhq62xqhlkivm5h29j4b48zcaglpmgdwfrg2jlqvvjax0h7lmv";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/com/tunnelvisionlabs/antlr4-runtime/4.5/antlr4-runtime-4.5" = {
    host = jcenter;
    path = "com/tunnelvisionlabs/antlr4-runtime/4.5/antlr4-runtime-4.5";
    type = "jar";
    pom-sha256 = "17hpdp5821c4hf22alm4j4slv42nlifc5f5y6qlis1i01scw1bpw";
    jar-sha256 = "1gfp423s00i5iaflhsyw51jllm4h56r5pgkrkrpvykyis9vrcwdq";
  };
  "https://jcenter.bintray.com/com/tunnelvisionlabs/antlr4/4.5/antlr4-4.5" = {
    host = jcenter;
    path = "com/tunnelvisionlabs/antlr4/4.5/antlr4-4.5";
    type = "jar";
    pom-sha256 = "1fq2v58lf2c4hqxdm3kvpf9h3p8x9p00sjh7qfipsj04hz7r2y1z";
    jar-sha256 = "0vlqbcihfsgvkzsczp0h6ynz7wnf2d88kvhqfbn1hibry1y7h9wh";
  };
  "https://jcenter.bintray.com/commons-codec/commons-codec/1.4/commons-codec-1.4" = {
    host = jcenter;
    path = "commons-codec/commons-codec/1.4/commons-codec-1.4";
    type = "jar";
    pom-sha256 = "13s0k9nv1zbmppf5b5fwc4j0506cgm061rzcdgki780v89lh1wzm";
    jar-sha256 = "06v3ccc5srj9w2v459i5f5rc7j37b1a24n52a5bh78gkfi62793a";
  };
  "https://jcenter.bintray.com/commons-io/commons-io/2.4/commons-io-2.4" = {
    host = jcenter;
    path = "commons-io/commons-io/2.4/commons-io-2.4";
    type = "jar";
    pom-sha256 = "1wiikf78kr9k4pk68hb9jb9whrv19w8ir2kgxckad3wrrx3dvddj";
    jar-sha256 = "108mw2v8ncig29kjvzh8wi76plr01f4x5l3b1929xk5a7vf42snc";
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
  "https://jcenter.bintray.com/org/abego/treelayout/org.abego.treelayout.core/1.0.1/org.abego.treelayout.core-1.0.1" = {
    host = jcenter;
    path = "org/abego/treelayout/org.abego.treelayout.core/1.0.1/org.abego.treelayout.core-1.0.1";
    type = "jar";
    pom-sha256 = "04gay2b7ma8jggas3z4l0cm6j03cm4b3azr89w6l8w79ck5bz33w";
    jar-sha256 = "1l5ihcxdr8ci4bcgh5bzpcfsin40fzwpiyhna88qqw6c39iy7fc2";
  };
  "https://jcenter.bintray.com/org/antlr/ST4/4.0.8/ST4-4.0.8" = {
    host = jcenter;
    path = "org/antlr/ST4/4.0.8/ST4-4.0.8";
    type = "jar";
    pom-sha256 = "0z610q9vn39vf408p0110imzy08r6jgcl16llcxynx0iqzg9021w";
    jar-sha256 = "0fvszknribdgm98s5rllj1sw0l2ayvh6in1zk6sv0x4z1k2apjjq";
  };
  "https://jcenter.bintray.com/org/antlr/antlr-master/3.5.2/antlr-master-3.5.2" = {
    host = jcenter;
    path = "org/antlr/antlr-master/3.5.2/antlr-master-3.5.2";
    type = "jar";
    pom-sha256 = "1mhv6gqiwdrgypibcwxxhbrfh25rqc1a06jlkfq0w4553r9imna2";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/antlr/antlr-runtime/3.5.2/antlr-runtime-3.5.2" = {
    host = jcenter;
    path = "org/antlr/antlr-runtime/3.5.2/antlr-runtime-3.5.2";
    type = "jar";
    pom-sha256 = "09bzm8h181dj9jh53j19hf89k47lgw4sb9sa2bbjpcdq1chc5aa6";
    jar-sha256 = "0d47khwbkhvkzvk1h7hb7p6xjwnja3ijrfywrniyjf8gn7nchgyf";
  };
  "https://jcenter.bintray.com/org/antlr/antlr/3.5.2/antlr-3.5.2" = {
    host = jcenter;
    path = "org/antlr/antlr/3.5.2/antlr-3.5.2";
    type = "jar";
    pom-sha256 = "1r12z9l1jjqs7ghmww2rjhqxsyr42ivizh6kjn3qbsrgcs05wph6";
    jar-sha256 = "1ps3ws5p7hh38pa4iih0s3i469hgaxdhpqmggp9z785hrwm6rhss";
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
  "https://jcenter.bintray.com/org/apache/apache/9/apache-9" = {
    host = jcenter;
    path = "org/apache/apache/9/apache-9";
    type = "jar";
    pom-sha256 = "1p8qrz7swd6ylwfiv6x4kr3gip6sy2vca8xwydlxm3kwah5fcij9";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/apache/commons/commons-compress/1.8.1/commons-compress-1.8.1" = {
    host = jcenter;
    path = "org/apache/commons/commons-compress/1.8.1/commons-compress-1.8.1";
    type = "jar";
    pom-sha256 = "1f4z7dc667b0d07s1yfiqhvbhbj37q00ihyxphm0nlp82kdpl4qh";
    jar-sha256 = "1gbzd7aang7xp3k98m3sbjs5d7nm6yqigg8znrncqvpq0dji7jjz";
  };
  "https://jcenter.bintray.com/org/apache/commons/commons-lang3/3.3.2/commons-lang3-3.3.2" = {
    host = jcenter;
    path = "org/apache/commons/commons-lang3/3.3.2/commons-lang3-3.3.2";
    type = "jar";
    pom-sha256 = "0h3r4wajfcx9r6jqw7hhz17hmhq8s6h08hy3vribr7lrfpsmp1z1";
    jar-sha256 = "0zfl2fa2nh5l9znmqmik7l6crhh9cmp4h4b0716iipysah3x30bb";
  };
  "https://jcenter.bintray.com/org/apache/commons/commons-parent/11/commons-parent-11" = {
    host = jcenter;
    path = "org/apache/commons/commons-parent/11/commons-parent-11";
    type = "jar";
    pom-sha256 = "0am014qph77cw2n596d9z8g1naf5a191199zi9di0q1l75pk1q5r";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/apache/commons/commons-parent/25/commons-parent-25" = {
    host = jcenter;
    path = "org/apache/commons/commons-parent/25/commons-parent-25";
    type = "jar";
    pom-sha256 = "1flhjyg2b14ch0wvsbimqli7vmpxim8yg54h6xkni1rf8i8fcyj6";
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
  "https://jcenter.bintray.com/org/jacoco/org.jacoco.build/0.7.4.201502262128/org.jacoco.build-0.7.4.201502262128" = {
    host = jcenter;
    path = "org/jacoco/org.jacoco.build/0.7.4.201502262128/org.jacoco.build-0.7.4.201502262128";
    type = "jar";
    pom-sha256 = "0xxi1alqnlr9bavaqg2izzvrnbqd1i8vyjiylqqf5cw1l9hysmph";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/jacoco/org.jacoco.core/0.7.4.201502262128/org.jacoco.core-0.7.4.201502262128" = {
    host = jcenter;
    path = "org/jacoco/org.jacoco.core/0.7.4.201502262128/org.jacoco.core-0.7.4.201502262128";
    type = "jar";
    pom-sha256 = "0xyhchpmj5b13c8p1y9svn92q61prcmdn583rgzxx7cjw4q3czng";
    jar-sha256 = "1fvsbnq5sax4pycaakpshbhcjd8vz634f5jhcc8wbyhj8dap8k7c";
  };
  "https://jcenter.bintray.com/org/ow2/asm/asm-analysis/5.0.3/asm-analysis-5.0.3" = {
    host = jcenter;
    path = "org/ow2/asm/asm-analysis/5.0.3/asm-analysis-5.0.3";
    type = "jar";
    pom-sha256 = "1l570la07zr6ay1c9i5caglj029dyjwrv24zmn9n511xpjhh5z4z";
    jar-sha256 = "17c4j2r94xgbjh5wylgqdwv7fjr6w4jmbhinrmymb5ic8rijmyp8";
  };
  "https://jcenter.bintray.com/org/ow2/asm/asm-debug-all/5.0.1/asm-debug-all-5.0.1" = {
    host = jcenter;
    path = "org/ow2/asm/asm-debug-all/5.0.1/asm-debug-all-5.0.1";
    type = "jar";
    pom-sha256 = "0zsmb8dfmk23b2zcw3sxfxa82h4g9ib9zll6p16vk0fa6xb5x7qz";
    jar-sha256 = "0mzdg3cybprgpnry5ghby7k70pwf0vxp2sfvjq04niasa5dxwd27";
  };
  "https://jcenter.bintray.com/org/ow2/asm/asm-parent/5.0.1/asm-parent-5.0.1" = {
    host = jcenter;
    path = "org/ow2/asm/asm-parent/5.0.1/asm-parent-5.0.1";
    type = "jar";
    pom-sha256 = "0brabhbilxzxaxzwf6hlsg9a41mjydg5nrr7zghn8hr48wnw114q";
    jar-sha256 = "";
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
  "https://jcenter.bintray.com/org/sonatype/oss/oss-parent/9/oss-parent-9" = {
    host = jcenter;
    path = "org/sonatype/oss/oss-parent/9/oss-parent-9";
    type = "jar";
    pom-sha256 = "0yl2hbwz2kn1hll1i00ddzn8f89bfdcjwdifz0pj2j15k1gjch7v";
    jar-sha256 = "";
  };
}
