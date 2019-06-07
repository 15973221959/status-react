{}:

let
  jcenter = "https://jcenter.bintray.com";
  google = "https://dl.google.com/dl/android/maven2";
  fabric-io = "https://maven.fabric.io/public";

in {
  "https://dl.google.com/dl/android/maven2/com/android/databinding/baseLibrary/3.1.2/baseLibrary-3.1.2" = {
    host = google;
    path = "com/android/databinding/baseLibrary/3.1.2/baseLibrary-3.1.2";
    type = "jar";
    pom-sha256 = "182z8iiydz9vz39692ggbvhfkkqsp6sm54grp2kan74c17v3y4zn";
    jar-sha256 = "1phr1ilfmp0l5hz8zb08bxpscavcjfpzs3qjvd19fq0qrniwvnm6";
  };
  "https://dl.google.com/dl/android/maven2/com/android/databinding/compilerCommon/3.1.2/compilerCommon-3.1.2" = {
    host = google;
    path = "com/android/databinding/compilerCommon/3.1.2/compilerCommon-3.1.2";
    type = "jar";
    pom-sha256 = "1dd101hfsmx4d4a2k1jm01j7h16kfci13wyrs2snvwijvyc4jkzb";
    jar-sha256 = "11xqq8ckjsyvjcyw9xmlwv3yxx2lsmz724y3vnh3k23yv31r7krf";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/analytics-library/protos/26.1.2/protos-26.1.2" = {
    host = google;
    path = "com/android/tools/analytics-library/protos/26.1.2/protos-26.1.2";
    type = "jar";
    pom-sha256 = "0si4bldygjl6p89smkwy5ngas8hg9wxvvr9x3sj63kijlp2g5im1";
    jar-sha256 = "1fydllfnmhfkhl89z6qjbphw0ipbd185sly3xima0wmm885jlrsj";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/analytics-library/shared/26.1.2/shared-26.1.2" = {
    host = google;
    path = "com/android/tools/analytics-library/shared/26.1.2/shared-26.1.2";
    type = "jar";
    pom-sha256 = "1s3bqr4jdsz45m415kp4azh4j4cgsmp1a0gkhkal2s7ssfwd0miw";
    jar-sha256 = "1sg8z5sidb9izs7f6wpv865jk8xax03wfa1np3p7zy6633d0wzjw";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/analytics-library/tracker/26.1.2/tracker-26.1.2" = {
    host = google;
    path = "com/android/tools/analytics-library/tracker/26.1.2/tracker-26.1.2";
    type = "jar";
    pom-sha256 = "0f447iz6d6qa4jy5n5bc5qa5njr51mnbwh4i9jxcyfjg46vkzp19";
    jar-sha256 = "10j2vybdplxq8bffs88xyq43l5dyv5wnl738z03gnkzlmnh7my86";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/annotations/26.1.2/annotations-26.1.2" = {
    host = google;
    path = "com/android/tools/annotations/26.1.2/annotations-26.1.2";
    type = "jar";
    pom-sha256 = "0khk0ijs5xa4j8ny5svz7bx4hidh3z691qp85xra29l95xwii3yy";
    jar-sha256 = "0640j9wr7kwgp2md32jzrjrh12lanzqng11wis1cmk64yp53sxvj";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/aapt2-proto/0.1.0/aapt2-proto-0.1.0" = {
    host = google;
    path = "com/android/tools/build/aapt2-proto/0.1.0/aapt2-proto-0.1.0";
    type = "jar";
    pom-sha256 = "1h9vlambl1k51nx86qm0yfyavpf9qfrh7gja1q3qsqj10jinb8kc";
    jar-sha256 = "1x69zbk977hyc97hpc3bxmh19yiiq3hkl3hadjmwps08z4g13990";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/apksig/3.1.2/apksig-3.1.2" = {
    host = google;
    path = "com/android/tools/build/apksig/3.1.2/apksig-3.1.2";
    type = "jar";
    pom-sha256 = "0jszfw0p511w1r43pw3kksj0lsny7dkv2jv6a38znp2w5mv718nf";
    jar-sha256 = "1gm88k34ldqqgnf5d793b64d8ng0xrbqh81xhxbisk8jb52nlsa0";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/builder-model/3.1.2/builder-model-3.1.2" = {
    host = google;
    path = "com/android/tools/build/builder-model/3.1.2/builder-model-3.1.2";
    type = "jar";
    pom-sha256 = "1qsz27kq3licd7x6y0haxz940q3lyx54qi0smr8hm2kfh56pin79";
    jar-sha256 = "05sf2cx8pbhx7pcx3hklf0n5i8fg6qh3993srav655aw2cmgm6yl";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/builder-test-api/3.1.2/builder-test-api-3.1.2" = {
    host = google;
    path = "com/android/tools/build/builder-test-api/3.1.2/builder-test-api-3.1.2";
    type = "jar";
    pom-sha256 = "1r3frklphcqg9whyazwrqcgjbnq849vj5vlcyhbjxhcdz734xc9x";
    jar-sha256 = "17ry6qcwf40pslm4k6p88y3d30s29m1n440hi48v2h8dfh5sbqnz";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/builder/3.1.2/builder-3.1.2" = {
    host = google;
    path = "com/android/tools/build/builder/3.1.2/builder-3.1.2";
    type = "jar";
    pom-sha256 = "0rss89rjhym8zjnpkq7w59hbfb1lamx23xhwbxflgiiknbdcn3q4";
    jar-sha256 = "050xxf61i0b8sd4909qqnz5gxvyrydcwgyqr6r1yivz289d843xn";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/bundletool/0.1.0-alpha01/bundletool-0.1.0-alpha01" = {
    host = google;
    path = "com/android/tools/build/bundletool/0.1.0-alpha01/bundletool-0.1.0-alpha01";
    type = "jar";
    pom-sha256 = "1271sgv8jxdgsf60chx8yjc0jsfs38xd2jhdmdv8d8h4iwrpcrvc";
    jar-sha256 = "05bgc6sh1cz4l8mbw6jyx78pq275szq011z5cgpjdkdbpzq5dnrv";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/gradle-api/3.1.2/gradle-api-3.1.2" = {
    host = google;
    path = "com/android/tools/build/gradle-api/3.1.2/gradle-api-3.1.2";
    type = "jar";
    pom-sha256 = "1aw93l3hdpbxmsyg8q3flvpgrbxckkf45c6j4al15yifrnwb3k9x";
    jar-sha256 = "14s8glsya8bd2767z7as80mj1kmw9nfgi3jw1ymq6i9yi5dwr2z5";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/gradle-core/3.1.2/gradle-core-3.1.2" = {
    host = google;
    path = "com/android/tools/build/gradle-core/3.1.2/gradle-core-3.1.2";
    type = "jar";
    pom-sha256 = "14wwbj3c9g8jkg0mjrbcqqzj80xhz7h0g4pcwkxjbcx6jl3n0d8s";
    jar-sha256 = "09fd4m3v28yk6cwc40jd7aky06bv4cz114rfmmchbzdzjnvzcywc";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/gradle/3.1.2/gradle-3.1.2" = {
    host = google;
    path = "com/android/tools/build/gradle/3.1.2/gradle-3.1.2";
    type = "jar";
    pom-sha256 = "06nvcyk06zzl37iif8v42818pafxl668902gz2d536ff9dak09sj";
    jar-sha256 = "05g93qf2ws4jmwd1p2jfscza1lmcgbkc2zv2lklhnz0v8g3hq3bd";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/manifest-merger/26.1.2/manifest-merger-26.1.2" = {
    host = google;
    path = "com/android/tools/build/manifest-merger/26.1.2/manifest-merger-26.1.2";
    type = "jar";
    pom-sha256 = "1lwzlhgm49yl8vgm74qi7masb2xqinzj4nmz459aly0dng82wli4";
    jar-sha256 = "0709xdkvgcnn15jasvxfwzbz966k2q9cyjlmgc876r96lmzc4qcw";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/transform-api/2.0.0-deprecated-use-gradle-api/transform-api-2.0.0-deprecated-use-gradle-api" = {
    host = google;
    path = "com/android/tools/build/transform-api/2.0.0-deprecated-use-gradle-api/transform-api-2.0.0-deprecated-use-gradle-api";
    type = "jar";
    pom-sha256 = "0xc5zmf6125cpdb4144mblznjs16q4k5z4jpjbbs3g4ads2z6qkw";
    jar-sha256 = "0h2p4b7900j3girbkayq6b92pfw283rqnn74p4nlqlqwbv8a7r2d";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/common/26.1.2/common-26.1.2" = {
    host = google;
    path = "com/android/tools/common/26.1.2/common-26.1.2";
    type = "jar";
    pom-sha256 = "1qb8561nzalbh0c5i4a8d6hv327kb0l97i9hzw675fki1sllxadv";
    jar-sha256 = "1rl8wxslw5gyg42sn0pgcayhiv159mf71fwn314w9jvxq7q20hza";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/ddms/ddmlib/26.1.2/ddmlib-26.1.2" = {
    host = google;
    path = "com/android/tools/ddms/ddmlib/26.1.2/ddmlib-26.1.2";
    type = "jar";
    pom-sha256 = "0v6n0y0saf0fykwbjlkmyi84ygg9gj4c37awg3j714m6dzcif8j4";
    jar-sha256 = "0i2ch21bbr1nyyzmrgvph4jghp50xdcrwarws03jz21nxzxsb8hq";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/dvlib/26.1.2/dvlib-26.1.2" = {
    host = google;
    path = "com/android/tools/dvlib/26.1.2/dvlib-26.1.2";
    type = "jar";
    pom-sha256 = "13kx9y9x8vw7lz4y9pb87h6h2vrgp79nnqxzqh9xid2kdd52h36q";
    jar-sha256 = "1sihc6a358j91crl6qlzvrmkjzjb1w7brdnyqjb5qnb6nr7sm1qi";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/layoutlib/layoutlib-api/26.1.2/layoutlib-api-26.1.2" = {
    host = google;
    path = "com/android/tools/layoutlib/layoutlib-api/26.1.2/layoutlib-api-26.1.2";
    type = "jar";
    pom-sha256 = "0mkcwz7hjknx6z7c8v393x38wyyr9k6w13gyyrsa9p7diybirbvy";
    jar-sha256 = "0krbcvmyni9piwjl9asm1xy4a177cbwfyfqm53wrkmy7zhwh08i0";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/lint/lint-gradle-api/26.1.2/lint-gradle-api-26.1.2" = {
    host = google;
    path = "com/android/tools/lint/lint-gradle-api/26.1.2/lint-gradle-api-26.1.2";
    type = "jar";
    pom-sha256 = "0p1jnzg1k5cfh6g01x5d5ji9fz73x6lp8s2qlsajw6pxlvy34q8v";
    jar-sha256 = "141xy21m3gwanz35yci4afwgp141dyrrw72ijiffbhq3icm4ya3i";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/repository/26.1.2/repository-26.1.2" = {
    host = google;
    path = "com/android/tools/repository/26.1.2/repository-26.1.2";
    type = "jar";
    pom-sha256 = "0y5f5yjq40grjzkzkav2c63z901wfyqlbcnsby93n22x59sqmm3l";
    jar-sha256 = "06p00r3g3yis2z5mxmnlwvy5w7r75gzfwlclk1vbsckdml9fb1lb";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/sdk-common/26.1.2/sdk-common-26.1.2" = {
    host = google;
    path = "com/android/tools/sdk-common/26.1.2/sdk-common-26.1.2";
    type = "jar";
    pom-sha256 = "0qr8zzvs7vlv1c4519xmsfpy675ngjwb1ccdf2d3hdzy1p1npc53";
    jar-sha256 = "15hq4p7cg7n3jv33q5axh3b8jdq1wdlh5h8ybg5ws88alqh4fn13";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/sdklib/26.1.2/sdklib-26.1.2" = {
    host = google;
    path = "com/android/tools/sdklib/26.1.2/sdklib-26.1.2";
    type = "jar";
    pom-sha256 = "1wb42y65i5b03szgb52281s54b616ymwa9ip2fmj6v8zfc0vhsh4";
    jar-sha256 = "0kmzlmr74gc5x037ndsqbgnci6g21vs4k5iz1nkyzf4sqnphz1yk";
  };
  "https://jcenter.bintray.com/com/google/auto/auto-parent/3/auto-parent-3" = {
    host = jcenter;
    path = "com/google/auto/auto-parent/3/auto-parent-3";
    type = "jar";
    pom-sha256 = "1a78xc2zm7ixc1499amka8i6kq0cqm0f123kq894glngg04ip3y9";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/com/google/auto/value/auto-value/1.5.2/auto-value-1.5.2" = {
    host = jcenter;
    path = "com/google/auto/value/auto-value/1.5.2/auto-value-1.5.2";
    type = "jar";
    pom-sha256 = "13b8ghy1fmqbx8p688l87kspnr491d6b8yrsap0f1y0f192b27wh";
    jar-sha256 = "1kzlnzsb15n1fb1z446426cscqll0gnqqwvnmbsbjfcv8chr9b3c";
  };
  "https://jcenter.bintray.com/com/google/code/findbugs/jsr305/1.3.9/jsr305-1.3.9" = {
    host = jcenter;
    path = "com/google/code/findbugs/jsr305/1.3.9/jsr305-1.3.9";
    type = "jar";
    pom-sha256 = "0fm9gfc8ris3mq3zp06ra8fks3f8mxj60vdnybp7lg8w668r3azy";
    jar-sha256 = "1vf98qdxy0l4v1f0mvqxz92ydrd29vpyczmv999q22m9xsh22mwh";
  };
  "https://jcenter.bintray.com/com/google/code/gson/gson-parent/2.7/gson-parent-2.7" = {
    host = jcenter;
    path = "com/google/code/gson/gson-parent/2.7/gson-parent-2.7";
    type = "jar";
    pom-sha256 = "0ar7wk4zrdlb56jywsk6v3cpji8cjsi3kzmcr061yc12fdrqq7l7";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/com/google/code/gson/gson/2.7/gson-2.7" = {
    host = jcenter;
    path = "com/google/code/gson/gson/2.7/gson-2.7";
    type = "jar";
    pom-sha256 = "1phf2qksjf75ykwgp39189jdbqsn5zrmi07g8h522yxq0zn3cfbj";
    jar-sha256 = "0clda1xrjfja969xsbrhc61ip588xvsi9k054kpd4cz1m5gfnhrd";
  };
  "https://jcenter.bintray.com/com/google/errorprone/error_prone_annotations/2.1.2/error_prone_annotations-2.1.2" = {
    host = jcenter;
    path = "com/google/errorprone/error_prone_annotations/2.1.2/error_prone_annotations-2.1.2";
    type = "jar";
    pom-sha256 = "04894gp1inj1cpgp68cjh0rzpv0svfzhv2spak2prvdlp25mh2hq";
    jar-sha256 = "0akq6zj7fd6i0abyz5c0q5aikp7lldrbind4lbvn0q60blxqfg6y";
  };
  "https://jcenter.bintray.com/com/google/errorprone/error_prone_parent/2.1.2/error_prone_parent-2.1.2" = {
    host = jcenter;
    path = "com/google/errorprone/error_prone_parent/2.1.2/error_prone_parent-2.1.2";
    type = "jar";
    pom-sha256 = "0hx1cf07n7kwcl2pwiab628fm8fmymgy7bwdkvr3nswgvcfxddn6";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/com/google/google/1/google-1" = {
    host = jcenter;
    path = "com/google/google/1/google-1";
    type = "jar";
    pom-sha256 = "10by4ybrjnl8zwfg4ca74d0gcl4p9l7dzlfb9iwxw7m325xb2vfd";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/com/google/guava/guava-parent/22.0/guava-parent-22.0" = {
    host = jcenter;
    path = "com/google/guava/guava-parent/22.0/guava-parent-22.0";
    type = "jar";
    pom-sha256 = "0f8m410r12g9y34szyhbk5l8qf9n0pvzpnqflx81qz4pw6193bqy";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/com/google/guava/guava/22.0/guava-22.0" = {
    host = jcenter;
    path = "com/google/guava/guava/22.0/guava-22.0";
    type = "jar";
    pom-sha256 = "0cxzz1aq5wprdhag9vkfqyb160slpgw2jsvdcvhnvpb51ysb7bdz";
    jar-sha256 = "17jpmgwz19jq9b4jr45ishihsk513m5apd7hfc44inp4gm6fjn0i";
  };
  "https://jcenter.bintray.com/com/google/j2objc/j2objc-annotations/1.1/j2objc-annotations-1.1" = {
    host = jcenter;
    path = "com/google/j2objc/j2objc-annotations/1.1/j2objc-annotations-1.1";
    type = "jar";
    pom-sha256 = "1gnn7b80krv19qjd9hhacahffqq9iwqgmw4ds56wp9wk3rbqrjgh";
    jar-sha256 = "1xpcvmnw2y3fa56hhk8dmknrq8afr6r3kdmzsg9hnwgjg3msg519";
  };
  "https://jcenter.bintray.com/com/google/jimfs/jimfs-parent/1.1/jimfs-parent-1.1" = {
    host = jcenter;
    path = "com/google/jimfs/jimfs-parent/1.1/jimfs-parent-1.1";
    type = "jar";
    pom-sha256 = "1g96idyskcl3ryfc3fd5lq12ay5f4nbars3h528yzq2p3rsma5f7";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/com/google/jimfs/jimfs/1.1/jimfs-1.1" = {
    host = jcenter;
    path = "com/google/jimfs/jimfs/1.1/jimfs-1.1";
    type = "jar";
    pom-sha256 = "0i3sgapm9r7wgmrm0qlb2hzarm6q6irfbjnz5x3ppw92v5f6xa7g";
    jar-sha256 = "1pbx4lf1y24bgdfaf9bw9p0abnm7mlxhnlc7jfpk1af0swl8x0n4";
  };
  "https://jcenter.bintray.com/com/google/protobuf/protobuf-java-util/3.4.0/protobuf-java-util-3.4.0" = {
    host = jcenter;
    path = "com/google/protobuf/protobuf-java-util/3.4.0/protobuf-java-util-3.4.0";
    type = "jar";
    pom-sha256 = "1f1wvrr4lpp0mrbf3l5r3gcfbpxznvhnm8x4faxfm87awxrk1i49";
    jar-sha256 = "0ll0lslidkl0hp78yf48y4a7b8swqyylzckh1wrz4p5ibazf12a1";
  };
  "https://jcenter.bintray.com/com/google/protobuf/protobuf-java/3.4.0/protobuf-java-3.4.0" = {
    host = jcenter;
    path = "com/google/protobuf/protobuf-java/3.4.0/protobuf-java-3.4.0";
    type = "jar";
    pom-sha256 = "01llqnrfxa04q1ndnrcvsg0j3vs2rva3101w79da38azdjl7pwc3";
    jar-sha256 = "194hchg28sckqycfg4c3cm41bdxcm3rsy36sk08insj569mydryw";
  };
  "https://jcenter.bintray.com/com/google/protobuf/protobuf-parent/3.4.0/protobuf-parent-3.4.0" = {
    host = jcenter;
    path = "com/google/protobuf/protobuf-parent/3.4.0/protobuf-parent-3.4.0";
    type = "jar";
    pom-sha256 = "0irsayf19snivcmhyr6122d5mvyb8cd677bn9ixfph2251arr414";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/com/googlecode/json-simple/json-simple/1.1/json-simple-1.1" = {
    host = jcenter;
    path = "com/googlecode/json-simple/json-simple/1.1/json-simple-1.1";
    type = "jar";
    pom-sha256 = "1avxrxknd1xy8ahq67rrkh8jlkb5hwsciljzvdvd9v8gzbh9pa27";
    jar-sha256 = "0fb49wk04rl324qclzb1cpp715vjzijr8iwsgzs0ixs9qvs8951d";
  };
  "https://jcenter.bintray.com/com/googlecode/juniversalchardet/juniversalchardet/1.0.3/juniversalchardet-1.0.3" = {
    host = jcenter;
    path = "com/googlecode/juniversalchardet/juniversalchardet/1.0.3/juniversalchardet-1.0.3";
    type = "jar";
    pom-sha256 = "0sq85z68pcf87j9vcis5garlxl325lz0q01skcm69kf76ndkjikq";
    jar-sha256 = "0xjakmvlyl4i2lwb1prc19vx0mbbgpb6rhlxwx8vdf4kc68gwyvm";
  };
  "https://jcenter.bintray.com/com/squareup/javapoet/1.8.0/javapoet-1.8.0" = {
    host = jcenter;
    path = "com/squareup/javapoet/1.8.0/javapoet-1.8.0";
    type = "jar";
    pom-sha256 = "0ixnvd7bd5cn3hm9h41y87cx29nr6minbxqqshn5nwwyy500yxmk";
    jar-sha256 = "12fpvxgkhy92cmnh3wkai9jgg2g5zg7i3yhhdwcjid3v0a98q44f";
  };
  "https://jcenter.bintray.com/com/squareup/javawriter/2.5.0/javawriter-2.5.0" = {
    host = jcenter;
    path = "com/squareup/javawriter/2.5.0/javawriter-2.5.0";
    type = "jar";
    pom-sha256 = "1c57k79i2dcyjm63k8xjjr1ck3i0i4hkwsa7k72y1xbc27qxgaz1";
    jar-sha256 = "1w4p04j3z05k7ihb69pid3j4h4q8k0ipksp7rz9rgam01vxhkyzw";
  };
  "https://jcenter.bintray.com/com/sun/activation/all/1.2.0/all-1.2.0" = {
    host = jcenter;
    path = "com/sun/activation/all/1.2.0/all-1.2.0";
    type = "jar";
    pom-sha256 = "1i62n3icq23pssrvvii30x9jx63wygg7ggppwh2a2ckmmkiii18x";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/com/sun/activation/javax.activation/1.2.0/javax.activation-1.2.0" = {
    host = jcenter;
    path = "com/sun/activation/javax.activation/1.2.0/javax.activation-1.2.0";
    type = "jar";
    pom-sha256 = "0piadm0mqh1p9747mzapw7kkvayphj1irvnvn006jk458plvcygq";
    jar-sha256 = "1km9if90zdgjzgc3rxqfj2s0p0as2xymgk3rwwhny1fpdjqh4cwr";
  };
  "https://jcenter.bintray.com/com/sun/istack/istack-commons-runtime/2.21/istack-commons-runtime-2.21" = {
    host = jcenter;
    path = "com/sun/istack/istack-commons-runtime/2.21/istack-commons-runtime-2.21";
    type = "jar";
    pom-sha256 = "073dc9605ak2zns72l3mac5ynnz5k8rkyzwsbxa51l5zbxxi7rzb";
    jar-sha256 = "1x2hm4dq75jg8cz4n1ac3b6gki6p5m0kk89d1qmg15bhh2h6fgn3";
  };
  "https://jcenter.bintray.com/com/sun/istack/istack-commons/2.21/istack-commons-2.21" = {
    host = jcenter;
    path = "com/sun/istack/istack-commons/2.21/istack-commons-2.21";
    type = "jar";
    pom-sha256 = "1341mibhpgaibhvdq6xzdkxjw8bp434nakkbc212j5lvz1vi41y3";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/com/sun/xml/bind/jaxb-bom-ext/2.2.11/jaxb-bom-ext-2.2.11" = {
    host = jcenter;
    path = "com/sun/xml/bind/jaxb-bom-ext/2.2.11/jaxb-bom-ext-2.2.11";
    type = "jar";
    pom-sha256 = "1sav7gq6a54zvkb113qhff3zp435767n1r5276q95jr1vs9hcpmj";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/com/sun/xml/bind/mvn/jaxb-parent/2.2.11/jaxb-parent-2.2.11" = {
    host = jcenter;
    path = "com/sun/xml/bind/mvn/jaxb-parent/2.2.11/jaxb-parent-2.2.11";
    type = "jar";
    pom-sha256 = "10041gr7ra0777sp4cyflhs5slpf6849lpv13dbpwm013iqinc5m";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/com/sun/xml/bind/mvn/jaxb-runtime-parent/2.2.11/jaxb-runtime-parent-2.2.11" = {
    host = jcenter;
    path = "com/sun/xml/bind/mvn/jaxb-runtime-parent/2.2.11/jaxb-runtime-parent-2.2.11";
    type = "jar";
    pom-sha256 = "144fllvdhajj6fzzzi046vch28k7h5frm40y4lj7z9pdwjw8r491";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/com/sun/xml/bind/mvn/jaxb-txw-parent/2.2.11/jaxb-txw-parent-2.2.11" = {
    host = jcenter;
    path = "com/sun/xml/bind/mvn/jaxb-txw-parent/2.2.11/jaxb-txw-parent-2.2.11";
    type = "jar";
    pom-sha256 = "0b1nn9m3wwvklk1ap3nr295dj9fy1s4nhv05y5b3ngsxk9lqqfcs";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/com/sun/xml/fastinfoset/FastInfoset/1.2.13/FastInfoset-1.2.13" = {
    host = jcenter;
    path = "com/sun/xml/fastinfoset/FastInfoset/1.2.13/FastInfoset-1.2.13";
    type = "jar";
    pom-sha256 = "1lsm4vzj4n82la9bs72kvj4sgl8i2vvnjr9ym96mnjbxrw75wl5p";
    jar-sha256 = "0f70phr6cysnrsbfsbmd318h81nvy5dcadqs1cy87hpk16wpv9r7";
  };
  "https://jcenter.bintray.com/com/sun/xml/fastinfoset/fastinfoset-project/1.2.13/fastinfoset-project-1.2.13" = {
    host = jcenter;
    path = "com/sun/xml/fastinfoset/fastinfoset-project/1.2.13/fastinfoset-project-1.2.13";
    type = "jar";
    pom-sha256 = "0qf2kzp2qx62a13a70sv82yjcylnizir3z9gghlf0rqhky4mf27x";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/commons-codec/commons-codec/1.6/commons-codec-1.6" = {
    host = jcenter;
    path = "commons-codec/commons-codec/1.6/commons-codec-1.6";
    type = "jar";
    pom-sha256 = "1298qykf61rrg2p3jnschq659ycqwkryp528v49vi9pkzz9kavm0";
    jar-sha256 = "11ix43vckkj5mbv9ccnv4vf745s8sgpkdms07syi854f3fa4xcsl";
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
  "https://jcenter.bintray.com/it/unimi/dsi/fastutil/7.2.0/fastutil-7.2.0" = {
    host = jcenter;
    path = "it/unimi/dsi/fastutil/7.2.0/fastutil-7.2.0";
    type = "jar";
    pom-sha256 = "1xry05xcp4r1r0kk16idpf4b0bw9db1z5qwhx7p7add745ji2fwm";
    jar-sha256 = "072r9dp1605ixwh31kjh5ynihlln2nxgl2gbwvvl41kl8f021ykl";
  };
  "https://jcenter.bintray.com/javax/xml/bind/jaxb-api/2.2.12-b140109.1041/jaxb-api-2.2.12-b140109.1041" = {
    host = jcenter;
    path = "javax/xml/bind/jaxb-api/2.2.12-b140109.1041/jaxb-api-2.2.12-b140109.1041";
    type = "jar";
    pom-sha256 = "0nqm2szwfjfmb9dnvrisr145r492csjdwjq9kg2rhmmwv7cag4i0";
    jar-sha256 = "1whi48cdrzjwp13hnpa9sg719gagiw0dvibl9b703zxmnzc0rrmm";
  };
  "https://jcenter.bintray.com/net/java/jvnet-parent/1/jvnet-parent-1" = {
    host = jcenter;
    path = "net/java/jvnet-parent/1/jvnet-parent-1";
    type = "jar";
    pom-sha256 = "1gv37n9aid7fdhwd0h4py024w8by56cchg3b4sg5vrk82a0l0518";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/net/java/jvnet-parent/3/jvnet-parent-3" = {
    host = jcenter;
    path = "net/java/jvnet-parent/3/jvnet-parent-3";
    type = "jar";
    pom-sha256 = "0nj7958drckwf634cw9gmwgmdi302bya7bas16bbzp9rzag7ix9h";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/net/java/jvnet-parent/4/jvnet-parent-4" = {
    host = jcenter;
    path = "net/java/jvnet-parent/4/jvnet-parent-4";
    type = "jar";
    pom-sha256 = "1fwgpdx4jznjhqspgzr040q932z06ad9p4zzr2bm4ja9amrra4s7";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/net/sf/jopt-simple/jopt-simple/4.9/jopt-simple-4.9" = {
    host = jcenter;
    path = "net/sf/jopt-simple/jopt-simple/4.9/jopt-simple-4.9";
    type = "jar";
    pom-sha256 = "14lh0wb5jml18h2w45z0nibpa6xxkx6a4z9b9kq9hisbnbcf5xvs";
    jar-sha256 = "1xgjp8k4d258pab0p4zxz7n6x75m35lvh4vgnx6qcpsbjmp8bi96";
  };
  "https://jcenter.bintray.com/net/sf/kxml/kxml2/2.3.0/kxml2-2.3.0" = {
    host = jcenter;
    path = "net/sf/kxml/kxml2/2.3.0/kxml2-2.3.0";
    type = "jar";
    pom-sha256 = "11d66hzaang99y3wkrqxzn2y2qgsg34jf3dvk5i9664m9rpn1kii";
    jar-sha256 = "1qixcdxqmw7ja9yhp0qw6g4y8jzjxwhk5igcwl6f3zd1g6gxsr7j";
  };
  "https://jcenter.bintray.com/net/sf/proguard/proguard-base/5.3.3/proguard-base-5.3.3" = {
    host = jcenter;
    path = "net/sf/proguard/proguard-base/5.3.3/proguard-base-5.3.3";
    type = "jar";
    pom-sha256 = "1jnr6zsxfimb8wglqlwa6rrdc3g3nqf1dyw0k2dq9cj0q4pgn7p5";
    jar-sha256 = "11nwdb9y84cghcx319nsjjf9m035s4s1184zrhzpvaxq2wvqhbhx";
  };
  "https://jcenter.bintray.com/net/sf/proguard/proguard-gradle/5.3.3/proguard-gradle-5.3.3" = {
    host = jcenter;
    path = "net/sf/proguard/proguard-gradle/5.3.3/proguard-gradle-5.3.3";
    type = "jar";
    pom-sha256 = "03v9zm3ykfkyb5cs5ald07ph103fh68d5c33rv070r29p71dwszj";
    jar-sha256 = "0shhpsjfc5gam15jnv1hk718v5c7vi7dwdc3gvmnid6dc85kljzk";
  };
  "https://jcenter.bintray.com/net/sf/proguard/proguard-parent/5.3.3/proguard-parent-5.3.3" = {
    host = jcenter;
    path = "net/sf/proguard/proguard-parent/5.3.3/proguard-parent-5.3.3";
    type = "jar";
    pom-sha256 = "0mv0zbwyw8xa4mkc5kw69y5xqashkz9gp123akfvh9f6152l3202";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/antlr/antlr4-master/4.5.3/antlr4-master-4.5.3" = {
    host = jcenter;
    path = "org/antlr/antlr4-master/4.5.3/antlr4-master-4.5.3";
    type = "jar";
    pom-sha256 = "195jkdpq6v34aa1wd3hznlvzcch3x8s89wfgnvlrkm8awzapf620";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/antlr/antlr4/4.5.3/antlr4-4.5.3" = {
    host = jcenter;
    path = "org/antlr/antlr4/4.5.3/antlr4-4.5.3";
    type = "jar";
    pom-sha256 = "11334fj7p4yxfvqincdpn1fp4hp7vzqjrqaplxv2k9ysxqr4nkla";
    jar-sha256 = "1d2g3f5pyrabib54al6bwlqpfbkxd6limycnwrs5flfzrwwyfbd3";
  };
  "https://jcenter.bintray.com/org/apache/apache/13/apache-13" = {
    host = jcenter;
    path = "org/apache/apache/13/apache-13";
    type = "jar";
    pom-sha256 = "07c4yg52q1qiz2b982pcsiwf9ahmpil4jy7lpqvi5m0z6sq3slgz";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/apache/apache/16/apache-16" = {
    host = jcenter;
    path = "org/apache/apache/16/apache-16";
    type = "jar";
    pom-sha256 = "03m4fjgg98zcyjlsp64z21lyiszhwyg43ys7mabk1jynswpzz1cz";
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
  "https://jcenter.bintray.com/org/apache/commons/commons-compress/1.12/commons-compress-1.12" = {
    host = jcenter;
    path = "org/apache/commons/commons-compress/1.12/commons-compress-1.12";
    type = "jar";
    pom-sha256 = "0fi1am0cs09axqn5lzzg9a07ksj12spbjs1945v5wl2ir1sdb1xp";
    jar-sha256 = "1dn09ncg5ggxvr57m26kjp8dcwalmv45agcwmdy5n623ygx4459c";
  };
  "https://jcenter.bintray.com/org/apache/commons/commons-parent/22/commons-parent-22" = {
    host = jcenter;
    path = "org/apache/commons/commons-parent/22/commons-parent-22";
    type = "jar";
    pom-sha256 = "1frwdic537d95l0ikgkvfpb4wjfjx2h5h211zysdsyhawdamx37v";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/apache/commons/commons-parent/25/commons-parent-25" = {
    host = jcenter;
    path = "org/apache/commons/commons-parent/25/commons-parent-25";
    type = "jar";
    pom-sha256 = "1flhjyg2b14ch0wvsbimqli7vmpxim8yg54h6xkni1rf8i8fcyj6";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/apache/commons/commons-parent/39/commons-parent-39" = {
    host = jcenter;
    path = "org/apache/commons/commons-parent/39/commons-parent-39";
    type = "jar";
    pom-sha256 = "19qxlq7zcvf8cw2hyrcb5d6b9cbbd6ccwnahv2v3wp1al3hjgkc7";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/apache/commons/commons-parent/5/commons-parent-5" = {
    host = jcenter;
    path = "org/apache/commons/commons-parent/5/commons-parent-5";
    type = "jar";
    pom-sha256 = "0am0vbkyp6b6srwhvw52xkc4ghaj4kqn0ay26vgag06z1g035mlb";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/apache/httpcomponents/httpclient/4.2.6/httpclient-4.2.6" = {
    host = jcenter;
    path = "org/apache/httpcomponents/httpclient/4.2.6/httpclient-4.2.6";
    type = "jar";
    pom-sha256 = "0a03lbcyjia7vavlvd4qbh29nzmvblnmdjiym1md63hhmbpa387m";
    jar-sha256 = "0l2m2ijv8d5dwnmsghdjy7rxwzbka9xhf84y4whpwsbwxqj96bin";
  };
  "https://jcenter.bintray.com/org/apache/httpcomponents/httpcomponents-client/4.1/httpcomponents-client-4.1" = {
    host = jcenter;
    path = "org/apache/httpcomponents/httpcomponents-client/4.1/httpcomponents-client-4.1";
    type = "jar";
    pom-sha256 = "09wq25fxxrsj8c01313w9f4wr95mpz9l4dnykjdqsv4cg54bc5aj";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/apache/httpcomponents/httpcomponents-client/4.2.6/httpcomponents-client-4.2.6" = {
    host = jcenter;
    path = "org/apache/httpcomponents/httpcomponents-client/4.2.6/httpcomponents-client-4.2.6";
    type = "jar";
    pom-sha256 = "0s9d2yl36q2zyvkyhl7ksrlcc317hjl1ybvmpr6y2h12mwpgvdjj";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/apache/httpcomponents/httpcomponents-core/4.2.5/httpcomponents-core-4.2.5" = {
    host = jcenter;
    path = "org/apache/httpcomponents/httpcomponents-core/4.2.5/httpcomponents-core-4.2.5";
    type = "jar";
    pom-sha256 = "1943dz9r27v4961ahq06f62xclsxjryxpy31r6xn6kisyaf7lkhn";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/apache/httpcomponents/httpcore/4.1/httpcore-4.1" = {
    host = jcenter;
    path = "org/apache/httpcomponents/httpcore/4.1/httpcore-4.1";
    type = "jar";
    pom-sha256 = "09vk7if9fwc6zsmr0912mgk57m23qwmmrx6ck6qgmjg973x6mj2g";
    jar-sha256 = "1kr95c4q7w32yk81klyj2plgjhc5s2l5fh0qdn66c92f3zjqvqrw";
  };
  "https://jcenter.bintray.com/org/apache/httpcomponents/httpcore/4.2.5/httpcore-4.2.5" = {
    host = jcenter;
    path = "org/apache/httpcomponents/httpcore/4.2.5/httpcore-4.2.5";
    type = "jar";
    pom-sha256 = "1ijb7yalv7c7vd7k1vix7zr08dcyqhspmm80ak5490rb6vqrksxn";
    jar-sha256 = "1sfgh7mcqnxnvpdwjzsyfci8bziffp0f6hzppcbxkj36rjj2vs75";
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
  "https://jcenter.bintray.com/org/apache/httpcomponents/project/7/project-7" = {
    host = jcenter;
    path = "org/apache/httpcomponents/project/7/project-7";
    type = "jar";
    pom-sha256 = "1hs8m8cgyypd6vb882zjyns58nhzrkm7cpbb0kg5i9amhm1blvix";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/bouncycastle/bcpkix-jdk15on/1.56/bcpkix-jdk15on-1.56" = {
    host = jcenter;
    path = "org/bouncycastle/bcpkix-jdk15on/1.56/bcpkix-jdk15on-1.56";
    type = "jar";
    pom-sha256 = "0n82rzpyr0p0vvnkppma904zdfgby5wwfwycp4718wsa8zy6h5ij";
    jar-sha256 = "1jn60798vsqnp7l6fmkmbwy8kjqyxjqlavxkw29mw5z7x7jdwhvh";
  };
  "https://jcenter.bintray.com/org/bouncycastle/bcprov-jdk15on/1.56/bcprov-jdk15on-1.56" = {
    host = jcenter;
    path = "org/bouncycastle/bcprov-jdk15on/1.56/bcprov-jdk15on-1.56";
    type = "jar";
    pom-sha256 = "0vjw9dx654i469fb87jdjp08sy3xvs3qpi597ccp665hwwv37p4g";
    jar-sha256 = "0jc3hy7p50iy626v2zl6vbfr3yi8vgf8v13xi6czp3w09zhiwgln";
  };
  "https://jcenter.bintray.com/org/codehaus/codehaus-parent/4/codehaus-parent-4" = {
    host = jcenter;
    path = "org/codehaus/codehaus-parent/4/codehaus-parent-4";
    type = "jar";
    pom-sha256 = "1pqlw2ilzagcl5ahq9dv60cxw59yrvrwf9q6z0679qf2x1yj71vb";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/codehaus/mojo/animal-sniffer-annotations/1.14/animal-sniffer-annotations-1.14" = {
    host = jcenter;
    path = "org/codehaus/mojo/animal-sniffer-annotations/1.14/animal-sniffer-annotations-1.14";
    type = "jar";
    pom-sha256 = "09ianw2880ch3x6xq5d2cz2b15ik6f4ndf8hb7ckw7lr0ndg2y8q";
    jar-sha256 = "0pchd4360mim0f0a6vwr33szigihgvv4ic1scz1l9mxssq5k4s10";
  };
  "https://jcenter.bintray.com/org/codehaus/mojo/animal-sniffer-parent/1.14/animal-sniffer-parent-1.14" = {
    host = jcenter;
    path = "org/codehaus/mojo/animal-sniffer-parent/1.14/animal-sniffer-parent-1.14";
    type = "jar";
    pom-sha256 = "1skf65rbw52shb2akgs7xykn06lj1ggp23nbc94vs40ldfh505gm";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/codehaus/mojo/mojo-parent/34/mojo-parent-34" = {
    host = jcenter;
    path = "org/codehaus/mojo/mojo-parent/34/mojo-parent-34";
    type = "jar";
    pom-sha256 = "171a71yz4np2h0nlkm9zx82q6f97wm66kj6afhvrmh23pipmsf9y";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/glassfish/jaxb/jaxb-bom/2.2.11/jaxb-bom-2.2.11" = {
    host = jcenter;
    path = "org/glassfish/jaxb/jaxb-bom/2.2.11/jaxb-bom-2.2.11";
    type = "jar";
    pom-sha256 = "0f4d9p4vgidkq2v6s6gqkc5rpm5gz9fv5l2i59x9zp3hy186hbm5";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/glassfish/jaxb/jaxb-core/2.2.11/jaxb-core-2.2.11" = {
    host = jcenter;
    path = "org/glassfish/jaxb/jaxb-core/2.2.11/jaxb-core-2.2.11";
    type = "jar";
    pom-sha256 = "094v2z4ai9v6jnq4jwsb9mfqfvapq0g26nagafcvzjiv42gl0cgc";
    jar-sha256 = "1amjhysjp6hbn6bccr2img6px5l63cigcnra6p464hxhxglaxg1p";
  };
  "https://jcenter.bintray.com/org/glassfish/jaxb/jaxb-runtime/2.2.11/jaxb-runtime-2.2.11" = {
    host = jcenter;
    path = "org/glassfish/jaxb/jaxb-runtime/2.2.11/jaxb-runtime-2.2.11";
    type = "jar";
    pom-sha256 = "1am4rkx34dli4rxnyrpmgwgbkzl5vz6dadkqx51q3awmnlqpncp5";
    jar-sha256 = "0w7d41amvf7jab7jmfjj3grahvcaq482s073dfaf5a7v3hsz4x58";
  };
  "https://jcenter.bintray.com/org/glassfish/jaxb/txw2/2.2.11/txw2-2.2.11" = {
    host = jcenter;
    path = "org/glassfish/jaxb/txw2/2.2.11/txw2-2.2.11";
    type = "jar";
    pom-sha256 = "1n3b8233l5qa5l44866grbrs1l4vacp66ar7ryjmkjjg9drcn545";
    jar-sha256 = "1smgbsxjcmfsahlskir0aa6sng656f32mk9034si2iassk53qai7";
  };
  "https://jcenter.bintray.com/org/jetbrains/annotations/13.0/annotations-13.0" = {
    host = jcenter;
    path = "org/jetbrains/annotations/13.0/annotations-13.0";
    type = "jar";
    pom-sha256 = "15y3p0xicxjx6y38pj39vm3q56xqnfhgf6yyplcrhdpzxlmynnln";
    jar-sha256 = "0y0l26ys36zlrsw98335a7wc1cl894zc1jjyj8sgvmg2r06s3qmc";
  };
  "https://jcenter.bintray.com/org/jetbrains/kotlin/kotlin-reflect/1.2.0/kotlin-reflect-1.2.0" = {
    host = jcenter;
    path = "org/jetbrains/kotlin/kotlin-reflect/1.2.0/kotlin-reflect-1.2.0";
    type = "jar";
    pom-sha256 = "1pdlcfjas68p8bvzcc0apjjx2b01swhn5mrm0zbf6sgd3gd1alpk";
    jar-sha256 = "1pb9c3ri3f2yvl1rmhcdwpbjl0g4246n3bglag0dkr6np9rahj2g";
  };
  "https://jcenter.bintray.com/org/jetbrains/kotlin/kotlin-stdlib-jre7/1.2.0/kotlin-stdlib-jre7-1.2.0" = {
    host = jcenter;
    path = "org/jetbrains/kotlin/kotlin-stdlib-jre7/1.2.0/kotlin-stdlib-jre7-1.2.0";
    type = "jar";
    pom-sha256 = "0j3ka2lfxqxb37dmx4nzm07q88a77yf5wf1mrqs7gayk49y42xr0";
    jar-sha256 = "09i91z5r1ynll65sa6v61hqkz875q7vaz049zrx7jdyla6whz8n7";
  };
  "https://jcenter.bintray.com/org/jetbrains/kotlin/kotlin-stdlib-jre8/1.2.0/kotlin-stdlib-jre8-1.2.0" = {
    host = jcenter;
    path = "org/jetbrains/kotlin/kotlin-stdlib-jre8/1.2.0/kotlin-stdlib-jre8-1.2.0";
    type = "jar";
    pom-sha256 = "0f5c4jjw8rs5jgyjb2d9j6h1nf5nqcj6aq61s7p5cw6546z78ykb";
    jar-sha256 = "0an8j2wwpx0mapmpl179xqf242kvjbipxaqxrgvl26ggwvp28db3";
  };
  "https://jcenter.bintray.com/org/jetbrains/kotlin/kotlin-stdlib/1.2.0/kotlin-stdlib-1.2.0" = {
    host = jcenter;
    path = "org/jetbrains/kotlin/kotlin-stdlib/1.2.0/kotlin-stdlib-1.2.0";
    type = "jar";
    pom-sha256 = "083s6ddd4a4j43h6b4awxh562zz4kmy2ajrq14dl5kgv9bd7i5ya";
    jar-sha256 = "1m7wxad6iw86a4fdvzx24ydr15d425r5z4m80c3r2h8bmksxkkq5";
  };
  "https://jcenter.bintray.com/org/jvnet/staxex/stax-ex/1.7.7/stax-ex-1.7.7" = {
    host = jcenter;
    path = "org/jvnet/staxex/stax-ex/1.7.7/stax-ex-1.7.7";
    type = "jar";
    pom-sha256 = "1b9yhymk6zp9sva68v0829xsrvsdz03rjlb2hfyh0yindh5lbjr7";
    jar-sha256 = "1d1zh4ymilfc2shm51fcwb7c4i5f6nnmkvkzksqdxh33f7bzf7x3";
  };
  "https://jcenter.bintray.com/org/ow2/asm/asm-analysis/5.1/asm-analysis-5.1" = {
    host = jcenter;
    path = "org/ow2/asm/asm-analysis/5.1/asm-analysis-5.1";
    type = "jar";
    pom-sha256 = "16yma32h7zzniq7xi2v1yb7iy8xc5f6igqc9jw74y329sns0s4l1";
    jar-sha256 = "1a8zpdmfbhlfn89g6i3r81k7npy25k1iq4r1xwz5fjyyqpsmhim3";
  };
  "https://jcenter.bintray.com/org/ow2/asm/asm-commons/5.1/asm-commons-5.1" = {
    host = jcenter;
    path = "org/ow2/asm/asm-commons/5.1/asm-commons-5.1";
    type = "jar";
    pom-sha256 = "0l2psg8h5i04b8i4ji87z3hlbndxg9h01p9nm5azxx4m2hgv6dnd";
    jar-sha256 = "1gl1976jbfxvy0v7pzdpyv0wpfrk1vsjn45dz3flprsm3xp7icwp";
  };
  "https://jcenter.bintray.com/org/ow2/asm/asm-parent/5.1/asm-parent-5.1" = {
    host = jcenter;
    path = "org/ow2/asm/asm-parent/5.1/asm-parent-5.1";
    type = "jar";
    pom-sha256 = "01qh2la7mkwyfx8gbz5n3p0j22490js12skycihkya7wz9c85v45";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/ow2/asm/asm-tree/5.1/asm-tree-5.1" = {
    host = jcenter;
    path = "org/ow2/asm/asm-tree/5.1/asm-tree-5.1";
    type = "jar";
    pom-sha256 = "088sg1xdmlfvv2f1g2w1px0i7y4p3lzj883gip7y7cm4gvy6jlzv";
    jar-sha256 = "042cx29y8hsiri25a7swvp8pw1qxxpafq4wqclcp8adq9jy2ppn0";
  };
  "https://jcenter.bintray.com/org/ow2/asm/asm-util/5.1/asm-util-5.1" = {
    host = jcenter;
    path = "org/ow2/asm/asm-util/5.1/asm-util-5.1";
    type = "jar";
    pom-sha256 = "03i4sxdsws79pc9hva8rfigfbbr9rjwffssw3gbx2l62awc9s3hc";
    jar-sha256 = "0h206v70i27sw0zf14ybwl9xd7jg2aiakfwg2jcx0g2ymqwjq0zf";
  };
  "https://jcenter.bintray.com/org/ow2/asm/asm/5.1/asm-5.1" = {
    host = jcenter;
    path = "org/ow2/asm/asm/5.1/asm-5.1";
    type = "jar";
    pom-sha256 = "01dj1vq56rz9k9sdsczkfjajxkzmxqxlcs6cmjx27wrrpxd9n1vd";
    jar-sha256 = "0m2bfr224dqpvk5cm0ih48n2516jg7x5d4kk4459zik7k6d3knnj";
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
  "https://maven.fabric.io/public/io/fabric/tools/gradle/1.25.4/gradle-1.25.4" = {
    host = fabric-io;
    path = "io/fabric/tools/gradle/1.25.4/gradle-1.25.4";
    type = "jar";
    pom-sha256 = "08xr1zhb50n6n08hc0vbwxqjfsdhmxp0z0b5py39vmsa9vwq2rfw";
    jar-sha256 = "0xkfv4py1mpwvz4bc2zsi4vzz0g81n6zhgkpfxx9j0f429ndwpqs";
  };
}
