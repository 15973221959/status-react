{}:

let
  jcenter = "https://jcenter.bintray.com";
  google = "https://dl.google.com/dl/android/maven2";
  fabric-io = "https://maven.fabric.io/public";

in {
  "https://dl.google.com/dl/android/maven2/com/android/databinding/baseLibrary/3.0.1/baseLibrary-3.0.1" = {
    host = google;
    path = "com/android/databinding/baseLibrary/3.0.1/baseLibrary-3.0.1";
    type = "jar";
    pom-sha256 = "0lflcv6vcd4rmxwmq2qr6g2zgk7iimi8pg5ch5bg32z35qzhp1wb";
    jar-sha256 = "00b0cbxyl5lym35ncfrq4b181lpvabp21r8w3fldz3g91mnr130z";
  };
  "https://dl.google.com/dl/android/maven2/com/android/databinding/compilerCommon/3.0.1/compilerCommon-3.0.1" = {
    host = google;
    path = "com/android/databinding/compilerCommon/3.0.1/compilerCommon-3.0.1";
    type = "jar";
    pom-sha256 = "0ca0d9g04ns60y2vgcrzfcfcplc6b3p8naynzavz0a11jiim99f9";
    jar-sha256 = "0c3g3wz4fbz4ay82a0w2d5yx932gf3p8m037gf21w12b4rf96978";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/analytics-library/protos/26.0.1/protos-26.0.1" = {
    host = google;
    path = "com/android/tools/analytics-library/protos/26.0.1/protos-26.0.1";
    type = "jar";
    pom-sha256 = "1qzy02yznfk941jgkazdiqap8rsarxkn303vg5al5vzdiwbhch4d";
    jar-sha256 = "13r27dq4sp4rdw35nbl9v7h8fzyby8xpwkjsxm4xxp1ddm5mwpyz";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/analytics-library/shared/26.0.1/shared-26.0.1" = {
    host = google;
    path = "com/android/tools/analytics-library/shared/26.0.1/shared-26.0.1";
    type = "jar";
    pom-sha256 = "1mjpx8jpr8bg11jbry0nhy4kzj2wib1hynzyn64rvxmsbhmnknrq";
    jar-sha256 = "0laz70kx25p8223vixw7sknzq0c3lilsam66rqjfkpi9120sb9z7";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/analytics-library/tracker/26.0.1/tracker-26.0.1" = {
    host = google;
    path = "com/android/tools/analytics-library/tracker/26.0.1/tracker-26.0.1";
    type = "jar";
    pom-sha256 = "1pk8qmljrcxfwfd45gmswjl4wb6pqazv2h9a0krfya8ik6c7h40h";
    jar-sha256 = "0j14icndz0nyyc52d3vx74zhfllkn85k1pwcqwl885jrz9lf75b8";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/annotations/26.0.1/annotations-26.0.1" = {
    host = google;
    path = "com/android/tools/annotations/26.0.1/annotations-26.0.1";
    type = "jar";
    pom-sha256 = "0dyq765sdf7sxl2nygkfym6nksb68rchjipq0qwz0jy68a8si22g";
    jar-sha256 = "11ck1rmwdvxhh5wnwq05w0ip8n7v96n4cqlkf9rskz2srjn5vmm9";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/apksig/3.0.1/apksig-3.0.1" = {
    host = google;
    path = "com/android/tools/build/apksig/3.0.1/apksig-3.0.1";
    type = "jar";
    pom-sha256 = "02j0ji9hk730va4pxfdg08ka2cs2ibrmbswxf5z2cf6hnfpbd3k6";
    jar-sha256 = "08yky2gjrk4y5hd1q3jh2vyprs13ipd3v2lk63kirdr09nbpqbbi";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/builder-model/3.0.1/builder-model-3.0.1" = {
    host = google;
    path = "com/android/tools/build/builder-model/3.0.1/builder-model-3.0.1";
    type = "jar";
    pom-sha256 = "0189i0hgjbbgxkaqa8j1cf1cxi5zv7mvjz4wkkxp6smcr0pf16g8";
    jar-sha256 = "1d9xlr54i63f6nkva5fvflqnl8za3i82caxi09bk134gcb66bd33";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/builder-test-api/3.0.1/builder-test-api-3.0.1" = {
    host = google;
    path = "com/android/tools/build/builder-test-api/3.0.1/builder-test-api-3.0.1";
    type = "jar";
    pom-sha256 = "0xl86xji19i4081d8gziima23s9hhn9x2yxb6gbah34r9sksh2lb";
    jar-sha256 = "0kc9g0sz59qy65m5mw3qxr4iclf20kjkks2p0h8l6j6vi8ja2rz7";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/builder/3.0.1/builder-3.0.1" = {
    host = google;
    path = "com/android/tools/build/builder/3.0.1/builder-3.0.1";
    type = "jar";
    pom-sha256 = "1jfi3wymgpb0pk23q53wgjxqz4zvj71wvx7rm0da2cfhj332ghsx";
    jar-sha256 = "169b6i7rv968k1hkxw4h5hbnmy77ms2yv0w6cwg7vpdqxnaf6fw0";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/gradle-api/3.0.1/gradle-api-3.0.1" = {
    host = google;
    path = "com/android/tools/build/gradle-api/3.0.1/gradle-api-3.0.1";
    type = "jar";
    pom-sha256 = "1kfvp0xvwkd3z35ba7fy5wafqhixpqkpd6q1bjss5w7r21vb3xz2";
    jar-sha256 = "0z8k76ah3f25wmb28rc8ni8f12dyd0fpbhg24ld4p7iawsqkyqr4";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/gradle-core/3.0.1/gradle-core-3.0.1" = {
    host = google;
    path = "com/android/tools/build/gradle-core/3.0.1/gradle-core-3.0.1";
    type = "jar";
    pom-sha256 = "130pcxhs4bify71i3ydapjx5pmndr0qfbcl0phzb8yyhyw3d1ri3";
    jar-sha256 = "09w0jxh7vkmgy6ing49l8xpyp7jrcz56g2j5sfsyipacdyahyr0a";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/gradle/3.0.1/gradle-3.0.1" = {
    host = google;
    path = "com/android/tools/build/gradle/3.0.1/gradle-3.0.1";
    type = "jar";
    pom-sha256 = "0ya15fr2c9a842799igv6kwzs1hb9rrsa0ix0b7f0snlm4jirxqy";
    jar-sha256 = "0ahm0ks11x14yxzakpwbpbr8kq7j8dfxm4qbhqlk92d39sx7rmzw";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/manifest-merger/26.0.1/manifest-merger-26.0.1" = {
    host = google;
    path = "com/android/tools/build/manifest-merger/26.0.1/manifest-merger-26.0.1";
    type = "jar";
    pom-sha256 = "1wx8dcqn62ybi66qyzql23x6qgs72iadyjznwhq4zpz5b0mhsaq5";
    jar-sha256 = "1x9v755fa6dlqaj8kxdyaihpbl2vc93nxk46yqbbj4n89q477pmy";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/transform-api/2.0.0-deprecated-use-gradle-api/transform-api-2.0.0-deprecated-use-gradle-api" = {
    host = google;
    path = "com/android/tools/build/transform-api/2.0.0-deprecated-use-gradle-api/transform-api-2.0.0-deprecated-use-gradle-api";
    type = "jar";
    pom-sha256 = "0xc5zmf6125cpdb4144mblznjs16q4k5z4jpjbbs3g4ads2z6qkw";
    jar-sha256 = "0h2p4b7900j3girbkayq6b92pfw283rqnn74p4nlqlqwbv8a7r2d";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/common/26.0.1/common-26.0.1" = {
    host = google;
    path = "com/android/tools/common/26.0.1/common-26.0.1";
    type = "jar";
    pom-sha256 = "0sifm4h7cnhn09p4r7kxgihprcpckgw0kwrf24k5yx4g0nv1s3z3";
    jar-sha256 = "0xvwx56xn3dvzj85vw3g5vv57af9147hqj5cy27vx5ws4sfvnyxj";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/ddms/ddmlib/26.0.1/ddmlib-26.0.1" = {
    host = google;
    path = "com/android/tools/ddms/ddmlib/26.0.1/ddmlib-26.0.1";
    type = "jar";
    pom-sha256 = "06dn2lz2ssg2jlzp26hcjnrzwhlpd29bbqcpsklhc941bhqqpw07";
    jar-sha256 = "1yagm7jjqa748hxj6sfcd041sfk5xyy7nwir1ajgac5l4n5kzxw1";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/dvlib/26.0.1/dvlib-26.0.1" = {
    host = google;
    path = "com/android/tools/dvlib/26.0.1/dvlib-26.0.1";
    type = "jar";
    pom-sha256 = "0yqpmvlsln76jyb33bfnbm1d43a5n910w3rxcb33ycbj3xs7kirf";
    jar-sha256 = "1bhxs4y9xib2jpg6rcr30xdsif027basfl6z3ns1f0q4yvf5ii3h";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/external/com-intellij/intellij-core/26.0.1/intellij-core-26.0.1" = {
    host = google;
    path = "com/android/tools/external/com-intellij/intellij-core/26.0.1/intellij-core-26.0.1";
    type = "jar";
    pom-sha256 = "198mnrksc5v6k49zs9yydg1wp0rl4gf6wsy15nzqnbk8xcxin17f";
    jar-sha256 = "136zki4af3nnf152pbiywlgwazf93r54jixwq22f945fzbw4vajq";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/external/org-jetbrains/uast/26.0.1/uast-26.0.1" = {
    host = google;
    path = "com/android/tools/external/org-jetbrains/uast/26.0.1/uast-26.0.1";
    type = "jar";
    pom-sha256 = "0hvzk9kw34i2g4y58cg9sxm9siw35k5wdk55f8xihjkbxk7w3a9b";
    jar-sha256 = "1065hwagil897vshy0w30006r59g0xldnr0xn9gb64jjkz36vm7c";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/layoutlib/layoutlib-api/26.0.1/layoutlib-api-26.0.1" = {
    host = google;
    path = "com/android/tools/layoutlib/layoutlib-api/26.0.1/layoutlib-api-26.0.1";
    type = "jar";
    pom-sha256 = "08n748g4yrfb8c5pkf60dhp1v3dvr3r0kqsdqjz5l6yrfrg97d72";
    jar-sha256 = "0qdqy7gqs4xfcbqbjnfbb20q65gcdrc7chxy0i682p1sl48xjnkb";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/lint/lint-api/26.0.1/lint-api-26.0.1" = {
    host = google;
    path = "com/android/tools/lint/lint-api/26.0.1/lint-api-26.0.1";
    type = "jar";
    pom-sha256 = "13fx1cpi63sz9qjqc1z6ikyh1v6kjc3q777jslqqzkdl09agrc6x";
    jar-sha256 = "03p4g8wa8g00d2ybpm16nbsa7krhgyc31f85nmxgp1ihj4ga3s2z";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/lint/lint-checks/26.0.1/lint-checks-26.0.1" = {
    host = google;
    path = "com/android/tools/lint/lint-checks/26.0.1/lint-checks-26.0.1";
    type = "jar";
    pom-sha256 = "14gdz9qdfk6lglx9da65vqpa8f1i70vj5jcngmkcsabisj2ja9ax";
    jar-sha256 = "1jv651slb64v6sbq08d8lzklxkndd21g6ah8i4rm98081h14d4ll";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/lint/lint/26.0.1/lint-26.0.1" = {
    host = google;
    path = "com/android/tools/lint/lint/26.0.1/lint-26.0.1";
    type = "jar";
    pom-sha256 = "1cyrkkpy9v8nndwlnz77sz21fk40i30xav5xdxzgcqdvgs7l9h8k";
    jar-sha256 = "0h1fz04y6jcz11mjpn6vj4lghlc09y3j4gi3p4nbvdigi8argcdf";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/repository/26.0.1/repository-26.0.1" = {
    host = google;
    path = "com/android/tools/repository/26.0.1/repository-26.0.1";
    type = "jar";
    pom-sha256 = "0srpskbq3n92mz1k5f147lpzvghyy3y83rk961xgzc75imnb43x2";
    jar-sha256 = "1301sjifc5qcpilybgq7k0mj6c7a212j30dj65qxcyqi7a9vllkg";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/sdk-common/26.0.1/sdk-common-26.0.1" = {
    host = google;
    path = "com/android/tools/sdk-common/26.0.1/sdk-common-26.0.1";
    type = "jar";
    pom-sha256 = "1andaam7b2a0dxsrgkijim2arz4fglc502n74r14fkxpcj5hyrm9";
    jar-sha256 = "1zcwbcglf3rj1qvr5cm065pv82hwgy1m825892nnvjxxh6idr2vy";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/sdklib/26.0.1/sdklib-26.0.1" = {
    host = google;
    path = "com/android/tools/sdklib/26.0.1/sdklib-26.0.1";
    type = "jar";
    pom-sha256 = "07dl93g9g83pg8g53wl4z7h6hh4va0q120n5sf07zhjzcb17a11a";
    jar-sha256 = "19hmrd4pdkr535wm4pl4qfx6wr9q0jpjc718dfdqhq2ra1wi49fg";
  };
  "https://jcenter.bintray.com/com/android/tools/external/lombok/lombok-ast/0.2.3/lombok-ast-0.2.3" = {
    host = jcenter;
    path = "com/android/tools/external/lombok/lombok-ast/0.2.3/lombok-ast-0.2.3";
    type = "jar";
    pom-sha256 = "1gbljyqw5r2vc31dl1flclsvdias3049pdrc6dicizcbvph92qcc";
    jar-sha256 = "0vg8w757n8a8v6lafrg4xb9nl7dqw3x56p1qzfsp20j395p0vss5";
  };
  "https://jcenter.bintray.com/com/google/code/findbugs/jsr305/1.3.9/jsr305-1.3.9" = {
    host = jcenter;
    path = "com/google/code/findbugs/jsr305/1.3.9/jsr305-1.3.9";
    type = "jar";
    pom-sha256 = "0fm9gfc8ris3mq3zp06ra8fks3f8mxj60vdnybp7lg8w668r3azy";
    jar-sha256 = "1vf98qdxy0l4v1f0mvqxz92ydrd29vpyczmv999q22m9xsh22mwh";
  };
  "https://jcenter.bintray.com/com/google/code/gson/gson/2.3/gson-2.3" = {
    host = jcenter;
    path = "com/google/code/gson/gson/2.3/gson-2.3";
    type = "jar";
    pom-sha256 = "1qj9kd2yn4h2h0nasxpys85v0r6s60dp1fdnj788z0qgqlprjmhs";
    jar-sha256 = "0k3s5k4gavwj8kh9c46ivspxxczpfd9nhq9ni9mdc2ygv0n52rll";
  };
  "https://jcenter.bintray.com/com/google/errorprone/error_prone_annotations/2.0.18/error_prone_annotations-2.0.18" = {
    host = jcenter;
    path = "com/google/errorprone/error_prone_annotations/2.0.18/error_prone_annotations-2.0.18";
    type = "jar";
    pom-sha256 = "11w4193x21z1zzqhngmq60qxbc13xgmdq9b86v115xnnj9qi4i4i";
    jar-sha256 = "0sv5i2kyl7qbchcb786d1x2fq39zfsjvwglz343klmmzf3cglk6b";
  };
  "https://jcenter.bintray.com/com/google/errorprone/error_prone_parent/2.0.18/error_prone_parent-2.0.18" = {
    host = jcenter;
    path = "com/google/errorprone/error_prone_parent/2.0.18/error_prone_parent-2.0.18";
    type = "jar";
    pom-sha256 = "1i46saxm7vx68cdl9drxswqfjsaa2v0qayc13vqx81wv4xarj56g";
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
  "https://jcenter.bintray.com/com/google/protobuf/protobuf-java/3.0.0/protobuf-java-3.0.0" = {
    host = jcenter;
    path = "com/google/protobuf/protobuf-java/3.0.0/protobuf-java-3.0.0";
    type = "jar";
    pom-sha256 = "1k6f973cdghqag5cnwq9h9fm5c69q8f02anl329ibanpy2waziz4";
    jar-sha256 = "1nsmiiisyp0s9149lb7k86nd05vzfpwfq58vixy3qw61xyrxdrx1";
  };
  "https://jcenter.bintray.com/com/google/protobuf/protobuf-parent/3.0.0/protobuf-parent-3.0.0" = {
    host = jcenter;
    path = "com/google/protobuf/protobuf-parent/3.0.0/protobuf-parent-3.0.0";
    type = "jar";
    pom-sha256 = "0wpjg6ai9dz9n7xa6jqws5rv90yh0jgf5dxxifavr9r4kamnnblk";
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
  "https://jcenter.bintray.com/org/eclipse/jdt/core/compiler/ecj/4.6.1/ecj-4.6.1" = {
    host = jcenter;
    path = "org/eclipse/jdt/core/compiler/ecj/4.6.1/ecj-4.6.1";
    type = "jar";
    pom-sha256 = "0vvw2gypgf11z8b6cqwp53iz30bz6yabkkvqqhhi7q2dh3l4av2k";
    jar-sha256 = "1q5dxv28izkg23wrfiyzazvd15z8ldhpnkplffg4dd51yisxmpcw";
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
  "https://jcenter.bintray.com/org/jacoco/org.jacoco.report/0.7.4.201502262128/org.jacoco.report-0.7.4.201502262128" = {
    host = jcenter;
    path = "org/jacoco/org.jacoco.report/0.7.4.201502262128/org.jacoco.report-0.7.4.201502262128";
    type = "jar";
    pom-sha256 = "0hls8g109xj2ykghykjsrmwbdcp13phvjibfi8cjyqsgickillz6";
    jar-sha256 = "02gzckmh8hhs3zp2sbry6px4811z4ib4c25i4giyg2700p358dbs";
  };
  "https://jcenter.bintray.com/org/jetbrains/annotations/13.0/annotations-13.0" = {
    host = jcenter;
    path = "org/jetbrains/annotations/13.0/annotations-13.0";
    type = "jar";
    pom-sha256 = "15y3p0xicxjx6y38pj39vm3q56xqnfhgf6yyplcrhdpzxlmynnln";
    jar-sha256 = "0y0l26ys36zlrsw98335a7wc1cl894zc1jjyj8sgvmg2r06s3qmc";
  };
  "https://jcenter.bintray.com/org/jetbrains/kotlin/kotlin-reflect/1.1.3-2/kotlin-reflect-1.1.3-2" = {
    host = jcenter;
    path = "org/jetbrains/kotlin/kotlin-reflect/1.1.3-2/kotlin-reflect-1.1.3-2";
    type = "jar";
    pom-sha256 = "16fpliipjmz73xbrz414sb5k6bqfbp49wpm0jcqclispfl69az54";
    jar-sha256 = "1xmxvdy60js2pjxgybaxmxcik0ig84klsds8kfb6knxyrf8zdr8f";
  };
  "https://jcenter.bintray.com/org/jetbrains/kotlin/kotlin-stdlib/1.1.3-2/kotlin-stdlib-1.1.3-2" = {
    host = jcenter;
    path = "org/jetbrains/kotlin/kotlin-stdlib/1.1.3-2/kotlin-stdlib-1.1.3-2";
    type = "jar";
    pom-sha256 = "1js5wyzca5bmnvacjaicmcvbs4k5caa0s6f8bvz5v7f86x68gzgq";
    jar-sha256 = "090cwfvy0my5lyv78sp2bkax0kpjwwm94hn0l0jgi0lzhf45527j";
  };
  "https://jcenter.bintray.com/org/jetbrains/trove4j/trove4j/20160824/trove4j-20160824" = {
    host = jcenter;
    path = "org/jetbrains/trove4j/trove4j/20160824/trove4j-20160824";
    type = "jar";
    pom-sha256 = "03q95m5jv8lybigkfjb2n2czayfcj59f2iqrpvj0s845hnfmlhaw";
    jar-sha256 = "1ryvjrjzixf0gwwnv35rn2lgawj5lj3hqs44ll3q6ipbilf8f5qr";
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
}
