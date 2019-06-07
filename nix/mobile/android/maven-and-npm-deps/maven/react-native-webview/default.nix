{}:

let
  jcenter = "https://jcenter.bintray.com";
  google = "https://dl.google.com/dl/android/maven2";
  fabric-io = "https://maven.fabric.io/public";

in {
  "https://dl.google.com/dl/android/maven2/androidx/databinding/databinding-common/3.2.1/databinding-common-3.2.1" = {
    host = google;
    path = "androidx/databinding/databinding-common/3.2.1/databinding-common-3.2.1";
    type = "jar";
    pom-sha256 = "19pz16l1kdxklkgsf5vndjcyvvhkz1a02q12nm4h700gabxbdv45";
    jar-sha256 = "14y6shybpr9vh64swh44v38hnnrfm5xy2mcyzfp68qn6jgkrsi2c";
  };
  "https://dl.google.com/dl/android/maven2/androidx/databinding/databinding-compiler-common/3.2.1/databinding-compiler-common-3.2.1" = {
    host = google;
    path = "androidx/databinding/databinding-compiler-common/3.2.1/databinding-compiler-common-3.2.1";
    type = "jar";
    pom-sha256 = "0vi88mm7g6wnnk21gi48prrxvx29w4vmg7va8z56gp00bnj14hni";
    jar-sha256 = "1dlpkpsfpmb3hcjj462svpg4h0d0vz8raj70abikpm04lzfslica";
  };
  "https://dl.google.com/dl/android/maven2/com/android/databinding/baseLibrary/3.2.1/baseLibrary-3.2.1" = {
    host = google;
    path = "com/android/databinding/baseLibrary/3.2.1/baseLibrary-3.2.1";
    type = "jar";
    pom-sha256 = "0gnk3050z33zs6s6krim68jdz7nl27shzv6gpkr2cwhvsvvdczkh";
    jar-sha256 = "17rbz1j3ghg759fc6iq4p5pq0qkw8crcb7s136by904fli1kv6g6";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/analytics-library/crash/26.2.1/crash-26.2.1" = {
    host = google;
    path = "com/android/tools/analytics-library/crash/26.2.1/crash-26.2.1";
    type = "jar";
    pom-sha256 = "0nvyjigdq10vy15qq3ypad7rxpvxz29jcq8b15jvh1zwan8dfwwz";
    jar-sha256 = "1gb504zxf8c7fc3q5phbdjgprlnbpzha52c3clmmx2yfck465lqk";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/analytics-library/protos/26.2.1/protos-26.2.1" = {
    host = google;
    path = "com/android/tools/analytics-library/protos/26.2.1/protos-26.2.1";
    type = "jar";
    pom-sha256 = "0gifp743ymw6d4ackqwfyb3q7gpzyhr4bwafzqk3pl51ldy8lxf7";
    jar-sha256 = "07v9pybbgwwq6amgdsyimx23s6s7fcl6lkdy12mqa7jm3xdiydrg";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/analytics-library/shared/26.2.1/shared-26.2.1" = {
    host = google;
    path = "com/android/tools/analytics-library/shared/26.2.1/shared-26.2.1";
    type = "jar";
    pom-sha256 = "00gglcwhcv6ykl4ll2gjmb72f7hy8gmpcarjga60jlnb8c2wmb2f";
    jar-sha256 = "0nkzm4k7n7bwg0vn6acw5l0mb0ahyrbha8zam8imzm54bxq4w7jc";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/analytics-library/tracker/26.2.1/tracker-26.2.1" = {
    host = google;
    path = "com/android/tools/analytics-library/tracker/26.2.1/tracker-26.2.1";
    type = "jar";
    pom-sha256 = "142zs2n27fs70jjx2dy55sw1bjs0s67j30r46sqwvrlh6b3266gx";
    jar-sha256 = "0xgvlwf703ypvfca0ngcrwk87l6k5py8vfxhvmazffb5jz64wqja";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/annotations/26.2.1/annotations-26.2.1" = {
    host = google;
    path = "com/android/tools/annotations/26.2.1/annotations-26.2.1";
    type = "jar";
    pom-sha256 = "1v7xxmyxgcbfvd1q8nrs24iwlbrsv6ym2q0lb3n1zz6zwa1rgmv4";
    jar-sha256 = "090gz4k2285ycpnf1znjlbcf876kmn6hgsscwsb4n5w0w2hwd4bk";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/aapt2-proto/0.3.1/aapt2-proto-0.3.1" = {
    host = google;
    path = "com/android/tools/build/aapt2-proto/0.3.1/aapt2-proto-0.3.1";
    type = "jar";
    pom-sha256 = "07fhxx0czmrmw9cfpxkcxy6z53hpsrpkk85l791w39srdgjz1r9i";
    jar-sha256 = "1nj9vkbz3yx43r56haq7373acmg6d5yv3cy9qsvzgamhi6dj20x5";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/apksig/3.2.1/apksig-3.2.1" = {
    host = google;
    path = "com/android/tools/build/apksig/3.2.1/apksig-3.2.1";
    type = "jar";
    pom-sha256 = "19iydx6q8n3wij887rybja0n9k7zxg3qwwpd2nls5z70g7s60dg2";
    jar-sha256 = "19fb5izibsqihnappybf98cr1h9k4hdjdr19mdx06rpazzzg4iib";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/apkzlib/3.2.1/apkzlib-3.2.1" = {
    host = google;
    path = "com/android/tools/build/apkzlib/3.2.1/apkzlib-3.2.1";
    type = "jar";
    pom-sha256 = "1cg7xrkfhy5dbca74pij3pjhy28gragsyibmcmzkjawgswngjcal";
    jar-sha256 = "1ajj2i32yhr59fmdqkkcmn94v6pk5jf8kj41zqqj94q574qx16n3";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/builder-model/3.2.1/builder-model-3.2.1" = {
    host = google;
    path = "com/android/tools/build/builder-model/3.2.1/builder-model-3.2.1";
    type = "jar";
    pom-sha256 = "07z3fcjgmfid0b0xmv3w1vg69z4a1wl1d35n6x6yxbxr7j1kaajd";
    jar-sha256 = "1zv5r7110qy14m6mxafwpjn07sx30lzjsdddnnf2y4pcpim8xxm9";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/builder-test-api/3.2.1/builder-test-api-3.2.1" = {
    host = google;
    path = "com/android/tools/build/builder-test-api/3.2.1/builder-test-api-3.2.1";
    type = "jar";
    pom-sha256 = "0bg1lj9kgafw233xn9cl5mwnwsjx3mblrsw15sd51kn9x1ny5i0n";
    jar-sha256 = "1gmng51kvd1klcbr9wqsk33zmzcdcbr92dx3cx4vajw8np1ccfjk";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/builder/3.2.1/builder-3.2.1" = {
    host = google;
    path = "com/android/tools/build/builder/3.2.1/builder-3.2.1";
    type = "jar";
    pom-sha256 = "0kz85c6cplwsmfh1xlzhpfpj1jbs1qwgk4j80phs8fkv1m9arcv3";
    jar-sha256 = "10xfgywcjryqn4rz3cm31a6mb6sqa3pncghinh4ivsfv2p8vzp5f";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/bundletool/0.5.0/bundletool-0.5.0" = {
    host = google;
    path = "com/android/tools/build/bundletool/0.5.0/bundletool-0.5.0";
    type = "jar";
    pom-sha256 = "0bqw882xwyzzxfd3hm218k448aja8m56xybaabl45k9ajsmvz6vw";
    jar-sha256 = "0cdqjbh78iq8ymhqzkrskhcv6symplm1pks9sysm9r0ql6077phz";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/gradle-api/3.2.1/gradle-api-3.2.1" = {
    host = google;
    path = "com/android/tools/build/gradle-api/3.2.1/gradle-api-3.2.1";
    type = "jar";
    pom-sha256 = "1vaf8y4vk96q246z2s1mmjwzvnss2gipqrpgrj9wqbswx9jynd0j";
    jar-sha256 = "1390vs9m382kzd23ik21lv7psvkpbqk98aznngyqmjhxmk2hmksp";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/gradle/3.2.1/gradle-3.2.1" = {
    host = google;
    path = "com/android/tools/build/gradle/3.2.1/gradle-3.2.1";
    type = "jar";
    pom-sha256 = "1jyqnv2s80dc41z0y13sva7pnsn44qx2bbqyj1s66q0m4r8jn1if";
    jar-sha256 = "0z6cimd0lbpm6y278qg8p1nj826ncxm37nj8931j21814dw1z2hh";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/jetifier/jetifier-core/1.0.0-alpha10/jetifier-core-1.0.0-alpha10" = {
    host = google;
    path = "com/android/tools/build/jetifier/jetifier-core/1.0.0-alpha10/jetifier-core-1.0.0-alpha10";
    type = "jar";
    pom-sha256 = "1gpvwnc67c6rh24ijnwim6c0ri51ww7syg49kqn605977bcbv5q8";
    jar-sha256 = "15m3hclhv9z935mcw2012j1ldzi2h7j0nqv09n8cshg49m03w1s0";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/jetifier/jetifier-processor/1.0.0-alpha10/jetifier-processor-1.0.0-alpha10" = {
    host = google;
    path = "com/android/tools/build/jetifier/jetifier-processor/1.0.0-alpha10/jetifier-processor-1.0.0-alpha10";
    type = "jar";
    pom-sha256 = "065fw0x0mwki888adbawnmjm8d0d9fsv8smnlga91pyh1ddhxkm0";
    jar-sha256 = "00kng40zp8j1v4mvfzbzvj5nfds5vq66bhmd6jgxyqyszi9z0qbw";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/manifest-merger/26.2.1/manifest-merger-26.2.1" = {
    host = google;
    path = "com/android/tools/build/manifest-merger/26.2.1/manifest-merger-26.2.1";
    type = "jar";
    pom-sha256 = "06dy9pahh78npcpp72k691f3xb48j4n514ssxisd3hmdnzq4pi2n";
    jar-sha256 = "19l18ac2g2mzbx7k68avhs9rq0llvgi53jzxik9ka41nccr5fc48";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/transform-api/2.0.0-deprecated-use-gradle-api/transform-api-2.0.0-deprecated-use-gradle-api" = {
    host = google;
    path = "com/android/tools/build/transform-api/2.0.0-deprecated-use-gradle-api/transform-api-2.0.0-deprecated-use-gradle-api";
    type = "jar";
    pom-sha256 = "0xc5zmf6125cpdb4144mblznjs16q4k5z4jpjbbs3g4ads2z6qkw";
    jar-sha256 = "0h2p4b7900j3girbkayq6b92pfw283rqnn74p4nlqlqwbv8a7r2d";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/common/26.2.1/common-26.2.1" = {
    host = google;
    path = "com/android/tools/common/26.2.1/common-26.2.1";
    type = "jar";
    pom-sha256 = "07xjg2nzhf3fv6cg7vz0733qv4n1ap8rb5s6qikrbydsjyjq7gf5";
    jar-sha256 = "1rbq5pm9g62l8qi3afzcl20fkn477plwg1sa03s6izqichnsn2m5";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/ddms/ddmlib/26.2.1/ddmlib-26.2.1" = {
    host = google;
    path = "com/android/tools/ddms/ddmlib/26.2.1/ddmlib-26.2.1";
    type = "jar";
    pom-sha256 = "1w2hy3pijxr97sxmwk7dabqk1is9shdjdn99krd9fnqx3wz9vfkr";
    jar-sha256 = "12r10nvliqgrha7p49kwnx86frb441wcqrcl4qkvz04rl4lhmgx4";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/dvlib/26.2.1/dvlib-26.2.1" = {
    host = google;
    path = "com/android/tools/dvlib/26.2.1/dvlib-26.2.1";
    type = "jar";
    pom-sha256 = "10bj0kkfxl3bdrvg128d6h2s2r8d32py7mbwnrc5l9w1096b388w";
    jar-sha256 = "1al82s7vxck2qwh47nklsxn9d7zv3dfs8yznzfqzj7cvhgr3pa3j";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/layoutlib/layoutlib-api/26.2.1/layoutlib-api-26.2.1" = {
    host = google;
    path = "com/android/tools/layoutlib/layoutlib-api/26.2.1/layoutlib-api-26.2.1";
    type = "jar";
    pom-sha256 = "19xw98bxnwba3jv3a44k6siqik5nvgc3sqd0csdkhpdr9y66vq11";
    jar-sha256 = "1piabivkrhxg3xqr02dn4z9px4y298gwrccm2l0zlcrp2b54zgyx";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/lint/lint-gradle-api/26.2.1/lint-gradle-api-26.2.1" = {
    host = google;
    path = "com/android/tools/lint/lint-gradle-api/26.2.1/lint-gradle-api-26.2.1";
    type = "jar";
    pom-sha256 = "1xllbdbfd3rzls1j9hxsg7fkvp7cc3kx7v155fhraj6az9g7flwq";
    jar-sha256 = "0bvcjwwfdgnxzlzznyj5gl3liin81wgm6zkr59gmc0g36apyg0r2";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/repository/26.2.1/repository-26.2.1" = {
    host = google;
    path = "com/android/tools/repository/26.2.1/repository-26.2.1";
    type = "jar";
    pom-sha256 = "1ppj1ilijp482fb7z35p7lsq2b3a341wxl009m9fgsh8nbgql1zb";
    jar-sha256 = "1hcw7lmjjcp9iccxd45zs6vcai52iynm11gk7mqfzyh3j7hdlx7s";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/sdk-common/26.2.1/sdk-common-26.2.1" = {
    host = google;
    path = "com/android/tools/sdk-common/26.2.1/sdk-common-26.2.1";
    type = "jar";
    pom-sha256 = "07qbivl4x5c2h685dxb9c7drrb9ma7d9rhnpl7l08rzk10sligys";
    jar-sha256 = "1x0w045fhhllk43815q8h74cd3qmaixkgjhzjv7kb6m65hllp7bm";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/sdklib/26.2.1/sdklib-26.2.1" = {
    host = google;
    path = "com/android/tools/sdklib/26.2.1/sdklib-26.2.1";
    type = "jar";
    pom-sha256 = "0a00r1llyxg8ls00pwg0yy0dxczrdpgf6jq74v0nm6c3jv8d4cbh";
    jar-sha256 = "0i8vp5l96wqsqb858vh5mj4pnjyyc1kwg563jrpynjmcbsnzg394";
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
  "https://jcenter.bintray.com/com/google/code/gson/gson-parent/2.8.0/gson-parent-2.8.0" = {
    host = jcenter;
    path = "com/google/code/gson/gson-parent/2.8.0/gson-parent-2.8.0";
    type = "jar";
    pom-sha256 = "1ljcy2x9lbwvlp1ik12wbfnklwsrigfwz3bw8r5vrwg9n5lq678g";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/com/google/code/gson/gson/2.8.0/gson-2.8.0" = {
    host = jcenter;
    path = "com/google/code/gson/gson/2.8.0/gson-2.8.0";
    type = "jar";
    pom-sha256 = "0vya2vs781xzrldxwgbnjzl232n38fncg4svmd19k3slrc71lcx5";
    jar-sha256 = "0988qi0f75hqyz2184xqcyrkifx055ghnxbzvk1z3i3rpmiif8n6";
  };
  "https://jcenter.bintray.com/com/google/errorprone/error_prone_annotations/2.2.0/error_prone_annotations-2.2.0" = {
    host = jcenter;
    path = "com/google/errorprone/error_prone_annotations/2.2.0/error_prone_annotations-2.2.0";
    type = "jar";
    pom-sha256 = "1m9dwhdyyign3633w1z49chspxs84k10ns226w9imrd43gm5h0jy";
    jar-sha256 = "06j838kxxyblsfg5y0s2gdhdk0b9b7h693fy85nw13lx3g525gbf";
  };
  "https://jcenter.bintray.com/com/google/errorprone/error_prone_parent/2.2.0/error_prone_parent-2.2.0" = {
    host = jcenter;
    path = "com/google/errorprone/error_prone_parent/2.2.0/error_prone_parent-2.2.0";
    type = "jar";
    pom-sha256 = "0f0iajykris8ynl83b56ydfb0a095bjiq9vb5j1nikjyvwnr0q64";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/com/google/google/1/google-1" = {
    host = jcenter;
    path = "com/google/google/1/google-1";
    type = "jar";
    pom-sha256 = "10by4ybrjnl8zwfg4ca74d0gcl4p9l7dzlfb9iwxw7m325xb2vfd";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/com/google/guava/guava-parent/23.0/guava-parent-23.0" = {
    host = jcenter;
    path = "com/google/guava/guava-parent/23.0/guava-parent-23.0";
    type = "jar";
    pom-sha256 = "0karnf4ycikvfmv3mwh25xhv56ss3s82rnszc0qf37wz686kmk36";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/com/google/guava/guava/23.0/guava-23.0" = {
    host = jcenter;
    path = "com/google/guava/guava/23.0/guava-23.0";
    type = "jar";
    pom-sha256 = "0kijfr2a28qpxf9zkd7i2fcpzfybpbxy50vcgh4yak3bg8mc8amc";
    jar-sha256 = "15k53fyw6ikrcpzmilqvh2vsfpm8cz9ri6xi8nwya5s153gq1akv";
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
  "https://jcenter.bintray.com/commons-codec/commons-codec/1.9/commons-codec-1.9" = {
    host = jcenter;
    path = "commons-codec/commons-codec/1.9/commons-codec-1.9";
    type = "jar";
    pom-sha256 = "143rq2sy3lp7qmzjn9a01qgz1mjg2jdlgi8x4266h2frkh1wzvz5";
    jar-sha256 = "1ki3lyadsy1v0685nfay63iw3a16w89l2fjwdfa0pgrs3ihd46dd";
  };
  "https://jcenter.bintray.com/commons-io/commons-io/2.4/commons-io-2.4" = {
    host = jcenter;
    path = "commons-io/commons-io/2.4/commons-io-2.4";
    type = "jar";
    pom-sha256 = "1wiikf78kr9k4pk68hb9jb9whrv19w8ir2kgxckad3wrrx3dvddj";
    jar-sha256 = "108mw2v8ncig29kjvzh8wi76plr01f4x5l3b1929xk5a7vf42snc";
  };
  "https://jcenter.bintray.com/commons-logging/commons-logging/1.2/commons-logging-1.2" = {
    host = jcenter;
    path = "commons-logging/commons-logging/1.2/commons-logging-1.2";
    type = "jar";
    pom-sha256 = "085vkxrh0hg2kwbyjblp0820hl1cpk38w5fc0zyzd1hdaymba6n9";
    jar-sha256 = "0dm61zgmgjkg67kf9dyrzgpayd18r656n05kiabmc3xyl0gfmpfs";
  };
  "https://jcenter.bintray.com/it/unimi/dsi/fastutil/7.2.0/fastutil-7.2.0" = {
    host = jcenter;
    path = "it/unimi/dsi/fastutil/7.2.0/fastutil-7.2.0";
    type = "jar";
    pom-sha256 = "1xry05xcp4r1r0kk16idpf4b0bw9db1z5qwhx7p7add745ji2fwm";
    jar-sha256 = "072r9dp1605ixwh31kjh5ynihlln2nxgl2gbwvvl41kl8f021ykl";
  };
  "https://jcenter.bintray.com/javax/inject/javax.inject/1/javax.inject-1" = {
    host = jcenter;
    path = "javax/inject/javax.inject/1/javax.inject-1";
    type = "jar";
    pom-sha256 = "1ylb39if9gqyj98fccb54s0ad25p19d811d2ixih8y3202qi4gll";
    jar-sha256 = "1zz7gnahy2352345411rjlhsf64ikkc6z49dqcv1cj0clm271iwi";
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
  "https://jcenter.bintray.com/net/sf/proguard/proguard-base/6.0.3/proguard-base-6.0.3" = {
    host = jcenter;
    path = "net/sf/proguard/proguard-base/6.0.3/proguard-base-6.0.3";
    type = "jar";
    pom-sha256 = "01s9xmjv2v7hc5p0zgpghabr6php3dnlcffmi2lq4mb7y9wkv0ia";
    jar-sha256 = "0r6jm7qb28qaxmp3ip96v2qhlvs00aw3k96d9qvpavm6bxlqkg3m";
  };
  "https://jcenter.bintray.com/net/sf/proguard/proguard-gradle/6.0.3/proguard-gradle-6.0.3" = {
    host = jcenter;
    path = "net/sf/proguard/proguard-gradle/6.0.3/proguard-gradle-6.0.3";
    type = "jar";
    pom-sha256 = "0msslcrlhqrkwd3529kcn78l4kc1717bvn6980fhvs4csqbp6p2s";
    jar-sha256 = "13qcl1yb819arw80xxran9sghjhsq6zgwz381vfr252jiqbb24nc";
  };
  "https://jcenter.bintray.com/net/sf/proguard/proguard-parent/6.0.3/proguard-parent-6.0.3" = {
    host = jcenter;
    path = "net/sf/proguard/proguard-parent/6.0.3/proguard-parent-6.0.3";
    type = "jar";
    pom-sha256 = "1870dlyd0cvsp31jg05irr4c8dgs18qhkisadh1knb1isazncwnq";
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
  "https://jcenter.bintray.com/org/apache/commons/commons-parent/25/commons-parent-25" = {
    host = jcenter;
    path = "org/apache/commons/commons-parent/25/commons-parent-25";
    type = "jar";
    pom-sha256 = "1flhjyg2b14ch0wvsbimqli7vmpxim8yg54h6xkni1rf8i8fcyj6";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/apache/commons/commons-parent/32/commons-parent-32" = {
    host = jcenter;
    path = "org/apache/commons/commons-parent/32/commons-parent-32";
    type = "jar";
    pom-sha256 = "1s2bdgr10jshy5v0bs03c2d7hw3qxyfkf5fl90hh7x1gifpmilp4";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/apache/commons/commons-parent/34/commons-parent-34" = {
    host = jcenter;
    path = "org/apache/commons/commons-parent/34/commons-parent-34";
    type = "jar";
    pom-sha256 = "175p0hnjk93gmgh8fv63z0xmd9jf5sgdq9ii54xiy7b4dp86jbis";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/apache/commons/commons-parent/39/commons-parent-39" = {
    host = jcenter;
    path = "org/apache/commons/commons-parent/39/commons-parent-39";
    type = "jar";
    pom-sha256 = "19qxlq7zcvf8cw2hyrcb5d6b9cbbd6ccwnahv2v3wp1al3hjgkc7";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/apache/httpcomponents/httpclient/4.5.2/httpclient-4.5.2" = {
    host = jcenter;
    path = "org/apache/httpcomponents/httpclient/4.5.2/httpclient-4.5.2";
    type = "jar";
    pom-sha256 = "1w0rxkvljw15ccw4b189izsqjdmirj6x3gpgn0l4p6l246x03028";
    jar-sha256 = "0ms00zc28pwqk83nwwbafhq6p8zci9mrjzbqalpn6v0d80hwdzqd";
  };
  "https://jcenter.bintray.com/org/apache/httpcomponents/httpcomponents-client/4.5.2/httpcomponents-client-4.5.2" = {
    host = jcenter;
    path = "org/apache/httpcomponents/httpcomponents-client/4.5.2/httpcomponents-client-4.5.2";
    type = "jar";
    pom-sha256 = "00gdpvi7b5dmnkpir2ryp8nkak10nxpmlcwyc4l6pi75q6kcps0x";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/apache/httpcomponents/httpcomponents-core/4.4.5/httpcomponents-core-4.4.5" = {
    host = jcenter;
    path = "org/apache/httpcomponents/httpcomponents-core/4.4.5/httpcomponents-core-4.4.5";
    type = "jar";
    pom-sha256 = "1p0w158nih5h93cq292v8v0ga6syrky8kbcb6zalh26884sj9n8q";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/apache/httpcomponents/httpcore/4.4.5/httpcore-4.4.5" = {
    host = jcenter;
    path = "org/apache/httpcomponents/httpcore/4.4.5/httpcore-4.4.5";
    type = "jar";
    pom-sha256 = "1lpyhh30s80b7wm3492w3sph56bq1wv8l0ad0m84wlskyy6s2zql";
    jar-sha256 = "1hyfh02rwrwbhxwb91lpba25646am6lh3jv5f05f9dyafhw4bmb4";
  };
  "https://jcenter.bintray.com/org/apache/httpcomponents/httpmime/4.5.2/httpmime-4.5.2" = {
    host = jcenter;
    path = "org/apache/httpcomponents/httpmime/4.5.2/httpmime-4.5.2";
    type = "jar";
    pom-sha256 = "111pbflfshg414sx0m0bk62s8ipchqhskapwvsd0486qf9i5njq0";
    jar-sha256 = "01wg3zi4i8q3m2nximvx7a54bz385r1daqc4psr3s1b295z3y6i3";
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
  "https://jcenter.bintray.com/org/jdom/jdom2/2.0.6/jdom2-2.0.6" = {
    host = jcenter;
    path = "org/jdom/jdom2/2.0.6/jdom2-2.0.6";
    type = "jar";
    pom-sha256 = "13axlsl2zmqzj885rrq5wib545k8kl26wk23h8dp8srkzrwkmcj7";
    jar-sha256 = "1xbwm6z7izkifb30wxs0aqhw0iqrqal521blsq1mdl86lqdz2i8k";
  };
  "https://jcenter.bintray.com/org/jetbrains/annotations/13.0/annotations-13.0" = {
    host = jcenter;
    path = "org/jetbrains/annotations/13.0/annotations-13.0";
    type = "jar";
    pom-sha256 = "15y3p0xicxjx6y38pj39vm3q56xqnfhgf6yyplcrhdpzxlmynnln";
    jar-sha256 = "0y0l26ys36zlrsw98335a7wc1cl894zc1jjyj8sgvmg2r06s3qmc";
  };
  "https://jcenter.bintray.com/org/jetbrains/kotlin/kotlin-android-extensions/1.3.11/kotlin-android-extensions-1.3.11" = {
    host = jcenter;
    path = "org/jetbrains/kotlin/kotlin-android-extensions/1.3.11/kotlin-android-extensions-1.3.11";
    type = "jar";
    pom-sha256 = "0ixza4m7cv0ndx4sfpz56snbrr9fpb8ndkr74lpyrh9j4aldjk8a";
    jar-sha256 = "155kgb92xr53j3xvhcbvb9d5ra6h2n8llr4vp19ixwjf8bx1529l";
  };
  "https://jcenter.bintray.com/org/jetbrains/kotlin/kotlin-annotation-processing-gradle/1.3.11/kotlin-annotation-processing-gradle-1.3.11" = {
    host = jcenter;
    path = "org/jetbrains/kotlin/kotlin-annotation-processing-gradle/1.3.11/kotlin-annotation-processing-gradle-1.3.11";
    type = "jar";
    pom-sha256 = "1cjy54sm1vad6r08qzxgiafwnm215mpm617n3fhgq8p39zm2rc5l";
    jar-sha256 = "1j0yym9m73lc3ghah8vqvh9hdyc0aiabljz9z3vj1f66cc2j5rx8";
  };
  "https://jcenter.bintray.com/org/jetbrains/kotlin/kotlin-build-common/1.3.11/kotlin-build-common-1.3.11" = {
    host = jcenter;
    path = "org/jetbrains/kotlin/kotlin-build-common/1.3.11/kotlin-build-common-1.3.11";
    type = "jar";
    pom-sha256 = "0xkcd6219zh4rdxzm19jg9nkdchd5qwgb0wh87sr1zknf4jvh1fg";
    jar-sha256 = "1nrlgzwi7s4d2l0db16yj33z0lvbisc2gsk51sssvfr6h9qhjsv0";
  };
  "https://jcenter.bintray.com/org/jetbrains/kotlin/kotlin-compiler-embeddable/1.3.11/kotlin-compiler-embeddable-1.3.11" = {
    host = jcenter;
    path = "org/jetbrains/kotlin/kotlin-compiler-embeddable/1.3.11/kotlin-compiler-embeddable-1.3.11";
    type = "jar";
    pom-sha256 = "0648hi5yb62j1yarl33lkg4xcqyzg9xh8j99njndbrl833c7njvx";
    jar-sha256 = "19cxqx4g7lghsa4888r159x38dq0352rsyayjd5s4hy6jvhc61nl";
  };
  "https://jcenter.bintray.com/org/jetbrains/kotlin/kotlin-compiler-runner/1.3.11/kotlin-compiler-runner-1.3.11" = {
    host = jcenter;
    path = "org/jetbrains/kotlin/kotlin-compiler-runner/1.3.11/kotlin-compiler-runner-1.3.11";
    type = "jar";
    pom-sha256 = "0hkgwbghwjm9vlnwvhgm2msn1nr1pigdc8dyq0c3ypkwajwxs0q4";
    jar-sha256 = "042dakp46sdnsrhvzss38j40pn9g29a68aj34jfpifjxskhvqmam";
  };
  "https://jcenter.bintray.com/org/jetbrains/kotlin/kotlin-daemon-client/1.3.11/kotlin-daemon-client-1.3.11" = {
    host = jcenter;
    path = "org/jetbrains/kotlin/kotlin-daemon-client/1.3.11/kotlin-daemon-client-1.3.11";
    type = "jar";
    pom-sha256 = "0lz7029gikxcfy4zb6gjsc55mwdgs6npkyqa13lnrl0ggk1gy0mj";
    jar-sha256 = "17ypia2lkg3jy1j7hkjj53bp73ysvx84fm9gralwbdaw9ylffj1b";
  };
  "https://jcenter.bintray.com/org/jetbrains/kotlin/kotlin-gradle-plugin-api/1.3.11/kotlin-gradle-plugin-api-1.3.11" = {
    host = jcenter;
    path = "org/jetbrains/kotlin/kotlin-gradle-plugin-api/1.3.11/kotlin-gradle-plugin-api-1.3.11";
    type = "jar";
    pom-sha256 = "02zkgxkmv29zv0y48kswql4k2q82hd5l23lzs23gw10cjckywq66";
    jar-sha256 = "0z6sri6f37l6xb140lhxmc2cl066ji6sq1kfn4knaqqs6scagh6n";
  };
  "https://jcenter.bintray.com/org/jetbrains/kotlin/kotlin-gradle-plugin-model/1.3.11/kotlin-gradle-plugin-model-1.3.11" = {
    host = jcenter;
    path = "org/jetbrains/kotlin/kotlin-gradle-plugin-model/1.3.11/kotlin-gradle-plugin-model-1.3.11";
    type = "jar";
    pom-sha256 = "03hnxs0xzkpzd3xkq14ay6yx9pj4d50czq4z9cl9jynsfiicvzxf";
    jar-sha256 = "1q0xb4q9ly665qm5aw78hlr3f6pglfi588pvkj305agwq8sdhc5b";
  };
  "https://jcenter.bintray.com/org/jetbrains/kotlin/kotlin-gradle-plugin/1.3.11/kotlin-gradle-plugin-1.3.11" = {
    host = jcenter;
    path = "org/jetbrains/kotlin/kotlin-gradle-plugin/1.3.11/kotlin-gradle-plugin-1.3.11";
    type = "jar";
    pom-sha256 = "0s0q5cp6l7s600hci7cvfsrz0i93iw4yx7bvsqqvbf92bgksrb48";
    jar-sha256 = "1s3krvljb962fp86gxw63gflplxzw3cbzy287qz5lp3783lw4bjp";
  };
  "https://jcenter.bintray.com/org/jetbrains/kotlin/kotlin-native-utils/1.3.11/kotlin-native-utils-1.3.11" = {
    host = jcenter;
    path = "org/jetbrains/kotlin/kotlin-native-utils/1.3.11/kotlin-native-utils-1.3.11";
    type = "jar";
    pom-sha256 = "1jn5103z7rkl7syjzbc2vqq211r1zmjq9h61qm56mwld72kv5kbs";
    jar-sha256 = "1gz79xyjm5cfm4xfcnfy5qrisj17fc3y890flfmlal32hcc5qam7";
  };
  "https://jcenter.bintray.com/org/jetbrains/kotlin/kotlin-reflect/1.3.11/kotlin-reflect-1.3.11" = {
    host = jcenter;
    path = "org/jetbrains/kotlin/kotlin-reflect/1.3.11/kotlin-reflect-1.3.11";
    type = "jar";
    pom-sha256 = "1bgghkd290vfv38sv3alkb0bz8380xaa7abzpd3hvcj4y462rmp7";
    jar-sha256 = "0va92bqn8n736dhc2gxcvb7z288mbflnmzpgc4wbbj0rhgxjyixq";
  };
  "https://jcenter.bintray.com/org/jetbrains/kotlin/kotlin-script-runtime/1.3.11/kotlin-script-runtime-1.3.11" = {
    host = jcenter;
    path = "org/jetbrains/kotlin/kotlin-script-runtime/1.3.11/kotlin-script-runtime-1.3.11";
    type = "jar";
    pom-sha256 = "1sr6lrdkfzgfaypq4mcc6h1n5q799vzincfngwfw028i3p7gpjhz";
    jar-sha256 = "1bimqf9rf6z3yg73bpc82zh9sbnfw2xirvj83y4bhp7wfcrr92z1";
  };
  "https://jcenter.bintray.com/org/jetbrains/kotlin/kotlin-scripting-compiler-embeddable/1.3.11/kotlin-scripting-compiler-embeddable-1.3.11" = {
    host = jcenter;
    path = "org/jetbrains/kotlin/kotlin-scripting-compiler-embeddable/1.3.11/kotlin-scripting-compiler-embeddable-1.3.11";
    type = "jar";
    pom-sha256 = "1w8n7mx6az8cg5lga18294jfpji3k1219dshwpmvl3fyb3jfpdn7";
    jar-sha256 = "0mlky818z269mdcnyy1k48kq2xclaa5lvc4xxhy013ljlmsf26ai";
  };
  "https://jcenter.bintray.com/org/jetbrains/kotlin/kotlin-stdlib-common/1.3.11/kotlin-stdlib-common-1.3.11" = {
    host = jcenter;
    path = "org/jetbrains/kotlin/kotlin-stdlib-common/1.3.11/kotlin-stdlib-common-1.3.11";
    type = "jar";
    pom-sha256 = "1psasklsaskhc3b87khszxg215irrizcvlarhp8y79qjmbkra42h";
    jar-sha256 = "1z7rr1fsk7s9b8rza85a3j09w2ljiwx69nxvjmdnlwzmxldx3gbm";
  };
  "https://jcenter.bintray.com/org/jetbrains/kotlin/kotlin-stdlib-jdk7/1.2.71/kotlin-stdlib-jdk7-1.2.71" = {
    host = jcenter;
    path = "org/jetbrains/kotlin/kotlin-stdlib-jdk7/1.2.71/kotlin-stdlib-jdk7-1.2.71";
    type = "jar";
    pom-sha256 = "1cw0f0cvc2fwx3rg4312w0liybxni6l1s6krjkkjij16apcjc54l";
    jar-sha256 = "10l0hm329liw5w624pxzlw048n5z3nyx606fj96pvq20n9hvsdmi";
  };
  "https://jcenter.bintray.com/org/jetbrains/kotlin/kotlin-stdlib-jdk8/1.2.71/kotlin-stdlib-jdk8-1.2.71" = {
    host = jcenter;
    path = "org/jetbrains/kotlin/kotlin-stdlib-jdk8/1.2.71/kotlin-stdlib-jdk8-1.2.71";
    type = "jar";
    pom-sha256 = "1cwljpvn11y44m69ipikbq694x9y7y3kkvfplay6cpzdx7q36p44";
    jar-sha256 = "1abfjjcx2d85aabsa4hnmihy0if1y19rll72yys682vr8yzqlg5c";
  };
  "https://jcenter.bintray.com/org/jetbrains/kotlin/kotlin-stdlib/1.3.11/kotlin-stdlib-1.3.11" = {
    host = jcenter;
    path = "org/jetbrains/kotlin/kotlin-stdlib/1.3.11/kotlin-stdlib-1.3.11";
    type = "jar";
    pom-sha256 = "0bmd8fy9bs6zcyw3dm2vi9mvzxz0lhimz4z4z6nksf2kqh0a47gg";
    jar-sha256 = "1v1zx1jw1lpv1h9rmd2hpiacwzvi348hcl64wvbl76vazrwskrsw";
  };
  "https://jcenter.bintray.com/org/jvnet/staxex/stax-ex/1.7.7/stax-ex-1.7.7" = {
    host = jcenter;
    path = "org/jvnet/staxex/stax-ex/1.7.7/stax-ex-1.7.7";
    type = "jar";
    pom-sha256 = "1b9yhymk6zp9sva68v0829xsrvsdz03rjlb2hfyh0yindh5lbjr7";
    jar-sha256 = "1d1zh4ymilfc2shm51fcwb7c4i5f6nnmkvkzksqdxh33f7bzf7x3";
  };
  "https://jcenter.bintray.com/org/ow2/asm/asm-analysis/6.0/asm-analysis-6.0" = {
    host = jcenter;
    path = "org/ow2/asm/asm-analysis/6.0/asm-analysis-6.0";
    type = "jar";
    pom-sha256 = "0m0h4mzcg9nw29rz76sl0s7bfz71wm6xxdakvamlmqp380pg9ank";
    jar-sha256 = "1x6113vddxcsy1m42v9ji90g5n9vn0hz50b4hp26qflw463n66ig";
  };
  "https://jcenter.bintray.com/org/ow2/asm/asm-commons/6.0/asm-commons-6.0" = {
    host = jcenter;
    path = "org/ow2/asm/asm-commons/6.0/asm-commons-6.0";
    type = "jar";
    pom-sha256 = "1pawmm9ydlf0h4wrn15wcwdwqxj9jm0mgsqnj0i5gfdgffag5x4h";
    jar-sha256 = "1gwsf7cvnvss04fbhybvznbm517rkbaya7yhvixh2sm9933fbg7i";
  };
  "https://jcenter.bintray.com/org/ow2/asm/asm-parent/6.0/asm-parent-6.0" = {
    host = jcenter;
    path = "org/ow2/asm/asm-parent/6.0/asm-parent-6.0";
    type = "jar";
    pom-sha256 = "053xrln1w9nj0aw23wsiqkgn8569fk6zx7mw8dns9jgcp55hc7br";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/ow2/asm/asm-tree/6.0/asm-tree-6.0" = {
    host = jcenter;
    path = "org/ow2/asm/asm-tree/6.0/asm-tree-6.0";
    type = "jar";
    pom-sha256 = "1d01wx74alk2xd8d1ic5yc87zacnwm2ylf4a6y95l66vgw6px21n";
    jar-sha256 = "05b2229fwn5cndkabjnlkwzy6098h9bghlyjwicqfz3jd7xrhyc8";
  };
  "https://jcenter.bintray.com/org/ow2/asm/asm-util/6.0/asm-util-6.0" = {
    host = jcenter;
    path = "org/ow2/asm/asm-util/6.0/asm-util-6.0";
    type = "jar";
    pom-sha256 = "04hz479zzrz6jk1ldsss43s212w0ln1dl69la2bpn5ldk0gwdprw";
    jar-sha256 = "1xm5pi4v9m041csb1956lni7mcd3163qy675c991fw7qn2yzwsim";
  };
  "https://jcenter.bintray.com/org/ow2/asm/asm/6.0/asm-6.0" = {
    host = jcenter;
    path = "org/ow2/asm/asm/6.0/asm-6.0";
    type = "jar";
    pom-sha256 = "0bn3281hli8z7dx2cs6s0a0bxc0sbfsbn9jl12cyc4ki35z4kg62";
    jar-sha256 = "0q8489h5grwm2xxvkikd91nflq47xbjalp79m2cphsaf9b3p32fx";
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
