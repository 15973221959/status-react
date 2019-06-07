{}:

let
  jcenter = "https://jcenter.bintray.com";
  google = "https://dl.google.com/dl/android/maven2";
  fabric-io = "https://maven.fabric.io/public";

in {
  "https://jcenter.bintray.com/com/android/databinding/baseLibrary/2.2.3/baseLibrary-2.2.3" = {
    host = jcenter;
    path = "com/android/databinding/baseLibrary/2.2.3/baseLibrary-2.2.3";
    type = "jar";
    pom-sha256 = "0jbcdi7b4n5w65s10d4lpclm4krh2535bprdqlmjfsi56qykv35c";
    jar-sha256 = "1sv4gs06aw7kshhc4p2nf4jqxip7dl0a6d6ci7fgv6zfjpn3wd2i";
  };
  "https://jcenter.bintray.com/com/android/databinding/compilerCommon/2.2.3/compilerCommon-2.2.3" = {
    host = jcenter;
    path = "com/android/databinding/compilerCommon/2.2.3/compilerCommon-2.2.3";
    type = "jar";
    pom-sha256 = "16c41n7lv97dy8agg7478m2zvsrcscy4z7dzhda2ksgqr3hm3rgj";
    jar-sha256 = "1dmkhpih05bf27zb9rcfhphc6q27y1bjjfdbw5bp3325d7h63ddm";
  };
  "https://jcenter.bintray.com/com/android/tools/analytics-library/protos/25.2.3/protos-25.2.3" = {
    host = jcenter;
    path = "com/android/tools/analytics-library/protos/25.2.3/protos-25.2.3";
    type = "jar";
    pom-sha256 = "1kgb73nna3g3xx8ppcljnvkw5pcjicawfm3m8zkm8xw4xlpk7kri";
    jar-sha256 = "0scy1laly7qra41w6wvh4svrg4j1r9mpdsdp1755ajw2vif0wy8v";
  };
  "https://jcenter.bintray.com/com/android/tools/analytics-library/shared/25.2.3/shared-25.2.3" = {
    host = jcenter;
    path = "com/android/tools/analytics-library/shared/25.2.3/shared-25.2.3";
    type = "jar";
    pom-sha256 = "0z9w8fnjzvp08kpmh18mgjviwds0z40w1my02mcjczkh0apa1qqf";
    jar-sha256 = "13gqv286zh5pbcf6f2b1d0vki247yfgmwqip4h759c70jm61d4sa";
  };
  "https://jcenter.bintray.com/com/android/tools/analytics-library/tracker/25.2.3/tracker-25.2.3" = {
    host = jcenter;
    path = "com/android/tools/analytics-library/tracker/25.2.3/tracker-25.2.3";
    type = "jar";
    pom-sha256 = "0fvcp5i61kzkppj5j6q5dx4w72qi5ln1ihpqhq89566xxxws962k";
    jar-sha256 = "062y6gcaanbz8cmzmbnvpmd3c08jggpbg7czk38fqfh6cxb6gxm1";
  };
  "https://jcenter.bintray.com/com/android/tools/annotations/24.5.0/annotations-24.5.0" = {
    host = jcenter;
    path = "com/android/tools/annotations/24.5.0/annotations-24.5.0";
    type = "jar";
    pom-sha256 = "0m2wj96mar7cij4yzvcx1da9ab7ffcaa1s8mcgl9z242h7srpjf3";
    jar-sha256 = "08nrgm8j7q49ds9r0k1cbn1jsxdv0if9bhi37szxpjz7ks7azyr9";
  };
  "https://jcenter.bintray.com/com/android/tools/annotations/25.2.3/annotations-25.2.3" = {
    host = jcenter;
    path = "com/android/tools/annotations/25.2.3/annotations-25.2.3";
    type = "jar";
    pom-sha256 = "0smwn3m7xy8blg3glng7720vh47bfjnkhhzz4hq6qbh0nik4pdmf";
    jar-sha256 = "01mr9q7hg91rcbvn7kqz0g6cbm0krra76kiswf7s0jv94frjh5xz";
  };
  "https://jcenter.bintray.com/com/android/tools/build/builder-model/2.2.3/builder-model-2.2.3" = {
    host = jcenter;
    path = "com/android/tools/build/builder-model/2.2.3/builder-model-2.2.3";
    type = "jar";
    pom-sha256 = "0vpsmjqi6vl4limpf6kjvz0nmbdg81szp97wxbsadd4m7swmqkxd";
    jar-sha256 = "1k00s7zrhvrhiq88m3k4k0nqwnlg69hhfppf3vbijr6dibnjhqd3";
  };
  "https://jcenter.bintray.com/com/android/tools/build/builder-test-api/2.2.3/builder-test-api-2.2.3" = {
    host = jcenter;
    path = "com/android/tools/build/builder-test-api/2.2.3/builder-test-api-2.2.3";
    type = "jar";
    pom-sha256 = "0mzah6h5i2hnf1fi7cbdixvh0wc6b73y49ff8xi4qp67ifiwqvvh";
    jar-sha256 = "17p19jn26wbdcn4dhjpcn4nrg0f9240kg2xyffml6rdrvkwccr7p";
  };
  "https://jcenter.bintray.com/com/android/tools/build/builder/2.2.3/builder-2.2.3" = {
    host = jcenter;
    path = "com/android/tools/build/builder/2.2.3/builder-2.2.3";
    type = "jar";
    pom-sha256 = "0838z1iwk5kjh7sa4mycj38cc1d38vzpw5kwxhwrhag148frdmf1";
    jar-sha256 = "026kxz8lf59k1vkd9da3vcn41rzcrgwdff0ai88j58jsg1p5k6x6";
  };
  "https://jcenter.bintray.com/com/android/tools/build/gradle-api/2.2.3/gradle-api-2.2.3" = {
    host = jcenter;
    path = "com/android/tools/build/gradle-api/2.2.3/gradle-api-2.2.3";
    type = "jar";
    pom-sha256 = "0a342sd1nvzz81qadnaimrw3yx35scaxljqj8vy8a3jr2w8bmigp";
    jar-sha256 = "0dv81kmr4nrjs65macr4k8j890g747aiq7c02ca7hfxnywmmmyrw";
  };
  "https://jcenter.bintray.com/com/android/tools/build/gradle-core/2.2.3/gradle-core-2.2.3" = {
    host = jcenter;
    path = "com/android/tools/build/gradle-core/2.2.3/gradle-core-2.2.3";
    type = "jar";
    pom-sha256 = "1p2xckdbd4qjn3drmfvs0xypkhnnh4rl410b49y508lhj2fp6xm9";
    jar-sha256 = "107pwwxddwb63jp2lvz4lhggh48c2ycaahry85vsmiim696fyl9w";
  };
  "https://jcenter.bintray.com/com/android/tools/build/gradle/2.2.3/gradle-2.2.3" = {
    host = jcenter;
    path = "com/android/tools/build/gradle/2.2.3/gradle-2.2.3";
    type = "jar";
    pom-sha256 = "0n923gbykymnya9jlyfvwbqhjdi0dy18yhvazvh6jmx5rmwpn5kw";
    jar-sha256 = "1f5bmkgd54sgs4nma6vqmgvp2gnran8q58fpr8mbyr30jnpcvycl";
  };
  "https://jcenter.bintray.com/com/android/tools/build/manifest-merger/25.2.3/manifest-merger-25.2.3" = {
    host = jcenter;
    path = "com/android/tools/build/manifest-merger/25.2.3/manifest-merger-25.2.3";
    type = "jar";
    pom-sha256 = "05d357pc8pv8mczfai9f7pjflybxg9kphgss18prby5wfl18wl8l";
    jar-sha256 = "0dqqshpb27ygdwlr6d933xlsgdgi2b9qvr81pcqc0y5kp2wv9s4z";
  };
  "https://jcenter.bintray.com/com/android/tools/build/transform-api/2.0.0-deprecated-use-gradle-api/transform-api-2.0.0-deprecated-use-gradle-api" = {
    host = jcenter;
    path = "com/android/tools/build/transform-api/2.0.0-deprecated-use-gradle-api/transform-api-2.0.0-deprecated-use-gradle-api";
    type = "jar";
    pom-sha256 = "1sr2ksrjzr9gv0y70w17l97x9a2anlv63lx7n803lgvxf2gig7fl";
    jar-sha256 = "16nfz9w0zpbbb0ghsab3gfp53imkr4d3gvhlgaz1m7v7w4d1bd78";
  };
  "https://jcenter.bintray.com/com/android/tools/common/25.2.3/common-25.2.3" = {
    host = jcenter;
    path = "com/android/tools/common/25.2.3/common-25.2.3";
    type = "jar";
    pom-sha256 = "0gc2fk3frcfm3b8amnyymywzyq36cj80gg7i28cg9hh3ywd16vy8";
    jar-sha256 = "0prpydayr8acblgi2z1ib200i4bv7fjza8lfc3hxzb9vnlapli7a";
  };
  "https://jcenter.bintray.com/com/android/tools/ddms/ddmlib/25.2.3/ddmlib-25.2.3" = {
    host = jcenter;
    path = "com/android/tools/ddms/ddmlib/25.2.3/ddmlib-25.2.3";
    type = "jar";
    pom-sha256 = "1mi46px82jk0252nfsrvr95g90prn8xz328a6j8z2csgsnxldxa1";
    jar-sha256 = "1nkjvsqbssy43c2x1v2g9v3flrjbaanli8zkym3gzlvjykz5pw11";
  };
  "https://jcenter.bintray.com/com/android/tools/dvlib/25.2.3/dvlib-25.2.3" = {
    host = jcenter;
    path = "com/android/tools/dvlib/25.2.3/dvlib-25.2.3";
    type = "jar";
    pom-sha256 = "099hb2p87nilb7pgpw8mvfhkvzr4ccr72zp380bg7ynkl2y76n6w";
    jar-sha256 = "17lfq22x2d4sc2shg8yljadw2rsd4pfhygsnj94f7sb15a9aai15";
  };
  "https://jcenter.bintray.com/com/android/tools/external/com-intellij/uast/145.597.4/uast-145.597.4" = {
    host = jcenter;
    path = "com/android/tools/external/com-intellij/uast/145.597.4/uast-145.597.4";
    type = "jar";
    pom-sha256 = "046zmm0fvxskb9vwa5rsd0k1hsxv1j5jgy31ys72dr8jpmqwpcs0";
    jar-sha256 = "1k3vd204dmpa69cxws3lkxa158x169f6rd40yk1nbir0ylahxb8h";
  };
  "https://jcenter.bintray.com/com/android/tools/external/lombok/lombok-ast/0.2.3/lombok-ast-0.2.3" = {
    host = jcenter;
    path = "com/android/tools/external/lombok/lombok-ast/0.2.3/lombok-ast-0.2.3";
    type = "jar";
    pom-sha256 = "1gbljyqw5r2vc31dl1flclsvdias3049pdrc6dicizcbvph92qcc";
    jar-sha256 = "0vg8w757n8a8v6lafrg4xb9nl7dqw3x56p1qzfsp20j395p0vss5";
  };
  "https://jcenter.bintray.com/com/android/tools/jack/jack-api/0.11.0/jack-api-0.11.0" = {
    host = jcenter;
    path = "com/android/tools/jack/jack-api/0.11.0/jack-api-0.11.0";
    type = "jar";
    pom-sha256 = "170p7q6rbfjvs2avag290v1i560amqsyq2vswnqx1dh4fplld2vz";
    jar-sha256 = "0v4a96vrsj6qsff6zdv18km3wqwvk9cyz7pgr4zrprxcnfnhvzra";
  };
  "https://jcenter.bintray.com/com/android/tools/jill/jill-api/0.10.0/jill-api-0.10.0" = {
    host = jcenter;
    path = "com/android/tools/jill/jill-api/0.10.0/jill-api-0.10.0";
    type = "jar";
    pom-sha256 = "1k2pl81nx3175v1nhw8j4q59l5mvx59zzdwn3x8k2pr144ql794i";
    jar-sha256 = "0vzn55g19plfx1bd3cn1kxdvzi88ra3j7bxgc08i667hymm918wq";
  };
  "https://jcenter.bintray.com/com/android/tools/layoutlib/layoutlib-api/25.2.3/layoutlib-api-25.2.3" = {
    host = jcenter;
    path = "com/android/tools/layoutlib/layoutlib-api/25.2.3/layoutlib-api-25.2.3";
    type = "jar";
    pom-sha256 = "1fxa5hvpaxmhz3iiq583ix4p4f63za4b68k8cfzclmpwrzygmxj2";
    jar-sha256 = "1a63n18l7fnmhydavmwyqxs1qjjvczzvngdc4rn3mqs3ni44jnv5";
  };
  "https://jcenter.bintray.com/com/android/tools/lint/lint-api/25.2.3/lint-api-25.2.3" = {
    host = jcenter;
    path = "com/android/tools/lint/lint-api/25.2.3/lint-api-25.2.3";
    type = "jar";
    pom-sha256 = "12cllgd4y7dmcxxbmmr4bm8rd52250kxr8iyy01azabmvswh1rk6";
    jar-sha256 = "15hgxnknq77ys9j6sa6pg4aalwzwq943dlb9kmss4n51adc6a76b";
  };
  "https://jcenter.bintray.com/com/android/tools/lint/lint-checks/25.2.3/lint-checks-25.2.3" = {
    host = jcenter;
    path = "com/android/tools/lint/lint-checks/25.2.3/lint-checks-25.2.3";
    type = "jar";
    pom-sha256 = "1ygna0q6l84y51xssbilrkvwy081n14kdlhzkqskkkl3s2yny17d";
    jar-sha256 = "1vr4l9iafpz4bdy6k4s9cmzdsbxp74dqyskdl2ccflg8vswg383s";
  };
  "https://jcenter.bintray.com/com/android/tools/lint/lint/25.2.3/lint-25.2.3" = {
    host = jcenter;
    path = "com/android/tools/lint/lint/25.2.3/lint-25.2.3";
    type = "jar";
    pom-sha256 = "078rrpxrrljlp7dlmiypc1ff81gxh56yxr76h24p3as1db3rmv11";
    jar-sha256 = "0c95vndd4pq749hp4b8cg0rc2kib9a0m1ys704wpmg84pnapc3cy";
  };
  "https://jcenter.bintray.com/com/android/tools/repository/25.2.3/repository-25.2.3" = {
    host = jcenter;
    path = "com/android/tools/repository/25.2.3/repository-25.2.3";
    type = "jar";
    pom-sha256 = "190yrkr7lmzsnf0zpj4bs7yg6gdah541nbvdyzymjbg1n36mkxxj";
    jar-sha256 = "0vqhlcv06kwfis6fqqjpzx12x56mnnqd9sy6sppmqnhphdvgjfh3";
  };
  "https://jcenter.bintray.com/com/android/tools/sdk-common/25.2.3/sdk-common-25.2.3" = {
    host = jcenter;
    path = "com/android/tools/sdk-common/25.2.3/sdk-common-25.2.3";
    type = "jar";
    pom-sha256 = "1n4mrhy606723yyvkwy4kqcfqci9xlmjc9nynbyr57avkqia0kpi";
    jar-sha256 = "1j35qhql8yiki0zgj0nw07kzfnzi9jw0vvb91jf02l6hmnx0k44f";
  };
  "https://jcenter.bintray.com/com/android/tools/sdklib/25.2.3/sdklib-25.2.3" = {
    host = jcenter;
    path = "com/android/tools/sdklib/25.2.3/sdklib-25.2.3";
    type = "jar";
    pom-sha256 = "1dn0ri9a9p1zjcgjsj11d5avvn7avs6glj6fny5606aqwk258qax";
    jar-sha256 = "1l62x7c9n3zjwi5gar3scrb11yv6q4sbc7sh9pxsykwyn6dfs5zq";
  };
  "https://jcenter.bintray.com/com/google/code/gson/gson/2.2.4/gson-2.2.4" = {
    host = jcenter;
    path = "com/google/code/gson/gson/2.2.4/gson-2.2.4";
    type = "jar";
    pom-sha256 = "106k9ynbhls8nkihxrhkj5033z7q2am6x1l98vffck4935flv65f";
    jar-sha256 = "1yw7qszcw1dsh54q6wyksr5mbkz8mzs1q36hmjjn7qx9gk88qcn0";
  };
  "https://jcenter.bintray.com/com/google/google/1/google-1" = {
    host = jcenter;
    path = "com/google/google/1/google-1";
    type = "jar";
    pom-sha256 = "10by4ybrjnl8zwfg4ca74d0gcl4p9l7dzlfb9iwxw7m325xb2vfd";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/com/google/guava/guava-parent/18.0/guava-parent-18.0" = {
    host = jcenter;
    path = "com/google/guava/guava-parent/18.0/guava-parent-18.0";
    type = "jar";
    pom-sha256 = "0rmia42xirzgk3r2h1syzs7n7ib1nv84yzh87yizcmz7jn4crb54";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/com/google/guava/guava/17.0/guava-17.0" = {
    host = jcenter;
    path = "com/google/guava/guava/17.0/guava-17.0";
    type = "jar";
    pom-sha256 = "0nxpdkb27m770j0c4xlg7l34aj87h7qja7dbmqrcf99q1l0ic39a";
    jar-sha256 = "1g7bhyvzsx61lrca01hvpivkdgjvgj1wy5qa0jwbdl0klq7ahdlc";
  };
  "https://jcenter.bintray.com/com/google/guava/guava/18.0/guava-18.0" = {
    host = jcenter;
    path = "com/google/guava/guava/18.0/guava-18.0";
    type = "jar";
    pom-sha256 = "0dlqf4baf5h1j9b2a1i96n5l3i21vm7r3xync305sszpfqfxchz7";
    jar-sha256 = "16bgkzvk3sprn5rs9hfcpvz9vgyhy40zni1amffcxrfj0gygnr6n";
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
  "https://jcenter.bintray.com/com/google/protobuf/protobuf-java/2.5.0/protobuf-java-2.5.0" = {
    host = jcenter;
    path = "com/google/protobuf/protobuf-java/2.5.0/protobuf-java-2.5.0";
    type = "jar";
    pom-sha256 = "0isgblny3w76jxiy7rdab3zg6d5f1sfw7vjal5y43al7mx97m0wx";
    jar-sha256 = "0x6c4pbsizvk3lm6nxcgi1g2iqgrxcna1ip74lbn01f0fm2wdhg0";
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
  "https://jcenter.bintray.com/org/eclipse/jdt/core/compiler/ecj/4.5.1/ecj-4.5.1" = {
    host = jcenter;
    path = "org/eclipse/jdt/core/compiler/ecj/4.5.1/ecj-4.5.1";
    type = "jar";
    pom-sha256 = "0irq5k7jsqaqvbvpnfg1f6vr8jzizp9hni5h0h3x53gjr0gr2d52";
    jar-sha256 = "153jkqjiva6f31zzr7lp46vikvvg14pi9wg0qzkpf50r2gxi7wqr";
  };
  "https://jcenter.bintray.com/org/jacoco/org.jacoco.build/0.7.5.201505241946/org.jacoco.build-0.7.5.201505241946" = {
    host = jcenter;
    path = "org/jacoco/org.jacoco.build/0.7.5.201505241946/org.jacoco.build-0.7.5.201505241946";
    type = "jar";
    pom-sha256 = "1f50lk9r5f9gq9asy1kx00clyymx35rwhcmiqwg4lqghm6l6xbv2";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/jacoco/org.jacoco.core/0.7.5.201505241946/org.jacoco.core-0.7.5.201505241946" = {
    host = jcenter;
    path = "org/jacoco/org.jacoco.core/0.7.5.201505241946/org.jacoco.core-0.7.5.201505241946";
    type = "jar";
    pom-sha256 = "1hdlv1hi8i0syca2rwvcabwv5i0sk82scrri91cwgcmhx7hkcs4d";
    jar-sha256 = "0278fx30h9hs2wxbp12159ykiizbkgqc7z4bfk83hr4jja0svwgc";
  };
  "https://jcenter.bintray.com/org/jacoco/org.jacoco.report/0.7.5.201505241946/org.jacoco.report-0.7.5.201505241946" = {
    host = jcenter;
    path = "org/jacoco/org.jacoco.report/0.7.5.201505241946/org.jacoco.report-0.7.5.201505241946";
    type = "jar";
    pom-sha256 = "1nx5pllc28s8a71ivwa8b8jm9kbnnc2szqbmlhpji6pgs9ii1csr";
    jar-sha256 = "1z5hd0a4ksmhamxizi7icbvv9nx9mxd2flizqb4dvxrs10pvl4bf";
  };
  "https://jcenter.bintray.com/org/ow2/asm/asm-analysis/5.0.4/asm-analysis-5.0.4" = {
    host = jcenter;
    path = "org/ow2/asm/asm-analysis/5.0.4/asm-analysis-5.0.4";
    type = "jar";
    pom-sha256 = "0p1jizbdgp70ch02f8v9ljns8lj6cdyajpx9dsynv6h67l6ndd8j";
    jar-sha256 = "0h1li8yf0snizchyazbzp6mi5g4zsag1gv4wyh7kf3wxr2h756cv";
  };
  "https://jcenter.bintray.com/org/ow2/asm/asm-commons/5.0.4/asm-commons-5.0.4" = {
    host = jcenter;
    path = "org/ow2/asm/asm-commons/5.0.4/asm-commons-5.0.4";
    type = "jar";
    pom-sha256 = "0yd1dg2yg5pj8lqnkx4z0ddlflq0g113cl33rb19n7sg9pn14fb4";
    jar-sha256 = "1bw6fiaq448zx9wmi8lnysxqfgk9npqpbqq3iyqm31mjj2i0wbsk";
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
  "https://jcenter.bintray.com/org/ow2/asm/asm-parent/5.0.4/asm-parent-5.0.4" = {
    host = jcenter;
    path = "org/ow2/asm/asm-parent/5.0.4/asm-parent-5.0.4";
    type = "jar";
    pom-sha256 = "0wjam9wl7l1bfd2n8qkigk0dq9z56l8z0pby38rknmr6yafmpw7x";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/org/ow2/asm/asm-tree/5.0.4/asm-tree-5.0.4" = {
    host = jcenter;
    path = "org/ow2/asm/asm-tree/5.0.4/asm-tree-5.0.4";
    type = "jar";
    pom-sha256 = "19i2jw14czb0imrsvcnjhhxlppihfmxqvqhida55wx9g1vmwi3gw";
    jar-sha256 = "16c3bizs9wysmr785qj4m2q2x8sjgld6f3w48gkwvi7s6sl0yry3";
  };
  "https://jcenter.bintray.com/org/ow2/asm/asm/5.0.4/asm-5.0.4" = {
    host = jcenter;
    path = "org/ow2/asm/asm/5.0.4/asm-5.0.4";
    type = "jar";
    pom-sha256 = "043zq0mg31zx5p6hp7jg7zmiibim45c23cd6lx2zh9cbpl4c5cmq";
    jar-sha256 = "0852x4qyrk9ykbw1a2ljwq41ljbw5gj7pg3q399049z6ibnihrl9";
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
