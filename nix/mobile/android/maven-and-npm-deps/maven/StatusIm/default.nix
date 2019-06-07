{}:

let
  jcenter = "https://jcenter.bintray.com";
  google = "https://dl.google.com/dl/android/maven2";
  fabric-io = "https://maven.fabric.io/public";

in {
  "https://dl.google.com/dl/android/maven2/androidx/databinding/databinding-common/3.3.2/databinding-common-3.3.2" = {
    host = google;
    path = "androidx/databinding/databinding-common/3.3.2/databinding-common-3.3.2";
    type = "jar";
    pom-sha256 = "1z22vzqz311a5y3qn2p2xgqlv1kr474iw56852i6371xfzbhyp60";
    jar-sha256 = "0f9iplfkvfm43jv3ii1jh4fq4m0xfg3isd95qsy68awsfcr8ckf5";
  };
  "https://dl.google.com/dl/android/maven2/androidx/databinding/databinding-compiler-common/3.3.2/databinding-compiler-common-3.3.2" = {
    host = google;
    path = "androidx/databinding/databinding-compiler-common/3.3.2/databinding-compiler-common-3.3.2";
    type = "jar";
    pom-sha256 = "17m3yjamqxd7w9g453lnsz6a589w2yrqba6516rjff1aw2rxlmp7";
    jar-sha256 = "0qrhk9wf6yzg4fz7nkvh58l4kmfmk4nax2f8dk410w5dw9nr1l5v";
  };
  "https://dl.google.com/dl/android/maven2/com/android/databinding/baseLibrary/3.3.2/baseLibrary-3.3.2" = {
    host = google;
    path = "com/android/databinding/baseLibrary/3.3.2/baseLibrary-3.3.2";
    type = "jar";
    pom-sha256 = "0rpspzijzfmakq8ash5gg2n9yzxxa0yfbhlapg60hzn880jvzrwz";
    jar-sha256 = "0bf65kyib0nxq898wjl1f0ppj9gbl9yb59id97gcir7qx2pr7pka";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/analytics-library/crash/26.3.2/crash-26.3.2" = {
    host = google;
    path = "com/android/tools/analytics-library/crash/26.3.2/crash-26.3.2";
    type = "jar";
    pom-sha256 = "0jc415x8xk7cjb5f7c30k6d6yp4ab99qigns6k4jmwv2r61xz3c1";
    jar-sha256 = "1rqfpf9p0526zfgsv9fiklc6arwibqr7g4vxvcybmy2qbyswsbdl";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/analytics-library/protos/26.3.2/protos-26.3.2" = {
    host = google;
    path = "com/android/tools/analytics-library/protos/26.3.2/protos-26.3.2";
    type = "jar";
    pom-sha256 = "1qh5b6jx27ssl0m0b14jalh3criqhzhx8agydq8s1h92f2dx87rx";
    jar-sha256 = "1w2i6asvbgahm06fnag8rrpm70sgq69wvfc4n4bp4acb56s8y8sh";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/analytics-library/shared/26.3.2/shared-26.3.2" = {
    host = google;
    path = "com/android/tools/analytics-library/shared/26.3.2/shared-26.3.2";
    type = "jar";
    pom-sha256 = "0awbhkfwhmqan98liw1b1ga407srq66lq284blj62vbg3i20kjk2";
    jar-sha256 = "0wgg6y1bz7x014k44lj99l44dx42f8msffsqq2vihl4h477hvn6x";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/analytics-library/tracker/26.3.2/tracker-26.3.2" = {
    host = google;
    path = "com/android/tools/analytics-library/tracker/26.3.2/tracker-26.3.2";
    type = "jar";
    pom-sha256 = "0c6yb756xr99vfjphgg28d7z4xp36dwv0rnq1r6rdm6piz4q76q3";
    jar-sha256 = "1bfhwx13p35aa23l7xnh5lx6gchhxrlacxdksyb3w05gs797bi98";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/annotations/26.3.2/annotations-26.3.2" = {
    host = google;
    path = "com/android/tools/annotations/26.3.2/annotations-26.3.2";
    type = "jar";
    pom-sha256 = "0qlkfq9b99kdd3kiznzr78rchhm9i6qavvsk70kavwmlw37maj9z";
    jar-sha256 = "18gpw4n48cancrsnaicyn7w3a2sfff7zvkpb2g7mqbvaiglyik2v";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/aapt2-proto/0.3.1/aapt2-proto-0.3.1" = {
    host = google;
    path = "com/android/tools/build/aapt2-proto/0.3.1/aapt2-proto-0.3.1";
    type = "jar";
    pom-sha256 = "07fhxx0czmrmw9cfpxkcxy6z53hpsrpkk85l791w39srdgjz1r9i";
    jar-sha256 = "1nj9vkbz3yx43r56haq7373acmg6d5yv3cy9qsvzgamhi6dj20x5";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/aapt2/3.3.2-5309881/aapt2-3.3.2-5309881" = {
    host = google;
    path = "com/android/tools/build/aapt2/3.3.2-5309881/aapt2-3.3.2-5309881";
    type = "jar";
    pom-sha256 = "1ik3cysnh4f2sbz6byamrl3af7778qkypyf7nwgrxz87xl6akjwx";
    jar-sha256 = "";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/aapt2/3.3.2-5309881/aapt2-3.3.2-5309881-linux" = {
    host = google;
    path = "com/android/tools/build/aapt2/3.3.2-5309881/aapt2-3.3.2-5309881-linux";
    type = "jar";
    pom-sha256 = "";
    jar-sha256 = "04i4lb1gvf3c57ixbg9zpjjv4gndmn556l7l9683amqhidzavvlv";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/aapt2/3.3.2-5309881/aapt2-3.3.2-5309881-osx" = {
    host = google;
    path = "com/android/tools/build/aapt2/3.3.2-5309881/aapt2-3.3.2-5309881-osx";
    type = "jar";
    pom-sha256 = "";
    jar-sha256 = "0k5liykzcblq5h5n0qsp547plaj508dc1y7gkg8703153cb3q9z0";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/apksig/3.3.2/apksig-3.3.2" = {
    host = google;
    path = "com/android/tools/build/apksig/3.3.2/apksig-3.3.2";
    type = "jar";
    pom-sha256 = "1wp8q47drb4w5ml1w31xwjw1jq2wmdcjm51xx8xp1mwhs0kdvhkv";
    jar-sha256 = "1kcaj1m3px0hiaf05s8b12nm87zpblzlncyxdgzc3ii706iami44";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/apkzlib/3.3.2/apkzlib-3.3.2" = {
    host = google;
    path = "com/android/tools/build/apkzlib/3.3.2/apkzlib-3.3.2";
    type = "jar";
    pom-sha256 = "0wkjdpskdb6iw94zh1qzibqwkshl32chmp2j81a4glnpzkljxy6q";
    jar-sha256 = "1b3nf7jfar9l5xzcfzdlqxf2nnsikl43rvx3xdjzbpz5g0r54knk";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/builder-model/3.3.2/builder-model-3.3.2" = {
    host = google;
    path = "com/android/tools/build/builder-model/3.3.2/builder-model-3.3.2";
    type = "jar";
    pom-sha256 = "1y6vka1hqah30pq08a9na7d8ygd8mqwir0pqzjh73amp7psnf9v6";
    jar-sha256 = "0ikzqnydhsqj89sv61sji2qjrncwlaclj0r4y2whd7pfxjq3sph5";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/builder-test-api/3.3.2/builder-test-api-3.3.2" = {
    host = google;
    path = "com/android/tools/build/builder-test-api/3.3.2/builder-test-api-3.3.2";
    type = "jar";
    pom-sha256 = "0jnmjfis73baj7cjh4r3c58d5qwpxm567yxfld0jwx497wpzhrnc";
    jar-sha256 = "063684jbrf38b5na02z40s6m14v6w9df87y9lcaavg2vc7blqbhb";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/builder/3.3.2/builder-3.3.2" = {
    host = google;
    path = "com/android/tools/build/builder/3.3.2/builder-3.3.2";
    type = "jar";
    pom-sha256 = "0f183nyzvvwmmcxvwfk63qsf2pwwbl7h5byl9a99xbxlf0xb1imf";
    jar-sha256 = "1xfr5fj0hvkf942g504fjp7xb7p9ybq2dlsz4f3h9vvsv829fr35";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/bundletool/0.6.0/bundletool-0.6.0" = {
    host = google;
    path = "com/android/tools/build/bundletool/0.6.0/bundletool-0.6.0";
    type = "jar";
    pom-sha256 = "08j6w1bh9n0cbwm31ams4qkiz3bnlrg07380kkr47blidkls4fk0";
    jar-sha256 = "1fjbbj4yrjqxq3a3y0dzg1lq2spfchl4l2v6s1cikdbc7503a1a0";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/gradle-api/3.3.2/gradle-api-3.3.2" = {
    host = google;
    path = "com/android/tools/build/gradle-api/3.3.2/gradle-api-3.3.2";
    type = "jar";
    pom-sha256 = "1cgdxqsmjhawnspimb10s2vs47y194sqavd6rrlgnr8xyj75nh0h";
    jar-sha256 = "0h70844f1s9dbadq40sa915mawv13vjryb7cfbfk00xp3gj4gg9w";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/gradle/3.3.2/gradle-3.3.2" = {
    host = google;
    path = "com/android/tools/build/gradle/3.3.2/gradle-3.3.2";
    type = "jar";
    pom-sha256 = "1hwjcgyhz87g58gf295gn46909xcn4an81xj521q0b6mjkq0j28g";
    jar-sha256 = "1czakvk7xwr9vnw6vp0rpl91904kyaly79w1iy3xm7ishnhw3q5k";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/jetifier/jetifier-core/1.0.0-beta02/jetifier-core-1.0.0-beta02" = {
    host = google;
    path = "com/android/tools/build/jetifier/jetifier-core/1.0.0-beta02/jetifier-core-1.0.0-beta02";
    type = "jar";
    pom-sha256 = "1ff3vxl3p5indy69ay2j78wqix4d6x1wkwdsg3j2hc1vas98d4ch";
    jar-sha256 = "14s7b8a5c378clgkn8d0cfi79v4hg9zgrhaq7371vd7q091zhqgg";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/jetifier/jetifier-processor/1.0.0-beta02/jetifier-processor-1.0.0-beta02" = {
    host = google;
    path = "com/android/tools/build/jetifier/jetifier-processor/1.0.0-beta02/jetifier-processor-1.0.0-beta02";
    type = "jar";
    pom-sha256 = "1bx1483qlxflrkia3q3syqcdqjy6v2m8hmd52c7yzp5pgn4k0dpv";
    jar-sha256 = "037l3v6659ra4djlhkhd9l40nm193r0zqcklc14b6d75ddjhbr97";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/manifest-merger/26.3.2/manifest-merger-26.3.2" = {
    host = google;
    path = "com/android/tools/build/manifest-merger/26.3.2/manifest-merger-26.3.2";
    type = "jar";
    pom-sha256 = "04p0xs54hshpqgnbw2jbymc30ss6slbkggckirbpcqdda7fz4lla";
    jar-sha256 = "0cvwz55a0kv8hvqb2schi8xc0iw5lv87fr478j3z1f9gn3cadi05";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/transform-api/2.0.0-deprecated-use-gradle-api/transform-api-2.0.0-deprecated-use-gradle-api" = {
    host = google;
    path = "com/android/tools/build/transform-api/2.0.0-deprecated-use-gradle-api/transform-api-2.0.0-deprecated-use-gradle-api";
    type = "jar";
    pom-sha256 = "0xc5zmf6125cpdb4144mblznjs16q4k5z4jpjbbs3g4ads2z6qkw";
    jar-sha256 = "0h2p4b7900j3girbkayq6b92pfw283rqnn74p4nlqlqwbv8a7r2d";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/common/26.3.2/common-26.3.2" = {
    host = google;
    path = "com/android/tools/common/26.3.2/common-26.3.2";
    type = "jar";
    pom-sha256 = "12ycc1v9ab4s3lxhiwcwxrjni82mhmbc7mm5mf93z8kdysjdi8ml";
    jar-sha256 = "1b6gbqhm1yb9x68rj741ls7jvwcwr23nmnz3d0ap16lycvqfgy6r";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/ddms/ddmlib/26.3.2/ddmlib-26.3.2" = {
    host = google;
    path = "com/android/tools/ddms/ddmlib/26.3.2/ddmlib-26.3.2";
    type = "jar";
    pom-sha256 = "1ljksjpaw7apmvgnzxgpcm14g9m7j4azahl4vw5i76rjf20skgx9";
    jar-sha256 = "11cbpy0s5mbjn85kqxnayh80bq3p90d3ggzbqz94cvixas5dlj6j";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/dvlib/26.3.2/dvlib-26.3.2" = {
    host = google;
    path = "com/android/tools/dvlib/26.3.2/dvlib-26.3.2";
    type = "jar";
    pom-sha256 = "0w4p68if995bllpq5v9ydxzwlrapkbbxjpslv2m4zf7dbmdhv1xr";
    jar-sha256 = "04g8vzj32cj5dr2pvzfxqrj7r2fnvqa9gmh3jdbp6xqw2rbasjnq";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/layoutlib/layoutlib-api/26.3.2/layoutlib-api-26.3.2" = {
    host = google;
    path = "com/android/tools/layoutlib/layoutlib-api/26.3.2/layoutlib-api-26.3.2";
    type = "jar";
    pom-sha256 = "1xx925h445sgckp1655j8iz1smd7mpqbrx71mn7i6z07lsaymmsf";
    jar-sha256 = "1f2mn863knsc4s21h002784f1ny9nnanm2yk1lsmqpxr9a3ixrnp";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/lint/lint-gradle-api/26.3.2/lint-gradle-api-26.3.2" = {
    host = google;
    path = "com/android/tools/lint/lint-gradle-api/26.3.2/lint-gradle-api-26.3.2";
    type = "jar";
    pom-sha256 = "084nf3nm0www53zrqlyp9jp4j7f77xkrm39n8g3vfr4gadadsg5f";
    jar-sha256 = "0bq64g626v72dm7mmz8xjsvqlg00igh2qc2i7znrnkq51hp2ijsl";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/repository/26.3.2/repository-26.3.2" = {
    host = google;
    path = "com/android/tools/repository/26.3.2/repository-26.3.2";
    type = "jar";
    pom-sha256 = "1zv0pnycqpqwhv3p090pmsigk88cp01wpk8jljqcdw6a8wz99k5r";
    jar-sha256 = "0h7a444rxlq3f65lrsq4cgnfbf0dw40jk7jvs988gaz90vmiwqfs";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/sdk-common/26.3.2/sdk-common-26.3.2" = {
    host = google;
    path = "com/android/tools/sdk-common/26.3.2/sdk-common-26.3.2";
    type = "jar";
    pom-sha256 = "02vimlax3wgialfygaxwdgrrz7zs916486a5paik3ihg7pqwj970";
    jar-sha256 = "0v7r9fs6p29hn14h2m1wvcn0riasrzq90r187b1zlr2yy8xkm0l2";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/sdklib/26.3.2/sdklib-26.3.2" = {
    host = google;
    path = "com/android/tools/sdklib/26.3.2/sdklib-26.3.2";
    type = "jar";
    pom-sha256 = "1qjhqh718r244w70pi8d2z838pqwm0sl4w57jjj1g8haryrlhv3i";
    jar-sha256 = "0awksv2kp5hnlkk1jnravrhap5bls938c8v316822wzn594iaka2";
  };
  "https://dl.google.com/dl/android/maven2/com/google/gms/google-services/4.0.1/google-services-4.0.1" = {
    host = google;
    path = "com/google/gms/google-services/4.0.1/google-services-4.0.1";
    type = "jar";
    pom-sha256 = "1rqj4lk53x1xknwcm55g5a4lywgc5dzwmji6sd7x37gij64i3x03";
    jar-sha256 = "1s1xm2dnkhpwa854saxq3rwy6lhhv3qw35fjyi6wq8r14xipwcrl";
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
  "https://jcenter.bintray.com/com/google/code/findbugs/jsr305/3.0.2/jsr305-3.0.2" = {
    host = jcenter;
    path = "com/google/code/findbugs/jsr305/3.0.2/jsr305-3.0.2";
    type = "jar";
    pom-sha256 = "1zldsximvzlag566i5r2i124d5vs2jw4brjy39hb4m5jy6yrv20r";
    jar-sha256 = "1iyh53li6y4b8gp8bl52fagqp8iqrkp4rmwa5jb8f9izg2hd4skn";
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
  "https://jcenter.bintray.com/com/google/guava/guava-parent/26.0-jre/guava-parent-26.0-jre" = {
    host = jcenter;
    path = "com/google/guava/guava-parent/26.0-jre/guava-parent-26.0-jre";
    type = "jar";
    pom-sha256 = "18za79by0g61arws1n0s4pyd68imsbyr4w72wgsxxmy02za5qzdv";
    jar-sha256 = "";
  };
  "https://jcenter.bintray.com/com/google/guava/guava/26.0-jre/guava-26.0-jre" = {
    host = jcenter;
    path = "com/google/guava/guava/26.0-jre/guava-26.0-jre";
    type = "jar";
    pom-sha256 = "02g1gsm1xbxxvk2q84mpgin99f41cn9qbnl1v92bid2akbf7lcqw";
    jar-sha256 = "0b7a7c1hgx5rmnx0ma5f2dp7agy0by7107xhsay21g35ssxcmsd0";
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
  "https://jcenter.bintray.com/de/undercouch/gradle-download-task/3.1.2/gradle-download-task-3.1.2" = {
    host = jcenter;
    path = "de/undercouch/gradle-download-task/3.1.2/gradle-download-task-3.1.2";
    type = "jar";
    pom-sha256 = "10jf8bm085rn13j2ch77829c80qq2kc1y9ndc4hg72zr851z1xjm";
    jar-sha256 = "1fqnkr5g3573ncb73sqnvf1l57ygpwb0np84wvass94a162522kx";
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
  "https://jcenter.bintray.com/net/sf/proguard/proguard-base/5.3.3/proguard-base-5.3.3" = {
    host = jcenter;
    path = "net/sf/proguard/proguard-base/5.3.3/proguard-base-5.3.3";
    type = "jar";
    pom-sha256 = "1jnr6zsxfimb8wglqlwa6rrdc3g3nqf1dyw0k2dq9cj0q4pgn7p5";
    jar-sha256 = "11nwdb9y84cghcx319nsjjf9m035s4s1184zrhzpvaxq2wvqhbhx";
  };
  "https://jcenter.bintray.com/net/sf/proguard/proguard-base/6.0.3/proguard-base-6.0.3" = {
    host = jcenter;
    path = "net/sf/proguard/proguard-base/6.0.3/proguard-base-6.0.3";
    type = "jar";
    pom-sha256 = "01s9xmjv2v7hc5p0zgpghabr6php3dnlcffmi2lq4mb7y9wkv0ia";
    jar-sha256 = "0r6jm7qb28qaxmp3ip96v2qhlvs00aw3k96d9qvpavm6bxlqkg3m";
  };
  "https://jcenter.bintray.com/net/sf/proguard/proguard-gradle/5.3.3/proguard-gradle-5.3.3" = {
    host = jcenter;
    path = "net/sf/proguard/proguard-gradle/5.3.3/proguard-gradle-5.3.3";
    type = "jar";
    pom-sha256 = "03v9zm3ykfkyb5cs5ald07ph103fh68d5c33rv070r29p71dwszj";
    jar-sha256 = "0shhpsjfc5gam15jnv1hk718v5c7vi7dwdc3gvmnid6dc85kljzk";
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
  "https://jcenter.bintray.com/org/checkerframework/checker-qual/2.5.2/checker-qual-2.5.2" = {
    host = jcenter;
    path = "org/checkerframework/checker-qual/2.5.2/checker-qual-2.5.2";
    type = "jar";
    pom-sha256 = "1plykj2ikqjyjpl2jdzk00m34ahhdlh8x31kq89x0qk4lcwd8k6w";
    jar-sha256 = "02h4iibbzqwy5i9bfqp6h5p2rsp7vi1fgqlf1xqfgm5rr28jdc34";
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
  "https://jcenter.bintray.com/org/jetbrains/kotlin/kotlin-reflect/1.3.21/kotlin-reflect-1.3.21" = {
    host = jcenter;
    path = "org/jetbrains/kotlin/kotlin-reflect/1.3.21/kotlin-reflect-1.3.21";
    type = "jar";
    pom-sha256 = "1k0b2w4zkvs24xqbsx1g3zjgpbml7dmc2s5xnwg9cfr2jkwymm22";
    jar-sha256 = "0sn2w3ly7k28xy3vp9k4515zqd62pqljmq9y7q51w69k4s15q1m3";
  };
  "https://jcenter.bintray.com/org/jetbrains/kotlin/kotlin-stdlib-common/1.3.21/kotlin-stdlib-common-1.3.21" = {
    host = jcenter;
    path = "org/jetbrains/kotlin/kotlin-stdlib-common/1.3.21/kotlin-stdlib-common-1.3.21";
    type = "jar";
    pom-sha256 = "0gc7szhlsph3d99xqbhnlv9qr8fd7f1b27ik2bfq645bl4q36i0y";
    jar-sha256 = "19sjvl52mkp060ciw887y61dbc0azibrg6jnb17yd58qc5xiz9nf";
  };
  "https://jcenter.bintray.com/org/jetbrains/kotlin/kotlin-stdlib-jdk7/1.3.21/kotlin-stdlib-jdk7-1.3.21" = {
    host = jcenter;
    path = "org/jetbrains/kotlin/kotlin-stdlib-jdk7/1.3.21/kotlin-stdlib-jdk7-1.3.21";
    type = "jar";
    pom-sha256 = "0xjs018bbgcznw413qvkgp9znydqjgfq1dl906jzhgg7di0vlir6";
    jar-sha256 = "1bwql4kbdf0m4rnzqvjkh921y231xqslvbiqd7d408fl9xh7ay58";
  };
  "https://jcenter.bintray.com/org/jetbrains/kotlin/kotlin-stdlib-jdk8/1.3.21/kotlin-stdlib-jdk8-1.3.21" = {
    host = jcenter;
    path = "org/jetbrains/kotlin/kotlin-stdlib-jdk8/1.3.21/kotlin-stdlib-jdk8-1.3.21";
    type = "jar";
    pom-sha256 = "11wrz5mzlp7s4xyg4lns4nrqh0kqphydb9l2j53dsvfdzp6c2bap";
    jar-sha256 = "1m3lgzjd5ww7lwqxqbjngzccqhx816iabg1f8iaiqahjmikfs8sq";
  };
  "https://jcenter.bintray.com/org/jetbrains/kotlin/kotlin-stdlib/1.2.20/kotlin-stdlib-1.2.20" = {
    host = jcenter;
    path = "org/jetbrains/kotlin/kotlin-stdlib/1.2.20/kotlin-stdlib-1.2.20";
    type = "jar";
    pom-sha256 = "1lwxx3clsnyr0wd4bsidjnghij5b4kyl1vz7j0cnrmhg70z9f14v";
    jar-sha256 = "1r4v0sij1asspidcjcqmhkksc8vl5nqg6s0kmf1p8icxpxckvckr";
  };
  "https://jcenter.bintray.com/org/jetbrains/kotlin/kotlin-stdlib/1.3.21/kotlin-stdlib-1.3.21" = {
    host = jcenter;
    path = "org/jetbrains/kotlin/kotlin-stdlib/1.3.21/kotlin-stdlib-1.3.21";
    type = "jar";
    pom-sha256 = "19drzlj9ykx1cxckdyf8f6fr1k0yav6s04l4xi4gwnvc74rymqcw";
    jar-sha256 = "0d5hf8y7kavrc2911x454wp3qiwlbdvwmch67r1m0vzhv5q27fiq";
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
