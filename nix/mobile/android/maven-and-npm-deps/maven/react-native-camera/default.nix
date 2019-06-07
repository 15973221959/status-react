{}:

let
  jcenter = "https://jcenter.bintray.com";
  google = "https://dl.google.com/dl/android/maven2";
  fabric-io = "https://maven.fabric.io/public";

in {
  "https://dl.google.com/dl/android/maven2/com/android/databinding/baseLibrary/3.0.0/baseLibrary-3.0.0" = {
    host = google;
    path = "com/android/databinding/baseLibrary/3.0.0/baseLibrary-3.0.0";
    type = "jar";
    pom-sha256 = "150bmgklpfinmsrlcg9s426pg9vs9szl1xdhz9vjmfdf6q2jpq1g";
    jar-sha256 = "18j8j2mlzg1crqk31529y8qgx0bdhr98bf8vfq6rsvv3rn49f7rf";
  };
  "https://dl.google.com/dl/android/maven2/com/android/databinding/compilerCommon/3.0.0/compilerCommon-3.0.0" = {
    host = google;
    path = "com/android/databinding/compilerCommon/3.0.0/compilerCommon-3.0.0";
    type = "jar";
    pom-sha256 = "04591ic395xxkrvnazik9hfsavh461r2fwsfjv4mjd0riwdmz5kv";
    jar-sha256 = "1bpvsq8jr4hpwa771mha0mx8v9fbn00k9bs7hhizb47phfdxdpg3";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/analytics-library/crash/26.3.2/crash-26.3.2" = {
    host = google;
    path = "com/android/tools/analytics-library/crash/26.3.2/crash-26.3.2";
    type = "jar";
    pom-sha256 = "0jc415x8xk7cjb5f7c30k6d6yp4ab99qigns6k4jmwv2r61xz3c1";
    jar-sha256 = "1rqfpf9p0526zfgsv9fiklc6arwibqr7g4vxvcybmy2qbyswsbdl";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/analytics-library/protos/26.0.0/protos-26.0.0" = {
    host = google;
    path = "com/android/tools/analytics-library/protos/26.0.0/protos-26.0.0";
    type = "jar";
    pom-sha256 = "1sw6pjch8qqhydwqmwwb84cwxmxrbb04x2bj5q0igg98rm2b83vc";
    jar-sha256 = "1c5dj1iygs8mv09qi6djwsb5ywsxb56yacpfg7k8pvrjx3ayncj2";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/analytics-library/shared/26.0.0/shared-26.0.0" = {
    host = google;
    path = "com/android/tools/analytics-library/shared/26.0.0/shared-26.0.0";
    type = "jar";
    pom-sha256 = "0cqymxirzrwpnjwnkagfgf9xx8wyf3w73wj8hs5dmyaznjwds02z";
    jar-sha256 = "0i3pr82km4pm22fdhw7nyr0cvwp92jnw9wvi6s38frlc2cj40ac9";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/analytics-library/tracker/26.0.0/tracker-26.0.0" = {
    host = google;
    path = "com/android/tools/analytics-library/tracker/26.0.0/tracker-26.0.0";
    type = "jar";
    pom-sha256 = "17id91rl0rg2xvvglqllrldjl7yxbcz4s8qwpw8zqp4vqf2mc9x9";
    jar-sha256 = "1cigmc4s4hav70g7ly8q0ml3awnqaja20xvrv6clrgcppikl49m6";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/annotations/26.0.0/annotations-26.0.0" = {
    host = google;
    path = "com/android/tools/annotations/26.0.0/annotations-26.0.0";
    type = "jar";
    pom-sha256 = "14n97s6yw56cyb4xy6mxlr3xah65ayd8wydp7fxh8sg9320172lh";
    jar-sha256 = "0iyqms07qadihcnha4li2gk88x2504w7xvdmichjqc52hsq47kik";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/apksig/3.0.0/apksig-3.0.0" = {
    host = google;
    path = "com/android/tools/build/apksig/3.0.0/apksig-3.0.0";
    type = "jar";
    pom-sha256 = "1ha5d3bh36q4kj0gh1qmg9ncqwbv2n209mfbfnmwxw4gnj4l9a18";
    jar-sha256 = "11jc8f5xb3i0mhr2r2sbrajlh26xkgrz4vs5l09sxi77y2iqqz3q";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/builder-model/3.0.0/builder-model-3.0.0" = {
    host = google;
    path = "com/android/tools/build/builder-model/3.0.0/builder-model-3.0.0";
    type = "jar";
    pom-sha256 = "0sv1v487fs5z5m131rpmdviqrysh7xdkwj3jga673wdd3phzm6x4";
    jar-sha256 = "1fii28knmbd3jxvmp0b06zv403vkwv3pap4s5i180cwc0wl7789p";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/builder-test-api/3.0.0/builder-test-api-3.0.0" = {
    host = google;
    path = "com/android/tools/build/builder-test-api/3.0.0/builder-test-api-3.0.0";
    type = "jar";
    pom-sha256 = "16vrjm0czd9alch5mghmfp4aldp5xnvfl1pq3xh5hr9x5rvmdc3b";
    jar-sha256 = "1kalacq30ydcivyp2pwql59j2xckifv1c6f517ll15pjdxskdnzb";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/builder/3.0.0/builder-3.0.0" = {
    host = google;
    path = "com/android/tools/build/builder/3.0.0/builder-3.0.0";
    type = "jar";
    pom-sha256 = "0a4pal1w33v90cfpd72brb2chqfqmpqs52klg9xigwrkbyjgkp4b";
    jar-sha256 = "07pwrxv5igr4v1886rjdfrrqx4sa5b64yz0p41450qywzm2n880n";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/gradle-api/3.0.0/gradle-api-3.0.0" = {
    host = google;
    path = "com/android/tools/build/gradle-api/3.0.0/gradle-api-3.0.0";
    type = "jar";
    pom-sha256 = "1f7im02c138frynchbkndrw6lrbc0pyh6vmkx7a99g4jplb1lgmn";
    jar-sha256 = "1cslx6qiffrq292ka2390yk4jypd3d5f3qx6kwcrr5k6cg1idw24";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/gradle-core/3.0.0/gradle-core-3.0.0" = {
    host = google;
    path = "com/android/tools/build/gradle-core/3.0.0/gradle-core-3.0.0";
    type = "jar";
    pom-sha256 = "1h4a1gfr432kxs6r3gpx7hpc4v6xrd4f48rahizmrix2dw2405i5";
    jar-sha256 = "1z6yh3aviy8hvrcihz1xnsggrzjp6yd4wipdw03l1l4f2w378x2p";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/gradle/3.0.0/gradle-3.0.0" = {
    host = google;
    path = "com/android/tools/build/gradle/3.0.0/gradle-3.0.0";
    type = "jar";
    pom-sha256 = "07qm0bfvivx0h2ww7z5sdws78j4gkn75x2hy2mhbhhwq9k20nf4d";
    jar-sha256 = "00ivxwq5rgrx7wfkr8g5z1n21vap9r2vaid9djdqfr9f3jygq8mb";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/manifest-merger/26.0.0/manifest-merger-26.0.0" = {
    host = google;
    path = "com/android/tools/build/manifest-merger/26.0.0/manifest-merger-26.0.0";
    type = "jar";
    pom-sha256 = "1s8gfdlhb08cg7g5lxibh9m9pkanyffv4qih488pkn2036pf1256";
    jar-sha256 = "02bmi5s1vd8n6a38fifhsva0ijqs0c6100k72cl8aa2fcc67dmf9";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/build/transform-api/2.0.0-deprecated-use-gradle-api/transform-api-2.0.0-deprecated-use-gradle-api" = {
    host = google;
    path = "com/android/tools/build/transform-api/2.0.0-deprecated-use-gradle-api/transform-api-2.0.0-deprecated-use-gradle-api";
    type = "jar";
    pom-sha256 = "0xc5zmf6125cpdb4144mblznjs16q4k5z4jpjbbs3g4ads2z6qkw";
    jar-sha256 = "0h2p4b7900j3girbkayq6b92pfw283rqnn74p4nlqlqwbv8a7r2d";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/common/26.0.0/common-26.0.0" = {
    host = google;
    path = "com/android/tools/common/26.0.0/common-26.0.0";
    type = "jar";
    pom-sha256 = "1xklng15h3pfafldlx36zm1w2y7a78nlxi7dxfhfzgqyxxv0bdr6";
    jar-sha256 = "0701n8yyfya6f931vv5jg5y4fafkhvrciv6pdbj3y3gp0hvmlmi8";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/ddms/ddmlib/26.0.0/ddmlib-26.0.0" = {
    host = google;
    path = "com/android/tools/ddms/ddmlib/26.0.0/ddmlib-26.0.0";
    type = "jar";
    pom-sha256 = "125hxfgm6aczic9w6xh1ri13fa1il0rn4dn6727x987wzcp9bj55";
    jar-sha256 = "18smk8glkclb77rrryz416lksswlbvvd357d34di5ggqd0xap7q5";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/dvlib/26.0.0/dvlib-26.0.0" = {
    host = google;
    path = "com/android/tools/dvlib/26.0.0/dvlib-26.0.0";
    type = "jar";
    pom-sha256 = "0na1h2kg479ikcxvdrqyxppgsxns4qjh11jlyj6grlnwqcfsv1z2";
    jar-sha256 = "1i37rn56dw92ldsw4qq0la8xjz6hc355675c792grpm2r1gl867m";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/external/com-intellij/intellij-core/26.0.0/intellij-core-26.0.0" = {
    host = google;
    path = "com/android/tools/external/com-intellij/intellij-core/26.0.0/intellij-core-26.0.0";
    type = "jar";
    pom-sha256 = "1c3m5sd0pagay1pp1ni0s86q1gz9y5dyc9c6lyw1m211dknrgi7w";
    jar-sha256 = "1ysyx3hi0m98w9a59jw96n5fkvx9g2zwbl3839qzg1fwrmc8194c";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/external/org-jetbrains/uast/26.0.0/uast-26.0.0" = {
    host = google;
    path = "com/android/tools/external/org-jetbrains/uast/26.0.0/uast-26.0.0";
    type = "jar";
    pom-sha256 = "0q97i1gj5g7f3vfkldn8dyf3f6x962x3ch4gdv7irgh6p9mcs5lb";
    jar-sha256 = "0wjbfnvhq1himxjj389rlxqr4cqa9ahfkjy74cad1b00mg8yjhgv";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/layoutlib/layoutlib-api/26.0.0/layoutlib-api-26.0.0" = {
    host = google;
    path = "com/android/tools/layoutlib/layoutlib-api/26.0.0/layoutlib-api-26.0.0";
    type = "jar";
    pom-sha256 = "0d9ya2bkidfl4i8nwmvzx3vvysw2fpa55m559jdgcy6mpblcngm5";
    jar-sha256 = "0ayxvqvr18pd7nygrkwyws6vnhscfv2g1a445zqj5zjdgw7rw3id";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/lint/lint-api/26.0.0/lint-api-26.0.0" = {
    host = google;
    path = "com/android/tools/lint/lint-api/26.0.0/lint-api-26.0.0";
    type = "jar";
    pom-sha256 = "0gqdixv8z2add5n30y7hq7hhrchqcmi66jffmdlpadbvn0gsmmwl";
    jar-sha256 = "0yy2fw3ircsz38imnwwvhh6si4cc9v3w1a6s2zwacgsvniysvsil";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/lint/lint-checks/26.0.0/lint-checks-26.0.0" = {
    host = google;
    path = "com/android/tools/lint/lint-checks/26.0.0/lint-checks-26.0.0";
    type = "jar";
    pom-sha256 = "1cvaagg9s0lcyla4fhay31nflcjb6wj6n2qwysnpwiqd4wxba705";
    jar-sha256 = "0gq4h53qx2iy9a343pknbr1zj83k4qs2wrh8gzvl8d10chqjwxd0";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/lint/lint/26.0.0/lint-26.0.0" = {
    host = google;
    path = "com/android/tools/lint/lint/26.0.0/lint-26.0.0";
    type = "jar";
    pom-sha256 = "1n0z0ky9v8qg03k0s7rahb4zi9q6rpl3jr91yzwmv5lp1yfq3h94";
    jar-sha256 = "07ywh5pg74sw1p60g8wwcdv82azd56h11161r4dzalz300l9g6ll";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/repository/26.0.0/repository-26.0.0" = {
    host = google;
    path = "com/android/tools/repository/26.0.0/repository-26.0.0";
    type = "jar";
    pom-sha256 = "0z8c67369rch3z1mc52ipfk85wsj2kcwlhk4s3jv1pmf31wzyczd";
    jar-sha256 = "0582xlsbbnwl6dzj67yjjjnh5ysiva5pbd10szfvdi4gcnkvn26q";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/sdk-common/26.0.0/sdk-common-26.0.0" = {
    host = google;
    path = "com/android/tools/sdk-common/26.0.0/sdk-common-26.0.0";
    type = "jar";
    pom-sha256 = "0915xd566szrd7zkmmq3f4swgx9s55qa9ys31dqpfvbqrpb361ap";
    jar-sha256 = "1f33bn2f8y83s02nzmw124lm217bj82kp8m516r69z1k3wz9nb67";
  };
  "https://dl.google.com/dl/android/maven2/com/android/tools/sdklib/26.0.0/sdklib-26.0.0" = {
    host = google;
    path = "com/android/tools/sdklib/26.0.0/sdklib-26.0.0";
    type = "jar";
    pom-sha256 = "1zmpmzaqh4124kh4py51s12ddgpw5dx35spiq1lxv8946kn4m62v";
    jar-sha256 = "1jyh2zzy1mnp3692c6v2lahm75wv2d87iwm9bq2rnsip3pjhzhwv";
  };
  "https://dl.google.com/dl/android/maven2/com/google/gms/google-services/4.0.1/google-services-4.0.1" = {
    host = google;
    path = "com/google/gms/google-services/4.0.1/google-services-4.0.1";
    type = "jar";
    pom-sha256 = "1rqj4lk53x1xknwcm55g5a4lywgc5dzwmji6sd7x37gij64i3x03";
    jar-sha256 = "1s1xm2dnkhpwa854saxq3rwy6lhhv3qw35fjyi6wq8r14xipwcrl";
  };
  "https://jcenter.bintray.com/com/android/tools/annotations/24.5.0/annotations-24.5.0" = {
    host = jcenter;
    path = "com/android/tools/annotations/24.5.0/annotations-24.5.0";
    type = "jar";
    pom-sha256 = "0m2wj96mar7cij4yzvcx1da9ab7ffcaa1s8mcgl9z242h7srpjf3";
    jar-sha256 = "08nrgm8j7q49ds9r0k1cbn1jsxdv0if9bhi37szxpjz7ks7azyr9";
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
  "https://jcenter.bintray.com/org/apache/httpcomponents/httpcomponents-core/4.1/httpcomponents-core-4.1" = {
    host = jcenter;
    path = "org/apache/httpcomponents/httpcomponents-core/4.1/httpcomponents-core-4.1";
    type = "jar";
    pom-sha256 = "1r2dyq3bgz1xamv3a167jd55in4z7vkd27zcvgh6s59xkkypyyag";
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
  "https://jcenter.bintray.com/org/jetbrains/kotlin/kotlin-stdlib-common/1.1.3-2/kotlin-stdlib-common-1.1.3-2" = {
    host = jcenter;
    path = "org/jetbrains/kotlin/kotlin-stdlib-common/1.1.3-2/kotlin-stdlib-common-1.1.3-2";
    type = "jar";
    pom-sha256 = "0i7b5gkjssibpjjjkzlk865ybs1rynxrzbiarr7cxah60l5v42hb";
    jar-sha256 = "16danimis7w2wh41yia7kdg2psjdb63665xpnz28mchrw0vsxq5j";
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
