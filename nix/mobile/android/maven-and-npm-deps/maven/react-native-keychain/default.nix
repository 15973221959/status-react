{}:

let
  jcenter = "https://jcenter.bintray.com";
  google = "https://dl.google.com/dl/android/maven2";
  fabric-io = "https://maven.fabric.io/public";

in {
  "https://jcenter.bintray.com/com/android/tools/annotations/24.1.3/annotations-24.1.3" = {
    host = jcenter;
    path = "com/android/tools/annotations/24.1.3/annotations-24.1.3";
    type = "jar";
    pom-sha256 = "1dsivp9xz98lw6pfn7rgak7sbx39x12fsgs49yplfv2vcmbnr0ks";
    jar-sha256 = "1a1plgmrrxykyb3b91ccqz9wkg3k01fs3rkwvsbdg8iaw90xsdy3";
  };
  "https://jcenter.bintray.com/com/android/tools/build/builder-model/1.1.3/builder-model-1.1.3" = {
    host = jcenter;
    path = "com/android/tools/build/builder-model/1.1.3/builder-model-1.1.3";
    type = "jar";
    pom-sha256 = "0c5vd5ca3v7d7rsbzijv5rh7ibv8sryfh8pdl46hzkr7r7321jsr";
    jar-sha256 = "1pww8x60m0r8safpr6ncf4h0zyzx26dflxyx7h6zs5ahvls7vqwi";
  };
  "https://jcenter.bintray.com/com/android/tools/build/builder-test-api/1.1.3/builder-test-api-1.1.3" = {
    host = jcenter;
    path = "com/android/tools/build/builder-test-api/1.1.3/builder-test-api-1.1.3";
    type = "jar";
    pom-sha256 = "1scfz6dw29imr2689f8bki3xa3q89q34npl2q1cakrgdkjx9vm64";
    jar-sha256 = "1vi1sigcwkcx6hmi2ylprbj72fn6yh6jihwm4ks1c0chwpiwmyzc";
  };
  "https://jcenter.bintray.com/com/android/tools/build/builder/1.1.3/builder-1.1.3" = {
    host = jcenter;
    path = "com/android/tools/build/builder/1.1.3/builder-1.1.3";
    type = "jar";
    pom-sha256 = "1zk2hh69xsk9gx01lcbpvza0if83wx8y626g5xlwqp6fwzmisv56";
    jar-sha256 = "0kxdvscdyail6ykcp07hnd2sw5kikz4cq83vdafqqmycbh4wfkhl";
  };
  "https://jcenter.bintray.com/com/android/tools/build/gradle-core/1.1.3/gradle-core-1.1.3" = {
    host = jcenter;
    path = "com/android/tools/build/gradle-core/1.1.3/gradle-core-1.1.3";
    type = "jar";
    pom-sha256 = "1w14j2y6dvm8ak2rhz6lp9wnqbi14q095k9m39rw69gnp9p6nazz";
    jar-sha256 = "0yd0bmpg5qx1zpmlcfna0fk52fmsrllrfm17ll06176qsbnl5dxj";
  };
  "https://jcenter.bintray.com/com/android/tools/build/gradle/1.1.3/gradle-1.1.3" = {
    host = jcenter;
    path = "com/android/tools/build/gradle/1.1.3/gradle-1.1.3";
    type = "jar";
    pom-sha256 = "1yhrjnnm6mpqi76fknbc6f68n14ls8sc6gchnmx3hhacnhl29swm";
    jar-sha256 = "1rpa2plnz2znkszgg77fsm7zmcjiv77kh95r1s8x9nynf5ys0am6";
  };
  "https://jcenter.bintray.com/com/android/tools/build/manifest-merger/24.1.3/manifest-merger-24.1.3" = {
    host = jcenter;
    path = "com/android/tools/build/manifest-merger/24.1.3/manifest-merger-24.1.3";
    type = "jar";
    pom-sha256 = "114rz6xnnb9h9zywabqs0pl57w968x9h6cxw84nyf1255mvay2af";
    jar-sha256 = "0by5zjrkaapg5cyzq8vv66pzfxnan524nh11l49vi4xakkajkh2l";
  };
  "https://jcenter.bintray.com/com/android/tools/common/24.1.3/common-24.1.3" = {
    host = jcenter;
    path = "com/android/tools/common/24.1.3/common-24.1.3";
    type = "jar";
    pom-sha256 = "1bxd8nnjzhkb8zakfil2sf8jmi8xshhhwyvyjbf814rw20223xdf";
    jar-sha256 = "0cdhgvmlwbrc233l4c9pzif3i8qhqgyhikm1zf055jiawrmb9nch";
  };
  "https://jcenter.bintray.com/com/android/tools/ddms/ddmlib/24.1.3/ddmlib-24.1.3" = {
    host = jcenter;
    path = "com/android/tools/ddms/ddmlib/24.1.3/ddmlib-24.1.3";
    type = "jar";
    pom-sha256 = "0c50xbjl0ygipmy2jpxla95ihr6kr4hq9f4dn62bb9s7wp2jn0la";
    jar-sha256 = "0vxq71zz8v32sgr3qpa2rg04ibg55wdbkhsib3q0ljbgnmiw4xlp";
  };
  "https://jcenter.bintray.com/com/android/tools/dvlib/24.1.3/dvlib-24.1.3" = {
    host = jcenter;
    path = "com/android/tools/dvlib/24.1.3/dvlib-24.1.3";
    type = "jar";
    pom-sha256 = "175lyn7gl84y47ix9igxi7llaywv60hqjji95k5w7v0snh6cqfsb";
    jar-sha256 = "1a8jib0h7l83vw2cd3akp4h4rnns3j0rw920x9sfd0r1zb1ya345";
  };
  "https://jcenter.bintray.com/com/android/tools/external/lombok/lombok-ast/0.2.3/lombok-ast-0.2.3" = {
    host = jcenter;
    path = "com/android/tools/external/lombok/lombok-ast/0.2.3/lombok-ast-0.2.3";
    type = "jar";
    pom-sha256 = "1gbljyqw5r2vc31dl1flclsvdias3049pdrc6dicizcbvph92qcc";
    jar-sha256 = "0vg8w757n8a8v6lafrg4xb9nl7dqw3x56p1qzfsp20j395p0vss5";
  };
  "https://jcenter.bintray.com/com/android/tools/layoutlib/layoutlib-api/24.1.3/layoutlib-api-24.1.3" = {
    host = jcenter;
    path = "com/android/tools/layoutlib/layoutlib-api/24.1.3/layoutlib-api-24.1.3";
    type = "jar";
    pom-sha256 = "11pxp107dam6xcbam2garn2adb51lcr1yjvx8a79mlhy1mg44gbz";
    jar-sha256 = "1swfzjpbnrf0wqiakjpgym06ndjyig8wkv93crllfiiw21klxppg";
  };
  "https://jcenter.bintray.com/com/android/tools/lint/lint-api/24.1.3/lint-api-24.1.3" = {
    host = jcenter;
    path = "com/android/tools/lint/lint-api/24.1.3/lint-api-24.1.3";
    type = "jar";
    pom-sha256 = "1286shvlk9nd7wp9zbcxayaggd76x42r8lqjbbisnqbqa5q7l4gf";
    jar-sha256 = "18g2dq6q8dfzkcbxwkhjzzwgqx83ymdqvpw6rw34h85sq2q3yzwj";
  };
  "https://jcenter.bintray.com/com/android/tools/lint/lint-checks/24.1.3/lint-checks-24.1.3" = {
    host = jcenter;
    path = "com/android/tools/lint/lint-checks/24.1.3/lint-checks-24.1.3";
    type = "jar";
    pom-sha256 = "0r10k23vsrscnmvc4w9rr3kcp3x0vsi5q7dvip2450jnbrwcyf0d";
    jar-sha256 = "0gaf3hm0bhjnbzqyy0i69l17a0d14qkgm2wy7j1qnwsffli6vk1a";
  };
  "https://jcenter.bintray.com/com/android/tools/lint/lint/24.1.3/lint-24.1.3" = {
    host = jcenter;
    path = "com/android/tools/lint/lint/24.1.3/lint-24.1.3";
    type = "jar";
    pom-sha256 = "1qbdmj9lq3pg93cwfg3fi6fbsrqvwirg7zvv4hdk5zfx755fqwas";
    jar-sha256 = "1rknkfg0vbl6wxdfkqhrqs42xhhwrkwsb5i9ng429l3bizkafv08";
  };
  "https://jcenter.bintray.com/com/android/tools/sdk-common/24.1.3/sdk-common-24.1.3" = {
    host = jcenter;
    path = "com/android/tools/sdk-common/24.1.3/sdk-common-24.1.3";
    type = "jar";
    pom-sha256 = "0b3kcjwri58p9nibrmpahhg1l5wnc0b0zgmalfmn34cfszzhqq89";
    jar-sha256 = "1m32bl87w7f34f8clfpzpv4q3d68km2bbv3fx0s26h2xkipl0610";
  };
  "https://jcenter.bintray.com/com/android/tools/sdklib/24.1.3/sdklib-24.1.3" = {
    host = jcenter;
    path = "com/android/tools/sdklib/24.1.3/sdklib-24.1.3";
    type = "jar";
    pom-sha256 = "1zs3mfspi3lwvddpy43dx0nksml5sv5hrng68ha1b7wyc1kiffmb";
    jar-sha256 = "16dkz0ql5zfxhfhvcir8jyqhagsab4ivwcgdmllbr89h4m0h60wa";
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
  "https://jcenter.bintray.com/net/sf/proguard/proguard-base/5.1/proguard-base-5.1" = {
    host = jcenter;
    path = "net/sf/proguard/proguard-base/5.1/proguard-base-5.1";
    type = "jar";
    pom-sha256 = "01j9i5akzpdxm66s0b6s3fzwmf38dz9jvx40yyz4afb0hwa920hb";
    jar-sha256 = "1cjmzwg2is2lm79ay4pmgb4qkij01sxjwq28kjysiys5wl6c3gpy";
  };
  "https://jcenter.bintray.com/net/sf/proguard/proguard-gradle/5.1/proguard-gradle-5.1" = {
    host = jcenter;
    path = "net/sf/proguard/proguard-gradle/5.1/proguard-gradle-5.1";
    type = "jar";
    pom-sha256 = "0a1sz8idwsa3rasjhcfcxnj5wk60vmvimjs1g907gpcc92vn0aqm";
    jar-sha256 = "0yrqzqbxf9jhf04d2mmjsmcj7cac7bg53kgz95hzcs7xxyg7iymh";
  };
  "https://jcenter.bintray.com/net/sf/proguard/proguard-parent/5.1/proguard-parent-5.1" = {
    host = jcenter;
    path = "net/sf/proguard/proguard-parent/5.1/proguard-parent-5.1";
    type = "jar";
    pom-sha256 = "1piwkby3x60jb4z1xkzsfs98h1yd5j313ci6i2ywrx2hkkd7v2ga";
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
  "https://jcenter.bintray.com/org/eclipse/jdt/core/compiler/ecj/4.4/ecj-4.4" = {
    host = jcenter;
    path = "org/eclipse/jdt/core/compiler/ecj/4.4/ecj-4.4";
    type = "jar";
    pom-sha256 = "1rdp3hfdn8ra0v2vff5wyb6va65q05m2lcm57k6i72f8jnn6y1ca";
    jar-sha256 = "1g7ys82plhprp0lv73grfnb463v59klny9zvkd0hjjz10824n5hn";
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
