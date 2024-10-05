(define-module (abbe packages helix-languages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:export (%helix-languages))

(define %helix-languages

  (list (list "ada"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-ada-ba0894ef.tar.gz")
                (uri "https://github.com/briot/tree-sitter-ada/archive/ba0894efa03beb70780156b91e28c716b7a4764d.tar.gz")
                (sha256 (base32 "1arraay585raz1a9vdq8bsjb6d9cwga5fd775k2jz9fjywxabqly")))
              )
        (list "adl"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-adl-2787d04b.tar.gz")
                (uri "https://github.com/adl-lang/tree-sitter-adl/archive/2787d04beadfbe154d3f2da6e98dc45a1b134bbf.tar.gz")
                (sha256 (base32 "01rjwiilsnb20ikpbsjhjx84cjm3in92bs526qr1v8j5isfpwhwn")))
              )
        (list "agda"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-agda-c21c3a0f.tar.gz")
                (uri "https://github.com/tree-sitter/tree-sitter-agda/archive/c21c3a0f996363ed17b8ac99d827fe5a4821f217.tar.gz")
                (sha256 (base32 "102b5fdrms1z7idjhr74knkrm5l7c1yyr5cyqnv30554cz89s40b")))
              )
        (list "astro"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-astro-947e9308.tar.gz")
                (uri "https://github.com/virchau13/tree-sitter-astro/archive/947e93089e60c66e681eba22283f4037841451e7.tar.gz")
                (sha256 (base32 "03ikkn58ig24ys621gzx4h59bx6g124dyrxqsmgca5cz14vzam1x")))
              )
        (list "awk"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-awk-a799bc5d.tar.gz")
                (uri "https://github.com/Beaglefoot/tree-sitter-awk/archive/a799bc5da7c2a84bc9a06ba5f3540cf1191e4ee3.tar.gz")
                (sha256 (base32 "0yvmlrdj971jkr2wy37dp2r7wjn089cbl18xdbbbrjizrjdq0i37")))
              )
        (list "bash"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-bash-f8fb3274.tar.gz")
                (uri "https://github.com/tree-sitter/tree-sitter-bash/archive/f8fb3274f72a30896075585b32b0c54cad65c086.tar.gz")
                (sha256 (base32 "19lsfs6w1frm4y9vm7dxwbgj770fdl9y9bzip6cqgxw11hi4z2yv")))
              )
        (list "bass"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-bass-501133e2.tar.gz")
                (uri "https://github.com/vito/tree-sitter-bass/archive/501133e260d768ed4e1fd7374912ed5c86d6fd90.tar.gz")
                (sha256 (base32 "125ws1nlfc104kabbk107h5vhpxz2xhpi652ar71lv6f2jx67mgn")))
              )
        (list "beancount"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-beancount-f3741a3a.tar.gz")
                (uri "https://github.com/polarmutex/tree-sitter-beancount/archive/f3741a3a68ade59ec894ed84a64673494d2ba8f3.tar.gz")
                (sha256 (base32 "1qrcwixjrch23jf9v382yfvckjgf61480gmvyrm1j7y428xia4hp")))
              )
        (list "bibtex"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-bibtex-ccfd77db.tar.gz")
                (uri "https://github.com/latex-lsp/tree-sitter-bibtex/archive/ccfd77db0ed799b6c22c214fe9d2937f47bc8b34.tar.gz")
                (sha256 (base32 "1s9rnq9av13dk4qnpd6xa988jibf09sqinharg7442gvs7f8ax79")))
              )
        (list "bicep"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-bicep-d8e097fc.tar.gz")
                (uri "https://github.com/the-mikedavis/tree-sitter-bicep/archive/d8e097fcfa143854861ef737161163a09cc2916b.tar.gz")
                (sha256 (base32 "0whvfv6yk5l3ahg94p2r9z913pww2hnkyc2kaaqkyq77vz52y8w4")))
              )
        (list "bitbake"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-bitbake-10bacac9.tar.gz")
                (uri "https://github.com/tree-sitter-grammars/tree-sitter-bitbake/archive/10bacac929ff36a1e8f4056503fe4f8717b21b94.tar.gz")
                (sha256 (base32 "1kcxgawplwg74v9mj1hlj5a0n65b9dzqpxl1fwd5rpzpykrwksqv")))
              )
        (list "blade"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-blade-4c66efe1.tar.gz")
                (uri "https://github.com/EmranMR/tree-sitter-blade/archive/4c66efe1e05c639c555ee70092021b8223d2f440.tar.gz")
                (sha256 (base32 "0ish5k9gv4lg6z4xz53d45r08yhvly3xli29wf867z501ldph33l")))
              )
        (list "blueprint"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-blueprint-863cea9f.tar.gz")
                (uri "https://gitlab.com/gabmus/tree-sitter-blueprint/-/archive/863cea9f83ad5637300478e0559262f1e791684b/tree-sitter-blueprint-863cea9f.tar.gz")
                (sha256 (base32 "1rgralncjfkvr94is4lkcw0xi2vhbl69i5hw0rs73dy71qj9wz7y")))
              )
        (list "c"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-c-7175a6dd.tar.gz")
                (uri "https://github.com/tree-sitter/tree-sitter-c/archive/7175a6dd5fc1cee660dce6fe23f6043d75af424a.tar.gz")
                (sha256 (base32 "0nrfns8g0jpvfkibg5nrfkqnf8ami24aa9nwnmgx8x8vd0vbjyk1")))
              )
        (list "c-sharp"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-c-sharp-5b60f995.tar.gz")
                (uri "https://github.com/tree-sitter/tree-sitter-c-sharp/archive/5b60f99545fea00a33bbfae5be956f684c4c69e2.tar.gz")
                (sha256 (base32 "07nsppx0k1wc10n95n58vspsa29azd5n7pfpx3f3w4yn4jb9gsz3")))
              )
        (list "cairo"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-cairo-e3a02122.tar.gz")
                (uri "https://github.com/starkware-libs/tree-sitter-cairo/archive/e3a0212261c125cb38248458cd856c0ffee2b398.tar.gz")
                (sha256 (base32 "0c3bmnpx8sxk9hnj7q9d2h6xwparskiydyr6g91x1cyd37sw8yj5")))
              )
        (list "capnp"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-capnp-fc6e2add.tar.gz")
                (uri "https://github.com/amaanq/tree-sitter-capnp/archive/fc6e2addf103861b9b3dffb82c543eb6b71061aa.tar.gz")
                (sha256 (base32 "0pibc37gv7j5g1qns2pxiwdli0hqs4kb96p66y14d6lmpy43pmc7")))
              )
        (list "cel"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-cel-9f2b65da.tar.gz")
                (uri "https://github.com/bufbuild/tree-sitter-cel/archive/9f2b65da14c216df53933748e489db0f11121464.tar.gz")
                (sha256 (base32 "1dvkkkp5dr09slm2qzlsbl88lda23gk6ap0lqjhlch707z0w101d")))
              )
        (list "clojure"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-clojure-e57c569a.tar.gz")
                (uri "https://github.com/sogaiu/tree-sitter-clojure/archive/e57c569ae332ca365da623712ae1f50f84daeae2.tar.gz")
                (sha256 (base32 "19rvm2yvsg22lx4zw0hhknq0px8qbd0ymhqm0qcznial1l65bb3x")))
              )
        (list "cmake"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-cmake-6e51463e.tar.gz")
                (uri "https://github.com/uyha/tree-sitter-cmake/archive/6e51463ef3052dd3b328322c22172eda093727ad.tar.gz")
                (sha256 (base32 "0g1vrsymc01yiy7qlbr80fjrdmc0axizc3k6cfzn77qsdmrj75g8")))
              )
        (list "comment"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-comment-aefcc281.tar.gz")
                (uri "https://github.com/stsewd/tree-sitter-comment/archive/aefcc2813392eb6ffe509aa0fc8b4e9b57413ee1.tar.gz")
                (sha256 (base32 "02c9gbsnnri5vpbxfbpvyz5l8swza39hrd9p2f9vrmkz6jc52vcs")))
              )
        (list "cpon"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-cpon-0d01fcda.tar.gz")
                (uri "https://github.com/fvacek/tree-sitter-cpon/archive/0d01fcdae5a53191df5b1349f9bce053833270e7.tar.gz")
                (sha256 (base32 "166b2r7jy3cxrl6kykr9mss3vxqavxds0wb3y4zw6cxs1nv4yhrb")))
              )
        (list "cpp"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-cpp-670404d7.tar.gz")
                (uri "https://github.com/tree-sitter/tree-sitter-cpp/archive/670404d7c689be1c868a46f919ba2a3912f2b7ef.tar.gz")
                (sha256 (base32 "19s49f2g7cyv3h1dp02ilq81kpakfmbk6qannklvh7478b6hcycg")))
              )
        (list "css"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-css-769203d0.tar.gz")
                (uri "https://github.com/tree-sitter/tree-sitter-css/archive/769203d0f9abe1a9a691ac2b9fe4bb4397a73c51.tar.gz")
                (sha256 (base32 "0xypd06ij9wvxkg734hd4syvrwgbjkx84znm653wkv5glbli15na")))
              )
        (list "cue"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-cue-61843e3b.tar.gz")
                (uri "https://github.com/eonpatapon/tree-sitter-cue/archive/61843e3beebf19417e4fede4e8be4df1084317ad.tar.gz")
                (sha256 (base32 "03j1ysrjbqnrqhckaqr6vd8akbsb1ljfbw410rrp13j9n8kcirc0")))
              )
        (list "d"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-d-5566f8ce.tar.gz")
                (uri "https://github.com/gdamore/tree-sitter-d/archive/5566f8ce8fc24186fad06170bbb3c8d97c935d74.tar.gz")
                (sha256 (base32 "1a2ml9ib9fncfwlbz4q9wmn88nq1qx9hq0hz8r17319imfaw2k5n")))
              )
        (list "dart"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-dart-e398400a.tar.gz")
                (uri "https://github.com/UserNobody14/tree-sitter-dart/archive/e398400a0b785af3cf571f5a57eccab242f0cdf9.tar.gz")
                (sha256 (base32 "0ml2wjj0fgg3l0xykqjaws6iilv7q7y1fqxs90h8am7ws5vpz20p")))
              )
        (list "dbml"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-dbml-2e2fa564.tar.gz")
                (uri "https://github.com/dynamotn/tree-sitter-dbml/archive/2e2fa5640268c33c3d3f27f7e676f631a9c68fd9.tar.gz")
                (sha256 (base32 "0nmqd33wxmf70nxcsxx2w2jkmhvz446dysvylgqw0byfm0v0apsc")))
              )
        (list "devicetree"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-devicetree-877adbfa.tar.gz")
                (uri "https://github.com/joelspadin/tree-sitter-devicetree/archive/877adbfa0174d25894c40fa75ad52d4515a36368.tar.gz")
                (sha256 (base32 "0dsjwqbbxb3pxy2wbrqfj5wg8w5j40bds0zjjzl5faldyyd71v96")))
              )
        (list "dhall"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-dhall-affb6ee3.tar.gz")
                (uri "https://github.com/jbellerb/tree-sitter-dhall/archive/affb6ee38d629c9296749767ab832d69bb0d9ea8.tar.gz")
                (sha256 (base32 "0mgn0dr2zhhmxwfhmdw6lkw1sq034as0g1id90gzbrmnnkfzxx75")))
              )
        (list "diff"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-diff-fd74c78f.tar.gz")
                (uri "https://github.com/the-mikedavis/tree-sitter-diff/archive/fd74c78fa88a20085dbc7bbeaba066f4d1692b63.tar.gz")
                (sha256 (base32 "076wz04p4sj3w79awkz4fw0qrwkfn9qwlcigbaj3rrihan6p0b7m")))
              )
        (list "dockerfile"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-dockerfile-8ee3a0f7.tar.gz")
                (uri "https://github.com/camdencheek/tree-sitter-dockerfile/archive/8ee3a0f7587b2bd8c45c8cb7d28bd414604aec62.tar.gz")
                (sha256 (base32 "1k9n9x79sv52iwirw31qffv19wb3a92pyy9a5bsvhxrq9l8nzj6i")))
              )
        (list "dot"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-dot-91723074.tar.gz")
                (uri "https://github.com/rydesun/tree-sitter-dot/archive/917230743aa10f45a408fea2ddb54bbbf5fbe7b7.tar.gz")
                (sha256 (base32 "10nmw4a1b0vcl2p39nhcljrpy0297d9fj0mg2dapc3d5dyi9g8pg")))
              )
        (list "dtd"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-dtd-6116becb.tar.gz")
                (uri "https://github.com/KMikeeU/tree-sitter-dtd/archive/6116becb02a6b8e9588ef73d300a9ba4622e156f.tar.gz")
                (sha256 (base32 "1dlkd273s6rphr3pk24a3gkj9c64qjrrknwddcdcr83c96kdpz9m")))
              )
        (list "earthfile"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-earthfile-dbfb970a.tar.gz")
                (uri "https://github.com/glehmann/tree-sitter-earthfile/archive/dbfb970a59cd87b628d087eb8e3fbe19c8e20601.tar.gz")
                (sha256 (base32 "0yllbpdn05gh9cry07wsvdc4bcjzfr7lvrggqq1swls8dxaqbg2j")))
              )
        (list "edoc"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-edoc-74774af7.tar.gz")
                (uri "https://github.com/the-mikedavis/tree-sitter-edoc/archive/74774af7b45dd9cefbf9510328fc6ff2374afc50.tar.gz")
                (sha256 (base32 "1ncxazvivffw9b702x712zqhjhsp1w9dkkyd7k3954gdqkplcpda")))
              )
        (list "eex"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-eex-f742f2fe.tar.gz")
                (uri "https://github.com/connorlay/tree-sitter-eex/archive/f742f2fe327463335e8671a87c0b9b396905d1d1.tar.gz")
                (sha256 (base32 "1mlyra95przqybzmjmqf2j2lz9xi9b2abrv7a95v1rdnk8klgks3")))
              )
        (list "elisp"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-elisp-e5524fdc.tar.gz")
                (uri "https://github.com/Wilfred/tree-sitter-elisp/archive/e5524fdccf8c22fc726474a910e4ade976dfc7bb.tar.gz")
                (sha256 (base32 "19hqcby2nmd3z1cim1ikbvsdkd94zrlhddxpdvaq9kavpwh7nldq")))
              )
        (list "elixir"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-elixir-b20eaa75.tar.gz")
                (uri "https://github.com/elixir-lang/tree-sitter-elixir/archive/b20eaa75565243c50be5e35e253d8beb58f45d56.tar.gz")
                (sha256 (base32 "0far8zz4nil726n43a8xpp3plx66vnm2fv1k73iryk5686n0mm30")))
              )
        (list "elm"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-elm-df4cb639.tar.gz")
                (uri "https://github.com/elm-tooling/tree-sitter-elm/archive/df4cb639c01b76bc9ac9cc66788709a6da20002c.tar.gz")
                (sha256 (base32 "0hzndcysp1aw4szgy5l72p34la0r9r66qkc151l5fzkkmxwf4f3b")))
              )
        (list "elvish"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-elvish-e50787ca.tar.gz")
                (uri "https://github.com/ckafi/tree-sitter-elvish/archive/e50787cadd3bc54f6d9c0704493a79078bb8a4e5.tar.gz")
                (sha256 (base32 "1sg1x60594y85n0n3blap1yfkqxrr9971gg68vwjb0h0v8v7vz7f")))
              )
        (list "embedded-template"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-embedded-template-d21df11b.tar.gz")
                (uri "https://github.com/tree-sitter/tree-sitter-embedded-template/archive/d21df11b0ecc6fd211dbe11278e92ef67bd17e97.tar.gz")
                (sha256 (base32 "0fbq7c7vszl6hzp33hwxrl0dfnnb3rw80xgmmlrqlqcfhlzqq9wb")))
              )
        (list "erlang"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-erlang-9d4b36a7.tar.gz")
                (uri "https://github.com/the-mikedavis/tree-sitter-erlang/archive/9d4b36a76d5519e3dbf1ec4f4b61bb1a293f584c.tar.gz")
                (sha256 (base32 "0nq2064w0s6fgzc9sl1wfmk080q6ih5l7m4cnrdmwvyvwp1yvsr6")))
              )
        (list "esdl"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-esdl-df83acc8.tar.gz")
                (uri "https://github.com/greym0uth/tree-sitter-esdl/archive/df83acc8cacd0cfb139eecee0e718dc32c4f92e2.tar.gz")
                (sha256 (base32 "1c05krfzjaxjvdaamdmyca3b17r0dqlbm6i8i27hgqcq7pv97991")))
              )
        (list "fidl"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-fidl-bdbb635a.tar.gz")
                (uri "https://github.com/google/tree-sitter-fidl/archive/bdbb635a7f5035e424f6173f2f11b9cd79703f8d.tar.gz")
                (sha256 (base32 "1vr33zjrg4qn08dly9g8c68ym6a8rf3i2vnrvn4q0waa0w3jg8g2")))
              )
        (list "fish"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-fish-84436cf2.tar.gz")
                (uri "https://github.com/ram02z/tree-sitter-fish/archive/84436cf24c2b3176bfbb220922a0fdbd0141e406.tar.gz")
                (sha256 (base32 "0vvpg65vdprfcnjak24fdsgh67lqayib94aqzhksz8nl1h13hsmp")))
              )
        (list "forth"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-forth-90189238.tar.gz")
                (uri "https://github.com/alexanderbrevig/tree-sitter-forth/archive/90189238385cf636b9ee99ce548b9e5b5e569d48.tar.gz")
                (sha256 (base32 "10p9pyhz7i6vmq0g4bbmk5xcz1vk3qnsgiiwkdkdc9gnwgmnklb0")))
              )
        (list "fortran"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-fortran-f0f2f100.tar.gz")
                (uri "https://github.com/stadelmanma/tree-sitter-fortran/archive/f0f2f100952a353e64e26b0fa710b4c296d7af13.tar.gz")
                (sha256 (base32 "152vwip6y9f261iybm2bcn3hn044z6jaxnfr3kgz7yiwz2kgafgj")))
              )
        (list "fsharp"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-fsharp-18da392f.tar.gz")
                (uri "https://github.com/kaashyapan/tree-sitter-fsharp/archive/18da392fd9bd5e79f357abcce13f61f3a15e3951.tar.gz")
                (sha256 (base32 "1pymf4rvqak9jw14v0h9vxa94vhhl63zkp3iykxpfhv08jmmpj16")))
              )
        (list "gas"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-gas-60f44364.tar.gz")
                (uri "https://github.com/sirius94/tree-sitter-gas/archive/60f443646b20edee3b7bf18f3a4fb91dc214259a.tar.gz")
                (sha256 (base32 "0xzl3hnkl9g86pw2amkgphwdyrqv6p353q1bfq4vscddq0a22fj1")))
              )
        (list "gdscript"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-gdscript-a4b57cc3.tar.gz")
                (uri "https://github.com/PrestonKnopp/tree-sitter-gdscript/archive/a4b57cc3bcbfc24550e858159647e9238e7ad1ac.tar.gz")
                (sha256 (base32 "19fg7shv2yp7hhq7jw5nk0x0x7px5vj90yfs1mjrjjq1fgdrw3df")))
              )
        (list "gemini"
              (origin
                (method git-fetch)
                (file-name "tree-sitter-gemini-3cc5e4bd")
                (uri (git-reference (url "https://git.sr.ht/~nbsp/tree-sitter-gemini") (commit "3cc5e4bdf572d5df4277fc2e54d6299bd59a54b3")))
                (sha256 (base32 "1r965z6cls7ahipf9bpvcvpv4xda7k1j64lvnwf2bkb83qpakdc2")))
              )
        (list "git-commit"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-git-commit-6f193a66.tar.gz")
                (uri "https://github.com/the-mikedavis/tree-sitter-git-commit/archive/6f193a66e9aa872760823dff020960c6cedc37b3.tar.gz")
                (sha256 (base32 "1qhra75caab02f3rxwa025asacxap7qdlsi42xwznsv6vp0wbi2r")))
              )
        (list "git-config"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-git-config-9c2a1b78.tar.gz")
                (uri "https://github.com/the-mikedavis/tree-sitter-git-config/archive/9c2a1b7894e6d9eedfe99805b829b4ecd871375e.tar.gz")
                (sha256 (base32 "09pln3j2swpnvxkvlv4pn88y5zv1zalwh71v4gxrd1jhrarnmxak")))
              )
        (list "git-rebase"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-git-rebase-d8a4207e.tar.gz")
                (uri "https://github.com/the-mikedavis/tree-sitter-git-rebase/archive/d8a4207ebbc47bd78bacdf48f883db58283f9fd8.tar.gz")
                (sha256 (base32 "0a1rmyl20c18hxv8kx6421caj46c6l4bv9pagrjsh7gqhrq3qv4a")))
              )
        (list "gitattributes"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-gitattributes-3dd50808.tar.gz")
                (uri "https://github.com/mtoohey31/tree-sitter-gitattributes/archive/3dd50808e3096f93dccd5e9dc7dc3dba2eb12dc4.tar.gz")
                (sha256 (base32 "11hdp02iw4x80s65ihans2vj64zibszh2lkwnpf1z4vrswsbf8dr")))
              )
        (list "gitignore"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-gitignore-f4685bf1.tar.gz")
                (uri "https://github.com/shunsambongi/tree-sitter-gitignore/archive/f4685bf11ac466dd278449bcfe5fd014e94aa504.tar.gz")
                (sha256 (base32 "172mmbvxhz8fb335ag9vvfxxlgbcgzvwfz8lbgc9px0wh1r7fwhm")))
              )
        (list "gleam"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-gleam-bcf9c45b.tar.gz")
                (uri "https://github.com/gleam-lang/tree-sitter-gleam/archive/bcf9c45b56cbe46e9dac5eee0aee75df270000ac.tar.gz")
                (sha256 (base32 "17ar43v84g9scpanr8mcw8wblihrlc9rhzad36hznywk6gfj7jgh")))
              )
        (list "glimmer"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-glimmer-5dc6d104.tar.gz")
                (uri "https://github.com/ember-tooling/tree-sitter-glimmer/archive/5dc6d1040e8ff8978ff3680e818d85447bbc10aa.tar.gz")
                (sha256 (base32 "0l05h01j0amzcylc335al6qi8fa7rilgaf4c4yfjvpqd13h7lxmf")))
              )
        (list "glsl"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-glsl-88408ffc.tar.gz")
                (uri "https://github.com/theHamsta/tree-sitter-glsl/archive/88408ffc5e27abcffced7010fc77396ae3636d7e.tar.gz")
                (sha256 (base32 "0wl4izaafknql220s1c1f2l046vw7wlck4kah77bnq6jvn7bzmgi")))
              )
        (list "gn"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-gn-e18d6e36.tar.gz")
                (uri "https://github.com/willcassella/tree-sitter-gn/archive/e18d6e36a79b20dafb58f19d407bd38b0e60260e.tar.gz")
                (sha256 (base32 "0yv956jfpysm64rzskwrfdzyzj0dv2z6c9clgyj7xcbf1vvdyg85")))
              )
        (list "go"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-go-64457ea6.tar.gz")
                (uri "https://github.com/tree-sitter/tree-sitter-go/archive/64457ea6b73ef5422ed1687178d4545c3e91334a.tar.gz")
                (sha256 (base32 "14v0k19s42wcm4ys24cgckjjh98cql11clbn5lzrlrgjg9y85783")))
              )
        (list "godot-resource"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-godot-resource-b6ef0768.tar.gz")
                (uri "https://github.com/PrestonKnopp/tree-sitter-godot-resource/archive/b6ef0768711086a86b3297056f9ffb5cc1d77b4a.tar.gz")
                (sha256 (base32 "11yyxl4xg0g792a0ns3f6w776g35s3zjbizccvrqznnx1dx04nsl")))
              )
        (list "gomod"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-gomod-e8f51f8e.tar.gz")
                (uri "https://github.com/camdencheek/tree-sitter-go-mod/archive/e8f51f8e4363a3d9a427e8f63f4c1bbc5ef5d8d0.tar.gz")
                (sha256 (base32 "15s57mbghfwkzbwxbb1p1x5gn46d0zmxr0d7kparndsj7wqb23sy")))
              )
        (list "gotmpl"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-gotmpl-395a33e0.tar.gz")
                (uri "https://github.com/dannylongeuay/tree-sitter-go-template/archive/395a33e08e69f4155156f0b90138a6c86764c979.tar.gz")
                (sha256 (base32 "0kbf05ya416yq8r7s9w9acd0k2m01nvrmz9k36dbi2wcszqlyf9r")))
              )
        (list "gowork"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-gowork-6dd9dd79.tar.gz")
                (uri "https://github.com/omertuc/tree-sitter-go-work/archive/6dd9dd79fb51e9f2abc829d5e97b15015b6a8ae2.tar.gz")
                (sha256 (base32 "01injg3xpwxc7h59wycvy8bnhf90syr0lw42afjd60j443w2f5il")))
              )
        (list "graphql"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-graphql-5e66e961.tar.gz")
                (uri "https://github.com/bkegley/tree-sitter-graphql/archive/5e66e961eee421786bdda8495ed1db045e06b5fe.tar.gz")
                (sha256 (base32 "01wmvgswa4zq3cp2mlf0w3m2w8fj0dks2vxk1c8iq7qvnpfjym7z")))
              )
        (list "groovy"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-groovy-235009aa.tar.gz")
                (uri "https://github.com/murtaza64/tree-sitter-groovy/archive/235009aad0f580211fc12014bb0846c3910130c1.tar.gz")
                (sha256 (base32 "0w4216dbxkspbr1yvdrcpgsm2f9734ywr82vpz7kvdkl1pbvpbn0")))
              )
        (list "hare"
              (origin
                (method git-fetch)
                (file-name "tree-sitter-hare-07035a24")
                (uri (git-reference (url "https://git.sr.ht/~ecs/tree-sitter-hare") (commit "07035a248943575444aa0b893ffe306e1444c0ab")))
                (sha256 (base32 "1wji09yzcc0jcnlpgapz3cz9lmq5xcbpkpp7pn9vfq5n76gl1qwq")))
              )
        (list "haskell"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-haskell-d7ac98f4.tar.gz")
                (uri "https://github.com/tree-sitter/tree-sitter-haskell/archive/d7ac98f49e3ed7e17541256fe3881a967d7ffdd3.tar.gz")
                (sha256 (base32 "0lhrm2pib3zpcvv9pym85a0pfpavl612jzwfd5mcywvqivck4r7m")))
              )
        (list "haskell-persistent"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-haskell-persistent-58a6ccfd.tar.gz")
                (uri "https://github.com/MercuryTechnologies/tree-sitter-haskell-persistent/archive/58a6ccfd56d9f1de8fb9f77e6c42151f8f0d0f3d.tar.gz")
                (sha256 (base32 "0mk0n34cd6j8kahz26vmyp1l18clqkcw1v7rcjhd1q03kjz8jj2i")))
              )
        (list "hcl"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-hcl-3cb7fc28.tar.gz")
                (uri "https://github.com/MichaHoffmann/tree-sitter-hcl/archive/3cb7fc28247efbcb2973b97e71c78838ad98a583.tar.gz")
                (sha256 (base32 "0rhw662q7c1p2qzdnlij1ls2pldr5b6rxyv73q9jxg5vp1jh5mwp")))
              )
        (list "heex"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-heex-2e1348c3.tar.gz")
                (uri "https://github.com/phoenixframework/tree-sitter-heex/archive/2e1348c3cf2c9323e87c2744796cf3f3868aa82a.tar.gz")
                (sha256 (base32 "0m4pxq9fy83gm9ik1wj337qayf77mqgggld4w74nfb2qdm4gxrih")))
              )
        (list "hocon"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-hocon-c390f105.tar.gz")
                (uri "https://github.com/antosha417/tree-sitter-hocon/archive/c390f10519ae69fdb03b3e5764f5592fb6924bcc.tar.gz")
                (sha256 (base32 "0hjcrw4pv9ixfkvv8hrwhjmzksiqc01pwad9avxvr7l7rn1zzjda")))
              )
        (list "hoon"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-hoon-1d5df35a.tar.gz")
                (uri "https://github.com/urbit-pilled/tree-sitter-hoon/archive/1d5df35af3e0afe592832a67b9fb3feeeba1f7b6.tar.gz")
                (sha256 (base32 "0n2z65wh4p826vgfn9n3hj5vdbdyz5pv23c1skyib0lai1j4l2i5")))
              )
        (list "hosts"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-hosts-301b9379.tar.gz")
                (uri "https://github.com/ath3/tree-sitter-hosts/archive/301b9379ce7dfc8bdbe2c2699a6887dcb73953f9.tar.gz")
                (sha256 (base32 "0pir9cd80hi3fxyrw6783ml2ng14kd8ayckfk8h0vghap6b4jigg")))
              )
        (list "html"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-html-29f53d8f.tar.gz")
                (uri "https://github.com/tree-sitter/tree-sitter-html/archive/29f53d8f4f2335e61bf6418ab8958dac3282077a.tar.gz")
                (sha256 (base32 "1h11sbq7av5ii54w9cv705mbls13gqj1c98jjikwyi0bq62n4ylz")))
              )
        (list "hurl"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-hurl-cd1a0ada.tar.gz")
                (uri "https://github.com/pfeiferj/tree-sitter-hurl/archive/cd1a0ada92cc73dd0f4d7eedc162be4ded758591.tar.gz")
                (sha256 (base32 "1mf197dkfik8kjmr7vihcpspcrgaghg5cwvw9dhwnaxzfdmnwzwj")))
              )
        (list "hyprlang"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-hyprlang-27af9b74.tar.gz")
                (uri "https://github.com/tree-sitter-grammars/tree-sitter-hyprlang/archive/27af9b74acf89fa6bed4fb8cb8631994fcb2e6f3.tar.gz")
                (sha256 (base32 "144yx1frfpm8rjwmyzjd3bxsgmn9q4ccykr0z4a4pqnm7ci275f1")))
              )
        (list "iex"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-iex-39f20bb5.tar.gz")
                (uri "https://github.com/elixir-lang/tree-sitter-iex/archive/39f20bb51f502e32058684e893c0c0b00bb2332c.tar.gz")
                (sha256 (base32 "1mlj4pna6al8c9dqph51j6fxd9dws886vdgm3kk7p3vx3r74yx5d")))
              )
        (list "ini"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-ini-4d247fb8.tar.gz")
                (uri "https://github.com/justinmk/tree-sitter-ini/archive/4d247fb876b4ae6b347687de4a179511bf67fcbc.tar.gz")
                (sha256 (base32 "18qjly6ysk3gvb01lnrgn0w2gw1lsqm982x2fjl35l6fh5rzlqy2")))
              )
        (list "inko"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-inko-7860637c.tar.gz")
                (uri "https://github.com/inko-lang/tree-sitter-inko/archive/7860637ce1b43f5f79cfb7cc3311bf3234e9479f.tar.gz")
                (sha256 (base32 "1gnz9q794p6k6x9h0np7nbdz49ycnkp28ralgbcjpi2pp2s0cczk")))
              )
        (list "janet-simple"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-janet-simple-51271e26.tar.gz")
                (uri "https://github.com/sogaiu/tree-sitter-janet-simple/archive/51271e260346878e1a1aa6c506ce6a797b7c25e2.tar.gz")
                (sha256 (base32 "0zirghbg8461qv81b2vchhnqbs7dwkdaf495hbqhfkv6q7wpmc79")))
              )
        (list "java"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-java-09d650de.tar.gz")
                (uri "https://github.com/tree-sitter/tree-sitter-java/archive/09d650def6cdf7f479f4b78f595e9ef5b58ce31e.tar.gz")
                (sha256 (base32 "0zv4hffyp01mspmmmkzvjrb1jdcar4as6crzkjswdxrj1x36ypd4")))
              )
        (list "javascript"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-javascript-f772967f.tar.gz")
                (uri "https://github.com/tree-sitter/tree-sitter-javascript/archive/f772967f7b7bc7c28f845be2420a38472b16a8ee.tar.gz")
                (sha256 (base32 "0iwwvmmqshz15a1b0qz66nr18k24g4cl4ia5il9p6iqmjcxb472m")))
              )
        (list "jinja2"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-jinja2-a533cd3c.tar.gz")
                (uri "https://github.com/varpeti/tree-sitter-jinja2/archive/a533cd3c33aea6acb0f9bf9a56f35dcfe6a8eb53.tar.gz")
                (sha256 (base32 "1v0ha6838n350pljp6xi9qwim2v9y5iahk01p0nv5jy38s7f498b")))
              )
        (list "jsdoc"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-jsdoc-189a6a48.tar.gz")
                (uri "https://github.com/tree-sitter/tree-sitter-jsdoc/archive/189a6a4829beb9cdbe837260653b4a3dfb0cc3db.tar.gz")
                (sha256 (base32 "10pv2xzwr13acihyayw9dch2jgsg8rpwzq8ma02znf8s37h7awgg")))
              )
        (list "json"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-json-73076754.tar.gz")
                (uri "https://github.com/tree-sitter/tree-sitter-json/archive/73076754005a460947cafe8e03a8cf5fa4fa2938.tar.gz")
                (sha256 (base32 "1pkxq5nnq4qvj0x2j23ak3vhnx8ax6gxznf1wwfn17py1ifdc5c9")))
              )
        (list "json5"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-json5-c23f7a9b.tar.gz")
                (uri "https://github.com/Joakker/tree-sitter-json5/archive/c23f7a9b1ee7d45f516496b1e0e4be067264fa0d.tar.gz")
                (sha256 (base32 "1p711gh0rshm74c9jlmmaw8gv4lijqd59820zqlwygs6m9xcvlb9")))
              )
        (list "jsonnet"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-jsonnet-0475a501.tar.gz")
                (uri "https://github.com/sourcegraph/tree-sitter-jsonnet/archive/0475a5017ad7dc84845d1d33187f2321abcb261d.tar.gz")
                (sha256 (base32 "06zxixrvry820q70z8sqp5clnqcip6ppbv1sk2pmj0nmzkmxqr5w")))
              )
        (list "julia"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-julia-8fb38abf.tar.gz")
                (uri "https://github.com/tree-sitter/tree-sitter-julia/archive/8fb38abff74652c4faddbf04d2d5bbbc6b4bae25.tar.gz")
                (sha256 (base32 "0j0b4qfhlpjh59c73b8krwz2gjfb0r5n1a11mwgwn4bsk823j8fb")))
              )
        (list "just"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-just-8af0aab7.tar.gz")
                (uri "https://github.com/IndianBoy42/tree-sitter-just/archive/8af0aab79854aaf25b620a52c39485849922f766.tar.gz")
                (sha256 (base32 "0c9gvn28zzm65ngxxvq57239svsbnl3fvyj92894gy9di2cnmy9w")))
              )
        (list "kdl"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-kdl-3ca569b9.tar.gz")
                (uri "https://github.com/amaanq/tree-sitter-kdl/archive/3ca569b9f9af43593c24f9e7a21f02f43a13bb88.tar.gz")
                (sha256 (base32 "0882j961bvwfhswjpis8ji19273846nlym0abhjsp9g9inrvppyj")))
              )
        (list "koka"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-koka-96d070c3.tar.gz")
                (uri "https://github.com/mtoohey31/tree-sitter-koka/archive/96d070c3700692858035f3524cc0ad944cef2594.tar.gz")
                (sha256 (base32 "0dn60942kb8p70iz1nlp5i3dcbkvkcwyciz4vn8pd1l6bwwdlx9j")))
              )
        (list "kotlin"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-kotlin-a4f71eb9.tar.gz")
                (uri "https://github.com/fwcd/tree-sitter-kotlin/archive/a4f71eb9b8c9b19ded3e0e9470be4b1b77c2b569.tar.gz")
                (sha256 (base32 "1nknpcmx1854vnwn33r41bvx9d0pca0gh7jkmwgyirzwiijdjfmp")))
              )
        (list "latex"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-latex-8c75e93c.tar.gz")
                (uri "https://github.com/latex-lsp/tree-sitter-latex/archive/8c75e93cd08ccb7ce1ccab22c1fbd6360e3bcea6.tar.gz")
                (sha256 (base32 "0c4hm79xlb6dwr8yj7c44nny3zs00rjkklgz1zinhmpsb3whcijw")))
              )
        (list "ld"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-ld-0e9695ae.tar.gz")
                (uri "https://github.com/mtoohey31/tree-sitter-ld/archive/0e9695ae0ede47b8744a8e2ad44d4d40c5d4e4c9.tar.gz")
                (sha256 (base32 "0ssrgnf122c5j8nlgprbdf1f8v8s8zpjyva3gf789dd4hrv7p536")))
              )
        (list "ldif"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-ldif-0a917207.tar.gz")
                (uri "https://github.com/kepet19/tree-sitter-ldif/archive/0a917207f65ba3e3acfa9cda16142ee39c4c1aaa.tar.gz")
                (sha256 (base32 "0038nnwq7pfi7snrbaa16jyarszmnv37ikf5dys3k3yikh2j6z8i")))
              )
        (list "lean"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-lean-d9842610.tar.gz")
                (uri "https://github.com/Julian/tree-sitter-lean/archive/d98426109258b266e1e92358c5f11716d2e8f638.tar.gz")
                (sha256 (base32 "14gj9s60zjwlqs8jj4igjca5pdshjbdvkhbd49gmvzhrx0fc4v31")))
              )
        (list "ledger"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-ledger-1f864fb2.tar.gz")
                (uri "https://github.com/cbarrete/tree-sitter-ledger/archive/1f864fb2bf6a87fe1b48545cc6adc6d23090adf7.tar.gz")
                (sha256 (base32 "056j8wqjqmxkpwvdzw1p06yh677bhdq31inbs5gq3g9ix108mpzc")))
              )
        (list "llvm"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-llvm-e9948edc.tar.gz")
                (uri "https://github.com/benwilliamgraham/tree-sitter-llvm/archive/e9948edc41e9e5869af99dddb2b5ff5cc5581af6.tar.gz")
                (sha256 (base32 "1yks2yj9zdmzllnbww276imnah3whxpbv9s96ad6z7kayxfxg5bs")))
              )
        (list "llvm-mir"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-llvm-mir-06fabca1.tar.gz")
                (uri "https://github.com/Flakebi/tree-sitter-llvm-mir/archive/06fabca19454b2dc00c1b211a7cb7ad0bc2585f1.tar.gz")
                (sha256 (base32 "1rcsigwj34inc4j767kli5lcrsp82qc4fc88g5rc8ffccsnbpki9")))
              )
        (list "log"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-log-62cfe307.tar.gz")
                (uri "https://github.com/Tudyx/tree-sitter-log/archive/62cfe307e942af3417171243b599cc7deac5eab9.tar.gz")
                (sha256 (base32 "02hgkvvyxjgk7jfygn5aai1qlids5k4bsfkwxzan5barxl8rf07w")))
              )
        (list "lpf"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-lpf-db7372e6.tar.gz")
                (uri "https://gitlab.com/TheZoq2/tree-sitter-lpf/-/archive/db7372e60c722ca7f12ab359e57e6bf7611ab126/tree-sitter-lpf-db7372e6.tar.gz")
                (sha256 (base32 "1rcr5axahb2rkwsy59y0kwmpx1a97206wyjn3m02apjf2hylvx5x")))
              )
        (list "lua"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-lua-88e44647.tar.gz")
                (uri "https://github.com/tree-sitter-grammars/tree-sitter-lua/archive/88e446476a1e97a8724dff7a23e2d709855077f2.tar.gz")
                (sha256 (base32 "16nypxbvg3dkpc5s31rsf7f3q6d9l0hcg1gd9yrw8wm81d89yd4k")))
              )
        (list "make"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-make-a4b91874.tar.gz")
                (uri "https://github.com/alemuller/tree-sitter-make/archive/a4b9187417d6be349ee5fd4b6e77b4172c6827dd.tar.gz")
                (sha256 (base32 "1q73kq3xg34vw2lsp0nwch4j7na4w14lqc205dbgwsy37x27iq51")))
              )
        (list "markdoc"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-markdoc-5ffe71b2.tar.gz")
                (uri "https://github.com/markdoc-extra/tree-sitter-markdoc/archive/5ffe71b29e8a3f94823913ea9cea51fcfa7e3bf8.tar.gz")
                (sha256 (base32 "0y8qymwmfbqlf47nqh3ykiydpqwryn4p40xqh30mawbmn8yp3jzi")))
              )
        (list "markdown"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-markdown-62516e8c.tar.gz")
                (uri "https://github.com/tree-sitter-grammars/tree-sitter-markdown/archive/62516e8c78380e3b51d5b55727995d2c511436d8.tar.gz")
                (sha256 (base32 "1yrxz503481cw9jzllqw8r7mvv7v2gv4i2sq93kphqjbcs6sz7mn")))
              )
        (list "markdown_inline"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-markdown_inline-62516e8c.tar.gz")
                (uri "https://github.com/tree-sitter-grammars/tree-sitter-markdown/archive/62516e8c78380e3b51d5b55727995d2c511436d8.tar.gz")
                (sha256 (base32 "1yrxz503481cw9jzllqw8r7mvv7v2gv4i2sq93kphqjbcs6sz7mn")))
              )
        (list "matlab"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-matlab-6071891a.tar.gz")
                (uri "https://github.com/acristoffers/tree-sitter-matlab/archive/6071891a8c39600203eba20513666cf93b4d650a.tar.gz")
                (sha256 (base32 "0i346awac28wlgi4brk4bw8w3s0i1jbmx3i6458j7aqnrgcv6xpb")))
              )
        (list "mermaid"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-mermaid-d787c662.tar.gz")
                (uri "https://github.com/monaqa/tree-sitter-mermaid/archive/d787c66276e7e95899230539f556e8b83ee16f6d.tar.gz")
                (sha256 (base32 "18d5r8q1sk5dfn6n9aqr7x2dawyxswvnanfbnng6b72xwzh3dhqp")))
              )
        (list "meson"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-meson-32a83e8f.tar.gz")
                (uri "https://github.com/staysail/tree-sitter-meson/archive/32a83e8f200c347232fa795636cfe60dde22957a.tar.gz")
                (sha256 (base32 "0mfh5sp23zhnw01nqjhfbmhrv1bgxd1001b2m11s2db8p2q16lhn")))
              )
        (list "mojo"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-mojo-3d7c53b8.tar.gz")
                (uri "https://github.com/lsh/tree-sitter-mojo/archive/3d7c53b8038f9ebbb57cd2e61296180aa5c1cf64.tar.gz")
                (sha256 (base32 "1jb3mrrn1npjcdmpnpjhvsdf1m24r4n1gv27gk1qvh0j5fb6rmgy")))
              )
        (list "move"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-move-8bc0d169.tar.gz")
                (uri "https://github.com/tzakian/tree-sitter-move/archive/8bc0d1692caa8763fef54d48068238d9bf3c0264.tar.gz")
                (sha256 (base32 "09zwrf3yr6gqnnnxq45qnypcdnlb9mgpikd5hmwnjydaf9bz3y7n")))
              )
        (list "nasm"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-nasm-a0db15db.tar.gz")
                (uri "https://github.com/naclsn/tree-sitter-nasm/archive/a0db15db6fcfb1bf2cc8702500e55e558825c48b.tar.gz")
                (sha256 (base32 "1mhxdfc2kky01zv79fd3662a38r3lrc0aniw5dmpwpr7q6q4629y")))
              )
        (list "nickel"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-nickel-e1d93378.tar.gz")
                (uri "https://github.com/nickel-lang/tree-sitter-nickel/archive/e1d9337864d209898a08c26b8cd4c2dd14c15148.tar.gz")
                (sha256 (base32 "17i375vkpqwgm1vfkz4mjf4j8h9mkkzgvv6z2qm8nwa54dg5r67x")))
              )
        (list "nim"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-nim-c5f0ce3b.tar.gz")
                (uri "https://github.com/alaviss/tree-sitter-nim/archive/c5f0ce3b65222f5dbb1a12f9fe894524881ad590.tar.gz")
                (sha256 (base32 "035pyiwvx09280lbklfsmxyzpgvi4ww32yl3x1w4a2l05k3m0xfi")))
              )
        (list "nix"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-nix-1b69cf1f.tar.gz")
                (uri "https://github.com/nix-community/tree-sitter-nix/archive/1b69cf1fa92366eefbe6863c184e5d2ece5f187d.tar.gz")
                (sha256 (base32 "04ff8vj3ihznn0yslvigp66r314wp9cwdsc3r76h3kb4w475a9sa")))
              )
        (list "nu"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-nu-358c4f50.tar.gz")
                (uri "https://github.com/nushell/tree-sitter-nu/archive/358c4f509eb97f0148bbd25ad36acc729819b9c1.tar.gz")
                (sha256 (base32 "1fsvwwsmrfbzmqh623cdw5bl7x0j8knz3285s0xyzgps6kfkmibk")))
              )
        (list "ocaml"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-ocaml-9965d208.tar.gz")
                (uri "https://github.com/tree-sitter/tree-sitter-ocaml/archive/9965d208337d88bbf1a38ad0b0fe49e5f5ec9677.tar.gz")
                (sha256 (base32 "1jyfmx7qmfv3vr9xj2ikp6rds04alf9s90lbjdwvbdbj9gpzz4fx")))
              )
        (list "ocaml-interface"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-ocaml-interface-9965d208.tar.gz")
                (uri "https://github.com/tree-sitter/tree-sitter-ocaml/archive/9965d208337d88bbf1a38ad0b0fe49e5f5ec9677.tar.gz")
                (sha256 (base32 "1jyfmx7qmfv3vr9xj2ikp6rds04alf9s90lbjdwvbdbj9gpzz4fx")))
              )
        (list "odin"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-odin-b5f668ef.tar.gz")
                (uri "https://github.com/tree-sitter-grammars/tree-sitter-odin/archive/b5f668ef8918aab13812ce73acd89fe191fb8c5e.tar.gz")
                (sha256 (base32 "0ggb6pjir3sq7v82lzlyhrjb20kpvb0s2c4yfxyr2v8m88hi0766")))
              )
        (list "ohm"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-ohm-80f14f0e.tar.gz")
                (uri "https://github.com/novusnota/tree-sitter-ohm/archive/80f14f0e477ddacc1e137d5ed8e830329e3fb7a3.tar.gz")
                (sha256 (base32 "1w5f43254jwpvq9494gahkppdqg1rpadvfkywwn672k8nvlh96zv")))
              )
        (list "opencl"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-opencl-8e1d24a5.tar.gz")
                (uri "https://github.com/lefp/tree-sitter-opencl/archive/8e1d24a57066b3cd1bb9685bbc1ca9de5c1b78fb.tar.gz")
                (sha256 (base32 "04vg8sxky6g76vpc6n4fd2dg5l9gjjj2qf5jh41nqrl7hxkrd7ci")))
              )
        (list "openscad"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-openscad-5c3ce93d.tar.gz")
                (uri "https://github.com/bollian/tree-sitter-openscad/archive/5c3ce93df0ac1da7197cf6ae125aade26d6b8972.tar.gz")
                (sha256 (base32 "10yzh22yhmqx0r9sk0cv3qx88q66r61p6dlbcannd1xr6h7mpzck")))
              )
        (list "org"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-org-698bb1a3.tar.gz")
                (uri "https://github.com/milisims/tree-sitter-org/archive/698bb1a34331e68f83fc24bdd1b6f97016bb30de.tar.gz")
                (sha256 (base32 "1rxiw6zhdlfw3hrlf60z0hw3fpbqsgxb7af2294izgpwin6hihmw")))
              )
        (list "pascal"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-pascal-2fd40f47.tar.gz")
                (uri "https://github.com/Isopod/tree-sitter-pascal/archive/2fd40f477d3e2794af152618ccfac8d92eb72a66.tar.gz")
                (sha256 (base32 "0yqm256cv78nqrsmzy4mfi6v86kjfkdczypxp5wyzgjkbvnlavbc")))
              )
        (list "passwd"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-passwd-20239395.tar.gz")
                (uri "https://github.com/ath3/tree-sitter-passwd/archive/20239395eacdc2e0923a7e5683ad3605aee7b716.tar.gz")
                (sha256 (base32 "19zmrx617315zp6a3abkw408003g0fmb7sfad88gxckjngjnv6bl")))
              )
        (list "pem"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-pem-be67a433.tar.gz")
                (uri "https://github.com/mtoohey31/tree-sitter-pem/archive/be67a4330a1aa507c7297bc322204f936ec1132c.tar.gz")
                (sha256 (base32 "1nzlfd31561zh781wnx13vn0zk77yjz35phvyhd723g50jq9mian")))
              )
        (list "perl"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-perl-e99bb528.tar.gz")
                (uri "https://github.com/tree-sitter-perl/tree-sitter-perl/archive/e99bb5283805db4cb86c964722d709df21b0ac16.tar.gz")
                (sha256 (base32 "135kp6wm30hzhz8sciydygj0b27frjr8mnkmr6fzqgpcnqc8nin5")))
              )
        (list "pest"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-pest-a8a98a82.tar.gz")
                (uri "https://github.com/pest-parser/tree-sitter-pest/archive/a8a98a824452b1ec4da7f508386a187a2f234b85.tar.gz")
                (sha256 (base32 "04rylb7v0jpdv0mqa1ppwm7y9rs8c1lgjic7bddagjflsi5za0kc")))
              )
        (list "php"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-php-f860e598.tar.gz")
                (uri "https://github.com/tree-sitter/tree-sitter-php/archive/f860e598194f4a71747f91789bf536b393ad4a56.tar.gz")
                (sha256 (base32 "0f1z7d5km9cnh4wb080wqvdzyjfgcax5m3ccm8x3wgbbfijr7p8h")))
              )
        (list "php-only"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-php-only-cf1f4a0f.tar.gz")
                (uri "https://github.com/tree-sitter/tree-sitter-php/archive/cf1f4a0f1c01c705c1d6cf992b104028d5df0b53.tar.gz")
                (sha256 (base32 "1xy4633abmz3d9l52qmgdmj9qhv79ywfaqn4gbd3przl53arvizz")))
              )
        (list "pkl"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-pkl-c03f04a3.tar.gz")
                (uri "https://github.com/apple/tree-sitter-pkl/archive/c03f04a313b712f8ab00a2d862c10b37318699ae.tar.gz")
                (sha256 (base32 "0b5ycnbwbblqx9w256x9s58jdf8hrwim53xbzhyd5yf87qrmhp9c")))
              )
        (list "po"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-po-417cee9a.tar.gz")
                (uri "https://github.com/erasin/tree-sitter-po/archive/417cee9abb2053ed26b19e7de972398f2da9b29e.tar.gz")
                (sha256 (base32 "1vi7b6wkmfzn6xkhqfqs8zavnwjq54bm5y56l3n0l005j1vhzfqy")))
              )
        (list "pod"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-pod-39da8599.tar.gz")
                (uri "https://github.com/tree-sitter-perl/tree-sitter-pod/archive/39da859947b94abdee43e431368e1ae975c0a424.tar.gz")
                (sha256 (base32 "0ff4v8gvp1fgyvg58915vswrbkmrlgk5hdl9x863cacwsbi8i43l")))
              )
        (list "ponylang"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-ponylang-ef66b151.tar.gz")
                (uri "https://github.com/mfelsche/tree-sitter-ponylang/archive/ef66b151bc2604f431b5668fcec4747db4290e11.tar.gz")
                (sha256 (base32 "1za7qh79l5jilghvjsa9my0jd0yh4s5djjbia7jwfl68scz89fzq")))
              )
        (list "powershell"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-powershell-c9316be0.tar.gz")
                (uri "https://github.com/airbus-cert/tree-sitter-powershell/archive/c9316be0faca5d5b9fd3b57350de650755f42dc0.tar.gz")
                (sha256 (base32 "1wi6qarywyxd6q34q1cfg76v55n9mabvzrsz3h7yjbm1zjxmd0xa")))
              )
        (list "prisma"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-prisma-eca2596a.tar.gz")
                (uri "https://github.com/victorhqc/tree-sitter-prisma/archive/eca2596a355b1a9952b4f80f8f9caed300a272b5.tar.gz")
                (sha256 (base32 "1dybg6dh773nlvdz78j1k0aszwrjjamgy5xvmbzyc4kmpw5wd5s5")))
              )
        (list "protobuf"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-protobuf-19c211a0.tar.gz")
                (uri "https://github.com/yusdacra/tree-sitter-protobuf/archive/19c211a01434d9f03efff99f85e19f967591b175.tar.gz")
                (sha256 (base32 "0lr97kn6nv64xl5filxmyjqv0hv4xan0zd547sdihb9shw82izf2")))
              )
        (list "prql"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-prql-09e158cd.tar.gz")
                (uri "https://github.com/PRQL/tree-sitter-prql/archive/09e158cd3650581c0af4c49c2e5b10c4834c8646.tar.gz")
                (sha256 (base32 "1sv943pz7f7f5vfdcwl1w62plb0mii69xdc79d2a6vz2wad3naza")))
              )
        (list "purescript"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-purescript-5ef55926.tar.gz")
                (uri "https://github.com/postsolar/tree-sitter-purescript/archive/5ef5592674ea42de75fc2792972e4ea0b6e3da6c.tar.gz")
                (sha256 (base32 "18bkgbw9cz0kp54sy6hm4n3p3i7dhw7z8sd5781yh9bns5xx7l5m")))
              )
        (list "python"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-python-4bfdd903.tar.gz")
                (uri "https://github.com/tree-sitter/tree-sitter-python/archive/4bfdd9033a2225cc95032ce77066b7aeca9e2efc.tar.gz")
                (sha256 (base32 "1dl0hdr8x9gvjrix44d78w1l1fj6gihjv4i74738l29spvnba5fq")))
              )
        (list "qmljs"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-qmljs-0b2b25bc.tar.gz")
                (uri "https://github.com/yuja/tree-sitter-qmljs/archive/0b2b25bcaa7d4925d5f0dda16f6a99c588a437f1.tar.gz")
                (sha256 (base32 "1wrwy436mil0bz9nz1j372n4yfp5arrfiy77zvbpv2w15h4fs2j3")))
              )
        (list "r"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-r-cc04302e.tar.gz")
                (uri "https://github.com/r-lib/tree-sitter-r/archive/cc04302e1bff76fa02e129f332f44636813b0c3c.tar.gz")
                (sha256 (base32 "076iwwmbcvqdsxlz2yyl1bx92xg3wpggpxk115md97a5x4fajkqm")))
              )
        (list "regex"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-regex-e1cfca3c.tar.gz")
                (uri "https://github.com/tree-sitter/tree-sitter-regex/archive/e1cfca3c79896ff79842f057ea13e529b66af636.tar.gz")
                (sha256 (base32 "09bhlylxbrfam2dyph584wzz3nskljvn1w6ch1djw8ij402z6xk8")))
              )
        (list "rego"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-rego-9ac75e71.tar.gz")
                (uri "https://github.com/FallenAngel97/tree-sitter-rego/archive/9ac75e71b2d791e0aadeef68098319d86a2a14cf.tar.gz")
                (sha256 (base32 "0z7namgh5bm1sr17a4cc3c8ffwx50nfpqjk7ii3n2c8w0vfmw10r")))
              )
        (list "rescript"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-rescript-467dcf99.tar.gz")
                (uri "https://github.com/jaredramirez/tree-sitter-rescript/archive/467dcf99f68c47823d7b378779a6b282d7ef9782.tar.gz")
                (sha256 (base32 "01a4p21fpgd09nd0rlranvf10kv5z60xk1jkxpr3z0gb7qj4kdai")))
              )
        (list "robot"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-robot-322e4cc6.tar.gz")
                (uri "https://github.com/Hubro/tree-sitter-robot/archive/322e4cc65754d2b3fdef4f2f8a71e0762e3d13af.tar.gz")
                (sha256 (base32 "1rdg8xzdn0hha5q5060ryd4r4qdswlbf45zwhd15ynphjlh0gm2w")))
              )
        (list "ron"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-ron-7762d709.tar.gz")
                (uri "https://github.com/zee-editor/tree-sitter-ron/archive/7762d709a0f7c1f9e269d0125a2e8a7a69006146.tar.gz")
                (sha256 (base32 "0h0w2xdysm4pi9lql81vp4cp117m1a0m66lyjgl5wix4md4xzb63")))
              )
        (list "rst"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-rst-25e63288.tar.gz")
                (uri "https://github.com/stsewd/tree-sitter-rst/archive/25e6328872ac3a764ba8b926aea12719741103f1.tar.gz")
                (sha256 (base32 "1x4hc3nx2w2r5s85rmsadkba75agcakh0b13lf75040yljz3cldr")))
              )
        (list "ruby"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-ruby-206c7077.tar.gz")
                (uri "https://github.com/tree-sitter/tree-sitter-ruby/archive/206c7077164372c596ffa8eaadb9435c28941364.tar.gz")
                (sha256 (base32 "1c5xmshn9cr53hy0d1dsh02ly7qwqq7pcyb9akm53sk8m38r69bq")))
              )
        (list "rust"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-rust-9c84af00.tar.gz")
                (uri "https://github.com/tree-sitter/tree-sitter-rust/archive/9c84af007b0f144954adb26b3f336495cbb320a7.tar.gz")
                (sha256 (base32 "0xbx6vs6d7l2mkag8zf1sixssyg8b26f4x580x4fcm2pra3vj2ly")))
              )
        (list "scala"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-scala-7891815f.tar.gz")
                (uri "https://github.com/tree-sitter/tree-sitter-scala/archive/7891815f42dca9ed6aeb464c2edc39d479ab965c.tar.gz")
                (sha256 (base32 "093720zg617s354hayag67bpiqfk18kincicss8pq97qd7hah5pf")))
              )
        (list "scheme"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-scheme-af3af6c9.tar.gz")
                (uri "https://github.com/6cdh/tree-sitter-scheme/archive/af3af6c9356b936f8a515a1e449c32e804c2b1a8.tar.gz")
                (sha256 (base32 "13c7pn7jhajl0wzjfpp5hzkssh9rc049whv8gg6b19c8id3x77dz")))
              )
        (list "scss"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-scss-c478c686.tar.gz")
                (uri "https://github.com/serenadeai/tree-sitter-scss/archive/c478c6868648eff49eb04a4df90d703dc45b312a.tar.gz")
                (sha256 (base32 "18nzc57y21a52hcycql3cwcwq19q1162qbfxw2dyl7vyl1dqn5yj")))
              )
        (list "slint"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-slint-0701312b.tar.gz")
                (uri "https://github.com/slint-ui/tree-sitter-slint/archive/0701312b74b87fe20e61aa662ba41c5815b5d428.tar.gz")
                (sha256 (base32 "1387qxbkj7sfjy5dw5ak7m1gfi51lgpaq5qj7savlgr0yd8is3m2")))
              )
        (list "smali"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-smali-5ae51e15.tar.gz")
                (uri "https://github.com/amaanq/tree-sitter-smali/archive/5ae51e15c4d1ac93cba6127caf3d1f0a072c140c.tar.gz")
                (sha256 (base32 "1bs6g7nqwylmhn5lc7596c240jd35j9qkvq27yihxjbg453isxpj")))
              )
        (list "smithy"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-smithy-8327eb84.tar.gz")
                (uri "https://github.com/indoorvivants/tree-sitter-smithy/archive/8327eb84d55639ffbe08c9dc82da7fff72a1ad07.tar.gz")
                (sha256 (base32 "13r83pmdwnn0vldpggkjm7gbvbfdykam1g1yckk18gxrcdipinxf")))
              )
        (list "sml"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-sml-bd4055d5.tar.gz")
                (uri "https://github.com/Giorbo/tree-sitter-sml/archive/bd4055d5554614520d4a0706b34dc0c317c6b608.tar.gz")
                (sha256 (base32 "0w5qyhm69wg18rwc08k0b2lywh7g4y92f8690ddsxj2cbrab8h4b")))
              )
        (list "solidity"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-solidity-08338dce.tar.gz")
                (uri "https://github.com/JoranHonig/tree-sitter-solidity/archive/08338dcee32603383fcef08f36321900bb7a354b.tar.gz")
                (sha256 (base32 "0pinqps0ziysgjrry2n6q3npfl47il6m3afbl0lfqmiy48565ayk")))
              )
        (list "spicedb"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-spicedb-a4e46456.tar.gz")
                (uri "https://github.com/jzelinskie/tree-sitter-spicedb/archive/a4e4645651f86d6684c15dfa9931b7841dc52a66.tar.gz")
                (sha256 (base32 "0k1p0pbdilg9a93l6nrg79vxa6j8zx9g70aqy24liqc2i255mfrr")))
              )
        (list "sql"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-sql-da2d1eff.tar.gz")
                (uri "https://github.com/DerekStride/tree-sitter-sql/archive/da2d1eff425b146d3c8cab7be8dfa98b11d896dc.tar.gz")
                (sha256 (base32 "11wq4r1dgn15wwfr2f386mpmfpsvkh1yvmgm5s636db7z51w344p")))
              )
        (list "sshclientconfig"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-sshclientconfig-e45c6d5c.tar.gz")
                (uri "https://github.com/metio/tree-sitter-ssh-client-config/archive/e45c6d5c71657344d4ecaf87dafae7736f776c57.tar.gz")
                (sha256 (base32 "16xva0q6vszb1ivld9rl7c9vzv49gsfnyxmldbcjm34214lqv3a4")))
              )
        (list "strace"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-strace-2b18fdf9.tar.gz")
                (uri "https://github.com/sigmaSd/tree-sitter-strace/archive/2b18fdf9a01e7ec292cc6006724942c81beb7fd5.tar.gz")
                (sha256 (base32 "14a28pgjyliyaqgy5nssm287rkkxvskjh1f7c5n3a9wykn1x3dvs")))
              )
        (list "supercollider"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-supercollider-3b35bd0f.tar.gz")
                (uri "https://github.com/madskjeldgaard/tree-sitter-supercollider/archive/3b35bd0fded4423c8fb30e9585c7bacbcd0e8095.tar.gz")
                (sha256 (base32 "1975gra80d527mvs5cc77596qnv2xa0m8zlp659xv482qx1bblfl")))
              )
        (list "svelte"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-svelte-be7f2e7d.tar.gz")
                (uri "https://github.com/Himujjal/tree-sitter-svelte/archive/be7f2e7db1fc19f0852265ec60923fc058380739.tar.gz")
                (sha256 (base32 "1b8pikdnbkqik7k5wljhcs6900pd1f27prfp5q51ys3lhzhppmzv")))
              )
        (list "sway"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-sway-e491a005.tar.gz")
                (uri "https://github.com/FuelLabs/tree-sitter-sway/archive/e491a005ee1d310f4c138bf215afd44cfebf959c.tar.gz")
                (sha256 (base32 "17i637nh9d1y5a0im7rg67xn4hpfxkimjxsb81c83vafjnbhk3q3")))
              )
        (list "swift"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-swift-57c1c6d6.tar.gz")
                (uri "https://github.com/alex-pinkus/tree-sitter-swift/archive/57c1c6d6ffa1c44b330182d41717e6fe37430704.tar.gz")
                (sha256 (base32 "1gk6blym4kvnndkag8ild0qxl8dpkdcrrxf1zpjxabnnmzzy83gb")))
              )
        (list "t32"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-t32-6da5e3cb.tar.gz")
                (uri "https://gitlab.com/xasc/tree-sitter-t32/-/archive/6da5e3cbabd376b566d04282005e52ffe67ef74a/tree-sitter-t32-6da5e3cb.tar.gz")
                (sha256 (base32 "12bf05xxmg8x9ricd6y7hlw0gnigiv7mfsrrlp8w2in2jnxy8f9l")))
              )
        (list "tablegen"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-tablegen-568dd8a9.tar.gz")
                (uri "https://github.com/Flakebi/tree-sitter-tablegen/archive/568dd8a937347175fd58db83d4c4cdaeb6069bd2.tar.gz")
                (sha256 (base32 "186dwzm8fji3c21dzzhwr4pg4ifm1crqqr9aw22dl6z3ij15drc3")))
              )
        (list "tact"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-tact-ec57ab29.tar.gz")
                (uri "https://github.com/tact-lang/tree-sitter-tact/archive/ec57ab29c86d632639726631fb2bb178d23e1c91.tar.gz")
                (sha256 (base32 "144ys7ibc631fin125b2xv1pczhxf8759b87z1kv7a7q4y5x3gak")))
              )
        (list "task"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-task-f2cb435c.tar.gz")
                (uri "https://github.com/alexanderbrevig/tree-sitter-task/archive/f2cb435c5dbf3ee19493e224485d977cb2d36d8b.tar.gz")
                (sha256 (base32 "05r32dxzqvgs6wwnxw0bm9asn2jjsqdkfjx72ckxpbard4i1q60y")))
              )
        (list "tcl"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-tcl-56ad1fa6.tar.gz")
                (uri "https://github.com/tree-sitter-grammars/tree-sitter-tcl/archive/56ad1fa6a34ba800e5495d1025a9b0fda338d5b8.tar.gz")
                (sha256 (base32 "1mn92mpckgf7xmcm9d0jfl0j5x7adb2h4ybifhz0h7f20pc5v4b9")))
              )
        (list "templ"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-templ-db662414.tar.gz")
                (uri "https://github.com/vrischmann/tree-sitter-templ/archive/db662414ccd6f7c78b1e834e7abe11c224b04759.tar.gz")
                (sha256 (base32 "1bj6nrm2cyrbns6qz055ryaxczkf9zjj9s2fwkqp8rs7xl8pknbd")))
              )
        (list "todotxt"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-todotxt-3937c5cd.tar.gz")
                (uri "https://github.com/arnarg/tree-sitter-todotxt/archive/3937c5cd105ec4127448651a21aef45f52d19609.tar.gz")
                (sha256 (base32 "108gsjivwy6bji9v6mziqcxph2a8yxm5yqhpmqvkywcxvbmnnf0k")))
              )
        (list "toml"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-toml-7cff70bb.tar.gz")
                (uri "https://github.com/ikatyang/tree-sitter-toml/archive/7cff70bbcbbc62001b465603ca1ea88edd668704.tar.gz")
                (sha256 (base32 "151jd5dpqdx3zv8rmxmivx1icv0qi0srqr7xmdyqch9h29kn1wwk")))
              )
        (list "tsq"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-tsq-48b5e9f8.tar.gz")
                (uri "https://github.com/the-mikedavis/tree-sitter-tsq/archive/48b5e9f82ae0a4727201626f33a17f69f8e0ff86.tar.gz")
                (sha256 (base32 "19v6j54wg8f56g6bc71nb07r16nmnv449nmr3n2whg3bc0v2s195")))
              )
        (list "tsx"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-tsx-b1bf4825.tar.gz")
                (uri "https://github.com/tree-sitter/tree-sitter-typescript/archive/b1bf4825d9eaa0f3bdeb1e52f099533328acfbdf.tar.gz")
                (sha256 (base32 "0pjvb40rpkxikrzr3wgd9jw9471nb073cbppk9h0yk5w9xxmq0f2")))
              )
        (list "twig"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-twig-807b293f.tar.gz")
                (uri "https://github.com/gbprod/tree-sitter-twig/archive/807b293fec3fead64f54c64fdf6fb05516c032b9.tar.gz")
                (sha256 (base32 "0an09xwrgip3m2dgr1rvb8h5z0a2xq1s93vsdl24yl6mc34xnl55")))
              )
        (list "typescript"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-typescript-b1bf4825.tar.gz")
                (uri "https://github.com/tree-sitter/tree-sitter-typescript/archive/b1bf4825d9eaa0f3bdeb1e52f099533328acfbdf.tar.gz")
                (sha256 (base32 "0pjvb40rpkxikrzr3wgd9jw9471nb073cbppk9h0yk5w9xxmq0f2")))
              )
        (list "typst"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-typst-13863ddc.tar.gz")
                (uri "https://github.com/uben0/tree-sitter-typst/archive/13863ddcbaa7b68ee6221cea2e3143415e64aea4.tar.gz")
                (sha256 (base32 "1vgh76cvd4ax60rc2zprn6wlx7hyrrxbz9dfaybi0qmf425vw88l")))
              )
        (list "ungrammar"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-ungrammar-a7e10462.tar.gz")
                (uri "https://github.com/Philipp-M/tree-sitter-ungrammar/archive/a7e104629cff5a8b7367187610631e8f5eb7c6ea.tar.gz")
                (sha256 (base32 "1qm2cm1dc42cag433c2ndvwb75lkgk8il9hkz4k3qzzs4dhnsmdx")))
              )
        (list "unison"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-unison-1f505e24.tar.gz")
                (uri "https://github.com/kylegoetz/tree-sitter-unison/archive/1f505e2447fa876a87aee47ff3d70b9e141c744f.tar.gz")
                (sha256 (base32 "0wzs2g8ybl1x9y6qxll4dh51351w53971lj12ly7vf9pxzvz0j8q")))
              )
        (list "uxntal"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-uxntal-d6840606.tar.gz")
                (uri "https://github.com/Jummit/tree-sitter-uxntal/archive/d68406066648cd6db4c6a2f11ec305af02079884.tar.gz")
                (sha256 (base32 "0dgqwmh9jj3iknlcc1z6x7hr077bx3r3mz4bx3jnlx36aak3f7fn")))
              )
        (list "v"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-v-e14fdf6e.tar.gz")
                (uri "https://github.com/v-analyzer/v-analyzer/archive/e14fdf6e661b10edccc744102e4ccf0b187aa8ad.tar.gz")
                (sha256 (base32 "19is1qpj35gid1jkas4hks6yv1x1q91p26dn5xcybds2fmzcivv3")))
              )
        (list "vala"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-vala-c9eea93b.tar.gz")
                (uri "https://github.com/vala-lang/tree-sitter-vala/archive/c9eea93ba2ec4ec1485392db11945819779745b3.tar.gz")
                (sha256 (base32 "1qrbsxc1ixlvjgcmhzm7jkds1nzx2ahb2mbv7vgllsz3kvxddir4")))
              )
        (list "verilog"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-verilog-4457145e.tar.gz")
                (uri "https://github.com/tree-sitter/tree-sitter-verilog/archive/4457145e795b363f072463e697dfe2f6973c9a52.tar.gz")
                (sha256 (base32 "1x2hg2yjzkxqns64620b015fvvfgf1qf0cvxs365wj3lhw97dhpl")))
              )
        (list "vhdl"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-vhdl-c57313ad.tar.gz")
                (uri "https://github.com/teburd/tree-sitter-vhdl/archive/c57313adee2231100db0a7880033f6865deeadb2.tar.gz")
                (sha256 (base32 "0y7xh1v9529arc2s7jmpbxjbn6wphp9by4si6s3b6dg7rx6p17kl")))
              )
        (list "vhs"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-vhs-9534865e.tar.gz")
                (uri "https://github.com/charmbracelet/tree-sitter-vhs/archive/9534865e614c95eb9418e5e73f061c32fa4d9540.tar.gz")
                (sha256 (base32 "0q4xpvmbwj1apdyknzq4g0xi29956gd07gs7lcvqg6g8wp8bsfrq")))
              )
        (list "vue"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-vue-91fe2754.tar.gz")
                (uri "https://github.com/ikatyang/tree-sitter-vue/archive/91fe2754796cd8fba5f229505a23fa08f3546c06.tar.gz")
                (sha256 (base32 "1485spmi27ka0599azn5sikdkqh58fhmmliyp3vsh5lxcvwxq7n7")))
              )
        (list "wast"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-wast-2ca28a9f.tar.gz")
                (uri "https://github.com/wasm-lsp/tree-sitter-wasm/archive/2ca28a9f9d709847bf7a3de0942a84e912f59088.tar.gz")
                (sha256 (base32 "1sqsj5nbj15bfnhhpyjqbxhg1jmy7jb4jyhg555cjpn7mc4f1x4n")))
              )
        (list "wat"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-wat-2ca28a9f.tar.gz")
                (uri "https://github.com/wasm-lsp/tree-sitter-wasm/archive/2ca28a9f9d709847bf7a3de0942a84e912f59088.tar.gz")
                (sha256 (base32 "1sqsj5nbj15bfnhhpyjqbxhg1jmy7jb4jyhg555cjpn7mc4f1x4n")))
              )
        (list "wgsl"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-wgsl-272e89ef.tar.gz")
                (uri "https://github.com/szebniok/tree-sitter-wgsl/archive/272e89ef2aeac74178edb9db4a83c1ffef80a463.tar.gz")
                (sha256 (base32 "1hq13baa68mcsi8id09z8p1r6bzhg27ixn71x7s5z34cn0qhg0lx")))
              )
        (list "wit"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-wit-c917790a.tar.gz")
                (uri "https://github.com/hh9527/tree-sitter-wit/archive/c917790ab9aec50c5fd664cbfad8dd45110cfff3.tar.gz")
                (sha256 (base32 "15dmw8py5d2ky0fx8yfbynpfv7fvl41lipy9jiyg0shpi9w3d98r")))
              )
        (list "wren"
              (origin
                (method git-fetch)
                (file-name "tree-sitter-wren-6748694b")
                (uri (git-reference (url "https://git.sr.ht/~jummit/tree-sitter-wren") (commit "6748694be32f11e7ec6b5faeb1b48ca6156d4e06")))
                (sha256 (base32 "1hlslg8vvxb5k74p0178yprlyblv2kl2n901w0bfxzhpir0kqk89")))
              )
        (list "xit"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-xit-7d790245.tar.gz")
                (uri "https://github.com/synaptiko/tree-sitter-xit/archive/7d7902456061bc2ad21c64c44054f67b5515734c.tar.gz")
                (sha256 (base32 "12m8mskjl16fhvznphk40ay2drhifh8nihlqkxyfz66l9cf77gi7")))
              )
        (list "xml"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-xml-48a7c2b6.tar.gz")
                (uri "https://github.com/RenjiSann/tree-sitter-xml/archive/48a7c2b6fb9d515577e115e6788937e837815651.tar.gz")
                (sha256 (base32 "0k6pipnivirkl9s1jv467fijq7djzzkrpr9aqbhyrmixm2ds0bn3")))
              )
        (list "xtc"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-xtc-7bc11b73.tar.gz")
                (uri "https://github.com/Alexis-Lapierre/tree-sitter-xtc/archive/7bc11b736250c45e25cfb0215db2f8393779957e.tar.gz")
                (sha256 (base32 "1x93wncj9a5ybbjx0wdvha0ifwmjlx5d6c69lhhzf0gq9gzxbc5y")))
              )
        (list "yaml"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-yaml-0e36bed1.tar.gz")
                (uri "https://github.com/ikatyang/tree-sitter-yaml/archive/0e36bed171768908f331ff7dff9d956bae016efb.tar.gz")
                (sha256 (base32 "123pr303n673mxzhx2mjcz5rhrs3vib5rfvg80ivn53ap0m0bdj6")))
              )
        (list "yuck"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-yuck-e3d91a3c.tar.gz")
                (uri "https://github.com/Philipp-M/tree-sitter-yuck/archive/e3d91a3c65decdea467adebe4127b8366fa47919.tar.gz")
                (sha256 (base32 "02xci9byxgnhis7jq5ssgxg0r7a9lxxqq12rnc1pd5fs2ww24fc1")))
              )
        (list "zig"
              (origin
                (method url-fetch)
                (file-name "tree-sitter-zig-0d08703e.tar.gz")
                (uri "https://github.com/maxxnino/tree-sitter-zig/archive/0d08703e4c3f426ec61695d7617415fff97029bd.tar.gz")
                (sha256 (base32 "03v01mnijq0bfr7sfwsqdgkav2qkjrp8i9j2z3c04606ngg2qq7k")))
              )))
