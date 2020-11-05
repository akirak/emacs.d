with (import ./nix/pinned-pkgs.nix);
let
  package = emacsUnstable;

  epkgOverrides = eself: esuper: {
    # Mozc has been actively developed recently.
    #
    # Unfortunately, this Nix build seem to be broken. I will wait for
    # other mozc packages to be updated:
    # https://github.com/NixOS/nixpkgs/blob/master/pkgs/tools/inputmethods/ibus-engines/ibus-mozc/default.nix
    mozc =
      let
        japanese_usage_dictionary = fetchFromGitHub {
          owner  = "hiroyuki-komatsu";
          repo   = "japanese-usage-dictionary";
          rev    = "e5b3425575734c323e1d947009dd74709437b684";
          sha256 = "0pyrpz9c8nxccwpgyr36w314mi8h132cis8ijvlqmmhqxwsi30hm";
        };
        python = python3.withPackages (pp: [pp.six ]);
        gyp = python3Packages.gyp;
      in
        clangStdenv.mkDerivation rec {
          inherit (esuper.mozc) name version meta src;

          nativeBuildInputs = [
            which
            ninja
            python
            gyp
            pkgconfig
          ];
          buildInputs = [
            (protobuf.overrideDerivation (oldAttrs: { stdenv = clangStdenv; }))
            xorg.libxcb
            gtk2
            zinnia
          ];

          patches = [
            # https://github.com/google/mozc/pull/444 - fix for gcc8 STL
            # (fetchpatch {
            #   url = "https://github.com/google/mozc/commit/82d38f929882a9c62289b179c6fe41efed249987.patch";
            #   sha256 = "07cja1b7qfsd3i76nscf1zwiav74h7d6h2g9g2w4bs3h1mc9jwla";
            # })
            # Support dates after 2019
            # (fetchpatch {
            #   url = "https://salsa.debian.org/debian/mozc/-/raw/master/debian/patches/add_support_new_japanese_era.patch";
            #   sha256 = "1dsiiglrmm8i8shn2hv0j2b8pv6miysjrimj4569h606j4lwmcw2";
            # })
          ];

          postUnpack = ''
              rmdir $sourceRoot/src/third_party/japanese_usage_dictionary/
              ln -s ${japanese_usage_dictionary} $sourceRoot/src/third_party/japanese_usage_dictionary
            '';

          configurePhase = ''
              export GYP_DEFINES="document_dir=$out/share/doc/mozc use_libzinnia=1 use_libprotobuf=1"
              cd src && python build_mozc.py gyp --gypdir=${gyp}/bin --server_dir=$out/lib/mozc --noqt
            '';

          buildPhase = ''
              PYTHONPATH="$PWD:$PYTHONPATH" python build_mozc.py build -c Release \
                server/server.gyp:mozc_server \
                unix/emacs/emacs.gyp:mozc_emacs_helper
              '';

          installPhase = ''
              install -d        $out/share/licenses/mozc
              head -n 29 server/mozc_server.cc > $out/share/licenses/mozc/LICENSE
              install -m 644    data/installer/*.html     $out/share/licenses/mozc/
              install -d $out/lib/mozc
              install -D -m 755 out_linux/Release/mozc_server $out/lib/mozc/mozc_server
              install -d $out/bin
              install    -m 755 out_linux/Release/mozc_emacs_helper $out/bin/mozc_emacs_helper
              install -d        $out/share/doc/mozc
              install -m 644    data/installer/*.html         $out/share/doc/mozc/
              install -d        $out/share/emacs/site-lisp/elpa/mozc
              elisp=$out/share/emacs/site-lisp/elpa/mozc/mozc.el
              install -m 644    unix/emacs/mozc.el            $elisp
              sed --in-place s/\"mozc_emacs_helper\"/\"$(echo $out/bin/ | sed s/\\//\\\\\\//g)mozc_emacs_helper\"/ $elisp
            '';
        };

    beancount = eself.melpaBuild {
      pname = "beancount";
      version = "2.2.3";

      src = fetchFromBitbucket {
        owner = "blais";
        repo = "beancount";
        rev = "2.2.3";
        sha256 = "0k53kjs81kp12y7w8j54kbp5saxrrvpkhgydizylnnjbfxj9zl4l";
      };

      recipe = writeText "recipe" ''
(beancount
  :fetcher bitbucket
  :repo "blais/beancount"
  :files ("editors/emacs/*.el" (:exclude "*-test.el")))
'';
    };

    # based on the configuration of vterm
    liberime-config = eself.melpaBuild rec {
      pname = "liberime-config";
      version = "0";

      src = fetchFromGitHub {
        owner = "merrickluo";
        repo = "liberime";
        rev = "282d37c44b9bda33bba2b6a23bd4b2ab5807ed15";
        # date = 2020-01-12T14:39:55+08:00;
        sha256 = "0s87pq3cp4wgvh14hjqcrr059c5sxrmb631x7008j1cy5lyn09z0";
      };

      recipe = writeText "recipe" ''
          (liberime-config :fetcher github :repo "merrickluo/liberime")
        '';

      buildInputs = [ package cmake librime ];

      # we need the proper out directory to exist, so we do this in the
      # postInstall instead of postBuild
      postInstall = ''
            cd $src
            cp -r -t $out src CMakeLists.txt Makefile
            cd $out
            make
            cd share/emacs/site-lisp/elpa/liberime-config-**
            mkdir build
            install -m 444 -v -t build $out/build/liberime.so
        '';
    };
  };

  emacsPackages = (emacsPackagesFor package).overrideScope' (
    eself: esuper: (esuper // (epkgOverrides eself esuper))
  );
in
emacsPackages.emacsWithPackages (epkgs: with epkgs; [
  melpaStablePackages.emacsql-sqlite
  vterm
  # mozc
  pdf-tools
  elisp-ffi
  exwm
  # beancount
  liberime-config
  #
  # These package have large Git repositories, so I will install them
  # using Nix to prevent cloning the entire history.
  ansible
  language-detection
  org
  # org-roam
  dimmer
  tide
])
