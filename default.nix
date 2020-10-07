with (import ./nix/pinned-pkgs.nix);
let
  package = emacsUnstable;

  epkgOverrides = eself: esuper: {
    # based on the configuration of vterm
    liberime-config = eself.melpaBuild rec {
      pname = "liberime-config";
      version = "0";

      src = pkgs.fetchFromGitHub {
        owner = "merrickluo";
        repo = "liberime";
        rev = "282d37c44b9bda33bba2b6a23bd4b2ab5807ed15";
        # date = 2020-01-12T14:39:55+08:00;
        sha256 = "0s87pq3cp4wgvh14hjqcrr059c5sxrmb631x7008j1cy5lyn09z0";
      };

      recipe = pkgs.writeText "recipe" ''
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
  mozc
  pdf-tools
  elisp-ffi
  exwm
  # beancount
  liberime-config
])
