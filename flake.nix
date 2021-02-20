{
  description = "My Emacs configuration";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.flake-compat.url = "github:edolstra/flake-compat";
  inputs.flake-compat.flake = false;
  inputs.emacs-overlay.url = "github:nix-community/emacs-overlay";
  inputs.epub2json.url = "github:akirak/epub2json";
  inputs.pdftotext.url = "github:akirak/haskell-pdftotext";

  outputs = { emacs-overlay, nixpkgs, flake-utils, flake-compat, ... }@inputs:
    (flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = (import nixpkgs {
          inherit system;
          overlays = [ emacs-overlay.overlay ];
        });
        # pkgs = nixpkgs.legacyPackages.${system};
        emacsPackage = pkgs.emacsUnstable;
        epkgOverrides = eself: esuper: with pkgs; {

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

            buildInputs = [ emacsPackage cmake librime ];

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

        # Executables required inside Emacs
        tools = pkgs.symlinkJoin {
          name = "tools-for-emacs";
          paths = with pkgs; [
            # defaultPackage provides an entire Haskell package, so
            # you have to specify individual packages such as
            # epub2json, pdftotext, etc.
            inputs.epub2json.packages.${system}.epub2json
            inputs.pdftotext.packages.${system}.pdftotext

            gitAndTools.delta

            ripgrep
            fd

            # find
            mlocate

            # Use dex to use counsel-linux-apps on NixOS
            dex

            # gif-screencast
            scrot
            imagemagick
            gifsicle
            # gif-progress

            # file converters
            pngquant
            # for document conversion
            unoconv
            pandoc

            # needed for helm-dash (really?)
            sqlite

            # ispell
            ispell
            hunspell
            hunspellDicts.en-us
            hunspellDicts.en-gb-ise
          ];
        };

      in
        rec {
          packages.emacs = ((pkgs.emacsPackagesFor emacsPackage).overrideScope' (
            eself: esuper: (esuper // (epkgOverrides eself esuper))
          )).emacsWithPackages (epkgs: with epkgs; [
            melpaStablePackages.emacsql-sqlite
            vterm
            # mozc
            pdf-tools
            org-pdftools
            elisp-ffi
            exwm
            # beancount
            liberime-config
            elpaPackages.project
            #
            # These package have large Git repositories, so I will install them
            # using Nix to prevent cloning the entire history.
            ansible
            language-detection
            org
            # org-roam
            dimmer
          ]);
          packages.emacsTools = tools;
          packages.emacsWithTools = pkgs.runCommandNoCC "emacs-with-tools" {
            preferLocalBuild = true;
            buildInputs = [
              pkgs.makeWrapper
            ];
            propagatedBuildInputs = [
              tools
            ];
          }
          ''
            mkdir -p $out/bin
            ln -s -t $out/bin ${packages.emacs}/bin/emacsclient
            makeWrapper ${packages.emacs}/bin/emacs $out/bin/emacs \
              --prefix PATH : ${tools}/bin
          '';

          defaultPackage = packages.emacsWithTools;

          defaultApp = flake-utils.lib.mkApp {
            name = "emacs";
            drv = defaultPackage;
          };
        }
    )) // { inherit flake-compat; };
}
