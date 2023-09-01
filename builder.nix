let
  pkgs = import <nixpkgs> { };
  emacs = pkgs.emacs;
  readmeBuilder = (pkgs.emacsPackagesNgGen emacs).emacsWithPackages
    (epkgs: (with epkgs.melpaPackages; [ org-make-toc ]));
in pkgs.mkShell { buildInputs = [ readmeBuilder ]; }
