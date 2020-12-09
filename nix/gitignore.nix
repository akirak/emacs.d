let
  pkgs = import <nixpkgs> {};
  sources = import ./sources.nix;
in
(
  import sources."gitignore.nix" {
    inherit (pkgs) lib;
  }
).gitignoreSource
