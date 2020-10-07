let
  pkgs = import ./pinned-pkgs.nix;
  sources = import ./sources.nix;
in
(
  import sources."gitignore.nix" {
    inherit (pkgs) lib;
  }
).gitignoreSource
