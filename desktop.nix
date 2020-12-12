let
  pkgs = import <nixpkgs> {};
in
{
  name = "emacs-akirak-2018";

  xdg.menu.applications.emacs-akirak-2018 = {
    Name = "Emacs (akirak/emacs.d)";
    Icon = "${pkgs.emacs}/share/icons/hicolor/scalable/apps/emacs.svg";
    Exec = "nix run --no-update-lock-file ${toString ./.}";
    StartupWMClass = "Emacs";
  };
}
