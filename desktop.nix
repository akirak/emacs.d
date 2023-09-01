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

  xdg.menu.applications.emacsclient-frame = {
    Name = "New Emacs Frame (emacsclient)";
    Icon = "${pkgs.emacs}/share/icons/hicolor/scalable/apps/emacs.svg";
    Exec = "nix run --no-update-lock-file \"${toString ./.}#emacsclient\" -- -c";
    StartupWMClass = "Emacs";
  };

  xdg.menu.applications.emacs-everywhere = {
    Name = "Edit textarea using Emacs Everywhere";
    Icon = "${pkgs.emacs}/share/icons/hicolor/scalable/apps/emacs.svg";
    Exec = "nix run --no-update-lock-file \"${toString ./.}#emacsclient\" -- --eval '(emacs-everywhere)'";
    StartupWMClass = "Emacs";
  };

}
