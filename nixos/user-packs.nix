# from: https://gitorious.org/goibhnix/configurations/source/27eda06365737d69592f4207cb099b63a745c07e:package-groups.nix
# https://nixos.org/wiki/FAQ#How_can_I_manage_software_with_nix-env_like_with_configuration.nix.3F
# first i removed the symlink ~/.nix-defexpr/channels_root (/nix/var/nix/profiles/per-user/root/channels)

# install/update all with:
# nix-env -f ~/conf/packs.nix -iA all
# nix-env -f ~/conf/packs.nix -uA all
# install/update one group e.g. emacsAll
# nix-env -f ~/conf/packs.nix -iA emacsAll
# nix-env -f ~/conf/packs.nix -uA emacsAll

let
  pkgs    = import <nixpkgs> {};
  hpkgs   = pkgs.haskellPackages;
  epkgsNg = pkgs.emacs24PackagesNg;
  epkgs   = pkgs.emacs24Packages;
  ppkgs   = pkgs.python27Packages;
in

rec {
  # all version control systems which I am using
  vcs = [ pkgs.gitAndTools.gitFull ];

  # all emacs packages from Ng
  emacsPackagesNg = with epkgsNg; [
    haskell-mode helm helm-swoop ace-jump-mode magit
    projectile helm-projectile
  ];

  # all other emacs packages
  emacsPackages = with epkgs; [
    jabber bbdb3
  ];

  # emacs editor and all packages
  emacsAll = [ pkgs.emacs ] ++ emacsPackagesNg ++ emacsPackages;

  # all browsers with I am using
  # chromium = pkgs.chromium.override { enablePepperFlash = true; };
  browsers = [ pkgs.chromium pkgs.firefox ];

  # misc stuff
  misc = with pkgs; [
    acpi bc cloc jq youtube-dl xosd unzip zip xpdf gv
    lsof notmuch unetbootin nix-repl sqlite
    cryptsetup libreoffice jmtpfs usbutils
  ];

  unfreePackages = with pkgs; [ skype ];

  # haskell libraries and tools
  haskellPackages = with hpkgs; [
    hlint ghc xmobar cabal-install
  ];

  haskellTools = with pkgs; [
    cabal2nix dzen2
  ];

  # tools for image manimulation and viewing
  imageTools = with pkgs; [
    imagemagick gimp feh exiftags
  ];

  mediaTools = with pkgs; [
    mplayer
  ];

  # tools for security
  securityTools = with pkgs; [
    gnupg gnutls
  ];

  emailTools = with pkgs; [
    procmail fetchmail
  ];

  pythonPackages = with pkgs; [
    python
  ];

  # tools for X
  xTools = with pkgs; [
    xlibs.xev xlibs.xmodmap evemu evtest xlockmore
  ];

  # collection of all defined groups
  all = vcs ++ emacsAll ++ browsers ++ misc ++
        haskellPackages ++ haskellTools ++
	imageTools ++ mediaTools ++ securityTools ++
	emailTools;
}
   
