# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
      # set hostname for work, home, ...; change this for all my computers
      /etc/nixos/rise.nix
    ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  networking.wireless.enable = true;  # Enables wireless.

  # Select internationalisation properties.
  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "de-latin1-nodeadkeys";
    defaultLocale = "en_US.UTF-8";
  };

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    wget
  ];

  # List services that you want to enable:

  # Enable zsh
  programs.zsh.enable = true;

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Enable NTPD daemon.
  time.timeZone = "Europe/Vienna";
  services.openntpd.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "de";
  services.xserver.xkbVariant = "nodeadkeys";
  services.xserver.synaptics.enable = true;

  services.xserver.displayManager.desktopManagerHandlesLidAndPower = false;

  # Disable xterm as desktopManager.
  services.xserver.desktopManager.default = "";
  services.xserver.desktopManager.xterm.enable = false;

  # Use xmonad as my windowManager.
  services.xserver.windowManager.default = "xmonad";
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;

  # Define some fonts.
  fonts = {
    fontconfig.antialias = true;
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      dejavu_fonts
    ];
  };

  # Define my user.
  users.extraUsers.odi = {
    isNormalUser = true;
    home = "/home/odi";
    description = "Oliver Dunkl";
    extraGroups = [ "wheel" ];
    shell = "${pkgs.zsh}/bin/zsh";
  };

}
