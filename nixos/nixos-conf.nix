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
  boot.kernelPackages = pkgs.linuxPackages_4_1;

  hardware.pulseaudio.enable = true;

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

  security.sudo.wheelNeedsPassword = false;

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

  # XServer configurations
  services.xserver = {
    # main configurations
    enable = true;
    videoDrivers = [ "intel" ]; # TODO: hardware dependent -> move!
    layout = "de";
    xkbVariant = "nodeadkeys";
    synaptics.enable = true;

    # configuration for display-manager
    displayManager.desktopManagerHandlesLidAndPower = false;

    # configuration for desktop-manager (deactive desktop-manager)
    desktopManager.default = "";
    desktopManager.xterm.enable = false;

    # configuration for window-manager (use xmonad)
    windowManager = {
      default = "xmonad";
      xmonad = {
        enable = true;
	enableContribAndExtras = true;
	# extraPackages -> use this for my lib
      };
    };
  };

  # enable atd service
  services.atd.enable = true;

  # Define some fonts.
  fonts = {
    fontconfig.antialias = true;
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      dejavu_fonts
      inconsolata
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

  # Define a test-user
  users.extraUsers.test = {
    isNormalUser = true;
    home = "/home/test";
    description = "A user for testing";
  };

  # NAT networking rules for containers
  networking.nat = {
    enable = true;
    internalInterfaces = ["ve-+"];
    externalInterface = "wlp4s0";
  };

  # containers
#  containers.hydra = {
#  };

}
