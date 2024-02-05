{ config, pkgs, ... }:

let
  unstable = import <unstable> { config = { allowUnfree = true; }; };
  monitorsXmlContent = builtins.readFile /home/kevin/.config/monitors.xml;
  monitorsConfig = pkgs.writeText "monitors.xml" monitorsXmlContent;
in
{
  imports =
    [
      # Include results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Bootloader.
  boot.loader = {
    # Disable systemd-boot.
    systemd-boot.enable = false;

    # Wait indefinitely.
    timeout = null;

    # Let's use grub please.
    grub = {
      enable = true;
      efiSupport = true;
      efiInstallAsRemovable = false;

      device = "nodev";
      useOSProber = true;
      gfxmodeEfi = "800x600,auto";
    };
  };

  # Turn on support for flakes.
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Networking settings.
  networking = {
    # Set your hostname.
    hostName = "nixos";

    # Don't let network manager touch /etc/resolv.conf.
    networkmanager = {
      enable = true;
      dns = "none";
    };

    # Forward dns to dnsmasq.
    nameservers = [ "127.0.0.1" ];
  };

  # Login settings.
  services.logind = {
    extraConfig = ''
    KillUserProcesses=no
    IdleAction=ignore
    IdleActionSec=0
    HandlePowerKey=poweroff
    HandlePowerKeyLongPress=ignore
    HandleRebootKey=ignore
    HandleRebootKeyLongPress=ignore
    HandleSuspendKey=ignore
    HandleSuspendKeyLongPress=ignore
    HandleHibernateKey=ignore
    HandleHibernateKeyLongPress=ignore
    HandleLidSwitch=ignore
    HandleLidSwitchExternalPower=ignore
    HandleLidSwitchDocked=ignore
    '';
  };

  # Configure dnsmasq.
  services.dnsmasq = {
    enable = true;
    settings = {
      server = [
        # Use Google's dns servers.
        "8.8.8.8"
        "8.8.4.4"

        # Exceptions for Strake domains.
        "/dev.strake.cloud/172.27.0.2"
        "/internal.strake.cloud/172.27.0.2"
      ];
    };
  };

  # Set your time zone.
  time.timeZone = "America/Denver";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Enable OpenGL.
  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
  };

  # Enable nvidia drivers.
  hardware.nvidia = {
    # Modesetting is needed most of the time.
    modesetting.enable = true;

    # Enable power management (do not disable this unless you have a reason to).
    powerManagement.enable = true;

    # User proprietary Nvidia drivers.
    open = false;

    # Enable the Nvidia settings menu.
    nvidiaSettings = true;

    # Select the stable driver.
    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };

  # Copy monitors file for gdm.
  systemd.tmpfiles.rules = [
    "L+ /run/gdm/.config/monitors.xml - - - - ${monitorsConfig}"
  ];

  # Configure xserver.
  services.xserver = {
    enable = true;
    autorun = true;
    layout = "us,no";
    xkbModel = "pc104";
    xkbOptions = "altwin:swap_alt_win,caps:none";
    videoDrivers = [ "nvidia" ];

    # Adjust monitor layout, force 144hz, and force gsync.
    screenSection = ''
    Option "metamodes" "DP-4: 3840x2160_144 +0+0 {AllowGSYNCCompatible=On}, DP-2: 3840x2160_144 +3840+0 {AllowGSYNCCompatible=On}"
    '';

    # Use gdm.
    displayManager = {
      gdm = {
        enable = true;
        wayland = false;
      };
    };

    # Use xmonad.
    windowManager = {
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
    };
  };

  # Disable printing.
  services.printing.enable = false;

  # Remove the sudo timeout.
  security.sudo = {
    enable = true;
    extraConfig = ''
    Defaults timestamp_timeout=-1
    '';
  };

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    # media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.kevin = {
    isNormalUser = true;
    description = "Kevin Depue";
    extraGroups = [ "networkmanager" "wheel" ];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    alacritty
    bash-completion
    bottom
    cmatrix
    curl
    dig
    direnv
    discord
    du-dust
    emacs
    eza
    fd
    firefox
    flameshot
    fzf
    git
    git-lfs
    gnumake
    go
    go-mockery
    goose
    gopls
    gparted
    imagemagick
    j4-dmenu-desktop
    jq
    mpv
    gnome.nautilus
    killall
    neofetch
    nitrogen
    nodejs_18
    numlockx
    openvpn
    pdftk
    playerctl
    procs
    pulseaudio
    ripgrep
    simp1e-cursors
    slack
    tmux
    tokei
    tree
    unstable._1password
    unstable._1password-gui
    unstable.bitwig-studio
    unstable.expressvpn
    unstable.jetbrains.datagrip
    (unstable.jetbrains.plugins.addPlugins unstable.jetbrains.clion [ "github-copilot" ])
    (unstable.jetbrains.plugins.addPlugins unstable.jetbrains.goland [ "github-copilot" ])
    (unstable.jetbrains.plugins.addPlugins unstable.jetbrains.rider [ "github-copilot" ])
    (unstable.jetbrains.plugins.addPlugins unstable.jetbrains.rust-rover [ "github-copilot" ])
    unstable.signal-desktop
    unzip
    wget
    xclip
    xscreensaver
    zoom-us
  ];

  # Do this to fix the browser / extension communication.
  programs._1password.enable = true;
  programs._1password-gui.enable = true;

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "no";
    };
  };

  # Configure ssh itself.
  programs.ssh = {
    startAgent = true;
  };

  # Configure expressvpn.
  services.expressvpn = {
    enable = true;
  };

  # Install fonts.
  fonts.packages = with pkgs; [
    ibm-plex
  ];

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
