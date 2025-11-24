{
  description = "My Config";
  nixConfig = {
    builders-use-substitutes = true;
    experimental-features = [ "nix-command" "flakes" ];
    extra-trusted-substituters =
      [ "https://cache.nixos.org/" "https://nix-community.cachix.org" ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };
  outputs = { self, nixpkgs, home-manager, sops-nix, ... }@inputs:
    let
      inherit (self) outputs;
      forAllSystems = nixpkgs.lib.genAttrs [ "x86_64-linux" ];
      userSetting = {
        username = "leeao";
        gitusername = "NestorLiao";
        hostname = "nixos";
        email = "llqingsong@qq.com";
        windowmanager = "sway";
      };
    in {
      nixosConfigurations = {
        ${userSetting.hostname} = nixpkgs.lib.nixosSystem {
          specialArgs = { inherit inputs outputs userSetting; };
          modules = [
            ./hardware-configuration.nix
            ({ pkgs, config, userSetting, sops, inputs, lib, outputs, ... }:
              let
                myEmacs =
                  inputs.emacs-overlay.packages.${pkgs.system}.emacs-git-pgtk.override {
                    withNativeCompilation = true;
                    withSQLite3 = true;
                  };
                emacsWithPackages =
                  (pkgs.unstable.emacsPackagesFor myEmacs).emacsWithPackages;
                switchframeScript = ''
                  #!/usr/bin/env bash
                  if [ $# -ne 1 ]; then
                      echo "Usage: $0 <direction>"
                      echo "Directions: 1=up, 2=down, 3=left, 4=right"
                      exit 1
                  fi
                  case $1 in
                      1) direction="up" ;;
                      2) direction="down" ;;
                      3) direction="left" ;;
                      4) direction="right" ;;
                      *)
                          echo "Invalid direction: $1"
                          echo "Valid directions: 1=up, 2=down, 3=left, 4=right"
                          exit 1
                          ;;
                  esac
                  # Check if focused window is in fullscreen mode
                  if swaymsg -t get_tree | jq -e '.. | select(.focused? == true and .fullscreen_mode == 1)' >/dev/null; then
                      # If in fullscreen: exit fullscreen, focus in direction, then re-enter fullscreen
                      swaymsg fullscreen
                      swaymsg focus "$direction"
                      swaymsg fullscreen
                  else
                      # If not in fullscreen: simply focus in direction
                      swaymsg focus "$direction"
                  fi
                '';
                onlyemacsScript = ''
                  #!/usr/bin/env bash
                  swaymsg workspace number 5
                  EMACS_SERVER=server
                  TIMEOUT=10  # seconds to wait for server
                  if ! emacsclient -s "$EMACS_SERVER" -e '(emacs-pid)' &>/dev/null; then
                      echo "Emacs server not running. Restarting..."
                      notify-send -t 5000 "booting emacs"
                      systemctl --user restart emacs.service
                      # Wait for server to be ready, with timeout
                      waited=0
                      interval=0.5
                      while ! emacsclient -s "$EMACS_SERVER" -e '(emacs-pid)' &>/dev/null; do
                          sleep $interval
                          waited=$(echo "$waited + $interval" | bc)
                          if (( $(echo "$waited >= $TIMEOUT" | bc -l) )); then
                              notify-send -t 10000 "Emacs failed！check with --debug-int"
                              echo "ERROR: Emacs server did not start within $TIMEOUT seconds."
                              exit 1
                          fi
                      done
                      notify-send -t 5000 "emacs rebooted"
                      echo "Emacs server started."
                  else
                      echo "Emacs server already running."
                      notify-send -t 5000 "事实是劳动者所知的最美的梦。"
                  fi
                  # Connect to Emacs
                  bash -c "emacsclient -n -c -s $EMACS_SERVER"
                  sleep 1.0
                  swaymsg fullscreen
                  sleep 0.5
                '';
                toggleWorkspaceScript = ''
                  #!/usr/bin/env bash
                  # Ensure exactly two arguments are provided
                  if [ "$#" -ne 2 ]; then
                      echo "Usage: $0 <workspace1> <workspace2>"
                      exit 1
                  fi
                  workspace1="$1"
                  workspace2="$2"
                  # Get the current workspace name
                  current_workspace=$(swaymsg -t get_workspaces | jq -r '.[] | select(.focused==true) | .name')
                  # Toggle between the provided workspaces
                  if [ "$current_workspace" = "$workspace2" ]; then
                      swaymsg workspace number "$workspace1"
                  elif [ "$current_workspace" = "$workspace1" ]; then
                      swaymsg workspace number "$workspace2"
                  else
                      # If not in either workspace, jump to workspace1 by default
                      swaymsg workspace number "$workspace1"
                  fi
                '';
                #   template= ''
                #     #!/usr/bin/env bash
                # '';
                zig-doc = pkgs.stdenv.mkDerivation {
                  name = "zig-documentation";
                  src = pkgs.fetchurl {
                    url = "https://ziglang.org/documentation/master/";
                    # You might want to use fetchzip if it's a zip file, or fetchFromGitHub if available
                    # For now using fetchurl as placeholder
                    hash =
                      "sha256-gtI/hCWfwo4lpp70IXThF9YnfKjP34c9bnJ+TlC/ICk=";
                  };
                  dontUnpack = true;
                  # Add build steps to extract/move files if needed
                  installPhase = ''
                    mkdir -p $out
                    cp $src $out/index.html
                  '';
                };
              in {
                environment.systemPackages = with pkgs; [
                  (writeShellScriptBin "toggle-workspace" toggleWorkspaceScript)
                  (writeShellScriptBin "onlyemacs" onlyemacsScript)
                  (writeShellScriptBin "switchframe" switchframeScript)
                  texliveFull
                  thunderbird
                  texlab
                  fish
                  fishPlugins.done
                  wmenu
                  grim
                  slurp
                  wl-clipboard
                  mako
                  cliphist
                  satty
                  libnotify
                  wf-recorder
                  wl-color-picker
                  c-intro-and-ref
                  glibcInfo
                  libtool
                  libvterm
                  nil
                  nixfmt-classic
                  poppler-utils
                  ccls
                  cmake # C/C++ 构建系统（部分依赖可能需要）
                  ninja # 现代构建系统（可加速 CMake 编译）
                  flex
                  bc
                  ### 🔹 **代码质量**
                  codespell # 拼写检查工具（用于检查拼写错误）
                  cppcheck # C/C++ 静态分析工具（检查潜在错误）
                  doxygen # 生成代码文档（用于 C/C++ 文档生成）
                  ### 🔹 **测试和覆盖率**
                  gtest # Google Test 单元测试框架
                  lcov # 代码覆盖率工具（用于 GCov）
                  ### 🔹 **调试工具**
                  rr # 录制和重放调试（类似 `gdb`，用于更好的调试体验）
                  ### 🔹 **Linux 内核编译相关**
                  cpio # 内核打包工具（生成 initramfs 时使用）
                  pahole # `dwarves` 工具集，用于 BTF（BPF Type Format）
                  kmod # 用于管理 Linux 内核模块（`modprobe` 相关）
                  pciutils # PCI 设备管理工具（`lspci`）
                  usbutils # USB 设备管理工具（`lsusb`）
                  util-linux # `lsblk`、`fdisk` 等基本 Linux 工具
                  bpftrace # BPF 追踪工具（用于调试 Linux 内核）
                  ### 🔹 **C++ 依赖管理**
                  conan # C++ 包管理工具（用于管理依赖）
                  vcpkg # C++ 包管理工具（微软开发的）
                  vcpkg-tool # `vcpkg` 相关工具
                  (pkgs.buildFHSEnv {
                    name = "kernel-env";
                    targetPkgs = pkgs:
                      with pkgs; [
                        bear
                        bc
                        gcc
                        flex
                        bison
                        openssl
                        openssl.dev
                        elfutils.dev
                        elfutils
                        libelf
                        ncurses.dev
                        binutils
                        gnumake
                        ncurses
                        kmod
                      ];
                  })
                  pkg-config
                  # SDL for emulation
                  SDL2
                  SDL2.dev
                  # Optional but useful
                  cmake
                  meson
                  ninja
                  # ARM cross-compilation tools
                  gcc-arm-embedded
                  qemu
                  # For full system emulation:
                  qemu-utils # qemu-img, etc.
                  age
                  alsa-utils
                  bear
                  btop
                  cachix
                  cargo
                  clang-tools
                  cliphist
                  cmake
                  coreutils-full
                  curlFull
                  dash
                  fd
                  file
                  fzf
                  gcc
                  gdb
                  gnumake
                  jq
                  just
                  opencc
                  pandoc
                  paperlike-go
                  python3
                  ripgrep
                  rust-analyzer
                  rustc
                  rustlings
                  samba
                  sdcv
                  tree
                  unrar-free
                  unstable.leetgo
                  unstable.nix-search-cli
                  unstable.lldb
                  man-pages
                  unstable.quickemu
                  unstable.sops
                  unzipNLS
                  usbutils
                  vim-full
                  wget
                  zip
                  zoxide
                ];
                # trust me bro, run  xdg-user-dirs-update --force after
                xdg.mime = {
                  enable = true;
                  defaultApplications = {
                    "inode/directory" = [ "thunar.desktop" ];
                    "x-directory/normal" = [ "thunar.desktop" ];
                  };
                };
                environment.etc = {
                  "xdg/user-dirs.defaults".text = ''
                    DESKTOP=.save
                    DOCUMENTS=.save
                    DOWNLOAD=.save
                    MUSIC=.save/.media
                    PICTURES=.save
                    PUBLICSHARE=.save
                    TEMPLATES=.save
                    VIDEOS=.save
                  '';
                };
                environment.shellAliases = {
                  np =
                    "nix-shell -p  --option substituters 'https://mirrors.ustc.edu.cn/nix-channels/store  https://cache.nixos.org'";
                  gcl = "git clone";
                  nd = "pwd | wl-copy; pwd";
                  ls = "ls --color=never";
                  weather = "curl -s 'wttr.in/chongqing?T0'";
                  ff = "fd  | fzf | zoxide";
                  c = "clear";
                  # unzip = "unzip -O gb18030";
                  unrar = "unrar-free";
                  win =
                    "quickemu --vm windows-10.conf --display spice; sleep 480; notify-send -t 1000  'out of windows, now!';sleep 10; pkill windows;";
                  garbage = "nix-collect-garbage -d";
                  sgarbage = ''
                    sudo rm /tmp/tmp.* -rf;
                     sudo nix-collect-garbage -d;
                     nix-collect-garbage -d;
                    sudo nix-store --optimise;'';
                  # sudo nix-collect-garbage -d
                  # sudo nix-store --optimise
                  # nix-store --query --roots
                  # ls -l /var/run/current-system
                  nixh = "nix-prefetch-url";
                  nixhu = "nix-prefetch-url --unpack";
                  rso =
                    "sudo nixos-rebuild switch --flake /home/${userSetting.username}/.config/nixos#${userSetting.hostname} --offline";
                  rs =
                    "sudo nixos-rebuild switch --flake /home/${userSetting.username}/.config/nixos#${userSetting.hostname}";
                  rsop =
                    "sudo nixos-rebuild switch --flake /home/${userSetting.username}/.config/nixos#${userSetting.hostname}  --option substituters 'https://mirrors.ustc.edu.cn/nix-channels/store  https://cache.nixos.org' --cores 6 -j 12";
                  rb =
                    "sudo nixos-rebuild boot --flake /home/${userSetting.username}/.config/nixos#${userSetting.hostname}";
                  rsu =
                    "sudo nixos-rebuild switch --flake /home/${userSetting.username}/.config/nixos#${userSetting.hostname} --upgrade";
                  hs =
                    "home-manager --flake /home/${userSetting.username}/.config/nixos#${userSetting.username}@${userSetting.hostname}";
                  npi = "nix path-info nixpkgs#";
                  sh = "nix shell nixpkgs#";
                  sys = "systemctl";
                  syss = "systemctl start";
                  syssu = "systemctl status";
                  sysst = "systemctl stop";
                  sysr = "systemctl restart";
                  sysu = "systemctl --user";
                  sysus = "systemctl --user  start";
                  sysusu = "systemctl --user  status";
                  sysust = "systemctl --user  stop";
                  sysur = "systemctl --user  restart";
                  randompeeword = "";
                  jo = "journalctl -xeu";
                  e = "emacsclient -n -s server";
                  en = "emacsclient -n -s server .";
                  cleanup = "doas nix-collect-garbage --delete-older-than 7d";
                  ytmp3 =
                    "yt-dlp -x --continue --add-metadata --embed-thumbnail --audio-format mp3 --audio-quality 0 --metadata-from-title = '%(artist)s - %(title)s' --prefer-ffmpeg -o '%(title)s.%(ext)s' ";
                  ".1" = "cd ..";
                  ".2" = "cd ../..";
                  ".3" = "cd ../../..";
                  cp = "cp -iv";
                  mv = "mv -iv";
                  rm = "rm -vI";
                  bc = "bc -ql";
                  mkd = "mkdir -pv";
                };
                programs.bash.interactiveShellInit = ''
                  eval "$(zoxide init bash)"
                '';
                home-manager = {
                  extraSpecialArgs = { inherit inputs outputs userSetting; };
                  useUserPackages = true;
                  users = {
                    ${userSetting.username} = {
                      home.pointerCursor = {
                        gtk.enable = true;
                        package = pkgs.bibata-cursors;
                        name = "Bibata-Modern-Ice";
                        size = 24;
                      };
                      services.cliphist.enable = true;
                      home.file.".local/share/fonts/bookerly"={
                        source = pkgs.fetchFromGitHub {
                          owner = "NestorLiao";
                          repo = "boboly";
                          rev = "master";
                          sha256 =
                            "sha256-boN16BYYFY5MWhbLhaqIpumqsi583fIrMp+2Hz3pzqQ=";
                        };
                      };
                      home.file.".local/share/fonts/sourcehan" ={
                        source= pkgs.fetchFromGitHub {
                          owner = "NestorLiao";
                          repo = "sourcehan";
                          rev = "master";
                          sha256 = "sha256-uyZcPv7jPv96KovMKFdp/qDv0/6b3x0We/vx7eIU6O4=";
                        };
                      };
                      home.file.".stardict/dic" = {
                        source= pkgs.stdenv.mkDerivation {
                          pname = "stardict-dictionaries";
                          version =
                            "2.4.2"; # adjust the version based on the zip contents
                          src = pkgs.fetchFromGitHub {
                            owner = "NestorLiao";
                            repo = "dict";
                            rev = "master";
                            sha256 =
                              "sha256-dd9dMrhPa4QeJ58uiLBNhoQy8EfSJrmLj0lpwtygR2U=";
                          };
                          buildInputs = [ pkgs.unzip ]; # Add unzip as a build input
                          unpackPhase = ''
                    # Unzip each dictionary zip file
                    unzip $src/stardict-ghycyzzd-2.4.2.zip -d $out
                    unzip $src/stardict-langdao-ce-gb-2.4.2.zip -d $out
                    unzip $src/stardict-ecdict-2.4.2.zip -d $out
                  '';
                        };
                      };
                      home.file.".config/sway/white.jpg" = {
                        source = pkgs.fetchurl {
                          url =
                            "https://raw.githubusercontent.com/NestorLiao/NestorLiao.github.io/main/figure/gerwinski-gnu-head.png";
                          sha256 =
                            "sha256-bG6VGf8tWP2jugWMsiXNcMNAOXN+UvAyNKl2kvXBoFM=";
                        };
                      };
                      programs.foot = {
                        package = pkgs.unstable.foot;
                        server.enable = false;
                        enable = true;
                        settings = {
                          main = {
                            font = "FiraCode Nerd Font:size=12";
                            selection-target =
                              "both"; # Save to clipboard and primary selection
                          };
                          colors = {
                            foreground = "000000";
                            background = "FFFFFF";
                            # Normal colors
                            regular0 = "000000"; # black
                            regular1 = "0C322F"; # red
                            regular2 = "019E07"; # green
                            regular3 = "B58900"; # yellow
                            regular4 = "068BD2"; # blue
                            regular5 = "033682"; # magenta
                            regular6 = "CAA198"; # cyan
                            regular7 = "FFFFFF"; # white
                            # Bright colors
                            bright0 = "000000"; # bright black
                            bright1 = "0C322F"; # bright red
                            bright2 = "019E07"; # bright green
                            bright3 = "A58900"; # bright yellow
                            bright4 = "068BD2"; # bright blue
                            bright5 = "033682"; # bright magenta
                            bright6 = "0AA198"; # bright cyan
                            bright7 = "FFFFFF"; # bright white
                          };
                        };
                      };
                      programs.gh = {
                        enable = true;
                        gitCredentialHelper = {
                          enable = true;
                          hosts = [ "https://github.com" ];
                        };
                        extensions = with pkgs; [ gh-markdown-preview ];
                        settings = {
                          git_protocol = "ssh";
                          prompt = "enabled";
                        };
                      };
                      programs.git = {
                        ignores =
                          [ "*~" "*.swp" "*result*" ".direnv" "node_modules" ];
                        settings = {
                          user.email = "${userSetting.email}";
                          user.name = "${userSetting.gitusername}";
                          credential.helper = "manager";
                          github.user = "${userSetting.gitusername}";
                          push.autoSetupRemote = true;
                          credential."https://github.com".username =
                            "${userSetting.gitusername}";
                          credential.credentialStore = "cache";
                        };
                        enable = true;
                      };
                      programs.nix-index = {
                        enable = true;
                        enableFishIntegration = true;
                        enableBashIntegration = true;
                      };
                      programs.direnv = {
                        enable = true;
                        enableBashIntegration = true;
                        enableFishIntegration = true;
                        nix-direnv.enable = true;
                        silent = true;
                      };
                      programs.zoxide = {
                        enable = true;
                        enableBashIntegration = true;
                        enableFishIntegration = true;
                      };
                      home.file.".config/fish/functions/rcdir.fish".text = ''
                        function rcdir
                            while true
                                read -l -P 'Do you want to continue? [y/N] ' confirm
                                switch $confirm
                                    case Y y
                                        rm -rf (pwd)
                                        cd ..
                                        return 0
                                    case \'\' N n
                                        return 1
                                end
                            end
                        end
                      '';
                      home.file.".config/fish/functions/mcdir.fish".text = ''
                        function mcdir
                        command mkdir $argv[1]
                        and cd $argv[1]
                        end
                      '';
                      home.file.".config/nixpkgs/config.nix".text = ''
                        { allowUnfree = true; }
                      '';
                      home.file.".config/mako/config".text = ''
                        font=Bookerly 14
                        background-color=#ffffff
                        text-color=#000000
                        border-size=0
                        border-color=#000000
                        padding=0
                        default-timeout=8888
                        layer=overlay
                      '';
                      home.file.".cargo/config.toml".text = ''
                        [source.crates-io]
                        replace-with = 'rsproxy-sparse'
                        [source.rsproxy]
                        registry = "https://rsproxy.cn/crates.io-index"
                        [source.rsproxy-sparse]
                        registry = "sparse+https://rsproxy.cn/index/"
                        [registries.rsproxy]
                        index = "https://rsproxy.cn/crates.io-index"
                        [net]
                        git-fetch-with-cli = true
                      '';
                      programs.command-not-found.enable = false;
                      home.packages = with pkgs; [ git-credential-manager ];
                      home.enableNixpkgsReleaseCheck = false;
                      nixpkgs = {
                        overlays = [
                          # Add overlays your own flake exports (from overlays and pkgs dir):
                          # outputs.overlays.additions
                          outputs.overlays.modifications
                          outputs.overlays.unstable-packages
                          # You can also add overlays exported from other flakes:
                          # neovim-nightly-overlay.overlays.default
                          # (final: prev: {
                          #   blender = prev.blender.override {cudaSupport = true;};
                          # })
                          # Or define it inline, for example:
                          # (final: prev: {
                          #   hi = final.hello.overrideAttrs (oldAttrs: {
                          #     patches = [ ./change-hello-to-hi.patch ];
                          #   });
                          # })
                        ];
                        # Configure your nixpkgs instance
                        config = {
                          # Disable if you don't want unfree packages
                          allowUnfree = true;
                          # Workaround for https://github.com/nix-community/home-manager/issues/2942
                          allowUnfreePredicate = _: true;
                        };
                      };
                      gtk = {
                        enable = true;
                        theme = {
                          package = pkgs.flat-remix-gtk;
                          name = "Flat-Remix-GTK-White";
                        };
                        iconTheme = {
                          package = pkgs.adwaita-icon-theme;
                          name = "Adwaita";
                        };
                        font = {
                          name = "Sans";
                          size = 16;
                        };
                      };
                      qt = {
                        platformTheme.name = "gtk";
                        enable = true;
                        style.name = "adwaita-highcontrast";
                        style.package = pkgs.adwaita-qt6;
                      };
                      home = {
                        username = "${userSetting.username}";
                        homeDirectory = "/home/${userSetting.username}";
                      };
                      systemd.user.startServices = "sd-switch";
                      programs.firefox = {
                        enable = false;
                        policies = {
                          DisableFirefoxStudies = true;
                          DisablePocket = true;
                          DisableTelemetry = true;
                          DisableFirefoxScreenshots = true;
                          DisplayBookmarksToolbar = "never";
                          OfferToSaveLogins = false;
                          FirefoxHome = {
                            Search = false;
                            Pocket = false;
                            Snippets = false;
                            Highlights = false;
                            TopSites = false;
                          };
                          AutofillAddressEnabled = false;
                          AutofillCreditCardEnabled = false;
                          DefaultDownloadDirectory =
                            "/home/${userSetting.username}/.save";
                          OfferToSaveLoginsDefault = false;
                          OverrideFirstRunPage = "";
                          OverridePostUpdatePage = "";
                          PromptForDownloadLocation = true;
                          SearchSuggestEnabled = false;
                          TranslateEnabled = false;
                          FirefoxSuggest = {
                            WebSuggestions = false;
                            SponsoredSuggestions = false;
                            ImproveSuggest = false;
                            Locked = false;
                          };
                          UserMessaging = {
                            ExtensionRecommendations = false;
                            SkipOnboarding = true;
                          };
                          SearchBar = "unified";
                          PasswordManagerEnabled =
                            true; # enabled password manager
                          NoDefaultBookmarks = true;
                          DontCheckDefaultBrowser = true;
                          DisableSetDesktopBackground = true;
                          DisableSystemAddonUpdate = false;
                          ExtensionUpdate = false;
                          EnableTrackingProtection = {
                            Value = true;
                            Locked = true;
                            Cryptomining = true;
                            Fingerprinting = true;
                          };
                          DisableFeedbackCommands = true;
                          SearchEngines.Default = "Google";
                          DisableFormHistory = true;
                          AppAutoUpdate = false;
                          DisableAppUpdate = true;
                          BlockAboutAddons = false;
                        };
                        profiles.firefox = {
                          userChrome = ''
                                          @-moz-document url(chrome://browser/content/browser.xhtml) {
                                                /* ########  Sidetabs Styles  ######### */
                                                /* Set Bookerly for all Firefox UI */
                                                * {
                                                    font-family: Bookerly !important
                                                }
                                                #navigator-toolbox { font-family:Bookerly !important }
                                                #TabsToolbar { font-family: Bookerly !important }
                                                #sidebar-header {
                                                  display: none;
                                          }
                                                #statuspanel { display: none !important; }
                                                :root[tabsintitlebar] #titlebar:-moz-window-inactive {
                                                  opacity: 1 !important;
                                        }
                                                #TabsToolbar {
                                                	display: none !important;
                                      }
                                                #navigator-toolbox[fullscreenShouldAnimate] {
                                                    transition: none !important;
                                    }
                                                #contentAreaContextMenu #context-openlinkincurrent,
                                                #contentAreaContextMenu #context-openlinkinusercontext-menu,
                                                #contentAreaContextMenu #context-bookmarklink,
                                                #contentAreaContextMenu #context-selectall,
                                                #contentAreaContextMenu #context-sendlinktodevice,
                                                #contentAreaContextMenu #context-sendpagetodevice,
                                                #contentAreaContextMenu #context-sep-sendlinktodevice,
                                                #contentAreaContextMenu #context-sep-sendpagetodevice,
                                                #contentAreaContextMenu #context-viewpartialsource-selection {
                                                	display: none !important;
                                  }
                                                :root {
                                                	scrollbar-color: #ffffff #FFFFFF;
                                                	scrollbar-width: none;
                                                }
                                                *{ scrollbar-width: none !important; } }
                                                *{ scrollbar-width: none }
                                                #navigator-toolbox,
                                                #TabsToolbar,
                                                #tabbrowser-tabs {
                                                  background-color: #FFFFFFF !important;
                            }
                                                :root{
                                                  --uc-autohide-toolbox-delay: 200ms; /* Wait 0.1s before hiding toolbars */
                                                  --uc-toolbox-rotation: 82deg;  /* This may need to be lower on mac - like 75 or so */
                                                }
                                                :root[sizemode="maximized"]{
                                                  --uc-toolbox-rotation: 88.5deg;
                                                      }
                                                @media  (-moz-platform: windows){
                                                  :root:not([lwtheme]) #navigator-toolbox{ background-color: -moz-dialog !important; }
                                                }
                                                :root[sizemode="fullscreen"],
                                                :root[sizemode="fullscreen"] #navigator-toolbox{ margin-top: 0 !important; }
                                                #navigator-toolbox{
                                                  --browser-area-z-index-toolbox: 3;
                                                  position: fixed !important;
                                                  background-color: var(--lwt-accent-color,black) !important;
                                                  transition: transform 82ms linear, opacity 82ms linear !important;
                                                  transition-delay: var(--uc-autohide-toolbox-delay) !important;
                                                  transform-origin: top;
                                                  transform: rotateX(var(--uc-toolbox-rotation));
                                                  opacity: 0;
                                                  line-height: 0;
                                                  z-index: 1;
                                                  pointer-events: none;
                                                }
                                                :root[sessionrestored] #urlbar[popover]{
                                                  pointer-events: none;
                                                  opacity: 0;
                                                  transition: transform 82ms linear var(--uc-autohide-toolbox-delay), opacity 0ms calc(var(--uc-autohide-toolbox-delay) + 82ms);
                                                  transform-origin: 0px calc(0px - var(--tab-min-height) - var(--tab-block-margin) * 2);
                                                  transform: rotateX(89.9deg);
                                                }
                                                #mainPopupSet:has(> [panelopen]:not(#ask-chat-shortcuts,#tab-preview-panel)) ~ toolbox #urlbar[popover],
                                                #navigator-toolbox:is(:hover,:focus-within) #urlbar[popover],
                                                #urlbar-container > #urlbar[popover]:is([focused],[open]){
                                                  pointer-events: auto;
                                                  opacity: 1;
                                                  transition-delay: 33ms;
                                                  transform: rotateX(0deg);
                                                }
                                                #mainPopupSet:has(> [panelopen]:not(#ask-chat-shortcuts,#tab-preview-panel)) ~ toolbox,
                                                #navigator-toolbox:has(#urlbar:is([open],[focus-within])),
                                                #navigator-toolbox:hover,
                                                #navigator-toolbox:focus-within{
                                                  transition-delay: 33ms !important;
                                                  transform: rotateX(0);
                                                  opacity: 1;
                                                }
                                                /* This makes things like OS menubar/taskbar show the toolbox when hovered in maximized windows.
                                                 * Unfortunately it also means that other OS native surfaces (such as context menu on macos)
                                                 * and other always-on-top applications will trigger toolbox to show up. */
                                                @media (-moz-bool-pref: "userchrome.autohide-toolbox.unhide-by-native-ui.enabled"){
                                                  :root[sizemode="maximized"]:not(:hover){
                                                    #navigator-toolbox:not(:-moz-window-inactive),
                                                    #urlbar[popover]:not(:-moz-window-inactive){
                                                      transition-delay: 33ms !important;
                                                      transform: rotateX(0);
                                                      opacity: 1;
                                                        }
                                                }
                                                }
                                                #navigator-toolbox > *{ line-height: normal; pointer-events: auto }
                                                #navigator-toolbox,
                                                #navigator-toolbox > *{
                                                  width: 100vw;
                                                  -moz-appearance: none !important;
                                                }
                                                /* These two exist for oneliner compatibility */
                                                #nav-bar{ width: var(--uc-navigationbar-width,100vw) }
                                                #TabsToolbar{ width: calc(100vw - var(--uc-navigationbar-width,0px)) }
                                                /* Don't apply transform before window has been fully created */
                                                :root:not([sessionrestored]) #navigator-toolbox{ transform:none !important }
                                                :root[customizing] #navigator-toolbox{
                                                  position: relative !important;
                                                  transform: none !important;
                                                  opacity: 1 !important;
                                                }
                                                #navigator-toolbox[inFullscreen] > #PersonalToolbar,
                                                #PersonalToolbar[collapsed="true"]{ display: none }
                                                /* Uncomment this if tabs toolbar is hidden with hide_tabs_toolbar.css */
                                                 /*#titlebar{ margin-bottom: -9px }*/
                                                /* Uncomment the following for compatibility with tabs_on_bottom.css - this isn't well tested though */
                                                /*
                                                #navigator-toolbox{ flex-direction: column; display: flex; }
                                                #titlebar{ order: 2 }
                                                */
                                                }
                          '';
                          settings = {
                            "browser.tabs.closeTabByDblclick" = true;
                            # disable first-run onboarding
                            "browser.aboutwelcome.enabled" = false;
                            "general.smoothscroll" = false;
                            "browser.tabs.closeWindowWithLastTab" = false;
                            "full-screen-api.transition.timeout" = 0;
                            "full-screen-api.warning.delay" = 0;
                            "full-screen-api.warning.timeout" = 0;
                            "browser.tabs.tabClipWidth" = 999;
                            "places.history.enabled" = false;
                            "services.sync.engine.history" = false;
                            "widget.non-native-theme.scrollbar.style" = 3;
                            "ui.key.menuAccessKeyFocuses" = false;
                            "ui.key.menuAccessKey" = 17;
                            "toolkit.legacyUserProfileCustomizations.stylesheets" =
                              true;
                            # "browser.display.os-zoom-behavior" = 0;
                            "app.update.auto" = false;
                            "app.update.service.enabled" = false;
                            "app.update.download.promptMaxAttempts" = 0;
                            "app.update.elevation.promptMaxAttempts" = 0;
                            # HTTPs only.
                            "dom.security.https_only_mode" = false;
                            "dom.security.https_only_mode_ever_enabled" = false;
                            # Privacy and fingerprinting.
                            "browser.download.dir" =
                              "/home/${userSetting.username}/.save";
                            "browser.download.lastDir" =
                              "/home/${userSetting.username}/.save";
                            "privacy.userContext.enabled" = false;
                            # Disable Pocket.
                            "extensions.pocket.enabled" = false;
                            # Recently used order for tab cycles.
                            "browser.ctrlTab.recentlyUsedOrder" = true;
                            # Catch fat fingered quits.
                            "browser.sessionstore.warnOnQuit" = true;
                            # Compact UI.
                            "browser.uidensity" = 1;
                            # Hide warnings when playing with config.
                            "browser.aboutConfig.showWarning" = false;
                            # Disable Shit
                            "permissions.default.image" = 2;
                            "media.autoplay.default" = 2;
                            "media.autoplay.block-event.enabled" = true;
                            "media.disabled" =
                              true; # optional, blocks all media playback
                            # Plain new tabs.
                            "browser.newtabpage.enabled" = false;
                            # when you open a link image or media in a new tab switch to it immediately
                            "browser.tabs.loadInBackground" = true;
                            # Locale.
                            "browser.search.region" = "US";
                            "browser.startup.homepage" =
                              "https://www.blank.org";
                            # Don't save passwords or try to fill forms.
                            "signon.rememberSignons" = true; # true is saving
                            "signon.autofillForms" = false;
                            # Tell Firefox not to trust fake Enterprise-injected certificates.
                            "security.enterprise_roots.auto-enabled" = false;
                            "security.enterprise_roots.enabled" = false;
                            "privacy.resistFingerprinting.block_mozAddonManager" =
                              true;
                            "extensions.webextensions.restrictedDomains" = "";
                            "extensions.webextensions.ExtensionStorageIDB.migrated.@firefoxinvertcolors" =	true	;
                            "extensions.webextensions.ExtensionStorageIDB.migrated.authenticator@mymindstorm" =	true	;
                            "extensions.webextensions.ExtensionStorageIDB.migrated.bar-breaker@ris58h" =	true	;
                            "extensions.webextensions.ExtensionStorageIDB.migrated.chatgpt-ctrl-enter-sender@chatgpt-extension.io" =	true	;
                            "extensions.webextensions.ExtensionStorageIDB.migrated.idcac-pub@guus.ninja" =	true	;
                            "extensions.webextensions.ExtensionStorageIDB.migrated.jid1-BoFifL9Vbdl2zQ@jetpack" =	true	;
                            "extensions.webextensions.ExtensionStorageIDB.migrated.myallychou@gmail.com" =	true	;
                            "extensions.webextensions.ExtensionStorageIDB.migrated.no-emoji@erikdesjardins.io" =	true	;
                            "extensions.webextensions.ExtensionStorageIDB.migrated.uBlock0@raymondhill.net" =	true	;
                            "extensions.webextensions.ExtensionStorageIDB.migrated.vimium-c@gdh1995.cn" =	true	;
                            "extensions.webextensions.ExtensionStorageIDB.migrated.{2ce7df96-558d-4c2c-8d88-68606ebbe8db}" =	true	;
                            "extensions.webextensions.ExtensionStorageIDB.migrated.{74145f27-f039-47ce-a470-a662b129930a}" =	true	;
                            "extensions.webextensions.ExtensionStorageIDB.migrated.{80f6f2e4-eda1-417f-bf54-9645e1e20f5d}" =	true	;
                            "extensions.webextensions.ExtensionStorageIDB.migrated.{88ebde3a-4581-4c6b-8019-2a05a9e3e938}" =	true	;
                            "extensions.webextensions.ExtensionStorageIDB.migrated.{9350bc42-47fb-4598-ae0f-825e3dd9ceba}" =	true	;
                            "extensions.webextensions.ExtensionStorageIDB.migrated.{9b8ce341-744f-4f5d-9ff7-b5d7078a7b34}" =	true	;
                            "extensions.webextensions.ExtensionStorageIDB.migrated.{b52acdad-e4a6-44da-afc9-9bd22572db99}" =	true	;
                            "extensions.webextensions.ExtensionStorageIDB.migrated.{b6840179-45a1-4a2d-a4ef-e2815c1faa28}" = true;
                            "general.smoothScroll" = false;
                            "privacy.trackingprotection.enabled" =
                              true; # Enhanced Tracking Protection
                            "privacy.trackingprotection.socialtracking.enabled" =
                              true; # Block social trackers
                            "privacy.trackingprotection.cryptomining.enabled" =
                              true; # Block crypto mining scripts
                            "privacy.trackingprotection.fingerprinting.enabled" =
                              true; # Block fingerprinting
                            "toolkit.telemetry.enabled" = false;
                            "toolkit.telemetry.unified" = false;
                            "toolkit.telemetry.archive.enabled" = false;
                            "toolkit.telemetry.rejected" = false;
                            "toolkit.crashreporter.enabled" = false;
                            "toolkit.crashreporter.dataDirectory" = "";
                            "browser.search.suggest.enabled" =
                              false; # Disable search suggestions
                            "browser.sessionstore.restore_on_demand" =
                              true; # Don't restore all tabs at once
                            "browser.sessionstore.max_tabs_undo" =
                              3; # Limit the number of tabs that can be reopened
                            "browser.tabs.warnOnClose" =
                              false; # Prevent the warning when closing multiple tabs
                            "media.peerconnection.enabled" =
                              false; # Disable WebRTC
                            "extensions.update.enabled" =
                              false; # No extension updates
                            "browser.slowStartup.notificationDisabled" =
                              true; # Disable slow startup notifications
                            "browser.cache.disk.enable" =
                              false; # Disable disk cache (use memory cache only)
                            "accessibility.force_disabled" =
                              1; # Disable all accessibility features
                            "browser.accessibility.typeaheadfind" =
                              false; # Disable typeahead find
                            "browser.newtabpage.activity-stream.showWeather" =
                              false;
                            "browser.anchor_color" = "#000000";
                            "browser.visited_color" = "#000000";
                            "browser.display.document_color_use" = 2;
                            "layout.css.prefers-color-scheme.content-override" =
                              1;
                            "browser.display.use_document_fonts" = 0;
                            "font.name-list.emoji" = "";
                            "extensions.activeThemeID" =
                              "{5f71ffe3-23e2-49b8-b75e-2c032ef4a1d9}";
                          };
                        };
                      };
                      programs.chromium = {
                        enable = false;
                        package = pkgs.ungoogled-chromium;
                      };
                      home.stateVersion = "25.05";
                    };
                  };
                };
                users.users = {
                  ${userSetting.username} = {
                    initialPassword = "t";
                    isNormalUser = true;
                    openssh.authorizedKeys.keys = [
                      # Add your SSH public key(s) here, if you plan on using SSH to connect
                    ];
                    shell = pkgs.fish;
                    extraGroups = [ "wheel" "networkmanager" ];
                  };
                };
                users.defaultUserShell = pkgs.fish;
                security.sudo.wheelNeedsPassword = false;
                programs.thunar.enable = true;
                programs.thunar.plugins = with pkgs.xfce; [
                  thunar-archive-plugin
                  thunar-volman
                ];
                nix.settings.cores = 10;
                nix.settings.max-jobs = lib.mkDefault 10;
                environment.variables = {
                  EDITOR = "emacsclient -n -s 'server'";
                  RUSTUP_DIST_SERVER = "https://rsproxy.cn";
                  RUSTUP_UPDATE_ROOT = "https://rsproxy.cn/rustup";
                  GOPATH = "~/.go";
                  XAPIAN_CJK_NGRAM = "true";
                  GTK_IM_MODULE = lib.mkForce "";
                };
                #  fc-cache -fv
                fonts.fontDir.enable = true;
                fonts.packages = with pkgs;
                  lib.mkForce [
                    nerd-fonts.fira-code
                    nerd-fonts.fira-mono
                    nerd-fonts.noto
                    nerd-fonts.terminess-ttf
                    nerd-fonts.ubuntu
                    nerd-fonts.ubuntu-mono
                    noto-fonts-emoji-blob-bin
                  ];
                fonts.fontconfig = let
                  sansFallback = [
                    "Fira Code Nerd Font"
                    "Fira Mono Nerd Font"
                    "Terminess Nerd Font"
                    "Ubuntu"
                    "Ubuntu Mono"
                  ];
                  serifFallback = [
                    "Fira Code Nerd Font"
                    "Fira Mono Nerd Font"
                    "Terminess Nerd Font"
                    "Ubuntu"
                    "Ubuntu Mono"
                  ];
                in {
                  defaultFonts = rec {
                    emoji = [ "Blobmoji" ];
                    serif = [ "Bookerly" "Source Han Sans SC" "Noto Serif" ]
                      ++ emoji ++ serifFallback;
                    sansSerif = [ "Bookerly" "Source Han Sans SC" "Ubuntu" ]
                      ++ emoji ++ sansFallback;
                    monospace =
                      [ "Bookerly" "Ubuntu Mono" "Noto Sans Mono CJK SC" ]
                      ++ emoji ++ sansFallback;
                  };
                };
                i18n = {
                  defaultLocale = "en_US.UTF-8";
                  extraLocaleSettings = {
                    LC_ADDRESS = "en_US.UTF-8";
                    LC_COLLATE = "en_US.UTF-8";
                    LC_CTYPE = "en_US.UTF-8";
                    LC_IDENTIFICATION = "en_US.UTF-8";
                    LC_MEASUREMENT = "en_US.UTF-8";
                    LC_MESSAGES = "en_US.UTF-8";
                    LC_MONETARY = "en_US.UTF-8";
                    LC_NAME = "en_US.UTF-8";
                    LC_NUMERIC = "en_US.UTF-8";
                    LC_PAPER = "en_US.UTF-8";
                    LC_TELEPHONE = "en_US.UTF-8";
                    LC_TIME = "en_US.UTF-8";
                    LC_ALL = "en_US.UTF-8";
                  };
                  supportedLocales = [ "en_US.UTF-8/UTF-8" ];
                };
                sops.defaultSopsFile = ./secrets.yaml;
                sops.defaultSopsFormat = "yaml";
                sops.age.keyFile =
                  "/home/${userSetting.username}/.config/sops/age/keys.txt"; # 私钥路径
                #AGE-SECRET-KEY-
                sops.secrets."gh_hosts.yml" = {
                  owner = userSetting.username;
                  path = "/home/${userSetting.username}/.config/gh/hosts.yml";
                };
                sops.secrets."github_token" = {
                  owner = userSetting.username;
                  path = "/home/${userSetting.username}/.github_token";
                };
                sops.secrets."authinfo" = {
                  owner = userSetting.username;
                  path = "/home/${userSetting.username}/.authinfo";
                };
                sops.secrets."git_credential" = {
                  owner = userSetting.username;
                  path = "/home/${userSetting.username}/.git-credential";
                };
                sops.secrets.deepseek_apikey = {
                  owner = userSetting.username;
                };
                sops.secrets.github_apikey = { owner = userSetting.username; };
                sops.secrets.tavily_apikey = { owner = userSetting.username; };
                sops.secrets.mojie = { owner = userSetting.username; };
                sops.secrets.oney = { owner = userSetting.username; };
                sops.secrets.ouo = { owner = userSetting.username; };
                networking.extraHosts = '' # 想想jjr吧
                # 工业革命及其后果，己经成为了廖青松的灾难。
                   ${builtins.readFile ./nosurf/hosts00} # 眼-色
                   ${builtins.readFile ./nosurf/hosts01} # 耳-音
                   ${builtins.readFile ./nosurf/hosts02} # 身-欲
                   ${builtins.readFile ./nosurf/hosts03} # 鼻-香
                   ${builtins.readFile ./nosurf/hosts04} # 口-味
                   ${builtins.readFile ./nosurf/hosts05} # 脑-靡
                   ${builtins.readFile ./nosurf/hosts06} # 人-弱
                   # 消费主义让我们沉迷于物质/精神消费中，
                   # 通过让我们接触各种光怪陆离的东西来丰富我们的身份认同感，
                   # 这也是当今时代互联网正在加速实现的事情…
                   # 但这是以牺牲掌握任何技能为代价换来的，
                   0.0.0.0 google.com
                   0.0.0.0 www.google.com
                   0.0.0.0 reddit.com
                   0.0.0.0 old.reddit.com
                   0.0.0.0 z-library.sk
                   0.0.0.0 emacs-china.org
                   0.0.0.0 chatgpt.com
                   # 我们沉迷得越深，想要掌握一项技能的愿望就会越来越淡化。
                   127.0.0.1 linux.doc
                   127.0.0.1 cpp.doc
                   127.0.0.1 c.doc
                   127.0.0.1 zig.doc
                '';
                services = {
                  nscd.enable = false;
                  irqbalance.enable = false;
                  udev.extraRules =
                    ''SUBSYSTEM=="i2c-dev", GROUP="i2c", MODE="0660"'';
                  xserver.videoDrivers = [ "modesetting" ];
                  udisks2.mountOnMedia = true;
                  udisks2.enable = true;
                  gvfs.enable = true;
                  devmon.enable = true;
                  # 修改音频设备在空闲时自动挂起（suspend）的行为
                  power-profiles-daemon.enable = false;
                  openssh = { enable = true; };
                  pulseaudio.enable = false;
                  pipewire = {
                    enable = true;
                    audio.enable = true;
                    pulse.enable = true;
                    alsa = {
                      enable = true;
                      support32Bit = true;
                    };
                    jack.enable = false;
                    wireplumber.enable = true;
                  };
                  create_ap = {
                    enable = false;
                    settings = {
                      INTERNET_IFACE = "enp46s0";
                      PASSPHRASE = "12345678";
                      SSID = "{$userSetting.hostname} Hotspot";
                      WIFI_IFACE = "wlp45s0";
                    };
                  };
                  v2raya.enable = false;
                  dae = {
                    package = pkgs.unstable.dae;
                    enable = true;
                    disableTxChecksumIpGeneric = false;
                    configFile =
                      "/home/${userSetting.username}/.config/dae/config.dae";
                    assets = with pkgs.unstable; [
                      v2ray-geoip
                      v2ray-domain-list-community
                    ];
                    openFirewall = {
                      enable = true;
                      port = 12345;
                    };
                  };
                  dnsmasq = {
                    enable = true;
                    settings = {
                      addn-hosts = "/etc/hosts";
                      cache-size = 1000;
                      local-ttl = 3600;
                      server = [ "223.5.5.5" "119.29.29.29" ];
                    };
                    resolveLocalQueries = true;
                  };
                  greetd = {
                    enable = true;
                    settings = {
                      default_session.command =
                        "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd ${userSetting.windowmanager} --theme 'text=black;container=white;prompt=black;input=black;border=black;title=black;greet=black;action=black;button=black;time=black'";
                      initial_session = {
                        command = "${userSetting.windowmanager}";
                        user = userSetting.username;
                      };
                    };
                  };
                  gnome.gnome-keyring.enable = true;
                  xserver = {
                    enable = true;
                    xkb.layout = "us";
                  };
                  nginx = {
                    enable = true;
                    virtualHosts = {
                      "linux.doc" = {
                        listen = [{
                          addr = "0.0.0.0";
                          port = 3000;
                        }];
                        root = "${pkgs.linux-doc}/share/doc/linux-doc";
                      };
                      "c.doc" = {
                        listen = [{
                          addr = "0.0.0.0";
                          port = 3001;
                        }];
                        root =
                          "${pkgs.cppreference-doc}/share/cppreference/doc/html/en/c";
                      };
                      "cpp.doc" = {
                        listen = [{
                          addr = "0.0.0.0";
                          port = 3002;
                        }];
                        root =
                          "${pkgs.cppreference-doc}/share/cppreference/doc/html/en/cpp";
                      };
                      "zig.doc" = {
                        listen = [{
                          addr = "0.0.0.0";
                          port = 3003;
                        }];
                        root = "${zig-doc}";
                      };
                    };
                  };
                  mysql = {
                    enable = false;
                    package = pkgs.mariadb;
                  };
                  influxdb.enable = false;
                  ollama = {
                    enable = false;
                    acceleration = "cuda";
                    loadModels = [ "llama3.2:3b" "qwen3:4b" ];
                  };
                  dictd = {
                    enable = false;
                    DBs = with pkgs.dictdDBs; [ wiktionary wordnet ];
                  };
                  xserver.dpi = 192;
                  emacs = {
                    enable = true;
                    package = emacsWithPackages (epkgs:
                      (with epkgs.melpaStablePackages; [ ])
                      ++ (with epkgs.melpaPackages; [
                        ### fast MAGIC for jumping everwhere
                        rg
                        dired-subtree
                        ztree
                        ## expand & edit
                        move-text
                        surround
                        expand-region
                        multiple-cursors
                        avy
                        yasnippet
                        ### project manager
                        magit
                        magit-todos
                        disproject
                        forge
                        ### do anything in emacs
                        consult-gh-embark
                        consult-gh-forge
                        consult-gh-with-pr-review
                        docker
                        kubernetes
                        doxymacs
                        git-link
                        git-timemachine
                        vterm
                        multi-vterm
                        with-editor
                        nov
                        pdf-tools
                        saveplace-pdf-view
                        pyim
                        syslog-mode
                        journalctl-mode
                        ### read log/manual/dict/gpt/... in emacs
                        quick-sdcv
                        tldr
                        gptel
                        helpful
                        sicp
                        posix-manual
                        devdocs-browser
                        ### the you-know-who guy created a huge list of amazing pkgs
                        consult
                        embark
                        vertico
                        marginalia
                        cape
                        corfu
                        orderless
                        embark-consult
                        ### for language fans
                        cmake-mode
                        dockerfile-mode
                        go-mode
                        haskell-mode
                        markdown-mode
                        nix-mode
                        racket-mode
                        rust-mode
                        zig-mode
                        ### save and format
                        aggressive-indent
                        elisp-autofmt
                        super-save
                        ### emacs look and feel
                        hide-mode-line
                        no-emoji
                        ligature
                        compile-angel
                        buffer-terminator
                        envrc
                        ### feel even better than drug
                        alert
                        trashed
                        wgrep
                        easysession
                        undo-fu
                        undo-fu-session
                        iedit
                        shift-number
                      ]) ++ (with epkgs.elpaPackages; [ plz ])
                      ++ (with pkgs; [ ]));
                  };
                };
                programs.nix-ld = {
                  enable = true;
                  package = pkgs.unstable.nix-ld;
                  libraries = with pkgs.unstable; [
                    stdenv.cc.cc
                    openssl.dev
                    pkg-config
                    openssl
                    # openssl_3_4
                    gcc13
                    xorg.libXcomposite
                    xorg.libXtst
                    xorg.libXrandr
                    xorg.libXext
                    xorg.libX11
                    xorg.libXfixes
                    libGL
                    libva
                    # pipewire.lib
                    xorg.libxcb
                    xorg.libXdamage
                    xorg.libxshmfence
                    xorg.libXxf86vm
                    nix-ld
                    sqlite
                    libelf
                    libayatana-appindicator
                    webkitgtk_4_1
                    # Required
                    glib
                    gtk2
                    gtk3
                    bzip2
                    libsoup_2_4
                    at-spi2-atk
                    atkmm
                    cairo
                    gdk-pixbuf
                    harfbuzz
                    librsvg
                    libsoup_3
                    pango
                    openssl
                    # Without these it silently fails
                    xorg.libXinerama
                    xorg.libXcursor
                    xorg.libXrender
                    xorg.libXScrnSaver
                    xorg.libXi
                    xorg.libSM
                    xorg.libICE
                    # gnome2.GConf
                    nspr
                    nss
                    cups
                    libcap
                    SDL2
                    libusb1
                    dbus-glib
                    ffmpeg
                    # Only libraries are needed from those two
                    libudev0-shim
                    # Verified games requirements
                    xorg.libXt
                    xorg.libXmu
                    libogg
                    libvorbis
                    SDL
                    SDL2_image
                    glew110
                    libidn
                    tbb
                    # Other things from runtime
                    flac
                    freeglut
                    libjpeg
                    libpng
                    libpng12
                    libsamplerate
                    libmikmod
                    libtheora
                    libtiff
                    pixman
                    speex
                    SDL_image
                    # SDL_ttf
                    SDL_mixer
                    # SDL2_ttf
                    SDL2_mixer
                    libappindicator-gtk2
                    libappindicator-gtk3
                    libdbusmenu-gtk2
                    libindicator-gtk2
                    libdbusmenu-gtk3
                    libindicator-gtk3
                    libcaca
                    libcanberra
                    libgcrypt
                    util-linux
                    libvpx
                    xorg.libXft
                    libvdpau
                    # gnome2.pango
                    atk
                    fontconfig
                    freetype
                    dbus
                    alsa-lib
                    expat
                    ncurses
                  ];
                };
                console = {
                  font = "latarcyrheb-sun32";
                  keyMap = "us";
                  colors = [
                    "FFFFFF" # Black → Off-White (Background)
                    "202124" # Red → Dark Gray (Text)
                    "252525" # Green → Dark Gray
                    "303030" # Yellow → Darker Gray
                    "3A3A3A" # Blue → Even Darker Gray
                    "444444" # Magenta → Almost Black
                    "4E4E4E" # Cyan → Blackish Gray
                    "555555" # White → Soft Black
                    "E0E0E0" # Bright Black → Light Gray
                    "A94442" # Bright Red → Muted Red
                    "3A7D44" # Bright Green → Darker Green
                    "B58332" # Bright Yellow → Muted Gold
                    "2955A3" # Bright Blue → Muted Blue
                    "8650A3" # Bright Magenta → Muted Purple
                    "31718C" # Bright Cyan → Muted Teal
                    "2A2A2A" # Bright White → Almost Black
                  ];
                };
                programs.sway = {
                  package = pkgs.unstable.sway;
                  enable = true;
                  wrapperFeatures.gtk = true;
                };
                systemd.services."getdaeconfig" = {
                  script = ''
                    mkdir -p "/home/${userSetting.username}/.config/dae"
                    config_file="/home/${userSetting.username}/.config/dae/config.dae"
                    source_file="/home/${userSetting.username}/.config/nixos/device/desktop/resource/daeconfig"
                    if [[ ! -f "$config_file" ]]; then
                    echo "
                    # -*- mode: conf-space -*-
                    global {
                      wan_interface: auto
                      log_level: info
                      allow_insecure: true
                      auto_config_kernel_parameter: true
                    }
                    subscription {
                                sub_airport_1: '$(cat ${config.sops.secrets.mojie.path})'
                                # sub_airport_2: '$(cat ${config.sops.secrets.ouo.path})'
                                # sub_airport_3: '$(cat ${config.sops.secrets.oney.path})'
                                }
                    dns {
                      upstream {
                        alidns: 'udp://dns.alidns.com:53'
                        cfdns: 'tcp+udp://1.1.1.3:53'
                      }
                      routing {
                        request {
                          qtype(https) -> reject
                          fallback: alidns
                        }
                        response {
                          upstream(cfdns) -> accept
                          ip(geoip:private) && !qname(geosite:cn) -> cfdns
                          fallback: accept
                        }
                      }
                    }
                    group {
                        proxy {
                              filter: name(keyword:'香港')
                              policy: min_moving_avg
                        }
                    }
                    routing {
                        pname(dnsmasq, dropbear) -> must_direct
                        dip(8.8.8.8) -> must_direct
                        dip(1.1.1.3) -> must_direct
                        domain(dns.alidns.com) -> must_direct
                        domain(dns.google) -> must_direct
                        domain(cloudflare-dns.com) -> must_direct
                        dip(224.0.0.0/3, 'ff00::/8') -> direct
                    domain(
                        geosite:category-social-media-!cn,
                        geosite:category-social-media-cn,
                        geosite:category-media,
                        geosite:category-media-cn,
                        geosite:category-entertainment,
                        geosite:category-entertainment-cn,
                        geosite:category-porn) -> block
                        domain(geosite:cn,api.tavily.com,ziggit.dev,api.deepseek.com) -> direct
                        fallback: proxy
                    }
                                " > /home/${userSetting.username}/.config/dae/config.dae
                    fi
                  '';
                  wantedBy = [ "hibernate.target" "multi-user.target" ];
                  description = "dae config generator";
                  restartIfChanged = true;
                  after = [ "hibernate.target" ];
                };
                systemd.services."getswayconfig" = {
                  script = ''
                                        mkdir -p "/home/${userSetting.username}/.config/sway"
                                        config_file="/home/${userSetting.username}/.config/sway/config"
                                        if [[ ! -f "$config_file" ]]; then
                                        echo "
                    # -*- mode: conf-space -*-
                    # Logo key. Use Mod1 for Alt.
                    set \$mod Mod4
                    # Preferred terminal and launcher
                    set \$term foot
                    set \$menu wmenu-run -N '#ffffff' -n '#000000' -M '#000000' -m '#ffffff' -S '#000000' -s '#ffffff' -f 'monospace 16' -b -i
                    # Background and output configuration
                    output * bg  ~/.config/sway/white.jpg fill
                    output * transform 270
                    # Screenshots and screen recording
                    bindsym Print exec grim -g '\$(slurp)' - | wl-copy && wl-paste > ~/save/.media/recordings/pic/Screenshot-\$(date +%F%T).png | dunstify 'Screenshot of the region taken' -t 1000
                    bindsym Shift+Print exec grim -g '\$(slurp -o -r -c '#ff0000ff')' -t ppm - | satty --filename - --fullscreen --output-filename ~/save/.media/recordings/pic/satty-\$(date '+%Y%m%d-%H:%M:%S').png
                    bindsym Mod1+Shift+Print exec wf-recorder
                    bindsym \$mod+Shift+j exec bash -c 'paperlike-cli -i2c /dev/i2c-4 -clear;swaylock -i ~/.config/sway/white.jpg;'

                    # Exit sway (logs you out of your Wayland session)
                    bindsym \$mod+Shift+q exec swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' -B 'Yes, exit sway' 'swaymsg exit'
                    bindsym \$mod+Shift+comma exec switchframe 2
                    bindsym \$mod+Shift+period  exec toggle-workspace 4 5
                    # Basic window management
                    bindsym \$mod+Shift+w kill
                    bindsym \$mod+Return exec myterm
                    # Application launcher and clipboard history
                    # bindsym \$mod+Shift+apostrophe exec \$menu -show drun
                    bindsym \$mod+Shift+apostrophe exec \$menu
                    bindsym \$mod+Shift+y exec cliphist list | wmenu -N '#ffffff' -n '#000000' -M '#000000' -m '#ffffff' -S '#000000' -s '#ffffff' -f 'monospace 16' -b -i -l 16  | cliphist decode | wl-copy
                    bindsym \$mod+Shift+d exec cliphist list | wmenu -N '#ffffff' -n '#000000' -M '#000000' -m '#ffffff' -S '#000000' -s '#ffffff' -f 'monospace 16' -b -i -l 10  | cliphist delete
                    floating_modifier \$mod normal
                    # Reload config
                    bindsym \$mod+Shift+r reload
                    bindsym \$mod+Shift+h fullscreen
                    bindsym \$mod+Shift+slash layout toggle split
                    bindsym \$mod+Shift+space floating toggle
                    bindsym \$mod+space focus mode_toggle
                    # Hide cursor after 1 second of inactivity
                    seat * hide_cursor 888
                    # Gaps
                    gaps top 0
                    gaps outer 0
                    gaps inner 0
                    # Bar configuration
                    bar {
                        position bottom
                        tray_output none
                        status_command while true; do date +'%H:%M'; sleep 1; done
                        mode hide
                        swaybar_command true
                        colors {
                            background #ffffff
                            statusline #000000
                            separator  #000000
                            focused_background #ffffff
                            focused_statusline #000000
                            focused_separator  #000000
                            focused_workspace  #ffffff #ffffff #000000
                            active_workspace   #ffffff #ffffff #000000
                            inactive_workspace #ffffff #000000 #ffffff
                            urgent_workspace   #ffffff #ffffff #000000
                            binding_mode       #ffffff #ffffff #000000
                        }
                    }
                    bindsym \$mod+Shift+b exec swaymsg bar mode toggle
                    # Client window styles
                    client.focused #ffffff  #ffffff  #000000  #ffffff  #ffffff
                    client.focused_inactive  #ffffff  #000000  #ffffff  #ffffff  #ffffff
                    client.focused_tab_title #ffffff  #ffffff  #000000
                    client.unfocused         #ffffff  #000000  #ffffff  #ffffff  #ffffff
                    client.urgent #ffffff  #ffffff  #000000  #ffffff  #ffffff
                    client.placeholder       #ffffff  #ffffff  #000000  #ffffff  #ffffff
                    client.background        #ffffff
                    # UI styling
                    font pango:Bookerly 1
                    titlebar_padding 1
                    titlebar_border_thickness 0
                    exec swaymsg workspace number 5
                    # Workspace bindings (keypad)
                    bindsym KP_1 workspace 1
                    bindsym KP_2 workspace 2
                    bindsym KP_3 workspace 3
                    bindsym KP_4 workspace 4
                    bindsym KP_5 workspace 5
                    bindsym KP_6 workspace 6
                    bindsym KP_7 workspace 7
                    bindsym KP_8 workspace 8
                    bindsym KP_9 workspace 9
                    bindsym KP_0 workspace next
                    bindsym \$mod+Shift+e exec  emacs
                    bindsym \$mod+e exec  emacsclient -n -c -s server
for_window [app_id='emacs'] border none
for_window [app_id='emacs'] titlebar_padding 0
for_window [app_id='emacs'] titlebar_border_thickness 0
for_window [app_id='emacsclient'] border none
for_window [app_id='emacsclient'] titlebar_padding 0
for_window [app_id='emacsclient'] titlebar_border_thickness 0
# Foot terminal styling
for_window [app_id='foot'] border none
for_window [app_id='foot'] titlebar_padding 0
for_window [app_id='foot'] titlebar_border_thickness 0
# Firefox beta styling
for_window [app_id='firefox'] border none
for_window [app_id='firefox'] titlebar_padding 0
for_window [app_id='firefox'] titlebar_border_thickness 0
# Firefox beta styling
for_window [app_id='firefox-beta'] border none
for_window [app_id='firefox-beta'] titlebar_padding 0
for_window [app_id='firefox-beta'] titlebar_border_thickness 0
for_window [app_id='xdg-desktop-portal-gtk'] floating enable
for_window [app_id='xdg-desktop-portal-gtk'] resize set 800 600
for_window [app_id='xdg-desktop-portal-gtk'] move position center
                    # Include additional config files
                    include /etc/sway/config.d/*
                                        " > "$config_file"
                                        fi
                  '';
                  # WorkingDirectory = "/home/${userSetting.username}";
                  wantedBy = [ "hibernate.target" "multi-user.target" ];
                  description = "sway";
                  restartIfChanged = true;
                  after = [ "hibernate.target" ];
                };
                systemd.services.adjustPaperLight = {
                  enable = true;
                  description = "no light";
                  after = [ "hibernate.target" ];
                  script =
                    "/run/current-system/sw/bin/paperlike-cli -i2c /dev/i2c-4 -light1 0;";
                  wantedBy = [ "hibernate.target" "multi-user.target" ];
                  path = [ "/nix/store" ];
                };
                networking = {
                  hostName = userSetting.hostname;
                  nameservers = [ "127.0.0.1" ];
                  networkmanager.enable = true;
                  firewall.enable = true;
                };
                system.nssModules = lib.mkForce [ ];
                hardware.i2c.enable = true;
                boot.kernelModules = [ "i2c-dev" "i915" "spi-ch341" ];
                boot.extraModulePackages = [ ];
                boot.binfmt.emulatedSystems =
                  [ "aarch64-linux" "riscv64-linux" "i686-linux" ];
                boot = {
                  kernel = {
                    sysctl = {
                      # forward network packets that are not destined for the interface on which they were received
                      "net.ipv4.conf.all.forwarding" = true;
                      "net.ipv6.conf.all.forwarding" = true;
                      "net.ipv4.conf.br-lan.rp_filter" = 1;
                      "net.ipv4.conf.wan.rp_filter" = 1;
                    };
                  };
                };
                boot.kernelPackages = pkgs.linuxPackages;
                # Bootloader
                boot.loader.timeout = 5;
                boot.loader.systemd-boot.enable = true;
                boot.loader.systemd-boot.configurationLimit = 5;
                boot.loader.efi.canTouchEfiVariables = true;
                boot.loader.efi.efiSysMountPoint = "/boot";
                boot.loader.grub.theme =
                  "${pkgs.libsForQt5.breeze-grub}/grub/themes/breeze";
                documentation = {
                  enable = true;
                  doc.enable = true;
                  info.enable = true;
                  man = {
                    enable = true;
                    generateCaches = true; # will take little time
                  };
                  dev.enable = true;
                };
                hardware = {
                  bluetooth.enable = true;
                  bluetooth.powerOnBoot = true;
                  # always enable graphics drivers and enable a bunch of layers for it (including vulkan validation)
                  graphics = {
                    enable = true;
                    extraPackages = with pkgs; [
                      intel-media-driver
                      vaapiIntel # video acceleration on intel inbuilt graphics
                      vulkan-validation-layers # helps catch and debug vulkan crashes
                    ];
                  };
                };
                hardware.enableAllFirmware = true;
                time.timeZone = "Asia/Shanghai";
                programs.bash.undistractMe.playSound = true;
                programs.soundmodem.enable = true;
                security.rtkit.enable = true;
                xdg.sounds.enable = true;
                environment.etc = {
                  "wireplumber/main.lua.d/90-suspend-timeout.lua".text = ''
                    apply_properties = {
                      ["session.suspend-timeout-seconds"] = 0;};
                  '';
                };
                boot.extraModprobeConfig = ''
                  options snd-hda-intel power_save=0 power_save_controller=N
                '';
                programs.fish = {
                  package = pkgs.fish;
                  enable = true;
                  interactiveShellInit = ''
                     zoxide init fish | source;
                     clear
                    set fish_greeting
                     function vterm_prompt_end;
                         vterm_printf '51;A'(whoami)'@'(hostname)':'(pwd)
                     end
                     functions --copy fish_prompt vterm_old_fish_prompt
                     function fish_prompt --description 'Write out the prompt; do not replace this. Instead, put this at end of your file.'
                         # Remove the trailing newline from the original prompt. This is done
                         # using the string builtin from fish, but to make sure any escape codes
                         # are correctly interpreted, use %b for printf.
                         printf "%b" (string join "\n" (vterm_old_fish_prompt))
                         vterm_prompt_end
                     end
                     function vterm_printf;
                         if begin; [  -n "$TMUX" ]  ; and  string match -q -r "screen|tmux" "$TERM"; end
                             # tell tmux to pass the escape sequences through
                             printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
                         else if string match -q -- "screen*" "$TERM"
                             # GNU screen (screen, screen-256color, screen-256color-bce)
                             printf "\eP\e]%s\007\e\\" "$argv"
                         else
                             printf "\e]%s\e\\" "$argv"
                         end
                     end
                     if [ "$INSIDE_EMACS" = 'vterm' ]
                         function clear
                             vterm_printf "51;Evterm-clear-scrollback";
                             tput clear;
                         end
                     end
                  '';
                  shellInit = ''
                    # set -x DIRENV_LOG_FORMAT ""
                    set -U fish_user_paths $HOME/.local/bin/ $fish_user_paths
                    # clear
                  '';
                };
                system.stateVersion = "25.05";
                nix = {
                  package = pkgs.nixVersions.latest;
                  # This will add each flake input as a registry
                  # To make nix3 commands consistent with your flake
                  registry = lib.mapAttrs (_: value: { flake = value; }) inputs;
                  # This will additionally add your inputs to the system's legacy channels
                  # Making legacy nix commands consistent as well, awesome!
                  nixPath =
                    lib.mapAttrsToList (key: value: "${key}=${value.to.path}")
                    config.nix.registry;
                  settings = {
                    substituters = [
                      "https://cache.nixos.org/"
                      "https://nix-community.cachix.org"
                      "https://mirrors.ustc.edu.cn/nix-channels/store"
                      # "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store"
                      # "https://mirror.sjtu.edu.cn/nix-channels/store"
                      # "https://xddxdd.cachix.org"
                    ];
                    trusted-public-keys = [
                      # "xddxdd.cachix.org-1:ay1HJyNDYmlSwj5NXQG065C8LfoqqKaTNCyzeixGjf8="
                      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
                      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
                    ];
                    # Enable flakes and new 'nix' command
                    experimental-features = "nix-command flakes";
                    # Deduplicate and optimize nix store
                    auto-optimise-store = true;
                    trusted-users = [ userSetting.username ];
                  };
                  gc = {
                    automatic = true;
                    dates = "daily";
                    options = "--delete-older-than 7d";
                  };
                };
                nixpkgs = {
                  # You can add overlays here
                  overlays = [
                    # Add overlays your own flake exports (from overlays and pkgs dir):
                    # outputs.overlays.additions
                    # (import (builtins.fetchTarball {
                    #   url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
                    # }))
                    outputs.overlays.modifications
                    outputs.overlays.unstable-packages
                    outputs.overlays.old-packages
                    # You can also add overlays exported from other flakes:
                    # neovim-nightly-overlay.overlays.default
                    # Or define it inline, for example:
                    # (final: prev: {
                    #   hi = final.hello.overrideAttrs (oldAttrs: {
                    #     patches = [ ./change-hello-to-hi.patch ];
                    #   });
                    # })
                  ];
                  # Configure your nixpkgs instance
                  config = {
                    # Disable if you don't want unfree packages
                    allowUnfree = true;
                    # Workaround for https://github.com/nix-community/home-manager/issues/2942
                    allowUnfreePredicate = _: true;
                  };
                };
              })
            inputs.sops-nix.nixosModules.sops
            inputs.home-manager.nixosModules.home-manager
          ];
        };
      };
      overlays = {
        modifications = final: prev: { };
        unstable-packages = final: _prev: {
          unstable = import inputs.nixpkgs-unstable {
            system = final.system;
            config = {
              allowUnfree = true;
              permittedInsecurePackages = [ "libsoup-2.74.3" ];
            };
          };
        };
        old-packages = final: _prev: {
          oldw = import inputs.nixpkgs-old {
            system = final.system;
            config = { allowUnfree = true; };
          };
        };
      };
    };
  inputs = {
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay/master";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
      inputs.nixpkgs-stable.follows = "nixpkgs";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix.url = "github:Mic92/sops-nix";
  };
}
