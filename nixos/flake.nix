{
  description = "My Config";
  nixConfig = {
    builders-use-substitutes = true;
    experimental-features = [ "nix-command" "flakes" ];
    trusted-substituters = [
      "https://cache.nixos.org"
      "https://nix-community.cachix.org"
      "https://mirrors.ustc.edu.cn/nix-channels/store"
    ];
    extra-trusted-substituters =
      [ "https://cache.nixos.org" "https://nix-community.cachix.org" ];
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
                  lock = pkgs.writeScript "lock" ''
                       exec sudo systemctl start physlock
                       '';
                myEmacs = pkgs.unstable.emacs-pgtk.override {
                  withNativeCompilation = true;
                  withSQLite3 = true;
                };
                  # Custom Latex distribution
                myTexLive = pkgs.texlive.combine {
                  inherit (pkgs.texlive)
                    scheme-basic latexmk
                    collection-latexextra
                    collection-xetex;
                };
                emacsWithPackages =
                  (pkgs.unstable.emacsPackagesFor myEmacs).emacsWithPackages;
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
                #   template= ''
                #     #!/usr/bin/env bash
                # '';
                lock-false = {Value = false; Status = "locked";};
                lock-true = {Value = true; Status = "locked";};
              in {
                imports = [ inputs.hosts.nixosModule ];
                # 我得道了，在怡凉乡间独自过着悠然的生活，在蓝天绿树巨石间…
                networking.stevenBlackHosts = { # 从来都没有什么戒色…
                  # 城市、单身、独生、男性、大学生、亚洲、
                  # 网瘾、中产、游戏瘾、计算机相关专业人群
                  # 一切只是“生物本性”与“传统文化”带来的欲望
                  # 配合“工业革命”与“数字革命”带来的富饶假象
                  enable = true; # 你知道的，开着不会影响学习编程
                  enableIPv6 = false; # 你知道的，开着不会影响学习编程
                  blockPorn = true; # 你知道的，虚假的情欲只会让你溺身
                  blockSocial = true; # 你知道的，虚假的参与只会让你躁心
                }; # 不依赖外物，不物物于物，
                # 内心拒绝即是自由
                # 外物自由即是强迫
                environment.etc = {
                  "nixos/flake.nix" = {
                    source = ./flake.nix;
                    mode = "0777";
                  };
                };
                environment.systemPackages = with pkgs; [

                  # # utilities
                  # file bash man-pages sudo sd bc pv rename vimv
                  # lsb-release moreutils unzip zip unrar envsubst
                  # # processes
                  # dtach pstree killall sysstat
                  # # monitoring
                  # htop btop iotop iftop s-tui multitail entr
                  # # dev tools
                  # jq tmux fzf silver-searcher git
                  # # hardware tools
                  # pciutils lm_sensors acpi pmutils usbutils dmidecode
                  # # networking
                  # wget curl nmap nettools traceroute dnsutils iperf
                  # # filesystems
                  # ncdu ranger lsof ntfs3g nfs-utils
                  # # hard drive management
                  # lsscsi hddtemp hdparm perf-tools parted gptfdisk
                  # # security
                  # pass gopass

                  (writeShellScriptBin "onlyemacs" onlyemacsScript)
                  # alsa-utils
                  # bear
                  # binutils
                  # bison
                  # bpftrace
                  # btop
                  # cachix
                  # ccls
                  # clang
                  # clang-tools
                  # codespell
                  # conan
                  # cpio
                  # cppcheck
                  # ctags
                  # doxygen
                  # elfutils
                  # elfutils.dev
                  # file
                  # flex
                  # foliate
                  # fzf
                  # gcc-arm-embedded
                  # gtest
                  # kmod
                  # lcov
                  # libelf
                  # libtool
                  # libvterm
                  # meson
                  # ncurses
                  # ncurses.dev
                  # nil
                  # ninja
                  # nixfmt-classic
                  # opencc
                  # openssl
                  # openssl.dev
                  # pahole
                  # pandoc
                  # pciutils
                  # qemu
                  # qemu-utils
                  # rr
                  # samba
                  # satty
                  # scc
                  # texlab
                  # texliveFull
                  # tree
                  # unstable.gemini-cli-bin
                  # unstable.quickemu
                  # usbutils
                  # usbutils
                  # util-linux
                  # vcpkg
                  # vcpkg-tool
                  # vim-full
                  age
                  bc
                  cliphist
                  cmake
                  coreutils-full
                  curl
                  dash
                  fd
                  fishPlugins.done
                  gcc
                  gdb
                  gnumake
                  grim
                  jq
                  just
                  libnotify
                  mako
                  paperlike-go
                  poppler-utils
                  python3
                  ripgrep
                  sdcv
                  slurp
                  thunderbird
                  trashy
                  unrar-free
                  unstable.leetgo
                  unstable.sops
                  unzipNLS
                  wf-recorder
                  wget
                  wl-clipboard
                  wl-color-picker
                  wmenu
                  wtype
                  zip

                  # cargo
                  # rust-analyzer
                  # rustc
                  # rustlings
                  # unstable.nix-search-cli
                  # unstable.lldb

                  c-intro-and-ref
                  glibcInfo
                  man-pages
                  man-pages-posix
                  # stdmanpages
                  # clang-manpages
                  # linux-manual

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
                    DESKTOP=Downloads
                    DOCUMENTS=Downloads
                    DOWNLOAD=Downloads
                    MUSIC=Downloads
                    PICTURES=Downloads
                    PUBLICSHARE=Downloads
                    TEMPLATES=Downloads
                    VIDEOS=Downloads
                  '';
                };
                environment.sessionVariables = {
                  XDG_CACHE_HOME = "$HOME/.cache";
                  XDG_CONFIG_HOME = "$HOME/.config";
                  XDG_DATA_HOME = "$HOME/.local/share";
                  XDG_BIN_HOME = "$HOME/.local/bin/";
                  __GL_GSYNC_ALLOWED = "0";
                  __GL_VRR_ALLOWED = "0";
                  WLR_DRM_NO_ATOMIC = "1";
                  _JAVA_AWT_WM_NONREPARENTING = "1";
                  QT_QPA_PLATFORM = "wayland;xcb";
                  GDK_BACKEND = "wayland,x11";
                  WLR_NO_HARDWARE_CURSORS = "1";
                  MOZ_ENABLE_WAYLAND = "1";
                  WLR_BACKEND = "vulkan";
                  WLR_RENDERER = "vulkan";
                  XCURSOR_SIZE = "24";
                  NIXOS_OZONE_WL = "1";
                  GTK_USE_PORTAL = "1";
                  # PATH = [
                  #   "$HOME/.local/bin/:$PATH"
                  # ];
                };
                security.polkit.extraConfig = ''
                  /* Allow users in wheel group to manage systemd units without authentication */
                  polkit.addRule(function(action, subject) {
                      if (action.id == "org.freedesktop.systemd1.manage-units" &&
                          subject.isInGroup("wheel")) {
                          return polkit.Result.YES;
                          }
                  });

                  /* Allow users in wheel group to run programs with pkexec without authentication */
                  polkit.addRule(function(action, subject) {
                      if (action.id == "org.freedesktop.policykit.exec" &&
                          subject.isInGroup("wheel")) {
                          return polkit.Result.YES;
                          }
                  });
                '';
                environment.shellAliases = {
                  np =
                    "nix-shell -p  --option substituters 'https://mirrors.ustc.edu.cn/nix-channels/store  https://cache.nixos.org'";
                  gcl = "git clone --depth 1";
                  minipdf="np texlive.combined.scheme-full --run 'pdfcrop input.pdf output.pdf'";
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
                  # rm = "rm -vI";
                  rm = "trash -c never ";
                  # bc = "bc -ql";
                  mkd = "mkdir -pv";
                };
                programs.bash.interactiveShellInit = ''
                  eval "$(zoxide init bash)"
                '';
                programs.foot = {
                  enable = true;
                  settings = {
                    main = {
                      font = "FiraCode Font:size=12";
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
                home-manager = {
                  extraSpecialArgs = { inherit inputs outputs userSetting; };
                  useUserPackages = true;
                  users = {
                    ${userSetting.username} = {
                      home.pointerCursor = {
                        gtk.enable = true;
                        package = pkgs.bibata-cursors;
                        name = "Bibata-Modern-Ice";
                        size = 32;
                      };
                      services.cliphist.enable = true;
                      home.file.".local/share/fonts/bookerly" = {
                        source = pkgs.fetchFromGitHub {
                          owner = "NestorLiao";
                          repo = "boboly";
                          rev = "master";
                          sha256 =
                            "sha256-boN16BYYFY5MWhbLhaqIpumqsi583fIrMp+2Hz3pzqQ=";
                        };
                      };
                      home.file.".local/share/fonts/sourcehan" = {
                        source = pkgs.fetchFromGitHub {
                          owner = "NestorLiao";
                          repo = "sourcehan";
                          rev = "master";
                          sha256 =
                            "sha256-uyZcPv7jPv96KovMKFdp/qDv0/6b3x0We/vx7eIU6O4=";
                        };
                      };
                      home.file.".local/share/mysource" = {
                        source = pkgs.fetchFromGitHub {
                          owner = "NestorLiao";
                          repo = "mutable-config";
                          rev = "master";
                          sha256 =
                            # lib.fakeSha256;
                            "sha256-K8d4b1AkKonzYVGpyDrRFPgUqombROJXHqBVIGBoM9A=";
                        };
                      };
                      home.file.".stardict/dic" = {
                        source = pkgs.stdenv.mkDerivation {
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
                          buildInputs =
                            [ pkgs.unzip ]; # Add unzip as a build input
                          unpackPhase = ''
                            # Unzip each dictionary zip file
                            unzip $src/stardict-ghycyzzd-2.4.2.zip -d $out
                            unzip $src/stardict-langdao-ce-gb-2.4.2.zip -d $out
                            unzip $src/stardict-ecdict-2.4.2.zip -d $out
                          '';
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
                      programs.helix = {
                        settings = {
                          theme = "eink";
                          # theme = "emacs";
                          editor = {
                            lsp = {
                              display-messages = true;
                              auto-signature-help =
                                false; # https://github.com/helix-editor/helix/discussions/6710
                            };
                            # gutters = ["diagnostics" "spacer" "diff"];
                            jump-label-alphabet = "gftnseriaodhcjxkblpvuwy";
                            bufferline = "multiple";
                            auto-info = true;
                            auto-save = true;
                            statusline = {
                              left = [
                                "mode"
                                "spinner"
                                "file-name"
                                "diagnostics"
                                "position-percentage"
                                "position"
                                "version-control"
                              ];
                              right = [ ];
                              mode = {
                                normal = "修";
                                insert = "入";
                                select = "选";
                              };
                            };
                            auto-pairs = {
                              "(" = ")";
                              "{" = "}";
                              "[" = "]";
                              "\"" = ''"'';
                              "`" = "`";
                            };
                            soft-wrap = {
                              enable = false;
                              max-wrap = 25;
                              max-indent-retain = 0;
                              wrap-indicator = "";
                            };
                            indent-guides = {
                              render = false;
                              character = "╎";
                              skip-levels = 1;
                            };
                            line-number = "relative";
                            mouse = true;
                            scrolloff = 0;
                          };
                        };
                        enable = true;
                        defaultEditor = true;
                        themes = {
                          eink = let
                            white = "#FFFFFF";
                            black = "#000000";
                          in {
                            # line
                            # curl
                            # dashed
                            # dotted
                            # double_line

                            "ui.background" = { bg = white; };
                            "ui.text" = black;
                            "ui.selection" = {
                              bg = white;
                              fg = black;
                              # modifiers = ["bold"];
                              underline = {
                                color = black;
                                style = "dashed";
                              };
                            };
                            "ui.cursorline" = { bg = black; };
                            "ui.statusline" = {
                              bg = white;
                              fg = black;
                            };
                            "ui.virtual.ruler" = { bg = black; };
                            "ui.cursor.match" = {
                              fg = white;
                              bg = black;
                            };
                            "ui.cursor" = {
                              fg = white;
                              bg = black;
                              # modifiers = ["bold"];
                              underline = {
                                color = white;
                                style = "curl";
                                # style = "dashed";
                              };
                            };
                            "ui.cursorline.primary" = { bg = black; };
                            "ui.linenr" = { fg = black; };
                            "ui.linenr.selected" = {
                              fg = black;
                              bg = white;
                            };
                            "ui.menu" = {
                              bg = white;
                              fg = black;
                            };
                            "ui.menu.selected" = { bg = white; };
                            "ui.popup" = { bg = white; };
                            "ui.popup.info" = {
                              bg = white;
                              fg = black;
                            };
                            "ui.help" = {
                              bg = white;
                              fg = black;
                            };
                            "ui.window" = { bg = white; };
                            "ui.statusline.normal" = {
                              fg = black;
                              bg = white;
                            };
                            "ui.statusline.insert" = {
                              fg = black;
                              bg = white;
                            };
                            "ui.statusline.select" = {
                              fg = black;
                              bg = white;
                            };
                            "diagnostic.error" = {
                              underline = {
                                color = black;
                                style = "curl";
                              };
                            };
                            "diagnostic.warning" = {
                              underline = {
                                color = black;
                                style = "curl";
                              };
                            };
                            "diagnostic.info" = {
                              underline = {
                                color = black;
                                style = "curl";
                              };
                            };
                            "diagnostic.hint" = {
                              underline = {
                                color = black;
                                style = "curl";
                              };
                            };
                            "constant.numeric" = {
                              fg = black;
                              modifiers = [ "italic" ];
                            };
                            "constant.builtin" = { fg = black; };
                            "keyword" = { fg = black; };
                            "keyword.control" = {
                              fg = black;
                              # modifiers = ["bold"];
                            };
                            "keyword.function" = {
                              fg = black;
                              # modifiers = ["bold"];
                            };
                            "function" = { fg = black; };
                            "function.macro" = {
                              fg = black;
                              # modifiers = ["bold"];
                            };
                            "function.method" = { fg = black; };
                            "function.builtin" = { fg = black; };
                            "variable.builtin" = { fg = black; };
                            "variable.other" = { fg = black; };
                            "variable" = { fg = black; };
                            "string" = black;
                            "comment" = {
                              fg = black;
                              modifiers = [ "italic" ];
                            };
                            "namespace" = { fg = black; };
                            "attribute" = { fg = black; };
                            "type" = {
                              fg = black;
                              # modifiers = ["bold"];
                            };
                            "markup.heading" = {
                              fg = black;
                              modifiers = [ "bold" ];
                            };
                            "markup.raw" = { fg = black; };
                            "markup.link.url" = { fg = black; };
                            "markup.link.text" = { fg = black; };
                            "markup.quote" = {
                              fg = black;
                              modifiers = [ "italic" ];
                            };
                            "markup.bold" = {
                              fg = black;
                              modifiers = [ "bold" ];
                            };
                            "markup.italic" = {
                              fg = black;
                              modifiers = [ "italic" ];
                            };
                            "markup.inline" = {
                              fg = black;
                              modifiers = [ "italic" ];
                            };
                            "diff.plus" = { fg = black; };
                            "diff.delta" = { fg = black; };
                            "diff.minus" = { fg = black; };
                          };
                        };
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
                      # home.file.".config/nixpkgs/config.nix".text = ''
                      #   {
                      #     allowUnfree = true;
                      #   }
                      # '';
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
                      #~/.mozilla/firefox/profiles.ini
                      # [General]
                      # StartWithLastProfile=1
                      # Version=2
                      #
                      # [Profile0]
                      # Default=1
                      # IsRelative=1
                      # Name=firefox
                      # Path=firefox
                      ### xxxx
                      programs.firefox = {
                        package = pkgs.firefox-beta;
                        enable = true;
                        languagePacks = ["en-US"];
                        # Check about:policies#documentation for options.
                        profiles = {
                          default = {
                            id = 0;
                            name = "firefox";
                            isDefault = true;
                            search.default = "Nix Packages";
                            search.force = true;
                            search.engines = {
                              "Rust Crates (lib.rs)" = {
                                urls = [{
                                  template = "https://lib.rs/search?q={searchTerms}";
                                }];
                                definedAliases = [ "@lr" ];
                              };

                              "Rust Std Documentation" = {
                                urls = [{
                                  template = "https://doc.rust-lang.org/std/?search={searchTerms}";
                                }];
                                definedAliases = [ "@rs" ];
                              };

                              "Crates.io" = {
                                urls = [{
                                  template = "https://crates.io/crates/{searchTerms}";
                                }];
                                definedAliases = [ "@ci" ];
                              };

                              "Google (Filtered, HK)" = {
                                urls = [{
                                  template = "https://www.google.com.hk/search?q=-youtube+-reddit+-dailymotion+-pbslearningmedia+-dcnewsnow+-ted+-facebook+-douyin+-cctv+-bilibili+-iqiyi+-youku+-tencentvideo+-tiktok+-instagram+-twitter+-cnn+-yahoo+-aljazeera+-foxnews+{searchTerms}";
                                }];
                                definedAliases = [ "@g" "@gg" ];
                              };

                              "Nix Packages" = {
                                urls = [{
                                  template = "https://search.nixos.org/packages?type=packages&channel=unstable&query={searchTerms}";
                                }];
                                icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                                definedAliases = [ "@np" ];
                              };

                              "NixOS Options" = {
                                urls = [{
                                  template = "https://search.nixos.org/options?type=options&channel=unstable&query={searchTerms}";
                                }];
                                icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                                definedAliases = [ "@no" ];
                              };

                              "NixOS Discourse" = {
                                urls = [{
                                  template = "https://discourse.nixos.org/search?q={searchTerms}";
                                }];
                                definedAliases = [ "@nd" ];
                              };

                              "Nixpkgs PR Tracker" = {
                                urls = [{
                                  template = "https://nixpk.gs/pr-tracker.html?pr={searchTerms}";
                                }];
                                definedAliases = [ "@npt" ];
                              };

                              "Arch Wiki" = {
                                urls = [{
                                  template = "https://wiki.archlinux.org/title/{searchTerms}";
                                }];
                                definedAliases = [ "@aw" ];
                              };

                              "NixOS Wiki" = {
                                urls = [{
                                  template = "https://nixos.wiki/index.php?search={searchTerms}";
                                }];
                                icon = "https://nixos.wiki/favicon.png";
                                updateInterval = 24 * 60 * 60 * 1000;
                                definedAliases = [ "@nw" ];
                              };

                              "Python Docs (zh-cn)" = {
                                urls = [{
                                  template = "https://docs.python.org/zh-cn/3/search.html?q={searchTerms}";
                                }];
                                definedAliases = [ "@py" ];
                              };

                              "Stack Overflow" = {
                                urls = [{
                                  template = "https://stackoverflow.com/search?q={searchTerms}";
                                }];
                                definedAliases = [ "@so" ];
                              };

                              "GitHub Repositories" = {
                                urls = [{
                                  template = "https://github.com/search?q={searchTerms}&type=repositories";
                                }];
                                definedAliases = [ "@gp" ];
                              };

                              "GitHub Nix Code" = {
                                urls = [{
                                  template = "https://github.com/search?q=language:nix+{searchTerms}&type=code";
                                }];
                                definedAliases = [ "@gcn" ];
                              };

                              "GitHub Emacs Lisp Code" = {
                                urls = [{
                                  template = "https://github.com/search?q=language:\"Emacs Lisp\"+{searchTerms}&type=code";
                                }];
                                definedAliases = [ "@gce" ];
                              };

                              "GitHub Rust Code" = {
                                urls = [{
                                  template = "https://github.com/search?q=language:Rust+{searchTerms}&type=code";
                                }];
                                definedAliases = [ "@gcr" ];
                              };

                              "GitHub Lua Code" = {
                                urls = [{
                                  template = "https://github.com/search?q=language:Lua+{searchTerms}&type=code";
                                }];
                                definedAliases = [ "@gcl" ];
                              };

                              "GitHub Python Code" = {
                                urls = [{
                                  template = "https://github.com/search?q=language:Python+{searchTerms}&type=code";
                                }];
                                definedAliases = [ "@gcp" ];
                              };

                              "GitHub C Code" = {
                                urls = [{
                                  template = "https://github.com/search?q=language:C+{searchTerms}&type=code";
                                }];
                                definedAliases = [ "@gcc" ];
                              };

                              "GitHub C++ Code" = {
                                urls = [{
                                  template = "https://github.com/search?q=language:C%2B%2B+{searchTerms}&type=code";
                                }];
                                definedAliases = [ "@gcpp" ];
                              };

                              "GitHub Issues" = {
                                urls = [{
                                  template = "https://github.com/search?type=issues&q={searchTerms}";
                                }];
                                definedAliases = [ "@gi" ];
                              };

                              "Nixpkgs Source Search" = {
                                urls = [{
                                  template = "https://github.com/search?q=repo:NixOS/nixpkgs+{searchTerms}&type=code";
                                }];
                                definedAliases = [ "@npk" ];
                              };

                              "Taobao" = {
                                urls = [{
                                  template = "https://s.taobao.com/search?q={searchTerms}";
                                }];
                                definedAliases = [ "@tb" ];
                              };

                              "Tiger Code (虎码)" = {
                                urls = [{
                                  template = "https://tiger-code.com/search?query={searchTerms}";
                                }];
                                definedAliases = [ "@tg" ];
                              };

                              "OSHWHub" = {
                                urls = [{
                                  template = "https://oshwhub.com/search?wd={searchTerms}";
                                }];
                                definedAliases = [ "@jlc" ];
                              };

                              "Thingiverse" = {
                                urls = [{
                                  template = "https://www.thingiverse.com/search?q={searchTerms}&page=1";
                                }];
                                definedAliases = [ "@th" ];
                              };

                              "Cambridge Dictionary (EN→ZH)" = {
                                urls = [{
                                  template = "https://dictionary.cambridge.org/zhs/词典/英语-汉语-简体/{searchTerms}";
                                }];
                                definedAliases = [ "@dc" ];
                              };

                              "bing".metaData.hidden = true;
                            };

                            userChrome =''
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
                            userContent = ''
/* =========================================================
   LEERE / ZEN — Pure Black & White userContent.css
   Typeface: Bookerly
   ========================================================= */

/* -------- Global reset -------- */

* {
	scrollbar-width: none !important;
}

* {
  background-color: #ffffff !important;
  color: #000000 !important;
  font-family: "Bookerly", serif !important;
  box-shadow: none !important;
  text-shadow: none !important;
  border-radius: 0 !important;
}

/* -------- Body & text -------- */

body {
  background-image: none !important;
  font-size: 18px !important;
  line-height: 1.7 !important;
  letter-spacing: 0.01em !important;
}

/* Headings: hierarchy by size & weight only */

h1, h2, h3, h4, h5, h6 {
  font-weight: 600 !important;
  line-height: 1.3 !important;
  margin-top: 1.6em !important;
  margin-bottom: 0.6em !important;
}

/* Paragraph spacing */

p {
  margin-top: 0.6em !important;
  margin-bottom: 0.6em !important;
}

/* -------- Links -------- */

a {
  text-decoration: none !important;
  font-weight: 500 !important;
}

/* Subtle link indication on hover only */

a:hover {
  text-decoration: underline !important;
}

/* -------- Buttons & interactive elements -------- */

button,
input,
select,
textarea {
  background-color: #ffffff !important;
  color: #000000 !important;
  border: 1px solid #000000 !important;
  font-family: "Bookerly", serif !important;
}

/* Remove excessive focus/hover effects */

button:hover,
input:hover,
textarea:hover {
  background-color: #ffffff !important;
}

/* Clear, non-distracting focus state */

:focus {
  outline: 2px solid #000000 !important;
  outline-offset: 2px !important;
}

/* -------- Icons & SVG -------- */

svg,
svg * {
  fill: #000000 !important;
  stroke: #000000 !important;
}

/* -------- Media -------- */

img,
video,
iframe {
  max-width: 100% !important;
  height: auto !important;
  filter: grayscale(100%) contrast(120%) !important;
}

/* -------- Lists -------- */

ul,
ol {
  padding-left: 1.5em !important;
}

li {
  margin-bottom: 0.4em !important;
}

/* -------- Tables -------- */

table {
  border-collapse: collapse !important;
}

th,
td {
  border: 1px solid #000000 !important;
  padding: 0.4em 0.6em !important;
}

/* -------- Remove visual noise -------- */

hr {
  border: none !important;
  border-top: 1px solid #000000 !important;
  margin: 2em 0 !important;
}

/* Disable animations & transitions */

* {
  transition: none !important;
  animation: none !important;
}

'';
                          };
                        };
                        policies = {
                          # Copied from https://discourse.nixos.org/t/declare-firefox-extensions-and-settings/36265
                          DisableDeveloperTools = false;
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
                            "/home/${userSetting.username}/Downloads";
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
                          PasswordManagerEnabled = true;
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
                          SearchEngines.Default = "ebay";
                          # DisableFormHistory = true;
                          DisableFormHistory = false;
                          AppAutoUpdate = false;
                          DisableAppUpdate = true;
                          BlockAboutAddons = false;

                          # rsop; rm .mozilla/; firefox-beta
                          ExtensionSettings = {
                            "*".installation_mode = "allowed"; # blocks all addons except the ones specified below

                            # Firefox Invert Colors
                            "firefoxinvertcolors" = {
                              installation_mode = "force_installed";
                              install_url = "https://addons.mozilla.org/firefox/downloads/latest/firefox-invert-colors/latest.xpi";
                            };

                            # Authenticator
                            "authenticator@mymindstorm" = {
                              installation_mode = "force_installed";
                              install_url = "https://addons.mozilla.org/firefox/downloads/latest/authenticator/latest.xpi";
                            };

                            # Bar Breaker
                            "bar-breaker@ris58h" = {
                              installation_mode = "force_installed";
                              install_url = "https://addons.mozilla.org/firefox/downloads/latest/bar-breaker/latest.xpi";
                            };

                            # ChatGPT Ctrl+Enter Sender
                            "chatgpt-ctrl-enter-sender@chatgpt-extension.io" = {
                              installation_mode = "force_installed";
                              install_url = "https://addons.mozilla.org/firefox/downloads/latest/chatgpt-ctrl-enter-sender/latest.xpi";
                            };

                            # IDCAC
                            "idcac-pub@guus.ninja" = {
                              installation_mode = "force_installed";
                              install_url = "https://addons.mozilla.org/firefox/downloads/latest/idcac/latest.xpi";
                            };

                            # HTTPS Everywhere (legacy Jetpack ID)
                            "jid1-BoFifL9Vbdl2zQ@jetpack" = {
                              installation_mode = "force_installed";
                              install_url = "https://addons.mozilla.org/firefox/downloads/latest/https-everywhere/latest.xpi";
                            };

                            # No Emoji
                            "no-emoji@erikdesjardins.io" = {
                              installation_mode = "force_installed";
                              install_url = "https://addons.mozilla.org/firefox/downloads/latest/no-emoji/latest.xpi";
                            };

                            # uBlock Origin
                            "uBlock0@raymondhill.net" = {
                              installation_mode = "force_installed";
                              install_url = "https://addons.mozilla.org/firefox/downloads/latest/ublock-origin/latest.xpi";
                            };

                            # Vimium C
                            "vimium-c@gdh1995.cn" = {
                              installation_mode = "force_installed";
                              install_url = "https://addons.mozilla.org/firefox/downloads/latest/vimium-c/latest.xpi";
                            };
                          };
                          Preferences = {
                            "accessibility.force_disabled" =1;
                            "app.update.auto" = lock-false;
                            "app.update.download.promptMaxAttempts" = 0;
                            "app.update.elevation.promptMaxAttempts" = 0;
                            "app.update.service.enabled" = lock-false;
                            "browser.aboutConfig.showWarning" = lock-false;
                            "browser.aboutwelcome.enabled" = lock-false;
                            "browser.accessibility.typeaheadfind" = lock-false;
                            "browser.anchor_color" = "#000000";
                            "browser.cache.disk.enable" =lock-true;
                            "browser.ctrlTab.recentlyUsedOrder" = lock-true;
                            "browser.display.document_color_use" = 2;
                            "browser.display.use_document_fonts" = 0;
                            "browser.download.dir" = "/home/${userSetting.username}/Downloads";
                            "browser.download.forbid_open_with" = lock-true;
                            "widget.use-xdg-desktop-portal.file-picker" = 1;
                            "browser.download.lastDir" = "/home/${userSetting.username}/Downloads";
                            "browser.download.open_pdf_attachments_inline" =lock-true;
                            "browser.download.start_downloads_in_tmp_dir" =lock-true;
                            "browser.newtabpage.activity-stream.showWeather" =lock-false;
                            "browser.newtabpage.enabled" = lock-false;
                            "browser.quitShortcut.disabled" = lock-true;
                            "browser.safebrowsing.downloads.enabled" = lock-false;
                            "browser.safebrowsing.malware.enabled" = lock-false;
                            "browser.safebrowsing.phishing.enabled" = lock-false;
                            "browser.search.region" = "US";
                            "browser.sessionstore.max_tabs_undo" =3;
                            "browser.sessionstore.restore_on_demand" =lock-true;
                            "browser.sessionstore.warnOnQuit" = lock-true;
                            "browser.slowStartup.notificationDisabled" =lock-true;
                            # instaed of blank.org or github, they are not reliable for chinese and eink user.
                            "browser.startup.homepage" = "https://search.nixos.org";
                            "browser.tabs.closeTabByDblclick" = lock-true;
                            "browser.tabs.closeWindowWithLastTab" = lock-false;
                            "browser.tabs.loadInBackground" = lock-true;
                            "browser.tabs.tabClipWidth" = 999;
                            "browser.tabs.warnOnClose" = lock-false;
                            "browser.toolbars.bookmarks.visibility" = "never";
                            "browser.translations.automaticallyPopup" = lock-false;
                            "browser.uidensity" = 1;
                            "browser.urlbar.oneOffSearches" = lock-false;
                            "browser.urlbar.shortcuts.tabs" = lock-false;
                            "browser.urlbar.suggest.quicksuggest.sponsored" = lock-false;
                            "browser.visited_color" = "#000000";
                            "canvas.capturestream.enabled" = lock-false;
                            "datareporting.healthreport.service.enabled" = lock-false;
                            "devtools.chrome.enabled" = lock-true;
                            "devtools.debugger.remote-enabled" = lock-true;
                            "dom.ipc.plugins.enabled" = lock-false;
                            "dom.ipc.plugins.enabled.libflashplayer.so" = lock-false;
                            "dom.media.mediasource.enabled" = lock-false;
                            "dom.mozTCPSocket.enabled" = lock-false;
                            "dom.netinfo.enabled" = lock-false;
                            "dom.webaudio.enabled" = lock-false;
                            "dom.webvtt.enabled" = lock-false;
                            "extensions.activeThemeID" = "{5f71ffe3-23e2-49b8-b75e-2c032ef4a1d9}";
                            "extensions.pocket.enabled" = lock-false;
                            "extensions.update.enabled" = lock-false;
                            "extensions.webextensions.restrictedDomains" = "";
                            "font.name-list.emoji" = "";
                            "font.name.monospace.x-western" ="JetBrainsMono Nerd Font Mono";
                            "full-screen-api.transition.timeout" = 0;
                            "full-screen-api.warning.delay" = 0;
                            "full-screen-api.warning.timeout" = 0;
                            "general.smoothScroll" = lock-false;
                            "general.useragent.compatMode.firefox" = lock-true;
                            "geo.enabled" = lock-false;
                            "gfx.font_rendering.fontconfig.max_generic_substitutions" = 127;
                            # "gfx.webrender.all" = lock-false;
                            "image.jxl.enabled" = lock-true;
                            "layout.css.prefers-color-scheme.content-override" = 1;
                            "browser.ml.linkPreview.enabled" = lock-false;
                            "media.autoplay.block-event.enabled" = lock-true;
                            "media.autoplay.default" = 2;
                            "media.videocontrols.picture-in-picture.keyboard-controls.enabled" = lock-false;
                            "media.av1.enabled" = lock-false;
                            "media.block-autoplay-until-in-foreground" = lock-true;
                            "media.block-play-until-visible" = lock-true;
                            "media.disabled" =lock-true;
                            "media.eme.enabled" = lock-false;
                            "media.ffmpeg.enabled" = lock-false;
                            "media.ffmpeg.vaapi.enabled" = lock-true;
                            "media.ffvpx.enabled" = lock-false;
                            "media.flac.enabled" = lock-false;
                            "media.getusermedia.screensharing.enabled" = lock-false;
                            "media.gmp-widevinecdm.enabled" = lock-false;
                            "media.h264.enabled" = lock-false;
                            "media.libvpx.enabled" = lock-false;
                            "media.mediasource.enabled" = lock-false;
                            "media.mediasource.mp4.enabled" = lock-false;
                            "media.mediasource.webm.enabled" = lock-false;
                            "media.mediasource.whitelist" = lock-false;
                            "media.mp4.enabled" = lock-false;
                            "media.navigator.audio.enabled" = lock-false;
                            "media.navigator.enabled" = lock-false;
                            "media.navigator.video.enabled" = lock-false;
                            "media.ogg.enabled" = lock-false;
                            "media.peerconnection.enabled" =lock-false;
                            "media.peerconnection.ice.default_address_only" =lock-true;
                            "media.wave.enabled" = lock-false;
                            "media.webm.enabled" = lock-false;
                            "media.webspeech.recognition.enable" = lock-false;
                            "media.webspeech.synth.enabled" = lock-false;
                            "network.dns.echconfig.enabled" = lock-true;
                            "network.dns.http3_echconfig.enabled" = lock-true;
                            "permissions.default.image" = 2;
                            "places.history.enabled" = lock-true;
                            "plugin.state.flash" = 0 ;
                            "privacy.donottrackheader.enabled" = lock-true;
                            "privacy.resistFingerprinting" = lock-true;
                            "privacy.resistFingerprinting.block_mozAddonManager" = lock-true;
                            "privacy.trackingprotection.cryptomining.enabled" =lock-true;
                            "privacy.trackingprotection.enabled" = lock-true;
                            "privacy.trackingprotection.fingerprinting.enabled" = lock-true;
                            "privacy.trackingprotection.socialtracking.enabled" = lock-true;
                            "reader.parse-on-load.enabled" = lock-false;
                            "security.enterprise_roots.auto-enabled" = lock-false;
                            "security.enterprise_roots.enabled" = lock-false;
                            "services.sync.engine.history" = lock-true;
                            "services.sync.prefs.sync-seen.services.sync.prefs.sync.capability.policy.maonoscript.sites" =lock-true;
                            "services.sync.prefs.sync.capability.policy.maonoscript.sites" =lock-true;
                            "signon.autofillForms" = lock-false;
                            # "signon.rememberSignons" = lock-true;
                            "svg.context-properties.content.enabled" = lock-true;
                            "toolkit.crashreporter.dataDirectory" = "";
                            "toolkit.crashreporter.enabled" = lock-false;
                            "toolkit.telemetry.archive.enabled" = lock-false;
                            "toolkit.telemetry.enabled" = lock-false;
                            "toolkit.telemetry.rejected" = lock-false;
                            "toolkit.telemetry.unified" = lock-false;
                            # Convenience
                            "browser.formfill.enable" = lock-false;
                            "browser.search.suggest.enabled" = lock-false;
                            "browser.search.suggest.enabled.private" = lock-false;
                            "browser.urlbar.suggest.searches" = lock-false;
                            "browser.urlbar.showSearchSuggestionsFirst" = lock-false;
                            "signon.rememberSignons" = lock-false; # Stop asking to save passwords
                            "extensions.formautofill.addresses.capture.enabled" = lock-false; # Stop asking to save addresses
                            # Styling
                            "browser.compactmode.show" = lock-true;
                            "toolkit.legacyUserProfileCustomizations.stylesheets" = lock-true; # This is needed for other userX.css files
                            # Containers
                            "privacy.userContext.enabled" = lock-true;
                            "privacy.userContext.ui.enabled" = lock-true;
                            # Downloads
                            "browser.download.useDownloadDir" = lock-true;
                            "browser.download.always_ask_before_handling_new_types" = lock-true;
                            # Privacy
                            "privacy.sanitize.sanitizeOnShutdown" = lock-true;
                            "privacy.clearOnShutdown_v2.cache" = lock-true;
                            "privacy.clearOnShutdown_v2.historyFormDataAndDownloads" = lock-true;
                            "privacy.clearOnShutdown_v2.browsingHistoryAndDownloads" = lock-true;
                            "privacy.clearOnShutdown_v2.downloads" = lock-true;
                            "privacy.clearOnShutdown_v2.formdata" = lock-true;
                            "privacy.clearOnShutdown_v2.cookiesAndStorage" = lock-true;
                            # # HTTPS only
                            # # "dom.security.https_only_mode" = lock-true;
                            "ui.key.menuAccessKey" = 17;
                            "ui.key.menuAccessKeyFocuses" = lock-false;
                            "webgl.disabled" = lock-true;
                            "widget.non-native-theme.scrollbar.style" = 3;
                            "xpinstall.signatures.required" = lock-false;
                          };
                        };
                      };

                      programs.chromium = {
                        enable = false;
                        package = pkgs.ungoogled-chromium;
                      };
                      home.stateVersion = "25.11";
                    };
                  };
                };
                users.users = {
                  ${userSetting.username} = {
                    initialPassword = "t";
                    isNormalUser = true;
                    initialHashedPassword = lib.mkForce null;
                    openssh.authorizedKeys.keys = [
                      # Add your SSH public key(s) here, if you plan on using SSH to connect
                    ];
                    shell = pkgs.fish;
                        extraGroups = [
                          "wheel" "audio" "dialout" "video" "disk"
                          "adm" "tty" "systemd-journal" "docker"
                          "networkmanager" "cdrom" "lp" "networkmanager" "i2c"
                        ];
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
                  PATH = "$PATH:$HOME/.config/bin:$HOME/.local/bin";
                  SOPS_AGE_KEY_FILE = "/etc/nixos/keys.txt";
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
                    fira-code
                    noto
                    ubuntu
                    ubuntu-mono
                    dejavu_fonts
                    noto-fonts-emoji-blob-bin
                  ];
                fonts.fontconfig = let
                  sansFallback = [
                    "Fira Code Font"
                    "Terminess Font"
                    "Ubuntu"
                    "Ubuntu Mono"
                  ];
                  serifFallback = [
                    "Fira Code Font"
                    "Terminess Font"
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
                sops.age.keyFile = "/etc/nixos/keys.txt";
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
                sops.secrets.nixAccessTokens = {
                  mode = "0440";
                  group = config.users.groups.keys.name;
                };
                sops.secrets.github_apikey = { owner = userSetting.username; };
                sops.secrets.tavily_apikey = { owner = userSetting.username; };
                sops.secrets.mojie = { owner = userSetting.username; };
                sops.secrets.oney = { owner = userSetting.username; };
                sops.secrets.ouo = { owner = userSetting.username; };
                networking.extraHosts = ''
                  # 其嗜欲深者，其天机浅
                  # 消费主义让我们沉迷于物质/精神消费中，
                  # 通过让我们接触各种光怪陆离的东西来丰富我们的身份认同感，
                  # 这也是当今时代互联网正在加速实现的事情…
                  # 但这是以牺牲掌握任何技能为代价换来的，
                  # 我们沉迷得越深，想要掌握一项技能的愿望就会越来越淡化。
                  127.0.0.1 c.doc
                  127.0.0.1 cpp.doc
                  127.0.0.1 linux.doc
                  0.0.0.0 emacs-china.org
                  0.0.0.0 chatgpt.com
                  0.0.0.0 www.google.com.hk
                  0.0.0.0 google.com.hk

                '';
                services = {
                  irqbalance.enable = false;
                  udev.extraRules = ''
                    KERNEL=="i2c-[0-9]*", GROUP="i2c", MODE="0777"
                  '';
                  xserver.videoDrivers = [ "modesetting" ];
                  udisks2.mountOnMedia = true;
                  udisks2.enable = true;
                  gvfs.enable = true;
                  devmon.enable = true;
                  # 修改音频设备在空闲时自动挂起（suspend）的行为
                  power-profiles-daemon.enable = false;
                  openssh = {
                    enable = true;
                    ports = [ 443 ];
                    settings = {
                      PasswordAuthentication = true;
                      AllowUsers =
                        null; # Allows all users by default. Can be [ "user1" "user2" ]
                      UseDns = true;
                      X11Forwarding = false;
                      PermitRootLogin =
                        "prohibit-password"; # "yes", "without-password", "prohibit-password", "forced-commands-only", "no"
                    };
                  };
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
                    enable = true;
                    disableTxChecksumIpGeneric = false;
                    package = pkgs.unstable.dae;
                    configFile =
                      "/home/${userSetting.username}/.config/dae/config.dae";
                    assets = with pkgs.unstable; [
                      v2ray-geoip
                      v2ray-domain-list-community
                    ];
                    openFirewall = {
                      enable = false;
                      port = 12345;
                    };
                  };
                  nscd.enable = false;
                  # and system.nssModules = lib.mkForce [ ]; # for dnsmasq
                  dnsmasq = {
                    enable = true;
                    settings = {
                      addn-hosts = "/etc/hosts";
                      cache-size = 1000;
                      local-ttl = 3600;
                      server = [ "223.5.5.5"  "1.1.1.1"];
                    };
                    resolveLocalQueries = true;
                  };
                  greetd = {
                    enable = true;
                    settings = {
                      default_session.command =
                        "${pkgs.tuigreet}/bin/tuigreet --time --cmd ${userSetting.windowmanager} --theme 'text=black;container=white;prompt=black;input=black;border=black;title=black;greet=black;action=black;button=black;time=black'";
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
                      # "linux.doc" = {
                      #   listen = [{
                      #     addr = "0.0.0.0";
                      #     port = 3000;
                      #   }];
                      #   root = "${pkgs.linux-doc}/share/doc/linux-doc";
                      # };
                      "c.doc" = {
                        listen = [{
                          addr = "0.0.0.0";
                          port = 3001;
                        }];
                        root =
                          "${pkgs.cppreference-doc}/share/cppreference/doc/html/en/c";
                      };
                      "ccpp.doc" = {
                        listen = [{
                          addr = "0.0.0.0";
                          port = 3003;
                        }];
                        root =
                          "${pkgs.cppreference-doc}/share/cppreference/doc/html/en";
                      };
                      "cpp.doc" = {
                        listen = [{
                          addr = "0.0.0.0";
                          port = 3002;
                        }];
                        root =
                          "${pkgs.cppreference-doc}/share/cppreference/doc/html/en/cpp";
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
                    DBs = with pkgs.dictdDBs; [
                      # wiktionary
                      wordnet
                    ];
                  };
                  xserver.dpi = 192;
                  emacs = {
                    enable = true;
                    package = emacsWithPackages (epkgs:
                      (with epkgs.melpaStablePackages; [ ])
                      ++ (with epkgs.melpaPackages; [

                        ### black MAGIC to jumping everwhere
                        rg
                        dired-subtree
                        ztree
                        goto-chg

                        ## expand & edit
                        move-text
                        surround
                        expand-region
                        multiple-cursors
                        iedit
                        # shift-number
                        avy
                        yasnippet

                        ### Do Anything In Emacs©

                        #### project manager
                        magit
                        magit-todos
                        disproject
                        forge
                        consult-gh-embark
                        consult-gh-forge
                        consult-gh-with-pr-review
                        docker
                        kubernetes
                        git-link
                        git-timemachine
                        dumb-jump
                        cff

                        #### get term
                        vterm
                        multi-vterm
                        eshell-toggle
                        with-editor
                        #### read books
                        nov
                        pdf-tools
                        saveplace-pdf-view
                        #### chinese input method
                        pyim
                        #### read log/manual/doc/dict/gpt/... in emacs
                        # doxymacs
                        syslog-mode
                        journalctl-mode
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

                        ### for language fans, and YOU DON'T HAVE TO LEARN THOSE
                        cmake-mode
                        dockerfile-mode
                        go-mode
                        markdown-mode
                        nix-mode
                        cargo-mode
                        rust-mode
                        zig-mode
                        python-mode
                        yaml-mode

                        ### why I need below mode??? I am no cult dev.
                        haskell-mode
                        lua-mode
                        web-mode
                        racket-mode
                        elixir-mode
                        erlang
                        scala-mode
                        d-mode
                        glsl-mode
                        tuareg
                        less-css-mode
                        graphviz-dot-mode
                        clojure-mode
                        csharp-mode
                        nim-mode
                        jinja2-mode
                        purescript-mode
                        toml-mode
                        nginx-mode
                        kotlin-mode
                        php-mode
                        qml-mode
                        typescript-mode
                        rfc-mode

                        ### save and format
                        aggressive-indent
                        elisp-autofmt
                        super-save

                        ### emacs look and feel
                        hide-mode-line
                        no-emoji
                        ligature
                        compile-angel
                        # buffer-terminator
                        envrc

                        ### feel even better than drug
                        alert
                        trashed
                        wgrep
                        # easysession
                        undo-fu
                        undo-fu-session
                      ]) ++ (with epkgs.elpaPackages; [ plz ])
                      ++ (with pkgs; [ ]));
                  };
                };
                programs.nix-ld = {
                  enable = true;
                  libraries = with pkgs; [
                    # stdenv.cc.cc
                    # openssl.dev
                    # pkg-config
                    # autotools
                    # libxml2
                    # openssl
                    # openssl_3_4
                    # gcc13
                    # xorg.libXcomposite
                    # xorg.libXtst
                    # xorg.libXrandr
                    # xorg.libXext
                    # xorg.libX11
                    # xorg.libXfixes
                    # libGL
                    # libva
                    # pipewire.lib
                    # xorg.libxcb
                    # xorg.libXdamage
                    # xorg.libxshmfence
                    # xorg.libXxf86vm
                    # sqlite
                    # libelf
                    # libayatana-appindicator
                    # webkitgtk_4_1
                    # glib
                    # gtk2
                    # gtk3
                    # bzip2
                    # at-spi2-atk
                    # atkmm
                    # cairo
                    # gdk-pixbuf
                    # harfbuzz
                    # librsvg
                    # libsoup_3
                    # pango

                    # xorg.libXinerama
                    # xorg.libXcursor
                    # xorg.libXrender
                    # xorg.libXScrnSaver
                    # xorg.libXi
                    # xorg.libSM
                    # xorg.libICE
                    # nspr
                    # nss
                    # cups
                    # libcap
                    # SDL2
                    # libusb1
                    # dbus-glib
                    # ffmpeg
                    # xorg.libXt
                    # xorg.libXmu
                    # libogg
                    # libvorbis
                    # SDL
                    # SDL2_image
                    # glew110
                    # libidn
                    # tbb
                    # flac
                    # freeglut
                    # libjpeg
                    # libpng
                    # libpng12
                    # libsamplerate
                    # libmikmod
                    # libtheora
                    # libtiff
                    # pixman
                    # speex
                    # SDL_image
                    # SDL_mixer
                    # SDL2_mixer
                    # libappindicator-gtk2
                    # libappindicator-gtk3
                    # libdbusmenu-gtk2
                    # libindicator-gtk2
                    # libdbusmenu-gtk3
                    # libindicator-gtk3
                    # libcaca
                    # libcanberra
                    # libgcrypt
                    # util-linux
                    # libvpx
                    # xorg.libXft
                    # libvdpau
                    # atk
                    # fontconfig
                    # freetype
                    # dbus
                    # alsa-lib
                    # expat
                    # ncurses
                  ];
                };
                console = {
                  useXkbConfig = true;
                  earlySetup = true;
                  font = "${pkgs.terminus_font}/share/consolefonts/ter-132n.psf.gz";
                  packages = with pkgs; [ terminus_font ];
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
                security.polkit.enable = true;
                programs.sway = {
                  package = pkgs.sway;
                  enable = true;
                  wrapperFeatures.gtk = true;
                };
                programs.clash-verge = {
                  enable = false;
                  package = pkgs.unstable.clash-verge-rev;
                    };
                services.mihomo.tunMode = false;
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
                    sub_airport_1: \"$(cat ${config.sops.secrets.mojie.path})\"
                    # sub_airport_2: \"$(cat ${config.sops.secrets.ouo.path})\"
                    # sub_airport_3: \"$(cat ${config.sops.secrets.oney.path})\"
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
                    filter: name(keyword:'香港','Hong')
                    policy: min_moving_avg
                    }
                    usa {
                    filter: name(keyword:'美国','USA','unite')
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

                    # 禁用 h3，因为它通常消耗很多 CPU 和内存资源
                    l4proto(udp) && dport(443) -> block
                    dip(geoip:private) -> direct
                    dip(geoip:cn) -> direct

                    domain(
                    geosite:category-social-media-!cn,
                    geosite:category-social-media-cn,
                    geosite:category-media,
                    geosite:category-media-cn,
                    geosite:category-entertainment,
                    geosite:category-entertainment-cn,
                    geosite:category-porn,reddit.com) -> block

                    domain(geosite:category-ai-!cn) -> usa

                    domain(geosite:cn,api.tavily.com,ziggit.dev,api.deepseek.com,mirrors.ustc.edu.cn) -> direct
                    fallback: proxy
                    }
                                                    " > "$config_file"
                                                      # Set the ownership and permissions of the config file so the user can edit it
                                                      chown ${userSetting.username}: "$config_file"
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

                    output * bg /run/current-system/sw/share/backgrounds/sway/Sway_Wallpaper_Blue_768x1024_Portrait.png fill
                    output * transform 270

                    exec swayidle -w \
                         timeout 1100 'emacsclient --eval \"(about-emacs)\"' \
                         timeout 1200 'systemctl hibernate' \
                         before-sleep 'notify-send \"I am sleeping\"'

                    # Screenshots and screen recording
                    bindsym Print exec grim -g '\$(slurp)' - | wl-copy && wl-paste > ~/Downloads/Screenshot-\$(date +%F%T).png | notify-send \"Screenshot of the region taken\"
                    bindsym Shift+Print exec grim -g '\$(slurp -o -r -c '#ff0000ff')' -t ppm - | satty --filename - --fullscreen --output-filename ~/Downloads/satty-\$(date '+%Y%m%d-%H:%M:%S').png
                    bindsym Mod1+Shift+Print exec wf-recorder

                    # bindsym \$mod+Shift+j exec bash -c 'paperlike-cli -i2c /dev/i2c-4 -clear;swaylock -i ~/.config/sway/white.jpg;'

                    # Exit sway (logs you out of your Wayland session)
                    bindsym \$mod+Shift+q exec swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' -B 'Yes, exit sway' 'swaymsg exit'

                    # Reload config
                    bindsym \$mod+Shift+r reload
                    # bindsym \$mod+Shift+l reload
                    # 一个工作区，两个窗口，只能全屏上下横竖
                    bindsym \$mod+Shift+h fullscreen
                    bindsym \$mod+Shift+m exec emacsclient --eval \"(type-explain-in-chinese)\";
                    bindsym \$mod+Shift+comma exec emacsclient --eval \"(swaywindow)\";
                    bindsym \$mod+Shift+period  exec emacsclient --eval \"(swayrotate)\";

                    # bindsym \$mod+Shift+period  exec toggle-workspace 4 5
                    # bindsym \$mod+Shift+slash layout toggle split
                    # bindsym \$mod+Shift+apostrophe exec \$menu

                    # bindsym \$mod+Shift+period  layout toggle split
                    bindsym \$mod+Shift+slash exec \$menu
                    bindsym \$mod+Shift+space floating toggle
                    bindsym \$mod+space focus mode_toggle

                    # Basic window management
                    bindsym \$mod+Shift+w kill
                    # bindsym \$mod+Shift+k kill
                    bindsym \$mod+Return exec \$term

                    # Application launcher and clipboard history
                    # bindsym \$mod+Shift+apostrophe exec \$menu -show drun
                    bindsym \$mod+Shift+y exec cliphist list | wmenu -N '#ffffff' -n '#000000' -M '#000000' -m '#ffffff' -S '#000000' -s '#ffffff' -f 'monospace 16' -b -i -l 16  | cliphist decode | wl-copy
                    # bindsym \$mod+Shift+d exec cliphist list | wmenu -N '#ffffff' -n '#000000' -M '#000000' -m '#ffffff' -S '#000000' -s '#ffffff' -f 'monospace 16' -b -i -l 10  | cliphist delete

                    floating_modifier \$mod normal

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

                    for_window [app_id=\"emacs\"] border none
                    for_window [app_id=\"emacs\"] titlebar_padding 0
                    for_window [app_id=\"emacs\"] titlebar_border_thickness 0

                    for_window [app_id=\"emacsclient\"] border none
                    for_window [app_id=\"emacsclient\"] titlebar_padding 0
                    for_window [app_id=\"emacsclient\"] titlebar_border_thickness 0

                    # Foot terminal styling
                    for_window [app_id=\"foot\"] border none
                    for_window [app_id=\"foot\"] titlebar_padding 0
                    for_window [app_id=\"foot\"] titlebar_border_thickness 0

                    # Firefox beta styling
                    for_window [app_id=\"firefox\"] border none
                    for_window [app_id=\"firefox\"] titlebar_padding 0
                    for_window [app_id=\"firefox\"] titlebar_border_thickness 0

                    # Firefox beta styling
                    for_window [app_id=\"firefox-beta\"] border none
                    for_window [app_id=\"firefox-beta\"] titlebar_padding 0
                    for_window [app_id=\"firefox-beta\"] titlebar_border_thickness 0

                    for_window [app_id=\"xdg-desktop-portal-gtk\"] floating enable
                    for_window [app_id=\"xdg-desktop-portal-gtk\"] resize set 800 600
                    for_window [app_id=\"xdg-desktop-portal-gtk\"] move position center

                    # Include additional config files
                    include /etc/sway/config.d/*
                                                            " > "$config_file"
                                                              # Set the ownership and permissions of the config file so the user can edit it
                                                              chown ${userSetting.username}: "$config_file"
                                                                fi
                  '';
                  # WorkingDirectory = "/home/${userSetting.username}";
                  wantedBy = [ "hibernate.target" "multi-user.target" ];
                  description = "sway";
                  restartIfChanged = true;
                  after = [ "hibernate.target" ];
                };

                systemd.services."getnixconfig" = {
                  script = ''
                    # Ensure the user-specific .config/nix directory exists
                    mkdir -p '/home/${userSetting.username}/.config/nix'

                    # Define the target config file path
                    config_file="/home/${userSetting.username}/.config/nix/nix.conf"

                      # Only create the file if it does not already exist
                      if [[ ! -f "$config_file" ]]; then
                        echo "access-tokens = github.com = $(cat ${config.sops.secrets.github_token.path})" > $config_file
                          # Set the ownership and permissions of the config file so the user can edit it
                          chown ${userSetting.username}: $config_file
                            fi
                  '';
                  # Trigger the service on system boot and hibernation
                  wantedBy = [ "hibernate.target" "multi-user.target" ];
                  description = "Generate Nix Config for User";
                  restartIfChanged = true;
                  # Ensure the script runs after the system is up and hibernation is set up
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
                  # nameservers = [ "127.0.0.1" "223.5.5.5" "119.29.29.29" ];
                networking = {
                  hostName = userSetting.hostname;
                  networkmanager.enable = false;
                  firewall.enable = true;
                };
                system.nssModules = lib.mkForce [ ]; # for dnsmasq
                hardware.i2c.enable = true;
                # boot.kernelModules = [ "i2c-dev" "i915" "spi-ch341" ];
                boot.kernelModules = [ "i2c-dev"];
                boot.extraModulePackages = [ ];
                # boot.binfmt.emulatedSystems =   [ "aarch64-linux" "riscv64-linux" "i686-linux" ];
                boot = {
                  kernel = {
                    sysctl = {

                      # SysRQ is useful when things hang.
                      "kernel.sysrq" = 1;
                      # Reclaim file pages as often as anon pages.
                      "vm.swappiness" = 100;

                      # forward network packets that are not destined for the interface on which they were received
                      "net.ipv4.conf.all.forwarding" = true;
                      "net.ipv6.conf.all.forwarding" = true;
                      "net.ipv4.conf.br-lan.rp_filter" = 1;
                      "net.ipv4.conf.wan.rp_filter" = 1;
                    };
                  };
                };
                boot.kernelPackages = pkgs.unstable.linuxPackages;
                # Bootloader
                # boot.loader.timeout = 5;
                boot.loader.systemd-boot.enable = true;
                # boot.loader.systemd-boot.configurationLimit = 5;
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
                    # generateCaches = true; # will take little time
                    generateCaches = false; # will take little time
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
                      intel-vaapi-driver # video acceleration on intel inbuilt graphics
                      vulkan-validation-layers # helps catch and debug vulkan crashes
                    ];
                  };
                };
                hardware.enableAllFirmware = true;
                # time.timeZone = "Asia/Shanghai";
                time.timeZone = "Asia/Hong_Kong";
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
                          fish_add_path $HOME/bin
                          fish_add_path $HOME/.local/bin/
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
                    # set -U fish_user_paths $HOME/.local/bin/ $fish_user_paths
                    # clear
                  '';
                };
                system.stateVersion = "25.11";
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
                    trusted-substituters = [
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
                    warn-dirty = false;
                  };
                  extraOptions = ''
                    !include ${config.sops.secrets.nixAccessTokens.path}
                  '';
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
            inherit (final.stdenv.hostPlatform) system;
            config = {
              allowUnfree = true;
              permittedInsecurePackages = [ "libsoup-2.74.3" ];
            };
          };
        };
        old-packages = final: _prev: {
          oldw = import inputs.nixpkgs-old {
            # system = final.system;
            inherit (final.stdenv.hostPlatform) system;
            config = { allowUnfree = true; };
          };
        };
      };
    };
  inputs = {
    hosts = {
      url = "github:NestorLiao/mhosts";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # nix flake update hosts
    # this is is "the game changer"
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix.url = "github:Mic92/sops-nix";
  };
}
