## this is my nixos config for surrive in post-modern world ###################
{
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
        sound = false;
      };
    in {
      nixosConfigurations = {
        ${userSetting.hostname} = nixpkgs.lib.nixosSystem {
          specialArgs = { inherit inputs outputs userSetting; };
          modules = [
            ./hardware-configuration.nix
            ({ pkgs, config, userSetting, sops, inputs, lib, outputs, ... }:
              let
                #   template= ''
                #     #!/usr/bin/env bash
                # '';
                lock-false = {Value = false; Status = "locked";};
                lock-true = {Value = true; Status = "locked";};
              in {
                # |-------------------+--------------+------------------------------------|
                # | procress          | url          | for                                |
                # |-------------------+--------------+------------------------------------|
                # | fag               | flag/cintro  | c, pretent iam tsoding             |
                # | embedded C book   | read book    | c, how compiler/os/c works         |
                # | Yeetcode          | leetgo/book  | cpp[rust], algo ds, solve task     |
                # | paperlike-c/el    | paperlike-go | elisp,paperlike emacs controler    |
                # | ziglings          | ziglings     | zig, basic ziglangs speed run      |
                # | zag               | fag          | zig, I have language erotic        |
                # | paperlike-zig     | paperlike-el | zig, make cli/tary                 |
                # | nixos r2s         | github-repos | nix, for network addiction         |
                # | TsurgizOS         | os.phil-opp  | zig[rust], make general os for cv  |
                # | nixos rasberry-pi | github-repos | nix, for embedded os/screen/driver |
                # | clings            | ziglings     | c, lings but clang                 |
                # | freertos emulator | rtos         | c, use general rtos                |
                # | lvgl eink rtos    | lvgl         | c, embedded ui/driver/sdl          |
                # | lvgl lora loc     | graphic      | c, embedded openstress/sdl/lvgl    |
                # | RZOS              | rtos         | zig, make general rtos             |
                # | Celest            | game         | zig, embedded game/sdl             |
                # | Safephone         | electronic   | lvgl/eink/openstress/lora/3Dprint  |
                # |                   |              | stm32/nix/electronic/network       |
                # |                   |              | nsfw image/text detected           |
                # |-------------------+--------------+------------------------------------|
                # - I was what I walk and see, come out to be.
                # - No Phone, No Internet, No Browser, No IDE, On Plain Text, Consume->Creat.
                # - Master NixOS/Emacs, For Into Boring Stable and Fluent MindFlow Mode.
                # - Read HTML Reference, Manual, Info, Help, Compilation, Source Code.
                # - Master C language and it's Std, and library often used with debug exprience.
                # - Do Leetcode, Learn Algorithm and Data Structure, Exciting to Solving Tasks.
                # - Do Flag.h/Paperlike-cli/WM/RTOS, Learn Real programming For Myself.
                # - Accumulate a codebase, Use Erlang/Zig/Rust to creat project You Like.
                imports = [ inputs.hosts.nixosModule ];
                # 我得道了，在怡凉乡间独自过着悠然的生活，在蓝天绿树巨石间…
                networking.stevenBlackHosts = {#一切都在网络上，编写人类。
                  enableIPv6 = false; # 文明演IPv6为传色情，文明演Hosts为戒色
                  enable = true; # 从来都没有什么戒色… 只不过是文明把玩自然本性
                  # 绿色、和谐、单调、劳作、无知、无欲、无我、相识、相恋、相爱
                  # 城市、单身、独生、男性、大学生、亚洲、网瘾、中产、游戏瘾、CS
                  # 一切只是“生物滥性”与“传统禁忌”带来的冲动，企业用情绩控制人，
                  # 配合“工业革命”与“数字革命”带来的"机器空间"替代性心流的富饶假象
                  # 开着不会影响学习编程，使用计算机不是为了让别人编程你。
                  blockPorn = true; # 你知道的，虚假的情欲只会让你溺身，大脑停滞
                  blockSocial = true; # 你知道的，虚假的参与只会让你躁心，久坐愤怒
                }; # 不依赖外物，不物物于物，游刃必有余地。
                #可以保身，可以全生，可以养亲，可以尽年。
                # 你是想做巴甫洛夫的狗还是斯金纳的鸽子?
                # 或是马斯克的脑机接口僵尸还是扎克博格的元宇宙coomer?
                # 越是依赖于算法与机械，身体与灵魂越是分离。
                # 理其心理，内心拒绝即是自由。
                # 外其外物，外物自由即是强迫。
                # 世界上大概会有人看到这个文字，所谓的文字不过是用来blogging。不是和柏拉图一样写“柏拉图式爱情”的男同诗不是写“曹雪芹恋爱幻想”小说只是BLOGGING，在我看来blog是PIM的一种方式。和VIM(Vi IMproved)一样所谓PIM指是的Porn IMproved，通过blogging一些自认为明白的事物给世界多一点躁动，多一点自恋式的垃圾，少一点自尊，少一点自我认同，少一点与家人的时间。通过头脑masterbating填满你的大脑，这就是我PIM的目标。blog应该少看，好了该关闭这个tab了，为的是别让你看到我不小心打上去的密码。好吧PIM的意思其实是Person Information Management，大概就是下面这样:
                # - 去健身房通过抖音学习锻炼身体
                # - 去钦点景点如哈尔滨淄博天水合川学习营销
                # - 去twitter上看懂王和elon谈论bitcoin
                # - 去囤一些Labubu和NFT以备下一次热潮
                # - 去关注女性主义作者向每个人说自己是女权男(喜欢强调自己蹲着拉尿)
                # - 学习前端，制作知乎b站小红书豆瓣热点聚合网站
                # - 每天花10小时上网，收集链接再发到自己的博客上
                # - 每天花10小时上网，复制一遍教程发到自己博客上
                # - 每日用30分钟用gpt生成30万字心得感悟到博客
                # - 使用LLM生成文本到自己的obsidian第二大脑
                # - 利用聚合网站的方法，制作色情网站入口
                # - 和缅甸太子达成协议，接入赌博网站广告
                # - 在公共厕所大量印刷网址和分发小卡片
                # - 每天关注国际新闻，为中国发声，强调爱国
                # - 大量投资低空经济与电动车产业的办证产业
                # - 每天查阅三联等30个公众号了解世界新编热点
                # - 大量投资团播/村超/城超/亚洲杯/世界杯等自发“热点”
                # - 保证能量值，去吃火锅，零食，肯德基可乐和奶茶
                # - 保证娱乐值，去看演唱会，逛商场，去三亚参与购物
                # - 相信品牌，把自我认同放在品牌上，我用什么是什么人
                # - 每天把名人名言放在桌面上，思考今天用什么签名
                # - 每天要依赖500项工续才能正常生活，增加社会连系
                # - 每天思的是车子房子奶子票子，保持自己的目标
                # - 关心名人/世界/国家/地位/热点，买书/点赞/分享
                # - 通过社交媒体学社交，通过新闻学习知识文化
                # - 每天花10h思考使用什么工具才能让5s的事效率更高
                # - 每天得用10h在电脑上用eletron浏览器和手机上用electron应用
                # - 作为文化人得在久坐上花10h以上，每5小时休息一次
                # - 多学习到需要眼镜，多久坐到需要小米su7, 多吃少动到去健身房
                # - 必须在国家间站队，必须在编程语言间站队，必须在编程工具间站队
                # - 少睡觉，少运动，多上网，多久坐，多比较 ，人生会更好
                # - Zig 比 C 更好，我听别人blogging说的
                # - Rust 比 C++ 更好，我听别人blogging说的
                # - Golang 比 Java 更好，我听别人blogging说的
                # - Mojo 比 Python 更好，我听别人blogging说的
                # - Common Lisp 比 Emacs Lisp 更好，我听别人blogging说的
                # - 新的东西一定是比旧的东西好! 别人的话一定比自己想的对!
                # - 学历很重要，每天关心大学排名，啥qs啥校友会什么抖音排行榜
                # - 钱很重要，不然怎么购买带音乐的智能车，带音乐的智能床，带音乐的智能床头柜。
                # - 领导: 特朗普/金正恩/… All
                # - 认同: LGBTQ++/左右翼/MBTI/xx党
                # - 消费: Labubu/谷子/头像壁纸/比特币
                # - 编程: C++/Rust/Java/Windows
                # - 生殖: 处男/剩女/生育率/性病/约炮
                # - 娱乐: Hentai/动漫/电影/电视剧
                # - 成就: 健身房/网红景点/高等教育
                # - 性欲: 戒色吧/吃瓜网/海角社区
                # - 现在: Sora2/AI女友/社交媒体
                # - 未来: 电车/机器人/低空经济
                # - 青年: 教育/数学/计算机/传销


                programs.nix-ld = {
                  enable = true;
                  libraries = with pkgs; [
                  ];
                };

                # services.displayManager.gdm.enable = true;
                # services.desktopManager.gnome.enable = true;
                # # To disable installing GNOME's suite of applications
                # # and only be left with GNOME shell.
               #  services.gnome.core-apps.enable = false;
               #  services.gnome.core-developer-tools.enable = false;
               #  services.gnome.games.enable = false;
               #  environment.gnome.excludePackages = with pkgs; [ gnome-tour gnome-user-docs ];

                environment.systemPackages = with pkgs; [
                  # bambu-studio
                  # (pkgs.buildFHSEnv {
                  #   name = "kernel-env";
                  #   targetPkgs = pkgs:
                  #     with pkgs; [
                  #       bear
                  #       bc
                  #       gcc
                  #       flex
                  #       bison
                  #       openssl
                  #       openssl.dev
                  #       elfutils.dev
                  #       elfutils
                  #       libelf
                  #       ncurses.dev
                  #       binutils
                  #       gnumake
                  #       ncurses
                  #       kmod
                  #     ];
                  # })

                  # alsa-utils
                  # bear
                  # binutils
                  # bison
                  # bpftrace
                  # cachix
                  # ccls
                  # clang
                  # codespell
                  # conan
                  # cpio
                  # doxygen
                  # elfutils.dev
                  # flex
                  # foliate
                  # fzf
                  # gcc-arm-embedded
                  # gtest
                  # kmod
                  # lcov
                  # meson
                  # nil
                  # ninja
                  btop
                  clang-tools
                  cppcheck
                  ctags
                  elfutils
                  file

                  # nixfmt-classic
                  # opencc
                  # openvpn
                  # pahole
                  # pandoc
                  # pciutils
                  # qemu
                  # qemu-utils
                  # quickemu
                  # samba
                  # texlab
                  # texliveFull
                  # tree
                  # unstable.gemini-cli-bin
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
                  just
                  poppler-utils
                  python3
                  ripgrep
                  rr
                  scc
                  sdcv
                  thunderbird
                  trashy
                  unrar-free
                  unstable.leetgo
                  unstable.sops
                  unzipNLS
                  wget

                  grim
                  jq
                  libnotify
                  mako
                  paperlike-go
                  satty
                  slurp
                  wf-recorder
                  wl-clipboard
                  wl-color-picker
                  wmenu
                  wtype
                  zip

                  # cargo
                  # rust-analyzer
                  # rustc
                  # rustlings
                  # unstable.lldb
                  unstable.nix-search-cli

                  c-intro-and-ref
                  glibcInfo
                  man-pages
                  man-pages-posix
                  # stdmanpages
                  # clang-manpages
                  # linux-manual

                  (texlive.combine ({ # For Writting my Book.
                    inherit (texlive) scheme-small;
                    "adforn" = texlive."adforn"; "amsfonts" = texlive."amsfonts";
                    "amsmath" = texlive."amsmath"; "anyfontsize" = texlive."anyfontsize";
                    "appendix" = texlive."appendix"; "apptools" = texlive."apptools";
                    "atbegshi" = texlive."atbegshi"; "atveryend" = texlive."atveryend";
                    "auxhook" = texlive."auxhook"; "babel" = texlive."babel";
                    "bbding" = texlive."bbding"; "bidi" = texlive."bidi";
                    "bigintcalc" = texlive."bigintcalc"; "bitset" = texlive."bitset";
                    "blindtext" = texlive."blindtext"; "booktabs" = texlive."booktabs";
                    "caption" = texlive."caption"; "catchfile" = texlive."catchfile";
                    "changepage" = texlive."changepage"; "collcell" = texlive."collcell";
                    "comment" = texlive."comment"; "csquotes" = texlive."csquotes";
                    "ctablestack" = texlive."ctablestack"; "ctex" = texlive."ctex";
                    "currfile" = texlive."currfile"; "enumitem" = texlive."enumitem";
                    "environ" = texlive."environ"; "esint" = texlive."esint";
                    "etex" = texlive."etex"; "etexcmds" = texlive."etexcmds";
                    "etoolbox" = texlive."etoolbox"; "everysel" = texlive."everysel";
                    "everyshi" = texlive."everyshi"; "fancyhdr" = texlive."fancyhdr";
                    "fancyvrb" = texlive."fancyvrb"; "filehook" = texlive."filehook";
                    "finstrut" = texlive."finstrut"; "float" = texlive."float";
                    "fontspec" = texlive."fontspec"; "footmisc" = texlive."footmisc";
                    "framed" = texlive."framed"; "fvextra" = texlive."fvextra";
                    "geometry" = texlive."geometry"; "gettitlestring" = texlive."gettitlestring";
                    "hologo" = texlive."hologo"; "hopatch" = texlive."hopatch";
                    "hycolor" = texlive."hycolor"; "hyperref" = texlive."hyperref";
                    "ifmtarg" = texlive."ifmtarg"; "ifplatform" = texlive."ifplatform";
                    "iftex" = texlive."iftex"; "incgraph" = texlive."incgraph";
                    "infwarerr" = texlive."infwarerr"; "intcalc" = texlive."intcalc";
                    "kvdefinekeys" = texlive."kvdefinekeys"; "kvoptions" = texlive."kvoptions";
                    "kvsetkeys" = texlive."kvsetkeys"; "latex2pydata" = texlive."latex2pydata";
                    "lineno" = texlive."lineno"; "lipsum" = texlive."lipsum";
                    "listings" = texlive."listings"; "listingsutf8" = texlive."listingsutf8";
                    "ltxcmds" = texlive."ltxcmds"; "luacode" = texlive."luacode";
                    "lualatex-math" = texlive."lualatex-math"; "luaotfload" = texlive."luaotfload";
                    "luatexbase" = texlive."luatexbase"; "luatexja" = texlive."luatexja";
                    "makecell" = texlive."makecell"; "manfnt" = texlive."manfnt";
                    "marvosym" = texlive."marvosym"; "minitoc" = texlive."minitoc";
                    "minted" = texlive."minted"; "multirow" = texlive."multirow";
                    "multitoc" = texlive."multitoc"; "mwe" = texlive."mwe";
                    "natbib" = texlive."natbib"; "newfloat" = texlive."newfloat";
                    "notoccite" = texlive."notoccite"; "ntheorem" = texlive."ntheorem";
                    "paralist" = texlive."paralist"; "pdfcol" = texlive."pdfcol";
                    "pdfescape" = texlive."pdfescape"; "pdftexcmds" = texlive."pdftexcmds";
                    "pgf" = texlive."pgf"; "pgfopts" = texlive."pgfopts";
                    "placeins" = texlive."placeins"; "preview" = texlive."preview";
                    "ragged2e" = texlive."ragged2e"; "refcount" = texlive."refcount";
                    "relsize" = texlive."relsize"; "rerunfilecheck" = texlive."rerunfilecheck";
                    "setspace" = texlive."setspace"; "showexpl" = texlive."showexpl";
                    "siunitx" = texlive."siunitx"; "stringenc" = texlive."stringenc";
                    "svn-prov" = texlive."svn-prov"; "tcolorbox" = texlive."tcolorbox";
                    "tex4ht" = texlive."tex4ht"; "threeparttable" = texlive."threeparttable";
                    "titlesec" = texlive."titlesec"; "tocloft" = texlive."tocloft";
                    "translations" = texlive."translations"; "translator" = texlive."translator";
                    "trimspaces" = texlive."trimspaces"; "type1cm" = texlive."type1cm";
                    "unicode-math" = texlive."unicode-math"; "uniquecounter" = texlive."uniquecounter";
                    "upquote" = texlive."upquote"; "url" = texlive."url";
                    "varwidth" = texlive."varwidth"; "xcolor" = texlive."xcolor";
                    "xifthen" = texlive."xifthen"; "xkeyval" = texlive."xkeyval";
                    "xstring" = texlive."xstring"; "xunicode" = texlive."xunicode";
                    "xurl" = texlive."xurl"; "zhnumber" = texlive."zhnumber";
                    "capt-of" = texlive."capt-of"; "cprotect" = texlive."cprotect";
                    "ulem" = texlive."ulem"; "wrapfig" = texlive."wrapfig";
                    "esvect" = texlive."esvect"; "yhmath" =texlive."yhmath";
                    "biblatex-gb7714-2015" =texlive."biblatex-gb7714-2015"; "biblatex" =texlive."biblatex";
                    "tikzfill" =texlive."tikzfill"; "bigfoot" =texlive."bigfoot";
                    "rsfs" =texlive."rsfs"; "tex-gyre" =texlive."tex-gyre";
                  } ))
                  (writeShellScriptBin "wtypefix" ''
                  #!/usr/bin/env bash
                  wtype \"
                  '')
                  (writeShellScriptBin "onlyemacs" ''
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
                '')

                ];
                xdg.mime = {
                  enable = true;
                  defaultApplications = {
                    "inode/directory" = [ "thunar.desktop" ];
                    "x-directory/normal" = [ "thunar.desktop" ];
                  };
                };# trust me bro, run  xdg-user-dirs-update --force after
                environment.etc = { # Just follow the wtf firefox auto creat.
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
                environment.etc = {
                  "nixos/flake.nix" = {
                    source = ./flake.nix;
                    mode = "0777";
                  };
                };
                environment.sessionVariables = {
                  XDG_CACHE_HOME = "$HOME/.cache";
                  XDG_CONFIG_HOME = "$HOME/.config";
                  XDG_DATA_HOME = "$HOME/.local/share";
                  XDG_BIN_HOME = "$HOME/.local/bin/";
                  XDG_DOWNLOAD_HOME = "$HOME/Downloads";
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
                  # ff = "fd  | fzf | zoxide";
                  c = "clear";
                  # unzip = "unzip -O gb18030";
                  unrar = "unrar-free";
                  win =
                   ''
                    quickemu --vm windows-10.conf --display spice
                     sleep 480
                      notify-send -t 1000  'out of windows, now!'
                      sleep 10
                       pkill windows
                     '';
                  garbage = "nix-collect-garbage -d";
                  sgarbage = ''
                    sudo rm /tmp/* -rf
                     sudo nix-collect-garbage -d
                     nix-collect-garbage -d
                    sudo nix-store --optimise
                    '';
                  # sudo nix-collect-garbage -d
                  # sudo nix-store --optimise
                  # nix-store --query --roots
                  # ls -l /var/run/current-system
                  # nix develop .#default --profile ~/.nix-profiles/game-dev
                  # rm -rf ~/.nix-profiles/game-dev
                  # nix profile remove 0 --profile ~/.nix-profiles/game-dev
                  nixh = "nix-prefetch-url";
                  nixhu = "nix-prefetch-url --unpack";
                  rsap=
                    "sudo nixos-rebuild switch --flake .#$hostname  --option substituters 'https://mirrors.ustc.edu.cn/nix-channels/store  https://cache.nixos.org' --cores 6 -j 12";
                  rsme=
                    "sudo nixos-rebuild switch --flake /home/$USER/.me#$hostname  --option substituters 'https://mirrors.ustc.edu.cn/nix-channels/store  https://cache.nixos.org' --cores 6 -j 12";
                  rsmeo=
                    "sudo nixos-rebuild switch --flake /home/$USER/.me#$hostname  --option substituters 'https://mirrors.ustc.edu.cn/nix-channels/store  https://cache.nixos.org' --cores 6 -j 12 --offline";
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
                  jo = "journalctl -xeu";
                  e = "emacsclient -n -s server";
                  en = "emacsclient -n -s server .";
                  cleanup = "doas nix-collect-garbage --delete-older-than 7d";
                  cp = "cp -iv";
                  mv = "mv -iv";
                  rm = "trash -c never ";
                  mkd = "mkdir -pv";
                };
                programs.bash.interactiveShellInit = ''
                  # eval "$(zoxide init bash)"
                  '';
                programs.foot = {
                  enable = true;
                  settings = {
                    main = {
                      font = "FiraCode Nerd Font:size=12";
                      selection-target = "both";
                    };
                    colors = {
                      foreground = "000000";
                      background = "FFFFFF";
                      regular0 = "000000";
                      regular1 = "0C322F";
                      regular2 = "019E07";
                      regular3 = "B58900";
                      regular4 = "068BD2";
                      regular5 = "033682";
                      regular6 = "CAA198";
                      regular7 = "FFFFFF";
                      bright0 = "000000";
                      bright1 = "0C322F";
                      bright2 = "019E07";
                      bright3 = "A58900";
                      bright4 = "068BD2";
                      bright5 = "033682";
                      bright6 = "0AA198";
                      bright7 = "FFFFFF";
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
                          # sha256 = lib.fakeSha256;
                          sha256 = "sha256-cauJLSk3dWSV98QmHj7cAbyUoeT5y8oXC3e1dpnxxOU=";
                        };
                      };
                      home.file.".stardict/dic" = {
                        source = pkgs.stdenv.mkDerivation {
                          pname = "stardict-dictionaries";
                          version = "2.4.2";
                          src = pkgs.fetchFromGitHub {
                            owner = "NestorLiao";
                            repo = "dict";
                            rev = "master";
                            sha256 =
                              "sha256-dd9dMrhPa4QeJ58uiLBNhoQy8EfSJrmLj0lpwtygR2U=";
                          };
                          buildInputs =[ pkgs.unzip ];
                          unpackPhase = ''
                            unzip $src/stardict-ghycyzzd-2.4.2.zip -d $out
                            unzip $src/stardict-langdao-ce-gb-2.4.2.zip -d $out
                            unzip $src/stardict-ecdict-2.4.2.zip -d $out
                          '';
                        };
                      };
                      programs.gh = {
                        enable = true;
                        gitCredentialHelper ={
                          enable = true;
                          hosts = [ "https://github.com" ];
                        };
                        extensions = with pkgs; [ gh-markdown-preview ];
                        settings = {
                          git_protocol = "ssh";
                          prompt = "enabled";
                        };
                      };
                      home.packages = with pkgs; [
                        git-credential-manager
                      ];
                      programs.git = {
                        enable = true;
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
                      };
                      programs.nix-index = {
                        enable = false;
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
                        enable = false;
                        settings = {
                          theme = "eink";
                          editor = {
                            lsp = {
                              display-messages = true;
                              auto-signature-help = false;
                            };
                            # gutters = ["diagnostics" "spacer" "diff"];
                            jump-label-alphabet = "aorisetndh";
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
                        defaultEditor = false; # I have emacs now, sorry!
                        themes = {
                          eink = let
                            white = "#FFFFFF";
                            black = "#000000";
                          in {
                            "ui.background" = { bg = white; };
                            "ui.text" = black;
                            "ui.selection" = {
                              bg = white;
                              fg = black;
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
                            };
                            "keyword.function" = {
                              fg = black;
                            };
                            "function" = { fg = black; };
                            "function.macro" = {
                              fg = black;
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
                        enable = false;
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
                      home.file.".config/mako/config".text = ''
                        font=Bookerly 20
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
                          name = "Bookerly";
                          size = 16;
                        };
                      };
                      qt = {
                        enable = true;
                        platformTheme.name = "gtk";
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
                      # Path=default
                      ### xxxx
                      programs.firefox = {
                        enable = true;
                        package = pkgs.firefox-beta;

                        # 浏览器就像是大脑的信息化改造工具
                        # 它让人感觉良好，全是浏览/点击，很少修改/写作
                        # 它带我们去博客/娱乐/社交/发现/新闻…
                        # 短的长的… 点击进入… cookie/弹窗/登录/付费/翻墙…
                        # 没有人会用网页修改网页代码，有，但是那种网站早己没落。
                        # 没有人会用网站评论长篇大论，有，但是人们只会一扫而过。
                        # 浏览->狩猎，点击->采集，用原始方式实行消费行为
                        # 缺少的是修改/写作，消灭工业自动化剩余的创造行为
                        # 它让各种事物依着它运作，数据/网络/信息/简历…
                        # 我们争先恐后的找着新奇的东西，好像不用就会被时代抛弃。
                        # gemini，chatgpt，豆包，deepseek，宇树，spacex…
                        # 我们争先恐后的上传最新的简历，好像向世界展示自恋自虑。
                        # github.io，教育经历，获奖，项目，论文，职位，头衔…
                        # 这些外部工具，试图用幻觉来强迫我们租用这些服务。
                        # 这些外部名号，试图用地位来强迫我们学习这些知识。
                        # 什么上云… 不过是科技公司的隐形房租
                        # 什么院士… 不过是技术统治的诱人娼妓
                        # 一切都在浏览器上，
                        # 编程人类。
                        # 在中国，更是如此。
                        # 技术上瘾，垄断传媒，算法人群，地位崇拜，地广人多。
                        # 中国人如此迷恋智能机，炒作5G… 只是生意…
                        # 24h在线 彩色大屏 完全闭源 缺少控制 全是传媒 信息封闭 随身携带。
                        # 里面的这么多的浏览器，一堆electron，只是卖更多手机…
                        # 小红书/知乎/qq/豆瓣/b站/微信/即刻/…
                        # 24h在线 视频图片 完全闭源 缺少控制 全是传媒 信息封闭 链接可达。
                        # 越是智能，越是浅薄。
                        # 越是浏览，越是迷茫。
                        # 网络 工业 技术 书写 文明 生物 宇宙
                        # 炒5G，因为手机卖不出去… 4G够用了
                        # 炒电车，因为手机卖不出去… 加四个轮子
                        # 中国独一档的:
                        # 51吃瓜，用中国的性保守文化为资本，随机域名网络为载体，
                        # 以传播谣言，泛娱乐集体性奋，失范式对广大国人产生影响。
                        # 电车/三折叠，用单一政府下的全国式宣传机器进行洗脑
                        # 摄像头与视觉算法，用单一政府下的全国式监视机器进行管制
                        #
                        # 51吃瓜用户上传了用视觉算法换脸的
                        # 被电车摄像头扫描的路边女子的形貌，
                        # 被一个用三折叠欣赏色情片的身边人发现。
                        # 谣言四起，性保守的中国人，在网络分享性放纵。
                        # 禁忌诱人，网络自由，意识趋向于资本与自由
                        #
                        # 地广人多社会新闻产生与消费多，大集体国家对国际新闻的消费多
                        # 高等教育水平低，泛娱乐水平高，有GFW，英语水平与认识水平低下
                        # 保守东亚文化、网络集体性放纵与日本泛二次元文化交织，色情成瘾多
                        # 无宗教/生育制度，随着城市化/工业化，内卷躺平、拜金与失范为常态
                        # 网络/手机/电车消费第一，空闲网络使用比例第一，幸福指数世界中下
                        # 中文极高的信息密度与无空格属性，产生b站以及各种技术成瘾
                        # 对技术知之甚少，对开源技术更是稀少，世界性创新技术更是少之又少
                        # 国内程序员多为web领域，大大加重和信息的网际过载与监管能力
                        # 成瘾算法 如temu/tiktok，擅长操控用户的行为，使其成瘾
                        #
                        # 中美，左右，百事可口可乐… 口中争吵，实则伸手向消费者
                        # 自媒体像寻觅腐肉秃鹫或吃剩饭的苍蝇…

                        languagePacks = ["en-US"];
                        profiles = {
                          default = {
                            settings  = {
                            "accessibility.force_disabled" =1;
                            "app.update.auto" = false;
                            "app.update.download.promptMaxAttempts" = 0;
                            "app.update.elevation.promptMaxAttempts" = 0;
                            "app.update.service.enabled" = false;
                            "browser.aboutConfig.showWarning" = false;
                            "browser.aboutwelcome.enabled" = false;
                            "browser.accessibility.typeaheadfind" = false;
                            "browser.anchor_color" = "#000000";
                            "browser.cache.disk.enable" =true;
                            "browser.ctrlTab.recentlyUsedOrder" = true;
                            "browser.display.document_color_use" = 2;
                            "browser.display.use_document_fonts" = 0;
                            "browser.download.dir" = "/home/${userSetting.username}/Downloads";
                            "browser.download.forbid_open_with" = true;
                            "widget.use-xdg-desktop-portal.file-picker" = 1;
                            "browser.download.lastDir" = "/home/${userSetting.username}/Downloads";

                            "browser.download.open_pdf_attachments_inline" =true;
                            "browser.download.start_downloads_in_tmp_dir" =true;
                            "browser.newtabpage.activity-stream.showWeather" =false;
                            "browser.newtabpage.enabled" = false;
                            "browser.quitShortcut.disabled" = true;
                            "browser.safebrowsing.downloads.enabled" = false;
                            "browser.safebrowsing.malware.enabled" = false;
                            "browser.safebrowsing.phishing.enabled" = false;
                            "browser.search.region" = "US";
                            "browser.sessionstore.max_tabs_undo" =3;
                            "browser.sessionstore.restore_on_demand" =true;
                            "browser.sessionstore.warnOnQuit" = true;
                            "browser.slowStartup.notificationDisabled" =true;
                            # instaed of blank.org or github, they are not reliable for chinese and eink user.
                            "browser.startup.homepage" = "https://search.nixos.org";
                            "browser.tabs.closeTabByDblclick" = true;
                            "browser.tabs.closeWindowWithLastTab" = false;
                            "browser.tabs.loadInBackground" = true;
                            "browser.tabs.tabClipWidth" = 999;
                            "browser.tabs.warnOnClose" = false;
                            "browser.toolbars.bookmarks.visibility" = "never";
                            "browser.translations.automaticallyPopup" = false;
                            "browser.uidensity" = 1;
                            "browser.urlbar.oneOffSearches" = false;
                            "browser.urlbar.shortcuts.tabs" = false;
                            "browser.urlbar.suggest.quicksuggest.sponsored" = false;
                            "browser.visited_color" = "#000000";
                            "canvas.capturestream.enabled" = false;
                            "datareporting.healthreport.service.enabled" = false;
                            "devtools.chrome.enabled" = true;
                            "devtools.debugger.remote-enabled" = true;
                            "dom.ipc.plugins.enabled" = false;
                            "dom.ipc.plugins.enabled.libflashplayer.so" = false;
                            "dom.media.mediasource.enabled" = false;
                            "dom.mozTCPSocket.enabled" = false;
                            "dom.netinfo.enabled" = false;
                            "dom.webaudio.enabled" = false;
                            "dom.webvtt.enabled" = false;
                            "extensions.activeThemeID" = "{5f71ffe3-23e2-49b8-b75e-2c032ef4a1d9}";
                            "extensions.pocket.enabled" = false;
                            "extensions.update.enabled" = false;
                            "extensions.webextensions.restrictedDomains" = "";
                            "font.name-list.emoji" = "";
                            "font.name.monospace.x-western" ="JetBrainsMono Nerd Font Mono";
                            "full-screen-api.transition.timeout" = 0;
                            "full-screen-api.warning.delay" = 0;
                            "full-screen-api.warning.timeout" = 0;
                            "general.smoothScroll" = false;
                            "general.useragent.compatMode.firefox" = true;
                            "geo.enabled" = false;
                            "gfx.font_rendering.fontconfig.max_generic_substitutions" = 127;
                            # "gfx.webrender.all" = false;
                            "image.jxl.enabled" = true;
                            "layout.css.prefers-color-scheme.content-override" = 1;
                            "browser.ml.linkPreview.enabled" = false;
                            "media.autoplay.block-event.enabled" = true;
                            "media.autoplay.default" = 2;
                            "media.videocontrols.picture-in-picture.keyboard-controls.enabled" = false;
                            "media.av1.enabled" = false;
                            "media.block-autoplay-until-in-foreground" = true;
                            "media.block-play-until-visible" = true;
                            "media.disabled" =true;
                            "media.eme.enabled" = false;
                            "media.ffmpeg.enabled" = false;
                            "media.ffmpeg.vaapi.enabled" = true;
                            "media.ffvpx.enabled" = false;
                            "media.flac.enabled" = false;
                            "toolkit.legacyUserProfileCustomizations.stylesheets" = lock-true; # This is needed for other userX.css files
                            "media.getusermedia.screensharing.enabled" = false;
                            "media.gmp-widevinecdm.enabled" = false;
                            "media.h264.enabled" = false;
                            "media.libvpx.enabled" = false;
                            "media.mediasource.enabled" = false;
                            "media.mediasource.mp4.enabled" = false;
                            "media.mediasource.webm.enabled" = false;
                            "media.mediasource.whitelist" = false;
                            "media.mp4.enabled" = false;
                            "media.navigator.audio.enabled" = false;
                            "media.navigator.enabled" = false;
                            "media.navigator.video.enabled" = false;
                            "media.ogg.enabled" = false;
                            "media.peerconnection.enabled" =false;
                            "media.peerconnection.ice.default_address_only" =true;
                            "media.wave.enabled" = false;
                            "media.webm.enabled" = false;
                            "media.webspeech.recognition.enable" = false;
                            "media.webspeech.synth.enabled" = false;
                            "network.dns.echconfig.enabled" = true;
                            "network.dns.http3_echconfig.enabled" = true;
                            "permissions.default.image" = 2;
                            "places.history.enabled" = true;
                            "plugin.state.flash" = 0;
                            "privacy.donottrackheader.enabled" = true;
                            "privacy.resistFingerprinting" = true;
                            "privacy.resistFingerprinting.block_mozAddonManager" = true;
                            "privacy.trackingprotection.cryptomining.enabled" =true;
                            "privacy.trackingprotection.enabled" = true;
                            "privacy.trackingprotection.fingerprinting.enabled" = true;
                            "privacy.trackingprotection.socialtracking.enabled" = true;
                            "reader.parse-on-load.enabled" = false;
                            "security.enterprise_roots.auto-enabled" = false;
                            "security.enterprise_roots.enabled" = false;
                            "services.sync.engine.history" = true;
                            "services.sync.prefs.sync-seen.services.sync.prefs.sync.capability.policy.maonoscript.sites" =true;
                            "services.sync.prefs.sync.capability.policy.maonoscript.sites" =true;
                            "signon.autofillForms" = false;
                            # "signon.rememberSignons" = true;
                            "svg.context-properties.content.enabled" = true;
                            "toolkit.crashreporter.dataDirectory" = "";
                            "toolkit.crashreporter.enabled" = false;
                            "toolkit.telemetry.archive.enabled" = false;
                            "toolkit.telemetry.enabled" = false;
                            "toolkit.telemetry.rejected" = false;
                            "toolkit.telemetry.unified" = false;
                            # Convenience
                            "browser.formfill.enable" = false;
                            "browser.search.suggest.enabled" = false;
                            "browser.search.suggest.enabled.private" = false;
                            "browser.urlbar.suggest.searches" = false;
                            "browser.urlbar.showSearchSuggestionsFirst" = false;
                            "signon.rememberSignons" = false; # Stop asking to save passwords
                            "extensions.formautofill.addresses.capture.enabled" = false; # Stop asking to save addresses
                            # Styling
                            "browser.compactmode.show" = true;
                            # Containers
                            "privacy.userContext.enabled" = true;
                            "privacy.userContext.ui.enabled" = true;
                            # Downloads
                            "browser.download.useDownloadDir" = true;
                            "browser.download.always_ask_before_handling_new_types" = true;
                            # Privacy
                            "privacy.sanitize.sanitizeOnShutdown" = true;
                            "privacy.clearOnShutdown_v2.cache" = true;
                            "privacy.clearOnShutdown_v2.historyFormDataAndDownloads" = true;
                            "privacy.clearOnShutdown_v2.browsingHistoryAndDownloads" = true;
                            "privacy.clearOnShutdown_v2.downloads" = true;
                            "privacy.clearOnShutdown_v2.formdata" = true;
                            "privacy.clearOnShutdown_v2.cookiesAndStorage" = true;
                            # # HTTPS only
                            # # "dom.security.https_only_mode" = true;
                            "ui.key.menuAccessKey" = 17;
                            "ui.key.menuAccessKeyFocuses" = false;
                            "ui.caretBlinkTime" = 0;
                            "ui.caretBlinkCount" = -1;
                            "ui.caretWidth" = 3;
                            "webgl.disabled" = true;
                            "widget.non-native-theme.scrollbar.style" = 3;
                            "xpinstall.signatures.required" = false;

                          };
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
                                  template = "https://wiki.nixos.org/index.php?search={searchTerms}";
                                }];
                                icon = "https://wiki.nixos.org/favicon.png";
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
/* Example for userContent.css (web pages) */
input, textarea, [contenteditable] {
  caret-color: black !important;    /* ensure a visible caret color */
  /* Note: caret-shape and caret-animation properties are not implemented in Firefox */
}
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

/* Example for userContent.css (web pages) */
input, textarea, [contenteditable] {
  caret-color: black !important;    /* ensure a visible caret color */
  /* Note: caret-shape and caret-animation properties are not implemented in Firefox */
}

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
                            "app.update.auto" = false;
                            "app.update.download.promptMaxAttempts" = "0";
                            "app.update.elevation.promptMaxAttempts" = "0";
                            "app.update.service.enabled" = false;
                            "browser.aboutConfig.showWarning" = false;
                            "browser.aboutwelcome.enabled" = false;
                            "browser.accessibility.typeaheadfind" = false;
                            "browser.anchor_color" = "#000000";
                            "browser.cache.disk.enable" =true;
                            "browser.ctrlTab.recentlyUsedOrder" = true;
                            "browser.display.document_color_use" = 2;
                            "browser.display.use_document_fonts" = "0";
                            "browser.download.dir" = "/home/${userSetting.username}/Downloads";
                            "browser.download.forbid_open_with" = true;
                            "widget.use-xdg-desktop-portal.file-picker" = 1;
                            "browser.download.lastDir" = "/home/${userSetting.username}/Downloads";

                            "browser.download.open_pdf_attachments_inline" =true;
                            "browser.download.start_downloads_in_tmp_dir" =true;
                            "browser.newtabpage.activity-stream.showWeather" =false;
                            "browser.newtabpage.enabled" = false;
                            "browser.quitShortcut.disabled" = true;
                            "browser.safebrowsing.downloads.enabled" = false;
                            "browser.safebrowsing.malware.enabled" = false;
                            "browser.safebrowsing.phishing.enabled" = false;
                            "browser.search.region" = "US";
                            "browser.sessionstore.max_tabs_undo" =3;
                            "browser.sessionstore.restore_on_demand" =true;
                            "browser.sessionstore.warnOnQuit" = true;
                            "browser.slowStartup.notificationDisabled" =true;
                            # instaed of blank.org or github, they are not reliable for chinese and eink user.
                            "browser.startup.homepage" = "https://search.nixos.org";
                            "browser.tabs.closeTabByDblclick" = true;
                            "browser.tabs.closeWindowWithLastTab" = false;
                            "browser.tabs.loadInBackground" = true;
                            "browser.tabs.tabClipWidth" = 999;
                            "browser.tabs.warnOnClose" = false;
                            "browser.toolbars.bookmarks.visibility" = "never";
                            "browser.translations.automaticallyPopup" = false;
                            "browser.uidensity" = 1;
                            "browser.urlbar.oneOffSearches" = false;
                            "browser.urlbar.shortcuts.tabs" = false;
                            "browser.urlbar.suggest.quicksuggest.sponsored" = false;
                            "browser.visited_color" = "#000000";
                            "canvas.capturestream.enabled" = false;
                            "datareporting.healthreport.service.enabled" = false;
                            "devtools.chrome.enabled" = true;
                            "devtools.debugger.remote-enabled" = true;
                            "dom.ipc.plugins.enabled" = false;
                            "dom.ipc.plugins.enabled.libflashplayer.so" = false;
                            "dom.media.mediasource.enabled" = false;
                            "dom.mozTCPSocket.enabled" = false;
                            "dom.netinfo.enabled" = false;
                            "dom.webaudio.enabled" = false;
                            "dom.webvtt.enabled" = false;
                            "extensions.activeThemeID" = "{5f71ffe3-23e2-49b8-b75e-2c032ef4a1d9}";
                            "extensions.pocket.enabled" = false;
                            "extensions.update.enabled" = false;
                            "extensions.webextensions.restrictedDomains" = "";
                            "font.name-list.emoji" = "";
                            "font.name.monospace.x-western" ="JetBrainsMono Nerd Font Mono";
                            "full-screen-api.transition.timeout" = "0";
                            "full-screen-api.warning.delay" = "0";
                            "full-screen-api.warning.timeout" = "0";
                            "general.smoothScroll" = false;
                            "general.useragent.compatMode.firefox" = true;
                            "geo.enabled" = false;
                            "gfx.font_rendering.fontconfig.max_generic_substitutions" = 127;
                            # "gfx.webrender.all" = false;
                            "image.jxl.enabled" = true;
                            "layout.css.prefers-color-scheme.content-override" = 1;
                            "browser.ml.linkPreview.enabled" = false;
                            "media.autoplay.block-event.enabled" = true;
                            "media.autoplay.default" = 2;
                            "media.videocontrols.picture-in-picture.keyboard-controls.enabled" = false;
                            "media.av1.enabled" = false;
                            "media.block-autoplay-until-in-foreground" = true;
                            "media.block-play-until-visible" = true;
                            "media.disabled" =true;
                            "media.eme.enabled" = false;
                            "media.ffmpeg.enabled" = false;
                            "media.ffmpeg.vaapi.enabled" = true;
                            "media.ffvpx.enabled" = false;
                            "media.flac.enabled" = false;
                            "toolkit.legacyUserProfileCustomizations.stylesheets" = lock-true; # This is needed for other userX.css files
                            "media.getusermedia.screensharing.enabled" = false;
                            "media.gmp-widevinecdm.enabled" = false;
                            "media.h264.enabled" = false;
                            "media.libvpx.enabled" = false;
                            "media.mediasource.enabled" = false;
                            "media.mediasource.mp4.enabled" = false;
                            "media.mediasource.webm.enabled" = false;
                            "media.mediasource.whitelist" = false;
                            "media.mp4.enabled" = false;
                            "media.navigator.audio.enabled" = false;
                            "media.navigator.enabled" = false;
                            "media.navigator.video.enabled" = false;
                            "media.ogg.enabled" = false;
                            "media.peerconnection.enabled" =false;
                            "media.peerconnection.ice.default_address_only" =true;
                            "media.wave.enabled" = false;
                            "media.webm.enabled" = false;
                            "media.webspeech.recognition.enable" = false;
                            "media.webspeech.synth.enabled" = false;
                            "network.dns.echconfig.enabled" = true;
                            "network.dns.http3_echconfig.enabled" = true;
                            "permissions.default.image" = 2;
                            "places.history.enabled" = true;
                            "plugin.state.flash" = "0";
                            "privacy.donottrackheader.enabled" = true;
                            "privacy.resistFingerprinting" = true;
                            "privacy.resistFingerprinting.block_mozAddonManager" = true;
                            "privacy.trackingprotection.cryptomining.enabled" =true;
                            "privacy.trackingprotection.enabled" = true;
                            "privacy.trackingprotection.fingerprinting.enabled" = true;
                            "privacy.trackingprotection.socialtracking.enabled" = true;
                            "reader.parse-on-load.enabled" = false;
                            "security.enterprise_roots.auto-enabled" = false;
                            "security.enterprise_roots.enabled" = false;
                            "services.sync.engine.history" = true;
                            "services.sync.prefs.sync-seen.services.sync.prefs.sync.capability.policy.maonoscript.sites" =true;
                            "services.sync.prefs.sync.capability.policy.maonoscript.sites" =true;
                            "signon.autofillForms" = false;
                            # "signon.rememberSignons" = true;
                            "svg.context-properties.content.enabled" = true;
                            "toolkit.crashreporter.dataDirectory" = "";
                            "toolkit.crashreporter.enabled" = false;
                            "toolkit.telemetry.archive.enabled" = false;
                            "toolkit.telemetry.enabled" = false;
                            "toolkit.telemetry.rejected" = false;
                            "toolkit.telemetry.unified" = false;
                            # Convenience
                            "browser.formfill.enable" = false;
                            "browser.search.suggest.enabled" = false;
                            "browser.search.suggest.enabled.private" = false;
                            "browser.urlbar.suggest.searches" = false;
                            "browser.urlbar.showSearchSuggestionsFirst" = false;
                            "signon.rememberSignons" = false; # Stop asking to save passwords
                            "extensions.formautofill.addresses.capture.enabled" = false; # Stop asking to save addresses
                            # Styling
                            "browser.compactmode.show" = true;
                            # Containers
                            "privacy.userContext.enabled" = true;
                            "privacy.userContext.ui.enabled" = true;
                            # Downloads
                            "browser.download.useDownloadDir" = true;
                            "browser.download.always_ask_before_handling_new_types" = true;
                            # Privacy
                            "privacy.sanitize.sanitizeOnShutdown" = true;
                            "privacy.clearOnShutdown_v2.cache" = true;
                            "privacy.clearOnShutdown_v2.historyFormDataAndDownloads" = true;
                            "privacy.clearOnShutdown_v2.browsingHistoryAndDownloads" = true;
                            "privacy.clearOnShutdown_v2.downloads" = true;
                            "privacy.clearOnShutdown_v2.formdata" = true;
                            "privacy.clearOnShutdown_v2.cookiesAndStorage" = true;
                            # # HTTPS only
                            # # "dom.security.https_only_mode" = true;
                            "ui.key.menuAccessKey" = 17;
                            "ui.key.menuAccessKeyFocuses" = false;
                            "ui.caretBlinkTime" = "0";
                            "ui.caretBlinkCount" = -1;
                            "ui.caretWidth" = 3;
                            "webgl.disabled" = true;
                            "widget.non-native-theme.scrollbar.style" = 3;
                            "xpinstall.signatures.required" = false;

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
                  EDITOR = "emacsclient --socket-name=/run/user/1000/emacs/server";
                  RUSTUP_DIST_SERVER = "https://rsproxy.cn";
                  RUSTUP_UPDATE_ROOT = "https://rsproxy.cn/rustup";
                  GOPATH = "~/.go";
                  GTK_IM_MODULE = lib.mkForce "";
                };
                fonts.fontDir.enable = true; #  fc-cache -fv
                fonts.packages = with pkgs;
                  lib.mkForce [ # fc-list : family | sed 's/,/\n/g' | sort -u

                    # Math fonts
                    tex-gyre.adventor
                    tex-gyre.bonum
                    tex-gyre.chorus
                    tex-gyre.cursor
                    tex-gyre.heros
                    tex-gyre.pagella
                    tex-gyre.schola
                    tex-gyre.termes

                    # wqy_zenhei

                    nerd-fonts.fira-code
                    nerd-fonts.fira-mono
                    # nerd-fonts.noto
                    # nerd-fonts.terminess-ttf
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
                sops.age.keyFile = "/etc/nixos/keys.txt";
                sops.secrets."gh_hosts.yml" = {
                  owner = userSetting.username;
                  path = "/home/${userSetting.username}/.config/gh/hosts.yml";
                };
                sops.secrets."gb" = {
                  owner = userSetting.username;
                  path = "/home/${userSetting.username}/.github_token";
                };
                sops.secrets."authinfo" = {
                  owner = userSetting.username;
                  path = "/home/${userSetting.username}/.authinfo";
                };
                sops.secrets."gc" = {
                  owner = userSetting.username;
                  path = "/home/${userSetting.username}/.git-credential";
                };
                sops.secrets.dk = {
                  owner = userSetting.username;
                };
                sops.secrets.nixAccessTokens = {
                  mode = "0440";
                  group = config.users.groups.keys.name;
                };
                sops.secrets.ty = { owner = userSetting.username; };
                sops.secrets.mj = { owner = userSetting.username; };
                sops.secrets.ph = { owner = userSetting.username; };
                sops.secrets.oy = { owner = userSetting.username; };
                sops.secrets.oo = { owner = userSetting.username; };
                networking.extraHosts = ''
                  # 其嗜欲深者，其天机浅
                  # 消费主义让我们沉迷于物质/精神消费中，
                  # 通过让我们接触各种光怪陆离的东西来丰富我们的身份认同感，
                  # 这也是当今时代互联网正在加速实现的事情…
                  # 但这是以牺牲掌握任何技能为代价换来的，
                  # 我们沉迷得越深，想要掌握一项技能的愿望就会越来越淡化。
                  # 0.0.0.0 emacs-china.org
                  # 0.0.0.0 hackaday.com
                  # 0.0.0.0 ziggit.dev
                  # 0.0.0.0 chatgpt.com
                  # 0.0.0.0 www.google.com.hk
                  # 0.0.0.0 www.google.com
                  # 0.0.0.0 google.com.hk
                  # 0.0.0.0 google.com
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
                  power-profiles-daemon.enable = true; # false 为不自动挂起
                  openssh = {
                    enable = false;
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
                    # enable = userSetting.sound;
                    audio.enable = userSetting.sound;
                    pulse.enable = userSetting.sound;
                    alsa = {
                      enable = userSetting.sound;
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
                      SSID = "My-NixOS";
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
                      enable = true;
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
                    enable = false;
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
                      "c-intro.doc" = {
                        listen = [{
                          addr = "0.0.0.0";
                          port = 3004;
                        }];
                        root =
                          "${pkgs.c-intro-and-ref}/share/doc/c-intro-and-ref/";
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
                      wiktionary
                      wordnet
                    ];
                  };
                  xserver.dpi = 192;
                  emacs = {
                    enable = true;
                    package =
                      (pkgs.unstable.emacsPackagesFor
                        (pkgs.unstable.emacs-pgtk.override {
                          withNativeCompilation = true;
                          withSQLite3 = true;
                        })).emacsWithPackages
                        (epkgs: (with epkgs.melpaStablePackages; [ ])
                      ++ (with epkgs.melpaPackages; [

                        ### black MAGIC to Jumping Everwhere
                        # dired-rsync
                        ztree
                        cff
                        dired-subtree
                        dumb-jump
                        goto-chg
                        rg

                        ## Expand & Edit
                        # rotate-text # no on melpa
                        avy
                        expand-region
                        iedit
                        move-text
                        multiple-cursors
                        surround
                        yasnippet # use tempel?(wtf.. it's 31 year old)

                        ### Do Anything In Emacs©
                        #### Service Manager
                        # docker
                        # kubernetes
                        # ovpn-mode
                        # journalctl-mode
                        # syslog-mode

                        #### Project Manager
                        # disproject
                        # magit-commit-mark
                        # magit-tbdiff
                        # magit-todos #why not just grep TODO??
                        consult-gh-embark # 集 emacs 牛包之大成
                        consult-gh-forge
                        consult-gh-with-pr-review
                        magit
                        forge
                        git-link
                        git-timemachine

                        #### Get Term
                        # eshell-toggle # still
                        # with-editor # magit have it already
                        multi-vterm
                        vterm

                        #### Read Books 文灭志，博溺心，少读书，多睡觉。
                        # pandoc-mode
                        nov
                        pdf-tools
                        saveplace-pdf-view

                        #### Chinese Input Method
                        # 为什么有人需要让浏览器能用vim(连文本框也要)
                        # TMD©，直接在emacs写完再复制不就行了吗?
                        # vimium/tridactyl/vimium C/surfinger…
                        # 去TM的假装编辑器的浏览器插件们，我就呆在编辑器!
                        pyim

                        #### read log/manual/doc/dict/gpt/... in emacs
                        # doxymacs # anything relate to c++ is a f**ked mess.
                        # posix-manual
                        # sicp
                        devdocs-browser
                        gptel # seems like doctor-plus. I think using woman is better.
                        # tldr # I just don't want to use it because it's too bloate.. which fetch 140mb just for 50 countries's languages...
                        helpful # Just use orginal help-mode is better.
                        quick-sdcv

                        ### the you-know-who guy created a huge list of amazing pkgs
                        consult
                        embark
                        embark-consult
                        vertico
                        orderless
                        marginalia
                        cape
                        corfu

                        ### for Language FANBoys: "YOU DON'T HAVE TO LEARN THOSE"

                        nix-mode
                        # moved to index.org
                        # 我实在想不通，语言为什么这么多? 需求为什么停不了?
                        # 这些程序也就几百行… 为什么会产生这么大的“不简洁感”
                        # 随便配个浏览器就远远大过这些了…
                        # 人们总是过于关心因自己产生的事物。
                        # 或是说“人们总是在意自己没用过的事物”，
                        # 而对轻视已拥有的事物。幻想… 自贱…
                        # 但确实，注释完这些，启动时间从1.76变成了1.01

                        ### save and format
                        electric-operator # 我再也不想要什么自动化了…
                        smart-semicolon # 算了，我再试试，懒化更重要。
                        aggressive-indent # Only for Elisp or some creepy lang.
                        super-save # The greatest thing I ever see in my life.
                        psession # 当然… 自动保存 还是必须的。

                        ### emacs look and feel
                        no-emoji # for my autism
                        hide-mode-line # for my adhd
                        ligature # for my lazy-eye
                        real-mono-themes # it's my THEME

                        ### feel even better
                        alert
                        # compile-angel
                        envrc
                        c-eval
                        basic-c-compile
                        trashed
                        undo-fu
                        undo-fu-session
                        wgrep

                        # smart-compile 算了吧，简单点，你一天写80种语言
                        # 可能连它们的hello world都写不出来。
                        # consult-notes 难道，哥们不知道写笔记不如写代码?
                        # obsidian/yuque/typora，你让的笔记传销坑还少吗?
                        # 笔记就是每天后悔，明天期待，复制粘贴，llm输出。
                        # 谁tm的用笔记就是脑瓜子弱智… 专门给llm加语料…
                        # fsrs/anki-editor 难道，哥们是出题人? 一天天的搞记忆法。
                        # 买本单词本记记吧…
                        # viking-mode 难道，哥们不知道expand-region?
                        # call-graph 难道，哥们以为画个图就理清楚了?
                        # el-fetch 难道，哥们真的写得来elisp就写个包?
                        # 天天关心天气质量和自己编辑器占用多少内存的男性
                        # 大概率是抖m的男娘吧，bro，get some help!
                        # 甩几下铁，不关心这些那些，只是用心做事…
                        # 那里需要管什么RAM和ROM的几百MB…
                        # BTW, Org-mode is SHIT, just write done plain list.
                        # Markdown > Org > Latex
                      ]) ++ (with epkgs.elpaPackages; [
                        plz # tavily-search
                      ])
                      ++ (with pkgs; [
                      ]));
                  };
                };
                console = {
                  font = "latarcyrheb-sun32";
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
                    sub_airport_1: \"$(cat ${config.sops.secrets.mj.path})\"
                    # sub_airport_2: \"$(cat ${config.sops.secrets.oo.path})\"
                    # sub_airport_3: \"$(cat ${config.sops.secrets.oy.path})\"
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
set \$mod Mod4
set \$term foot
set \$menu wmenu-run -N '#ffffff' -n '#000000' -M '#000000' -m '#ffffff' -S '#000000' -s '#ffffff' -f 'monospace 18' -b -i
input type:keyboard xkb_options fkeys:basic_13-24
output * bg ~/.local/share/mysource/meditate.jpg center #FFFFFF
output * transform 270

exec swayidle -w timeout 1140 'notify-send \"left 1min to Hibernate\"'   timeout 1200 'systemctl hibernate'      before-sleep 'notify-send \"In Hibernated\"'

bindsym Print exec grim -g \"\$(slurp)\" - | wl-copy && wl-paste > ~/Downloads/Screenshot-\$(date +%F%T).png | notify-send \"Screenshot of the region taken\"
bindsym Shift+Print exec grim -g \"\$(slurp -o -r -c '#ff0000ff')\" -t ppm - | satty --filename - --fullscreen --output-filename ~/Downloads/satty-\$(date '+%Y%m%d-%H:%M:%S').png
bindsym Mod1+Shift+Print exec wf-recorder

bindsym \$mod+p exec bash -c 'paperlike-cli -i2c /dev/i2c-4 -clear;swaylock -i ~/.local/share/mysource/meditate.jpg fill #FFFFFF;'

bindsym \$mod+Shift+q exec swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' -B 'Yes, exit sway' 'swaymsg exit' --background '#ffffff' --border '#ffffff' --text  '#000000' --button-text '#000000' --button-background '#ffffff'

bindsym \$mod+Shift+r reload
bindsym \$mod+Shift+h fullscreen
bindsym \$mod+Shift+comma exec emacsclient --eval \"(swaywindow)\";
bindsym \$mod+t fullscreen
bindsym \$mod+s exec emacsclient --eval \"(swaywindow)\";
bindsym \$mod+r  exec emacsclient --eval \"(swayrotate)\";

# xremap is just shit... just use wtype.
bindsym Ctrl+comma   exec wtype \"(\";
bindsym Ctrl+period   exec wtype \")\";
bindsym Ctrl+i    exec wtype \";\";
bindsym Ctrl+Shift+i    exec wtype \":\";
bindsym Mod1+comma    exec wtype \"{\";
bindsym Mod1+period    exec wtype \"}\";
bindsym Mod4+comma    exec wtype \"[\";
bindsym Mod4+period    exec wtype \"]\";
bindsym Mod1+Ctrl+comma    exec wtypefix;
bindsym Mod1+Ctrl+period    exec wtypefix;
bindsym Mod1+Ctrl+i    exec wtype \"-M\" \"ctrl\" \"n\" \"-m\" \"ctrl\"  \"-M\" \"ctrl\" \";\" \"-m\" \"ctrl\"

bindsym F13 exec  wtype llqingsong@qq.com;
bindsym F14 exec  wtype  $(cat ${config.sops.secrets.ph.path});
bindsym F15 exec  wtype explain in chinese thank you!;
bindsym F16 exec  wtype explain the code in chinese line by line;

bindsym \$mod+Shift+slash exec \$menu
bindsym \$mod+Shift+space floating toggle
bindsym \$mod+space focus mode_toggle

bindsym \$mod+Shift+w kill
bindsym \$mod+Return exec \$term

bindsym \$mod+Shift+y exec cliphist list | wmenu -N '#ffffff' -n '#000000' -M '#000000' -m '#ffffff' -S '#000000' -s '#ffffff' -f 'monospace 18' -b -i -l 16  | cliphist decode | wl-copy
bindsym \$mod+Shift+d exec cliphist list | wmenu -N '#ffffff' -n '#000000' -M '#000000' -m '#ffffff' -S '#000000' -s '#ffffff' -f 'monospace 18' -b -i -l 10  | cliphist delete

floating_modifier \$mod normal

seat * hide_cursor 888

gaps top 0
gaps outer 0
gaps inner 0

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

client.focused #ffffff  #ffffff  #000000  #ffffff  #ffffff
client.focused_inactive  #ffffff  #000000  #ffffff  #ffffff  #ffffff
client.focused_tab_title #ffffff  #ffffff  #000000
client.unfocused         #ffffff  #000000  #ffffff  #ffffff  #ffffff
client.urgent #ffffff  #ffffff  #000000  #ffffff  #ffffff
client.placeholder       #ffffff  #ffffff  #000000  #ffffff  #ffffff
client.background        #ffffff

font pango:Bookerly 1
titlebar_padding 1
titlebar_border_thickness 0

exec swaymsg workspace number 5

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

bindsym \$mod+Shift+e exec  emacs --debug-init
bindsym \$mod+e exec  emacsclient -n -c -s server

for_window [app_id=\"emacs\"] border none
for_window [app_id=\"emacs\"] titlebar_padding 0
for_window [app_id=\"emacs\"] titlebar_border_thickness 0

for_window [app_id=\"emacsclient\"] border none
for_window [app_id=\"emacsclient\"] titlebar_padding 0
for_window [app_id=\"emacsclient\"] titlebar_border_thickness 0

for_window [app_id=\"foot\"] border none
for_window [app_id=\"foot\"] titlebar_padding 0
for_window [app_id=\"foot\"] titlebar_border_thickness 0

for_window [app_id=\"firefox\"] border none
for_window [app_id=\"firefox\"] titlebar_padding 0
for_window [app_id=\"firefox\"] titlebar_border_thickness 0

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
                        echo "access-tokens = github.com = $(cat ${config.sops.secrets.gb.path})" > $config_file
                          # Set the ownership and permissions of the config file so the user can edit it
                          chown ${userSetting.username}: $config_file
                            fi
                  '';
                  wantedBy = [ "hibernate.target" "multi-user.target" ];
                  description = "Generate Nix Config for User";
                  restartIfChanged = true;
                  # Ensure the script runs after the system is up and hibernation is set up
                  after = [ "hibernate.target" ];
                };

                systemd.services.adjustPaperLight = {
                  enable = false;
                  description = "no light";
                  after = [ "hibernate.target" ];
                  script =
                    "/run/current-system/sw/bin/paperlike-cli -i2c /dev/i2c-4 -light1 0;";
                  wantedBy = [ "hibernate.target" "multi-user.target" ];
                  path = [ "/nix/store" ];
                };
                networking = {
                  hostName = userSetting.hostname;
                  networkmanager.enable = false;
                  firewall.enable = false;
                };
                system.nssModules = lib.mkForce [ ]; # for dnsmasq
                hardware.i2c.enable = true;
                boot.kernelModules = [ "i2c-dev"];
                boot.extraModulePackages = [ ];
                # boot.binfmt.emulatedSystems =   [ "aarch64-linux" "riscv64-linux" "i686-linux" ];
                # boot = {
                #   kernel = {
                #     sysctl = {
                #
                #       # SysRQ is useful when things hang.
                #       "kernel.sysrq" = 1;
                #       # Reclaim file pages as often as anon pages.
                #       "vm.swappiness" = 100;
                #
                #       # forward network packets that are not destined for the interface on which they were received
                #       "net.ipv4.conf.all.forwarding" = true;
                #       "net.ipv6.conf.all.forwarding" = true;
                #       "net.ipv4.conf.br-lan.rp_filter" = 1;
                #       "net.ipv4.conf.wan.rp_filter" = 1;
                #     };
                #   };
                # };
                boot.kernelPackages = pkgs.unstable.linuxPackages;
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
                    generateCaches = false; # I hate take time
                  };
                  dev.enable = true;
                };
                hardware = { bluetooth.enable = true;
                  bluetooth.powerOnBoot = true;
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
                time.timeZone = "Asia/Hong_Kong";
                security.rtkit.enable = true;
                programs.bash.undistractMe.playSound = userSetting.sound;
                programs.soundmodem.enable = userSetting.sound;
                xdg.sounds.enable = userSetting.sound;
                environment.etc = {
                  "wireplumber/main.lua.d/90-suspend-timeout.lua".text = ''
                    apply_properties = {["session.suspend-timeout-seconds"] = 0;};
                  '';
                };
                boot.extraModprobeConfig = ''
                  options snd-hda-intel power_save=0 power_save_controller=N
                '';
                programs.fish = {
                  enable = true;
                  interactiveShellInit = ''
                          # fish_add_path $HOME/bin
                          # fish_add_path $HOME/.local/bin/
                        # zoxide init fish | source;
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
                      "https://cache.nixos.org/" "https://nix-community.cachix.org"
                      "https://mirrors.ustc.edu.cn/nix-channels/store"
                      # "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store"
                      # "https://mirror.sjtu.edu.cn/nix-channels/store"
                    ];
                    trusted-substituters = [
                      "https://cache.nixos.org/" "https://nix-community.cachix.org"
                      "https://mirrors.ustc.edu.cn/nix-channels/store"
                      # "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store"
                      # "https://mirror.sjtu.edu.cn/nix-channels/store"
                    ];
                    trusted-public-keys = [
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
    }; # nix flake update hosts
    # nixpkgs-old.url = "github:nixos/nixpkgs/nixos-25.05";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Still, after 20 years passed, nobody fucking care about these.
    # sudo nix-channel --add https://nixos.org/channels/nixos-unstable
    # sudo nix-channel --update
    sops-nix.url = "github:Mic92/sops-nix";
  };
  description = "My Config";
  nixConfig = {
    builders-use-substitutes = true;
    experimental-features = [ "nix-command" "flakes" ];
    trusted-substituters = [
      "https://cache.nixos.org" "https://nix-community.cachix.org"
      "https://mirrors.ustc.edu.cn/nix-channels/store"
    ];
    extra-trusted-substituters =
      [ "https://cache.nixos.org" "https://nix-community.cachix.org" ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };
}
