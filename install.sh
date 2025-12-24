#!/usr/bin/env bash

mkdir -p ~/.emacs.d
ln -sf $PWD/nixos/ ~/.config/;
ln -sf $PWD/snippets ~/.config/;
ln -sf $PWD/nixos/hmdz.pyim ~/.config/;
ln -sf $PWD/nixos/post-init.el ~/.emacs.d/;
ln -sf $PWD/nixos/init.el ~/.emacs.d/;
ln -sf $PWD/nixos/early-init.el ~/.emacs.d/;
sudo cp /etc/nixos/hardware-configuration.nix $PWD/nixos/hardware-configuration.nix
chmod 777 $PWD/nixos/hardware-configuration.nix
sudo nixos-rebuild switch --flake /home/$USER/.config/nixos#$hostname  --option substituters 'https://mirrors.ustc.edu.cn/nix-channels/store  https://cache.nixos.org' --cores 6 -j 12;
sudo nix-channel --add https://nixos.org/channels/nixos-unstable
sudo nix-channel --update
