#!/usr/bin/env bash

# Keep It Simple and Stupid
mkdir -p ~/.emacs.d
mkdir -p  ~/.config/dae

sudo chmod 777 /etc/nixos/config.dae
sudo chmod 777 /etc/NestorLiao.github.io
sudo chmod 777 /etc/nixos/hardware-configuration.nix

sudo cp /etc/NestorLiao.github.io ~/.me
sudo cp /etc/nixos/config.dae ~/.config/dae
sudo cp /etc/nixos/hardware-configuration.nix ~/.me/hardware-configuration.nix

sudo nixos-rebuild switch --flake /home/$USER/.me#$hostname  --option substituters 'https://mirrors.ustc.edu.cn/nix-channels/store  https://cache.nixos.org' --cores 6 -j 12;

ln -sf $PWD/post-init.el ~/.emacs.d/;
ln -sf $HOME/.local/share/mysource/init.el ~/.emacs.d/;
ln -sf $HOME/.local/share/mysource/early-init.el ~/.emacs.d/;
