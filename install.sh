#!/usr/bin/env bash

# Stupid person like me,
# Can't tolerate tons of configuration files.
# God bless NixOS and Emacs.
# They are angles for saving Low IQ me.
mkdir -p ~/.emacs.d
mkdir -p  ~/.config/dae
mkdir -p  ~/.config/nixos
sudo chmod 777 /etc/nixos/config.dae
sudo mv /etc/nixos/config.dae ~/.config/dae
sudo chmod 777 /etc/NestorLiao.github.io
sudo mv /etc/NestorLiao.github.io ~/.me
cd ~/.me/
ln -sf $PWD/flake.nix ~/.config/nixos/;
ln -sf $PWD/secrets.yaml ~/.config/nixos/;
ln -sf $PWD/post-init.el ~/.emacs.d/;
sudo cp /etc/nixos/hardware-configuration.nix $PWD/hardware-configuration.nix
chmod 777 $PWD/hardware-configuration.nix
# sudo nix-channel --add https://nixos.org/channels/nixos-unstable
# sudo nix-channel --update
sudo nixos-rebuild switch --flake /home/$USER/.config/nixos#$hostname  --option substituters 'https://mirrors.ustc.edu.cn/nix-channels/store  https://cache.nixos.org' --cores 6 -j 12;

ln -sf $HOME/.local/share/mysource/early-init.el ~/.emacs.d/;
ln -sf $HOME/.local/share/mysource/init.el ~/.emacs.d/;
