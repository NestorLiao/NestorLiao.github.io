{
  description = "A Nix-flake-based Note development environment";

  inputs = {
    nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.1.*.tar.gz";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, rust-overlay, }:
    let
      supportedSystems =
        [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forEachSupportedSystem = f:
        nixpkgs.lib.genAttrs supportedSystems (system:
          f {
            pkgs = import nixpkgs {
              inherit system;
              overlays =
                [ rust-overlay.overlays.default self.overlays.default ];
            };
          });
    in {
      overlays.default = final: prev: {
        rustToolchain = let rust = prev.rust-bin;
        in if builtins.pathExists ./rust-toolchain.toml then
          rust.fromRustupToolchainFile ./rust-toolchain.toml
        else if builtins.pathExists ./rust-toolchain then
          rust.fromRustupToolchainFile ./rust-toolchain
        else
          rust.stable.latest.default.override {
            extensions = [ "rust-src" "rustfmt" ];
          };
      };

      devShells = forEachSupportedSystem ({ pkgs }: {
        default = pkgs.mkShell {
          venvDir = ".venv";
          packages = with pkgs;
            [
              python311

              clang-tools
              cmake
              codespell
              rocmPackages.llvm.clang
              conan
              cppcheck
              doxygen
              bear
              gtest
              lcov
              vcpkg
              gdb
              vcpkg-tool

              lua

              zig
              zls
              lldb

              texlive.combined.scheme-full
              texlab
              tectonic

            ] ++ (with pkgs.python311Packages; [ pip venvShellHook ]);
          env = {
            # Required by rust-analyzer
            RUST_SRC_PATH =
              "${pkgs.rustToolchain}/lib/rustlib/src/rust/library";
          };
        };
      });
    };
}
