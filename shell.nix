{ nixpkgs ? import <nixpkgs> {} }:

let
  pinnedPkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/23.11.tar.gz";
    sha256 = "1ndiv385w1qyb3b18vw13991fzb9wg4cl21wglk89grsfsnra41k";
  };
  pkgs = import pinnedPkgs {};
in

pkgs.mkShell {
  buildInputs = with pkgs; [
    pkgs.cargo
    pkgs.rustc

    pkgs.iconv
    pkgs.llvm_17
    pkgs.ncurses
    pkgs.zlib
  ];
}
