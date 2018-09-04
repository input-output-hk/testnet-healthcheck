{ pkgs ? import (fetchTarball https://github.com/NixOS/nixpkgs/archive/release-18.03.tar.gz) {}
, testnetHealthcheckSrc ? ./..
}:

(import ./stack.nix {
  inherit testnetHealthcheckSrc pkgs;
})."testnet-healthcheck"
