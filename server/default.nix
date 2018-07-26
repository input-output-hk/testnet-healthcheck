{ pkgs ? import <nixpkgs> {}
, testnetHealthcheckSrc ? ./.
}:

(import ./stack.nix {
  inherit testnetHealthcheckSrc pkgs;
})."testnet-healthcheck"
