{ nixpkgs ? <nixpkgs>
, testnetHealthcheckSrc ? ./..
}:
let pkgs = import nixpkgs {};
in {
  testnetHealthcheckClient = pkgs.callPackage ../client {
    inherit testnetHealthcheckSrc;
  };

  testnetHealthcheckServer = pkgs.callPackage ../server {
    inherit testnetHealthcheckSrc;
  };
}
