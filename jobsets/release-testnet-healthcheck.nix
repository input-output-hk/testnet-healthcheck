{ nixpkgs ? <nixpkgs>
, testnetHealthcheckSrc ? ../.
}:
let pkgs = import nixpkgs {};
in {
  testnetHealthcheckClient = pkgs.callPackage ../client {
    testnetHealthcheckSrc = builtins.toPath (testnetHealthcheckSrc + "/client");
  };

  testnetHealthcheckServer = pkgs.callPackage ../server {
    testnetHealthcheckSrc = builtins.toPath (testnetHealthcheckSrc + "/server");
  };
}
