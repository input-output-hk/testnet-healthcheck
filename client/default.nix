{ pkgs ? import <nixpkgs> {}
, testnetHealthcheckSrc ? ./..
}:

pkgs.stdenv.mkDerivation {
  buildInputs = with pkgs; [ elmPackages.elm ];
  name = "testnet-healthcheck-client";
  src = testnetHealthcheckSrc;

  configurePhase = ''
    export HOME="$NIX_BUILD_TOP"
    cd client
    elm package install --yes
  '';

  buildPhase = ''
    elm make src/Main.elm --output dist/app.js
    cp -r static/* dist/
  '';

  installPhase = ''
    mv dist $out
  '';
}
