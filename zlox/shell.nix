let
  pkgs = import <unstable> { };
  myTracy = pkgs.tracy.overrideAttrs (oldAttrs: {
    buildInputs = oldAttrs.buildInputs
      ++ pkgs.lib.optionals pkgs.stdenv.isDarwin
      [ pkgs.darwin.apple_sdk.frameworks.UniformTypeIdentifiers ];
  });
in with pkgs;
mkShell {
  buildInputs = [ myTracy ];

  nativeBuildInputs = [ ];
}