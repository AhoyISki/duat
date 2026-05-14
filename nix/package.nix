{ rustPlatform
, lib
, pkg-config

, makeWrapper
}:
let
  cargoToml = builtins.fromTOML (builtins.readFile ../Cargo.toml);
in
rustPlatform.buildRustPackage rec {
  pname = cargoToml.package.name;
  version = cargoToml.package.version;

  src = builtins.path {
    path = ../.;
  };

  nativeBuildInputs = [
    pkg-config
    makeWrapper
  ];
  buildInputs = [ ];

  doCheck = false;

  LD_LIBRARY_PATH = "$LD_LIBRARY_PATH:${lib.makeLibraryPath buildInputs}";

  cargoLock.lockFile = ../Cargo.lock;

  meta = {
    description = cargoToml.package.description;
    homepage = cargoToml.package.homepage;
    license = lib.licenses.gpl3;
    mainProgram = pname;
  };
}
