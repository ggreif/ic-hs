self: super: {
  candid = super.callPackage ./candid.nix { };
  ic-hs = super.callPackage ./ic-hs.nix { };
  leb128-cereal = super.callPackage ./leb128-cereal.nix { };
  winter = super.callPackage ./winter.nix { };
}
