{
  description = "lem";

  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  # cribbing a lot from https://github.com/dariof4/lem-flake
  #            and from https://github.com/eriedaberrie/my-nix-packages
  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems =
        [ "aarch64-darwin" "aarch64-linux" "x86_64-darwin" "x86_64-linux" ];
      perSystem = { self', pkgs, lib, ... }:
        let
          lem = pkgs.sbcl.buildASDFSystem rec {
            pname = "lem";
            version = "unstable";
            systems = [ "lem-fake-interface" ];
            src = ./.;

            nativeBuildInputs = with pkgs; [
              autoconf
              automake
              libffi
              libtool
              makeBinaryWrapper
              pkg-config
            ];

            nativeLibs = with pkgs; [ libffi openssl ];

            qlBundleLibs = pkgs.stdenvNoCC.mkDerivation {
              pname = "${pname}-qlot-bundle";
              inherit src version;

              nativeBuildInputs = with pkgs; [
                sbcl.pkgs.qlot-cli
                which
                git
                cacert
              ];

              installPhase = ''
                runHook preInstall

                export HOME=$(mktemp -d)
                qlot install --jobs $NIX_BUILD_CORES --no-deps --no-color
                qlot bundle --no-color

                # Unnecessary and also platform-dependent file
                rm .bundle-libs/bundle-info.sexp

                # Remove vendored .so files
                find .bundle-libs \
                  -type f '(' -name '*.so' -o -name '*.dll' ')' -exec rm '{}' ';'

                cp -r .bundle-libs $out

                runHook postInstall
              '';

              dontBuild = true;
              dontFixup = true;
              outputHashMode = "recursive";
              outputHash = if pkgs.stdenv.isDarwin then
                "sha256-BV1m58fUe1zfLH5iKbDP7KTNhv1p+g3W99tFQFYxPqs="
              else
                "sha256-NtCSBxEo/4uuhlLc9Xqlq+PM1fAbDfRBH64imMLvKYE=";
            };

            configurePhase = ''
              runHook preConfigure
              mkdir -p $out/share/lem
              pushd $out/share/lem
                cp -r $qlBundleLibs .bundle-libs
                chmod -R +w .bundle-libs

                # build async-process native part
                pushd .bundle-libs/software/async-process-*
                  chmod +x bootstrap
                  ./bootstrap
                popd

                # nixpkgs patch to fix cffi build on darwin
                pushd .bundle-libs/software/cffi-*
                  patch -p1 <${inputs.nixpkgs}/pkgs/development/lisp-modules/patches/cffi-libffi-darwin-ffi-h.patch
                popd
              popd

              source ${inputs.nixpkgs}/pkgs/development/lisp-modules/setup-hook.sh
              buildAsdfPath

              runHook postConfigure
            '';

            buildScript = pkgs.writeText "build-lem.lisp" ''
              (defpackage :nix-cl-user
                (:use :cl))

              (in-package :nix-cl-user)
              (load "${lem.asdfFasl}/asdf.${lem.faslExt}")

              ;; Avoid writing to the global fasl cache
              (asdf:initialize-output-translations
                '(:output-translations :disable-cache
                                       :inherit-configuration))

              (defvar *systems* (uiop:split-string (uiop:getenv "systems")))
              (defvar *out-path* (uiop:getenv "out"))
              (defvar *share-path* (concatenate 'string
                                    *out-path*
                                    "/share/lem"))
              (defvar *bundle-path* (concatenate 'string
                                    *share-path*
                                    "/.bundle-libs/bundle.lisp"))

              ;; Initial load
              (let ((asdf:*system-definition-search-functions*
                       (copy-list asdf:*system-definition-search-functions*)))
                (load *bundle-path*)
                (loop :for system :in *systems*
                      :do (asdf:load-system system)))

              ;; Load the bundle on every startup
              (uiop:register-image-restore-hook
                (lambda ()
                  (load *bundle-path*))
                   nil)

              (setf uiop:*image-entry-point* #'lem:main)
              (uiop:dump-image
                "lem"
                :executable t
                :compression t)
            '';
            installPhase = ''
              runHook preInstall

              mkdir -p $out/bin $out/share/lem
              mv lem $out/bin
              wrapProgram $out/bin/lem \
                --prefix LD_LIBRARY_PATH : "$LD_LIBRARY_PATH" \
                --prefix DYLD_LIBRARY_PATH : "$DYLD_LIBRARY_PATH"

              cp -r . $out/share/lem

              runHook postInstall
            '';
          };

          devShell = pkgs.mkShell {
            packages = with pkgs; [
              autoconf
              automake
              libffi.dev
              libtool
              ncurses.dev
              pkg-config
              sbcl
              sbcl.pkgs.qlot-cli
              which # this is available in the stdenv and most sane systems
            ];
            # Normally we would include pkg-config and list these dependencies
            # in the packages attribute, but it does not appear that pkg-config
            # is being used when available.
            shellHook = with pkgs; ''
              export LD_LIBRARY_PATH="''${LD_LIBRARY_PATH}:${ncurses.out}/lib:${SDL2}/lib:${SDL2_ttf}/lib:${SDL2_image}/lib:${libffi}/lib:${openssl.out}/lib"
            '';
          };
        in
        {
          devShells.default = devShell;
          packages.lem-ncurses = lem.overrideLispAttrs (o: {
            pname = "lem-ncurses";
            meta.mainProgram = "lem";
            systems = [ "lem-ncurses" ];
            nativeLibs = with pkgs; o.nativeLibs ++ [ ncurses ];
          });
          packages.lem-sdl2 = lem.overrideLispAttrs (o: {
            pname = "lem-sdl2";
            meta.mainProgram = "lem";
            systems = [ "lem-sdl2" ];
            nativeLibs = with pkgs;
              o.nativeLibs ++ [ SDL2 SDL2_ttf SDL2_image ];
          });

          packages.default = self'.packages.lem-ncurses;
        };
    };
}
