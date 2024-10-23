#!/usr/bin/env bash

function priv_clippit
(
    cat <<EOF
Usage: bash ${0} [OPTIONS]
Options:
    build   Build program
EOF
)

function pub_build
(
    git submodule update --init --recursive
    lazbuild --add-package-link 'bgrabitmap/bgrabitmap/bgrabitmappack.lpk'
    lazbuild --add-package-link 'bgracontrols/bgracontrols.lpk'
    lazbuild --add-package-link 'lazpaintcontrols/lazpaintcontrols.lpk'
    lazbuild --recursive --build-mode=release 'lazpaint/lazpaint.lpi'
    strip 'lazpaint/release/bin/lazpaint'
)

function priv_main
(
    set -euo pipefail
    if !(which lazbuild); then
        source '/etc/os-release'
        case ${ID:?} in
            debian | ubuntu)
                sudo apt-get update
                sudo apt-get install -y lazarus
            ;;
        esac
    fi
    if ((${#})); then
        case ${1} in
            build) pub_build;;
        esac
    else
        priv_clippit
    fi
)

priv_main "${@}"
