#!/usr/bin/env bash

function f_log
{
    declare -rAi TAG=(
        [error]=31
        [info]=32
        [audit]=33
    )
    printf '%(%y-%m-%d_%T)T\x1b[%dm\t%s:\t%b\x1b[0m\n' -1 "${TAG[${1,,:?}]}" "${1^^}" "${2:?}" 1>&2
    if [[ ${1} == 'error' ]]; then
        return 1
    fi
}

function f_build
{
    git submodule update --init --recursive
    lazbuild --add-package 'bgrabitmap/bgrabitmap/bgrabitmappack.lpk'
    lazbuild --add-package 'bgracontrols/bgracontrols.lpk'
    lazbuild --add-package 'lazpaintcontrols/lazpaintcontrols.lpk'
    lazbuild 'lazpaint/lazpaint.lpi'
}

function f_main
{
    set -eo pipefail
    if !(which lazbuild); then
        source '/etc/os-release'
        case ${ID:?} in
            debian | ubuntu)
                sudo apt-get update
                sudo apt-get install -y lazarus
            ;;
        esac
    fi
    case ${1} in
        build) f_build;;
    esac
}

f_main "${@}"
