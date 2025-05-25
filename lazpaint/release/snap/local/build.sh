#!/bin/bash

ln -s lazpaint/release/snap ../../../../snap

pushd ../../../..
snapcraft --debug --use-lxd -v $@
popd

rm ../../../../snap
