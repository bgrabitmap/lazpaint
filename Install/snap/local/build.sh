#!/bin/bash

ln -s Install/snap ../../../snap

pushd ../../..
snapcraft --debug --use-lxd
popd

rm ../../../snap
