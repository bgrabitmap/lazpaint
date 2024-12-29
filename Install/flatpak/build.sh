#!/bin/bash
set -x

BUILD_DIR=build
REPO_DIR=repo
FLATPAK_ID=io.github.bgrabitmap.LazPaint

# Install dependencies
flatpak install --system flathub org.kde.Sdk//5.15-24.08
flatpak install --system flathub org.kde.Platform//5.15-24.08
flatpak install --system flathub org.freedesktop.Sdk.Extension.freepascal//24.08
flatpak install --system flathub org.flatpak.Builder

# Build
flatpak run org.flatpak.Builder --force-clean --sandbox --user --install --ccache --mirror-screenshots-url=https://dl.flathub.org/media/ --repo=$REPO_DIR $BUILD_DIR $FLATPAK_ID.yml

# Run
flatpak run $FLATPAK_ID

# Linter
flatpak run --command=flatpak-builder-lint org.flatpak.Builder manifest $FLATPAK_ID.yml
flatpak run --command=flatpak-builder-lint org.flatpak.Builder repo $REPO_DIR
