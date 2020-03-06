#!/bin/bash
STAGING_DIR="./staging"
BIN_DIR="${STAGING_DIR}/usr/share/lazpaint"
SCRIPT_DIR="${BIN_DIR}/scripts"
SOURCE_SCRIPT_DIR="../../../scripts"
SOURCE_BIN="../bin"

rm -rf "${STAGING_DIR}"
if [ $(arch) = "x86_64" ]; then
  echo Creating 64-bit package...
  cp -r linux64 "${STAGING_DIR}"
  TARGET_OS="linux64"
else
  echo Creating 32-bit package...
  cp -r linux32 "${STAGING_DIR}"
  TARGET_OS="linux32"
fi

VERSION="$(sed -n 's/^Version: //p' $STAGING_DIR/DEBIAN/control)"
echo Version is $VERSION
PACKAGE_NAME="lazpaint${VERSION}_${TARGET_OS}"

rm "${BIN_DIR}/delete.me"
cp "${SOURCE_BIN}/lazpaint" "${BIN_DIR}"
cp "${SOURCE_BIN}/lazpaint_simplified.ini" "${BIN_DIR}"
cp "${SOURCE_BIN}/readme.txt" "${BIN_DIR}"

mkdir "${BIN_DIR}/i18n"
find "${SOURCE_BIN}/i18n" -maxdepth 1 -name *.po -exec cp {} "${BIN_DIR}/i18n" \;

cp -r "${SOURCE_BIN}/models" "${BIN_DIR}/models"

mkdir "${SCRIPT_DIR}"
find "${SOURCE_SCRIPT_DIR}" -maxdepth 1 -name *.py -exec cp {} "${SCRIPT_DIR}" \;
mkdir "${SCRIPT_DIR}/lazpaint" 
find "${SOURCE_SCRIPT_DIR}/lazpaint" -maxdepth 1 -name *.py -exec cp {} "${SCRIPT_DIR}/lazpaint" \;

dpkg-deb --build "${STAGING_DIR}" "${PACKAGE_NAME}.deb"
NO_INSTALL_ARCHIVE="${PACKAGE_NAME}_no_install.tar.gz"
echo "Making ${NO_INSTALL_ARCHIVE}..."
cd "$BIN_DIR"
cd ..
tar -czf "../../../${NO_INSTALL_ARCHIVE}" "lazpaint"
cd ../../..
rm -rf "${STAGING_DIR}"

