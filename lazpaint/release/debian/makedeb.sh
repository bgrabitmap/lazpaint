#!/bin/bash
STAGING_DIR="./staging"
USER_DIR="${STAGING_DIR}/usr"
BIN_DIR="${STAGING_DIR}/usr/bin"
SHARE_PARENT_DIR="${STAGING_DIR}/usr/share"
SHARE_DIR="${STAGING_DIR}/usr/share/lazpaint"
DOC_PARENT_DIR="${STAGING_DIR}/usr/share/doc"
DOC_DIR="${STAGING_DIR}/usr/share/doc/lazpaint"
SCRIPT_DIR="${SHARE_DIR}/scripts"
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
mv "${STAGING_DIR}/debian" "${STAGING_DIR}/DEBIAN"

VERSION="$(sed -n 's/^Version: //p' $STAGING_DIR/DEBIAN/control)"
echo Version is $VERSION
PACKAGE_NAME="lazpaint${VERSION}_${TARGET_OS}"

mkdir "${USER_DIR}"

mkdir "${BIN_DIR}" 
cp "${SOURCE_BIN}/lazpaint" "${BIN_DIR}"

mkdir "${SHARE_PARENT_DIR}"

mkdir "${DOC_PARENT_DIR}"
mkdir "${DOC_DIR}"
gzip -9 -n -c changelog >"${DOC_DIR}/changelog.gz"
cp copyright "${DOC_DIR}"

cp -r applications "${SHARE_PARENT_DIR}"
cp -r pixmaps "${SHARE_PARENT_DIR}"
cp -r "man" "${SHARE_PARENT_DIR}" 
gzip -9 -n "${SHARE_PARENT_DIR}/man/man1/lazpaint.1" 

mkdir "${SHARE_DIR}"
cp "${SOURCE_BIN}/lazpaint_simplified.ini" "${SHARE_DIR}"
cp "${SOURCE_BIN}/readme.txt" "${SHARE_DIR}"

mkdir "${SHARE_DIR}/i18n"
find "${SOURCE_BIN}/i18n" -maxdepth 1 -name *.po -exec cp {} "${SHARE_DIR}/i18n" \;

cp -r "${SOURCE_BIN}/models" "${SHARE_DIR}/models"

mkdir "${SCRIPT_DIR}"
find "${SOURCE_SCRIPT_DIR}" -maxdepth 1 -name *.py -exec cp {} "${SCRIPT_DIR}" \;
mkdir "${SCRIPT_DIR}/lazpaint" 
find "${SOURCE_SCRIPT_DIR}/lazpaint" -maxdepth 1 -name *.py -exec cp {} "${SCRIPT_DIR}/lazpaint" \;

dpkg-deb --build "${STAGING_DIR}" "${PACKAGE_NAME}.deb"

NO_INSTALL_ARCHIVE="${PACKAGE_NAME}_no_install.tar.gz"
echo "Making ${NO_INSTALL_ARCHIVE}..."
mv "${BIN_DIR}/lazpaint" "${SHARE_DIR}/lazpaint"
cd "$SHARE_DIR"
cd ..
tar -czf "../../../${NO_INSTALL_ARCHIVE}" "lazpaint"
cd ../../..
rm -rf "${STAGING_DIR}"

