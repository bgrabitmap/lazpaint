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
TARGET_ARCHITECTURE="$(dpkg --print-architecture)"
VERSION="$(sed -n 's/^Version: //p' debian/control)"

if [ ${TARGET_ARCHITECTURE} = "amd64" ]; then
  OS_NAME="linux64"
elif [ ${TARGET_ARCHITECTURE} = "i386" ]; then
  OS_NAME="linux32"
else
  OS_NAME="${TARGET_ARCHITECTURE}"
fi
PACKAGE_NAME="lazpaint${VERSION}_${OS_NAME}"

echo "Version is $VERSION"
echo "Target OS is ${OS_NAME}"

if [ ! -f "${SOURCE_BIN}/lazpaint" ]; then
  echo "Cannot find binary file."  
  exit 1
fi

echo "Creating package..."

rm -rf "${STAGING_DIR}"
mkdir "${STAGING_DIR}"
chmod 0755 "${STAGING_DIR}"

mkdir "${STAGING_DIR}/DEBIAN"
chmod 0755 "${STAGING_DIR}/DEBIAN"
cp "debian/control" "${STAGING_DIR}/DEBIAN"
sed -i -e "s/Architecture: any/Architecture: ${TARGET_ARCHITECTURE}/" "${STAGING_DIR}/DEBIAN/control"

mkdir "${USER_DIR}"
chmod 0755 "${USER_DIR}"

mkdir "${BIN_DIR}" 
chmod 0755 "${BIN_DIR}" 
cp "${SOURCE_BIN}/lazpaint" "${BIN_DIR}"
chmod 0755 "${BIN_DIR}/lazpaint"
chmod +x "${BIN_DIR}/lazpaint"
echo "Determining dependencies..."
dpkg-shlibdeps "${BIN_DIR}/lazpaint"
DEPENDENCIES="$(sed -n 's/^shlibs:Depends=//p' debian/substvars)"
sed -i -e "s/\\\${shlibs:Depends}/${DEPENDENCIES}/" "${STAGING_DIR}/DEBIAN/control"
rm "debian/substvars"
echo "Done determining dependencies."

mkdir "${SHARE_PARENT_DIR}"
chmod 0755 "${SHARE_PARENT_DIR}" 

mkdir "${DOC_PARENT_DIR}"
chmod 0755 "${DOC_PARENT_DIR}"
mkdir "${DOC_DIR}"
chmod 0755 "${DOC_DIR}"
gzip -9 -n -c "debian/changelog" >"${DOC_DIR}/changelog.gz"
cp "debian/copyright" "${DOC_DIR}"
cp "${SOURCE_BIN}/readme.txt" "${DOC_DIR}/README"

cp -r applications "${SHARE_PARENT_DIR}"
chmod 0755 "${SHARE_PARENT_DIR}/applications"
cp -r pixmaps "${SHARE_PARENT_DIR}"
chmod 0755 "${SHARE_PARENT_DIR}/pixmaps"
cp -r "man" "${SHARE_PARENT_DIR}" 
chmod 0755 "${SHARE_PARENT_DIR}/man"
chmod 0755 "${SHARE_PARENT_DIR}/man/man1"
gzip -9 -n "${SHARE_PARENT_DIR}/man/man1/lazpaint.1" 

mkdir "${SHARE_DIR}"
chmod 0755 "${SHARE_DIR}"
cp "${SOURCE_BIN}/lazpaint_simplified.ini" "${SHARE_DIR}"

mkdir "${SHARE_DIR}/i18n"
chmod 0755 "${SHARE_DIR}/i18n"
find "${SOURCE_BIN}/i18n" -maxdepth 1 -name *.po -exec cp {} "${SHARE_DIR}/i18n" \;

cp -r "${SOURCE_BIN}/models" "${SHARE_DIR}/models"
chmod 0755 "${SHARE_DIR}/models"

mkdir "${SCRIPT_DIR}"
chmod 0755 "${SCRIPT_DIR}"
find "${SOURCE_SCRIPT_DIR}" -maxdepth 1 -name *.py -exec cp {} "${SCRIPT_DIR}" \;
mkdir "${SCRIPT_DIR}/lazpaint" 
chmod 0755 "${SCRIPT_DIR}/lazpaint"
find "${SOURCE_SCRIPT_DIR}/lazpaint" -maxdepth 1 -name *.py -exec cp {} "${SCRIPT_DIR}/lazpaint" \;

SIZE_IN_KB="$(du -s ${STAGING_DIR} | awk '{print $1;}')"
echo "Installed-Size: ${SIZE_IN_KB}" >> "${STAGING_DIR}/DEBIAN/control"

dpkg-deb --root-owner-group --build "${STAGING_DIR}" "${PACKAGE_NAME}.deb"

NO_INSTALL_ARCHIVE="${PACKAGE_NAME}_no_install.tar.gz"

echo "Making ${NO_INSTALL_ARCHIVE}..."
mv "${BIN_DIR}/lazpaint" "${SHARE_DIR}/lazpaint"
mv "${DOC_DIR}/copyright" "${SHARE_DIR}/copyright"
mv "${DOC_DIR}/README" "${SHARE_DIR}/README"
cd "$SHARE_DIR"
cd ..
tar -czf "../../../${NO_INSTALL_ARCHIVE}" "lazpaint"
cd ../../..
rm -rf "${STAGING_DIR}"

