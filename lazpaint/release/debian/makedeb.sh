#!/bin/bash
cd `dirname $0`

STAGING_RELATIVEDIR="./staging"
STAGING_DIR=$(readlink --canonicalize "${STAGING_RELATIVEDIR}")
USER_DIR="${STAGING_DIR}/usr"
BIN_DIR="${USER_DIR}/bin"
SHARE_DIR="${USER_DIR}/share"
RESOURCE_DIR="${SHARE_DIR}/lazpaint"
DOC_PARENT_DIR="${SHARE_DIR}/doc"
DOC_DIR="${DOC_PARENT_DIR}/lazpaint"
SOURCE_BIN=$(readlink --canonicalize "../bin")
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

rm -rf "${STAGING_DIR}"
mkdir "${STAGING_DIR}"
pushd ../../..

if [ ! -f "${SOURCE_BIN}/lazpaint" ]; then
    if [ -z "$1" ]; then
        echo "Usage: ./makedeb [TARGET]"
        echo "where TARGET can be Gtk2, Win32 or Qt5"
        exit 1
    fi
    echo "Compiling..."
    make distclean
    ./configure --prefix=/usr
    make TARGET=$1
else
    echo "Using already compiled binary."
fi

echo "Creating package..."
./configure --prefix=/usr
make install "DESTDIR=$STAGING_DIR"
popd

mkdir "${STAGING_DIR}/DEBIAN"
cp "debian/control" "${STAGING_DIR}/DEBIAN"
sed -i -e "s/Architecture: any/Architecture: ${TARGET_ARCHITECTURE}/" "${STAGING_DIR}/DEBIAN/control"

mkdir "${DOC_PARENT_DIR}"
mkdir "${DOC_DIR}"
cp "debian/changelog" "${DOC_DIR}"
cp "debian/copyright" "${DOC_DIR}"
gzip -9 -n "${DOC_DIR}/changelog"

echo "Determining dependencies..."
dpkg-shlibdeps "${BIN_DIR}/lazpaint"
DEPENDENCIES="$(sed -n 's/^shlibs:Depends=//p' debian/substvars)"
sed -i -e "s/\\\${shlibs:Depends}/${DEPENDENCIES}/" "${STAGING_DIR}/DEBIAN/control"
rm "debian/substvars"
echo "Done determining dependencies."

SIZE_IN_KB="$(du -s ${STAGING_DIR} | awk '{print $1;}')"
echo "Installed-Size: ${SIZE_IN_KB}" >> "${STAGING_DIR}/DEBIAN/control"
find "${STAGING_DIR}" -type d -exec chmod 0755 {} \;
find "${STAGING_DIR}" -type f -exec chmod 0644 {} \;
chmod 0755 "${BIN_DIR}/lazpaint"

fakeroot dpkg-deb --build "${STAGING_DIR}" "${PACKAGE_NAME}.deb"

NO_INSTALL_ARCHIVE="${PACKAGE_NAME}_no_install.tar.gz"

echo "Making ${NO_INSTALL_ARCHIVE}..."
mv "${BIN_DIR}/lazpaint" "${RESOURCE_DIR}/lazpaint"
cp "debian/copyright" "${RESOURCE_DIR}/copyright"
cp "../bin/readme.txt" "${RESOURCE_DIR}/README"
pushd ${SHARE_DIR}
tar -czf "../../../${NO_INSTALL_ARCHIVE}" "lazpaint"
popd
rm -rf "${STAGING_DIR}"

