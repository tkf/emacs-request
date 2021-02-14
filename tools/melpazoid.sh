#!/bin/bash -ex

EMACS="${EMACS:=emacs}"
BASENAME=$(basename "$1")
PYTHON=$(which python3.6 || true)
PYTHON="${PYTHON:-python}"

if [[ -z $(du -s melpazoid-master 2>/dev/null | cut -f1) ]] || \
       [[ $(du -s melpazoid-master 2>/dev/null | cut -f1) -le "100" ]] ; then
    curl -sLk -O https://github.com/riscy/melpazoid/archive/master.zip
    unzip master.zip
    rm -f master.zip
fi

ROOT=$(git rev-parse --show-toplevel)
cd ${ROOT}
PKG_PATH="${ROOT}/melpazoid-master/$(basename $(pwd))"
PKG_NAME=$(basename "$PKG_PATH")
PKG_MAIN=$(cask files | egrep -- "pkg.el$" || true)
PKG_MAIN=$(basename ${PKG_MAIN:-''})
mkdir -p ${PKG_PATH}
rsync -av --exclude '*autoloads.el' $(cask files) ${PKG_PATH} --delete
if [ -s "${ROOT}/LICENSE" ]; then
  cp -p "${ROOT}/LICENSE" ${PKG_PATH}
fi
cd melpazoid-master
${PYTHON} -m pip install --user -U .
sed -i -e 's/ -it / -i /' Makefile
sed -i -e 's/ -ti / -i /' Makefile
if [ ! -s ./python ]; then rm -f ./python ; ln -s $PYTHON ./python ; fi
# PKG_PATH=${PKG_PATH} PKG_NAME=${PKG_NAME} PATH=.:${PATH} make run

# Later: melpazoid is now an elisp file?
cd ${ROOT}
PACKAGE_MAIN=$PKG_MAIN EMACS=$EMACS cask emacs -Q --batch -l package --eval "(setq package-user-dir \"$(cask package-directory)\")" -f package-initialize -L ${ROOT}/melpazoid-master/melpazoid --eval "(setq default-directory \"${PKG_PATH}\")" --eval "(let (eval-expression-print-length eval-expression-print-level) (prin1 package-user-dir (function external-debugging-output)) (prin1 package-directory-list (function external-debugging-output)) (prin1 package-alist (function external-debugging-output)))" -l melpazoid
