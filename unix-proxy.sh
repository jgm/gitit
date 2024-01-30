#!bash
# This script proxies the `gitit` top-level executable when using a prebuilt
# version of gitit. Really the only job of this script is to set the environment
# variables overriding the paths set by cabal. For more information on why this
# works see
# https://www.haskell.org/cabal/users-guide/developing-packages.html#accessing-data-files-from-package-code

# From https://stackoverflow.com/questions/4774054/reliable-way-for-a-bash-script-to-get-the-full-path-to-itself
SCRIPTPATH="$( cd "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"

export gitit_datadir=$SCRIPTPATH
export filestore_datadir=$SCRIPTPATH/vendor-data/filestore

exec $SCRIPTPATH/gitit-bin "$@"
