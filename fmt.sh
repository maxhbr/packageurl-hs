#!/usr/bin/env nix-shell
#! nix-shell -i bash -p haskellPackages.brittany

# SPDX-FileCopyrightText: Maximilian Huber <oss@maximilian-huber.de>
#
# SPDX-License-Identifier: MIT

set -euo pipefail

formatDir() {
    # formatters:
    # * hindent
    # * stylish-haskell
    # * brittany <- chosen
    find "$(dirname "$0")/$1" -iname '*.hs' -print -exec brittany --write-mode=inplace {} \;
}

formatDir src
formatDir test

