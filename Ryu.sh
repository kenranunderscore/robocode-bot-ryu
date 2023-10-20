#!/usr/bin/env bash

nix develop -c \
    bash -c \
    'cabal run 2>&1 | tee -a ~/projects/tank-royale-bots/Ryu/LOG'
