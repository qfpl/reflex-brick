#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix-prefetch-git

nix-prefetch-git https://github.com/reflex-frp/reflex-platform > reflex-platform.json
nix-prefetch-git https://github.com/dalaing/reflex-basic-host > reflex-basic-host.json
