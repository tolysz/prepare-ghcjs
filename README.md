# prepare-ghcjs

It is currently "designed" for one user.

I compile it and run via cron, but the minimal will be:

1. `git clone`
2. `stack ghci`
3. use one of the following commands
```
    syncLts
    latest lts
    syncNightly
    latest nightly
    sync (ltsCfg {checkResolver = return "bla-123.456"})
    for resolver bla-123.456
```

the output should be in the `archive` directory.

This is very manual, In the ideal world all changes here should go to `ghcjs/ghcjs` or to upstream packages
I upload my builds thus I have `ghcjs-host` entry in `~/.ssh/config`

I still explore the design space. The operation on boot packages should be:

```
override_by_copy
copy_from_hackage
copy_from_boot
patch_hackage
```

some of them might generate new dependencies in `boot.yaml`

## GHC 8

Building for GHC 8.0.2 still need some manual pathching, but for now

```bash
$ stack ghci
$> :set -XOverloadedStrings
$> sync (lts8Cfg {checkResolver = return "lts-8.21"})
```
