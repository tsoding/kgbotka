[![Build Status](https://github.com/tsoding/kgbotka/workflows/CI/badge.svg)](https://github.com/tsoding/kgbotka/actions)

# KGBotka

Twitch/Discord Chat Bot that works for KGB ![monkaS](https://cdn.betterttv.net/emote/56e9f494fff3cc5c35e5287e/1x)

## Quick Start

**WARNING! CONTAINS SUBMODULES!** Clone with `git clone --recursive`.

### Cabal

```console
$ cabal v2-build
$ cabal v2-run kgbotka secret.json database.db
```

### Stack

```console
$ stack init --resolver=snapshot.yaml
$ stack build
$ stack run kgbotka secret.json database.db
```

## secret.json

See [./secret.json.example](./secret.json.example).

### Twitch

The easiest way to obtain Twitch credentials right now is to
1. Go to https://tsoding.org/kgbotka-login/
2. Press `Login` button and follow the instructions
3. Copy paste the generated credentials to your `secret.json`. 
   Check [./secret.json.example](./secret.json.example) to learn about
   the format of the file.

<!-- TODO: document how to generate credetials with a custom Twitch Application -->

### Discord

See [OAuth2 for Bots](https://discord.com/developers/docs/topics/oauth2#bots) section of Discord documentation.

### GitHub

1. Generate the [Personal Access Token](https://github.com/settings/tokens)
2. Make sure you enable the `gist` scope, otherwise Friday Videos Gist synchronization won't work

## Features

### Friday Video Gist

TBD
<!-- TODO(#224): document Friday Video Gist synchronization feature -->
