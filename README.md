[![Build Status](https://github.com/tsoding/kgbotka/workflows/CI/badge.svg)](https://github.com/tsoding/kgbotka/actions)

# KGBotka

Twitch/Discord Chat Bot that works for KGB ![monkaS](https://cdn.betterttv.net/emote/56e9f494fff3cc5c35e5287e/1x)

## Quick Start

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

1. Login into your Bot account
2. Generate [Twitch Chat OAuth Password](https://twitchapps.com/tmi/). **REMOVE THE `oauth:` PREFIX**. Use it as the `token` in your [secret.json](./secret.json.example).
3. Set `clientId` to `q6batx0epp608isickayubi39itsckt`. This is the Client ID of the [Twitch Chat OAuth Password Generator](https://twitchapps.com/tmi/) itself. It's important to use this specific Client ID to match with the generated OAuth token otherwise the Twitch API queries made by the bot don't work.

### Discord

See [OAuth2 for Bots](https://discord.com/developers/docs/topics/oauth2#bots) section of Discord documentation.

### GitHub

1. Generate the [Personal Access Token](https://github.com/settings/tokens)
2. Make sure you enable the `gist` scope, otherwise Friday Videos Gist synchronization won't work

## Features

### Friday Video Gist

<!-- TODO: document Friday Video Gist synchronization feature -->
