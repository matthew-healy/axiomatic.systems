# axiomatic.systems

This is the source code for my blog, axiomatic.systems.

It's a static app, compiled with Hakyll and built with Nix.

## Building

You can build the app (into the `.result/dist` directory) with:

```sh
> nix build
```

## Development

You can launch a pre-configured development shell with:

```sh
> nix develop
```

You can build & watch for changes with:

```sh
> nix run . watch
```

## Acknowledgements

* This setup was heavily inspired by elements of @rpearce's [hakyll-nix-template](https://github.com/rpearce/hakyll-nix-template).