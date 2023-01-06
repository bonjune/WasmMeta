# Manuals

## Collecting Specification Files

This project references the [WebAssembly Spec Github Repository](https://github.com/WebAssembly/spec/tree/main/document/core) using `git submodule`.
Check [.gitmodules](.gitmodules) for the submodule settings.

Initialize and update the submodule via

``` bash
git submodule init
git submodule update
```

, or, you can clone this repository via

```bash
git clone --recurse-submodules git@github.com:bonjune/WasmMeta.git
```

Please change the uri to this repository for your environment setup if needed.

Once submodule initialization completes, you are done collecting specification files.
