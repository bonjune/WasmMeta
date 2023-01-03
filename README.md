# Manuals

## Collecting Specification Files

Run the script below.
The script runs `wget` to recursively download all WebAssembly Specification HTML files from https://webassembly.github.io/spec/core/.
Please check that you have installed `wget` in your system.

``` bash
# Make sure that you have set the file executable
chmod +x ./script/load.sh

./script/load.sh
```

## Testing `Extract`

`Extract` Module is responsible for extracting from specification HTML files.
The command below will run tests defined at [WasmMeta.Test](WasmMeta.Test).

```
dotnet test
```