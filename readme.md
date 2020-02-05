# haskell-cc

To generate the haskell files from the proto files, run the `generate.sh` script.

Note that the script requires the `compile-proto-file` binary. Install the binary from here https://github.com/awakesecurity/proto3-suite

Note: The generated haskell files with the latest version of the `compile-proto-file` binary generates code that doesn't type check. To fix it, convert all `HsJSONPB.SwaggerObject` to `Hs.Just HsJSONPB.SwaggerObject`
