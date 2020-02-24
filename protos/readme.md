# Generate script

Note: Due to [an issue](https://github.com/awakesecurity/proto3-suite/issues/119) with the latest `compile-proto-file` binary, it generates code that doesn't type check. Until this issue is resolved, you need to manually convert all `HsJSONPB.SwaggerObject` to `Hs.Just HsJSONPB.SwaggerObject` within the generated files.

The `generate.sh` script is used to generate the Haskell source files from the `.proto` files.

The script requires the `compile-proto-file` binary, which can be installed from here https://github.com/awakesecurity/proto3-suite
