# Generate script

A subset of proto files from [Hyperledger Fabric](https://github.com/awakesecurity/proto3-suite/issues/119#issuecomment-684391154) (commit 9848841) are used to create Haskell source files.

Note: Due to [an issue](https://github.com/awakesecurity/proto3-suite/issues/119) with the latest `compile-proto-file` binary (v0.4.0.2, commit 185517b), it generates code that doesn't type check. Add to `Chaincode.hs`:
```
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

instance {-# OVERLAPPING #-} HsJSONPB.ToSchema (HsJSONPB.OverrideToSchema (Hs.Map a Hs.ByteString)) where
  declareNamedSchema _ = Hs.return (HsJSONPB.NamedSchema Hs.Nothing Hs.mempty)
```

The `generate.sh` script is used to generate the Haskell source files from the `.proto` files.

The script requires the `compile-proto-file` binary, which can be installed from here https://github.com/awakesecurity/proto3-suite
