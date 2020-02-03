To compile the haskell files from proto, run from project dir:

```
compile-proto-file --proto peer/chaincode_shim.proto --out gen --includeDir google-protos --includeDir protos
compile-proto-file --proto peer/chaincode_event.proto --out gen --includeDir google-protos --includeDir protos
compile-proto-file --proto peer/chaincode.proto --out gen --includeDir google-protos --includeDir protos
compile-proto-file --proto peer/proposal_response.proto --out gen --includeDir google-protos --includeDir protos
compile-proto-file --proto peer/proposal.proto --out gen --includeDir google-protos --includeDir protos
compile-proto-file --proto msp/msp_principal.proto --out gen --includeDir google-protos --includeDir protos
compile-proto-file --proto common/policies.proto --out gen --includeDir google-protos --includeDir protos
compile-proto-file --proto google/protobuf/timestamp.proto --out gen --includeDir google-protos --includeDir protos
```

Install the binary from here https://github.com/awakesecurity/proto3-suite
