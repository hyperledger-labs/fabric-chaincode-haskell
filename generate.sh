#!/bin/sh

compile-proto-file --proto ledger/queryresult/kv_query_result.proto --out protos-hs --includeDir google-protos --includeDir protos
compile-proto-file --proto peer/chaincode_shim.proto --out protos-hs --includeDir google-protos --includeDir protos
compile-proto-file --proto peer/chaincode_event.proto --out protos-hs --includeDir google-protos --includeDir protos
compile-proto-file --proto peer/chaincode.proto --out protos-hs --includeDir google-protos --includeDir protos
compile-proto-file --proto peer/proposal_response.proto --out protos-hs --includeDir google-protos --includeDir protos
compile-proto-file --proto peer/proposal.proto --out protos-hs --includeDir google-protos --includeDir protos
compile-proto-file --proto msp/msp_principal.proto --out protos-hs --includeDir google-protos --includeDir protos
compile-proto-file --proto common/policies.proto --out protos-hs --includeDir google-protos --includeDir protos
compile-proto-file --proto google/protobuf/timestamp.proto --out protos-hs --includeDir google-protos --includeDir protos
