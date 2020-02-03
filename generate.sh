#!/bin/sh

compile-proto-file --proto peer/chaincode_shim.proto --out grpc-client/src --includeDir google-protos --includeDir protos
compile-proto-file --proto peer/chaincode_event.proto --out grpc-client/src --includeDir google-protos --includeDir protos
compile-proto-file --proto peer/chaincode.proto --out grpc-client/src --includeDir google-protos --includeDir protos
compile-proto-file --proto peer/proposal_response.proto --out grpc-client/src --includeDir google-protos --includeDir protos
compile-proto-file --proto peer/proposal.proto --out grpc-client/src --includeDir google-protos --includeDir protos
compile-proto-file --proto msp/msp_principal.proto --out grpc-client/src --includeDir google-protos --includeDir protos
compile-proto-file --proto common/policies.proto --out grpc-client/src --includeDir google-protos --includeDir protos
compile-proto-file --proto google/protobuf/timestamp.proto --out grpc-client/src --includeDir google-protos --includeDir protos
