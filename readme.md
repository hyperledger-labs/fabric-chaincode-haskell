# fabric-chaincode-haskell

NOTE: This project is currently a PRE-ALPHA and is NOT suitable for production use.

`fabric-chaincode-haskell` is a Haskell shim for Hyperledger Fabric to allow the authoring of smart contracts in Haskell.

The project has three main parts:

- `protos` and `google-protos/google/protobuf` - The source protobuf files that define the communication between the shim and the peer. The corresponding Haskell files are generated in `protos-hs` (see `generate script` section below)
- `src` - Contains the Shim
- `examples` - Contains the main executable which is an example usage of the shim

## Installation

To build the project, run the following from the root directory:

```
stack build
```

Note : It is possible that you might get a build error with `grpc-haskell-core`, like the following:

```
Missing dependencies on foreign libraries:
- Missing (or bad) header file: include/grpc_haskell.h
- Missing (or bad) C libraries: grpc, gpr
```

This is because the underlying C binaries are either not installed or are not installed correctly.
To fix this, try reinstalling the grpc binary with `brew install grpc`/`brew reinstall grpc`.

## Usage

Note: Since running chaincode in production mode depends on a language specific flag (e.g. `-l golang`, `-l java` or `-l node`), it is currently only possible to run Haskell chaincode in dev mode. Supporting Haskell chaincode in production mode will require some minor changes to be made to the peer source code.

### Running the Haskell chaincode

The Haskell chaincode process can be started with:

```
stack run
```

When the Fabric peer is running (see below), the Haskell process that is started does a number of things

1. It connects to the Fabric peer through gRPC
2. It sends a REGISTER message to the peer and receives a REGISTERED response
3. It receives a READY message from the peer
4. It listens for an INIT message from the peer
5. It listens for TRANSACTION messages from the peer

### Connecting to the Fabric peer

The Haskell shim can be used with any Fabric network with a peer running in development mode.
However, we have provided a very simple [Fabric network](https://github.com/airvin/fabric-network) for testing purposes.
If you would like to use this network, start the Fabric network with the peer in development mode and without a chaincode container.
This can be done with the `./start-no-cc.sh` script.
The peer needs to be told about the chaincode process running with the `install` and `instantiate` commands.

Open a second terminal tab for the fabric network.
In the first tab, run `docker logs peer -f` to keep tabs on the logs for the peer container.
In the second tab, run the following:

```
docker exec -it cli bash
peer chaincode install -n mycc -v v0 -l golang -p chaincodedev/chaincode/chaincode_example02/go
peer chaincode list --installed
peer chaincode instantiate -n mycc -v v0 -l golang -c '{"Args":["init","a","100"]}' -C myc -o orderer:7050
```

The chaincode can then be invoked with the following examples:

```
peer chaincode invoke -n mycc -c '{"Args":["get","a"]}' -C myc
peer chaincode invoke -n mycc -c '{"Args":["put","b","60"]}' -C myc
peer chaincode invoke -n mycc -c '{"Args":["del","a"]}' -C myc
```

## Generate script

Note: Due to [an issue](https://github.com/awakesecurity/proto3-suite/issues/119) with the latest `compile-proto-file` binary, it generates code that doesn't type check. Until this issue is resolved, you need to manually convert all `HsJSONPB.SwaggerObject` to `Hs.Just HsJSONPB.SwaggerObject` within the generated files.

The `generate.sh` script is used to generate the Haskell source files from the `.proto` files.

The script requires the `compile-proto-file` binary, which can be installed from here https://github.com/awakesecurity/proto3-suite

## TODO

- [x] Finish implementing shim functions and clean up shim module exports
- [x] Add examples directory
- [ ] Write unit tests for stub functions
- [ ] Add support for concurrent transactions
- [ ] Finish implementing all stub functions
- [ ] Publish to Hackage
