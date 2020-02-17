# haskell-cc

To generate the haskell files from the proto files, run the `generate.sh` script.

Note that the script requires the `compile-proto-file` binary. Install the binary from here https://github.com/awakesecurity/proto3-suite

Note: The generated haskell files with the latest version of the `compile-proto-file` binary generates code that doesn't type check. To fix it, convert all `HsJSONPB.SwaggerObject` to `Hs.Just HsJSONPB.SwaggerObject`.

To build the project use:

```
stack build
```

It is possible that you might get an error with `grpc-haskell-core`, like the following:

```
Missing dependencies on foreign libraries:
- Missing (or bad) header file: include/grpc_haskell.h
- Missing (or bad) C libraries: grpc, gpr
```

This is because the underlying C binaries are either not installed or are not installed correctly.
To fix this, try reinstalling the grpc binary with `brew install grpc`.

### Running the Haskell chaincode

The Haskell chaincode process can be started with:

```
stack run
```

When the Fabric peer is running (see below), the Haskell process that is started does a number of things

1. It connects to the Fabric peer through grpc (prints "okie dokey")
2. It sends a REGISTER message to the peer and receives a REGISTERED response (prints "YAY REGGED")
3. It receives a READY message from the peer (prints "YAY READY")
4. It listens for an INIT message from the peer (prints "YAY INIT")
5. It listens for TRANSACTION messages from the peer (prints "YAY TRANSACTION")

### Connecting to the Fabric peer

Start the [Fabric network](https://github.ibm.com/chaincode-haskell/fabric-network) with the peer in development mode and without a chaincode container.
This can be done with the `./start-no-cc.sh` script.
The peer needs to be told about the chaincode process running with the `install` and `instantiate` commands.

Open a second terminal tab for the fabric network.
In the first tab, run `docker logs peer -f` to keep tabs on the logs for the peer container.
In the second tab, run the following:

```
docker exec -it cli bash
peer chaincode install -n mycc -v v0 -l golang -p chaincodedev/chaincode/chaincode_example02/go
peer chaincode list --installed
peer chaincode instantiate -n mycc -v v0 -l golang -c '{"Args":["init","a","100"]}' -C myc -o orderer:705
```

The chaincode can then be invoked with the following examples:

```
peer chaincode invoke -n mycc -c '{"Args":["get","a"]}' -C myc
peer chaincode invoke -n mycc -c '{"Args":["put","b","60"]}' -C myc
peer chaincode invoke -n mycc -c '{"Args":["getArgSlice"," this ","should ", "be ", "printed "]}' -C my
```
