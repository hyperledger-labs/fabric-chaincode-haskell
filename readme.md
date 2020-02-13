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

Before starting the Haskell process, start the [Fabric network](https://github.ibm.com/chaincode-haskell/fabric-network) with the peer in development mode and without a chaincode container.
This can be done with the `./start-no-cc` script.

The Haskell chaincode process can then be started with:

```
stack run
```

The Haskell process that is started does a number of things

1. It connects to the Fabric peer through grpc (prints "okie dokey")
2. It sends a REGISTER message to the peer and receives a REGISTERED response (prints "YAY REGGED")
3. It receives a READY message from the peer (prints "YAY READY)

The peer then needs to be told about the chaincode process running with the `install` and `instantiate` commands.
Please see the instructions in the fabric-network repo for details on how to do this.
When instantiating the chaincode the peer will send the INIT message to the chaincode process (prints "YAY INIT").
