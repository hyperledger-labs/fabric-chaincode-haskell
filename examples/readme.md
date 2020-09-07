# Haskell Chaincode Examples

## Simple Application Chaincode (SACC)

The SACC chaincode can be instantiated with:

```
peer chaincode instantiate -n mycc -v v0 -l golang -c '{"Args":["init","a","100"]}' -C myc -o orderer:7050
```

The chaincode can then be invoked with the following examples:

```
peer chaincode invoke -n mycc -c '{"Args":["get","a"]}' -C myc
peer chaincode invoke -n mycc -c '{"Args":["set","b","60"]}' -C myc
```

## Marbles Chaincode

The Marbles chaincode can be instantiated with:

```
peer chaincode instantiate -n mycc -v v0 -l golang -c '{"Args":["initMarble","marble1","red","large","Al"]}' -C myc -o orderer:7050
```

The chaincode can then be invoked with the following examples:

```
peer chaincode invoke -n mycc -c '{"Args":["initMarble","marble1","red","large","Al"]}' -C myc
peer chaincode invoke -n mycc -c '{"Args":["initMarble","marble2","blue","large","Nick"]}' -C myc
peer chaincode invoke -n mycc -c '{"Args":["readMarble","marble1"]}' -C myc
peer chaincode invoke -n mycc -c '{"Args":["deleteMarble","marble1"]}' -C myc
peer chaincode invoke -n mycc -c '{"Args":["transferMarble","marble1", "Nick"]}' -C myc
peer chaincode invoke -n mycc -c '{"Args":["getMarblesByRange","marble1", "marble3"]}' -C myc
peer chaincode invoke -n mycc -c '{"Args":["getMarblesByRangeWithPagination","marble1", "marble3", "1", ""]}' -C myc
```

## Fabcar Chaincode

The Fabcar chaincode can be instantiated with:

```
peer chaincode instantiate -n mycc -v v0 -l golang -c '{"Args":["init"]}' -C myc -o orderer:7050
```

The chaincode can then be invoked with the following examples:

```
peer chaincode invoke -n mycc -c '{"Args":["initLedger"]}' -C myc
peer chaincode invoke -n mycc -c '{"Args":["createCar", "CAR10", "Ford", "Falcon", "White", "Al"]}' -C myc
peer chaincode invoke -n mycc -c '{"Args":["queryCar", "CAR10"]}' -C myc
peer chaincode invoke -n mycc -c '{"Args":["changeCarOwner", "CAR10", "Nick"]}' -C myc
peer chaincode invoke -n mycc -c '{"Args":["queryAllCars"]}' -C myc
```
