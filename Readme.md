Implementation of Distributed FLAC using naive proof search.

## Prerequisites

### Build Platform

* Ubuntu 18
* LXC

### Haskell Dependencies

* Cloud Haskell : distributed-process, network-transpor-tcp
* bindings-lxc

### How to Build

Dependencies have been frozen to ensure compatibility during serialization. Refer to cabal.config for dependency constraints.

* cabal sandbox init
* cabal install --dependencies-only
* cabal configure
* cabal build
* cabal install

### How to Run

- On manager node: sudo cabal run dflate
- On worker node : DISTRIBUTED_PROCESS_TRACE_CONSOLE=1 DISTRIBUTED_PROCESS_TRACE_FLAGS=pdusrl cabal run worker 8080


## Issues

1. TEE is implemented as a light-weight linux container. At the moment the container creation is hard-coded for testing.
2. Typed channels are used. However, _ReceivePort_ can not be serialized. So a TEE process has to create a _ReceivePort_ and send it to parent process for
communication.


## TODO


 Case Studies


## Design Notes for TEE's


A TEE is implemented as a light-weight linux container. Following steps are used to create container:

1. lxc-create -t download -n _tee\_name_
2. lxc-start -n _tee\_name_
3. lxc-attach -n _tee\_name>_ _bash\_script_
   * _bash\_script_ downloads cabal, ghc and cloud haskell
