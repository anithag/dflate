Implementation of Distributed FLAC using naive proof search.

## Prerequisites


* Cloud Haskell : distributed-process, network-transpor-tcp
* Ubuntu


## TODO


1. Support for TEE's
2. Case Studies


## Design Notes for TEE's


A TEE is implemented as a light-weight linux container. Following steps are used to create container:

1. lxc-create -t download -n _<tee name>_
2. lxc-start -n _<tee name>_
3. lxc-attach -n _<tee name>_ _<bash script>_
   * _<bash script>_ downloads cabal, ghc and cloud haskell
