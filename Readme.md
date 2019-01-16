Implementation of Distributed FLAC using naive proof search.

## Prerequisites


* Cloud Haskell : distributed-process, network-transpor-tcp
* Ubuntu


## TODO


1. Support for TEE's
2. Case Studies


## Design Notes for TEE's


A TEE is implemented as a light-weight linux container. Following steps are used to create container:

1. lxc-create -t download -n _tee\_name_
2. lxc-start -n _tee\_name_
3. lxc-attach -n _tee\_name>_ _bash\_script_
   * _bash\_script_ downloads cabal, ghc and cloud haskell
