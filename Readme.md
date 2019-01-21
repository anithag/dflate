Implementation of Distributed FLAC using naive proof search. Supports TEEs as separately spawned LXC containers.

## Prerequisites

### Build Platform

* Ubuntu 18.04 
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

(**Optional**) One time LXC container creation steps:

1. lxc-create -t download -n _tee\_name_
2. lxc-start -n _tee\_name_
3. lxc-attach -n _tee\_name>_
   1. (Inside bash shell) apt-get install haskell-platform
   2. git clone https://github.com/anithag/dflate.git
   3. cd dflate
   4. Build TEE worker using instructions listed above
   
 


### How to Run

- **On manager node:** sudo cabal run dflate
- **On worker node :** DISTRIBUTED_PROCESS_TRACE_CONSOLE=1 DISTRIBUTED_PROCESS_TRACE_FLAGS=pdusrl cabal run worker 8080


## Issues

1. TEE is implemented as a light-weight linux container. At the moment the IP addresses used for spawning remote processes are hard-coded.
    * (** on dflate node **) ip addr show lxcbr0
      > inet 10.0.3.1/24 scope global lxcbr0
    * (** on TEE node **) ip addr show eth0
      > inet 10.0.3.6/24 brd 10.0.3.255
          
2. Typed channels are used. However, _ReceivePort_ can not be serialized. So a TEE process has to create a _ReceivePort_ and send it to parent process for
communication.


## TODO


 Case Studies


## Design Notes for TEE's


A TEE is implemented as a light-weight linux container.

## Future Extensions

1. Using actual Intel SGX SDK
2. Using AMD SEV (Secure encrypted virtualization)
   https://github.com/AMDESE/AMDSEV
   

