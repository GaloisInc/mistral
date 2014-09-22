
Mistral
=======

Mistral is an experimental for distributed programming, with an emphasis on
the specification and maintenance of a virtual overlay network. The current Mistral compiler supports the first generation of Mistral, so not all features we architected for the language are supported at present. This open source package includes our compiler and an interpreter. Use of Mistral for running programs on distributed systems requires a run-time system not included in this package. Thus this Mistral package allows only for experimentation with the language.

Building
--------

To build the mistral compiler, simply run `build.sh`.  This will create a cabal
sandbox, called `build`, and place the `mistral` binary in its `bin` directory.

Examples
--------

The example programs in the `examples` directory should run as standalone
programs, so passing the `-i` switch to the `mistral` executable will invoke the
interpreter on them.

```sh
$ ./build/bin/mistral -i examples/PingPong.mst
```

When a program includes multiple modules, they must be compiled in dependency
order (using the `-c` switch for the `mistral` compiler).  Once this is
complete, the module containing the main function can be run with `-i`, which
will cause all other needed modules to be linked in.

Documentation
-------------

There is a preliminary language description located in `docs/Mistral.md`, and an
initial tutorial in `docs/Tutorial.md`. The Tutorial refers to some features not yet supported by the Mistal compiler included in this package.

##Future Development##

We see a clear path to development of a run-time system for Mistral. Mistral has been integrated in the past with such a run-time, so the requirements for functionality are relatively clear.

