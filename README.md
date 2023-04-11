# ladmm-sa

> A small application that makes it easy to enter and track data to qualify for
> artist status in Belgium. It is a pro bono application designed at the request
> of "[Les Amis d'ma mère](https://lesamisdmamere.be/)", a Belgian non-profit
> organisation that promotes (and supports) artists in Belgium.

## Local Installation

The most standard way to start a development environment is to build a "_local
switch_" by sequentially running these different commands (which assume that
[OPAM](https://opam.ocaml.org/) is installed on your machine).

```shell
opam update
opam switch create . ocaml-base-compiler.5.0.0 --deps-only -y
eval $(opam env)
opam install . --deps-only --with-test --with-doc -y
```

## Setting up the development environment

When the switch is properly initiated, you can install the various tools to
develop the project:

```shell
opam install merlin ocamlformat ocp-indent
```
