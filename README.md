[![Build Status](https://travis-ci.org/atcol/chaos.svg?branch=master)](https://travis-ci.org/squidnyan/chaos)

# chaos

An API, CLI and DSL for event generation & (un)predictable scheduling.

## Motivation

Chaos is an experiment exploring the ideas of a rich event generation DSL combined with parameterising unpredictability
for actions.

## Domain

There are a number of ideas that are central to the design & philosophy of the API & tooling that Chaos provides. 

| Concept | Description |
|---------|-------------|
| Actions | the encapsulation of a side-effect |
| Temporal | a parameter based on time |
| Spatial | a parameter based on the environment |
| Frequency | a multiplier |
| Integration | drawing from and writing to various toolchains |

### Temporal

The `Schedule` type represents the parameters time based parameters and its two most important constructors are `Offset` and `Window`. 

`Offset` represents an offset from the point of calculation, e.g. +200ms.

`Window` represents a section of time.

In each case, the general idea is to effectively "describe" the boundaries for
_when_ to run a computation and the API uses this to (optionally: randomly) pick the execution
point.

### Spatial

The `Spatial` type represents the parameters for deciding when to execute an action
based on non-temporal values e.g. a counter, disk space, a random value. The API
supports the `Reader` monad from the `mtl` package for this purpose.

`Spatial` values are ultimately used as the input for mapping to `Schedule` (temporal)
equivalents, as this allows the Chaos API to build a simple parameterised graph 
of execution flow with "simulated" randomness within API-user specific boundaries.

## Examples

There are a number of examples defined in `.chaos` files. See the `examples` folder.

### Offset

Execute an action within `[0, 100]` times with a random delay between "now" and 200ms:

    timesIn 100 (Offset 200) (getCurrentTime >>= print)

or asynchronously:

    asyncTimesIn 100 (Offset 200) (getCurrentTime >>= print)

This can also be demonstrated via the CLI:

    atc@atc-xps:~/src/chaos$ ./chaos between --endMs 200 --cmd "echo lol; date"
    lol
    Sat 23 Dec 15:45:41 GMT 2017
    [()]

There is also file-based support:

    every 6 hours {
      ssh my-server "apt-get update -y"
    }

## Building

Once you've cloned, just run with `stack`:

```
stack install
```
