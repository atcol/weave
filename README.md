[![Build Status](https://travis-ci.org/atcol/weave.svg?branch=master)](https://travis-ci.org/atcol/chaos)

# weave

An API, CLI and DSL for event generation & (un)predictable scheduling.

## Motivation

weave is an experiment exploring the ideas of a rich event generation DSL combined with parameterising unpredictability
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

The `Schedule` type represents time-based parameters. There are two constructors: `Offset` and `Window`. 

`Offset` represents an offset from the point of calculation, e.g. +200ms.

`Instant` represents a specific point in time

`Window` represents a section of time.

In each case, the general idea is to effectively "describe" the boundaries for
_when_ to run a computation and the API uses this to (optionally: randomly) pick the execution
point.

## Parsing

Chaos supports defining events and their actions through a DSL.

The BNF can be see in the `chaos.bnf` file, but generally the form for the DSL is:

`<trigger> <action-list>`

where `trigger` is the schedule or spatial specification, and `action-list` is
a list of declared actions, an in-line action (`{ ssh server -c "apt-get update" }`)
or a shell expression (`'blah | grep | tail -1`) to run, delimited by an operator.

### Operators

The delimiter for the action list supports basic bash-like operators:

 * `|`
 * `&&`
 * `||`
 * ``

## Examples

There are a number of examples defined in `.weave` files. See the `examples` folder.

Executing the chaos binary with the following flags and a file with the contents
as follows `chaos from apt-update.chaos`:

    every 6 hours {
      ssh my-server "apt-get update -y"
    }

or through "declared actions":

    action AptUpgrade {
      apt-get update -y && apt-get upgrade -y
    }

    action AutoClean {
      apt-get autoclean  
    }

    every 6 hours AptUpgrade, AutoClean

## Building

Once you've cloned, just run with `stack`:

```
stack install
```
