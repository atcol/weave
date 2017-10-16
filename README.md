# chaos

An API and CLI for (un)predictable scheduling.

## Motivation

Chaos is an experiment to understand the idea of parameterising unpredictability
when integrating with actions using these concepts:

 * an action that encapsulates a side-effect
 * temporal and spatial parameters
 * frequency

The intersection of these three concepts is represented in the API "Chaos"
and the demonstration CLI.

## Domain

The types that this API works with is particularly simple. A core design aim
is to maximise simplicity where possible by avoiding "type ceremony" which
should enable the user to quickly & concisely describe parameters & the computation
and the API takes care of the rest.

### Temporal

The `Schedule` type represents the parameters for picking the time of execution
for an action and it has two value constructors: `Period` and `Interval`. 

`Period` represents an offset from the point of calculation, e.g. +200ms.

`Interval` provides a section of time to randomly pick the point of execution. 

In each case, the general idea is to effectively "describe" the boundaries for
_when_ to run the computation and the API uses this to randomly pick the execution
point.

## Examples

### Interval 

Execute an action within `[0, 100]` times with a random delay between "now" and 200ms:

    interval 100 (Period 200) (getCurrentTime >>= print)

or asynchronously:

    asyncInterval 100 (Period 200) (getCurrentTime >>= print)

## Building

![Travis CI build status](https://travis-ci.org/atcol/chaos.svg?branch=master)

Once you've cloned, just run with `stack`:

```
stack install
```
