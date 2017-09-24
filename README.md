# chaos

An API and CLI for (un)predictable scheduling.

## Motivation

Chaos is an experiment to understand the idea of parameterising unpredictability
when integrating with actions -- system calls, anything "IO-like" be it local
or remote -- using several concepts:

 * an action that encapsulates a side-effect
 * temporal and spatial parameters
 * frequency

The intersection of these three concepts is represented in the API "Chaos"
and the demonstration CLI.

## Building

![Travis CI build status](https://travis-ci.org/atcol/chaos.svg?branch=master)

Once you've cloned, just run with `stack`:

```
stack install
```
