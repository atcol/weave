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

## Examples

### Interval 

Execute an action within `[0, 100]` times with a random delay between "now" and 200ms:

    interval 100 (Target (Period 200) (getCurrentTime >>= print))

or asynchronously:

    asyncInterval 100 (Target (Period 200) (getCurrentTime >>= print))

## Building

![Travis CI build status](https://travis-ci.org/atcol/chaos.svg?branch=master)

Once you've cloned, just run with `stack`:

```
stack install
```
