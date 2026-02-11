# The base set of duat crates

This directory contains all the "base crates" of duat, that is, those thatare
included by default, which are supposed to be opt-out rather thanopt-in.

There are three types of crate in here:

- There's `duat-core`, which is the common ground between all of the duat
  crates.
- There's `duat-term`, which is a terminal implementation of duat's UI
  protocol.
- And there's the remaining crates, which can be considered "plugins".

The difference between these plugins and externally written plugins, is that these plugins
