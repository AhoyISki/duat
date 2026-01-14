# Scripting Duat

This chapter will teach you how to do "advanced" configuration of duat. It 
covers more complex topics, and details how the API of duat actually works.

In order to understand duat's API, you have to understand its memory model. 
Since the text editor uses a "low level" language for configuration, it has no 
garbage collection, and you will have to deal with things like references and 
Rust's (in)famous borrow checker.

Duat's API is designed in a way that it should smooth most of these problems 
out as much as possible. But it is still a work in progress, so you can expect
improvements in the future.
