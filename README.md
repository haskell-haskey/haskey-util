haskey-util
===========

[![Travis](https://travis-ci.org/haskell-haskey/haskey-util.svg?branch=master)](https://travis-ci.org/haskell-haskey/haskey-util)

Utility executable and library for inspecting [Haskey databases](https://github.com/haskell-haskey/haskey).

How to use
----------

Build and execute using stack:

```
git clone https://github.com/haskell-haskey/haskey-util
stack build
stack exec haskey-util -- -o /tmp/analysis.svg -h 250
```

This should write an analysis diagram to `/tmp/analysis.svg`, which you can open in your favorite browser.

Example
-------

Below is an example of an analysis.

Click [here](.graphics/analysis-example.svg) to go directly to the image.

![Example Analysis](.graphics/analysis-example.svg)
