---
description: On this page we want to experiment on how to document examples in general.
---

# Examples

For instance, we have many examples in many languages, and it would be great if we could document it such a way that our engineers only have to make some minor adjustments to publish it on GitBook.

Lets see where that journey takes us.

```erlang
% Fibonacci numbers
% The Fibonacci sequence is given by 0, 1, 1, 2, 3, 5, … where subsequent values 
% are given by adding the two previous values in the sequence.
% Give a recursive definition of the function fib/1 computing the Fibonacci numbers
% and give a step-by-step evaluation of fib(4).
%
% Usage:
% 1> fibonacci:fib(5).
% 1> 8

-module(fibonacci).
-export([fib/1]).

fib(N) -> fib_iter(N, 0, 1).

fib_iter(0, Result, _Next) -> Result;
fib_iter(Iter, Result, Next) ->
fib_iter(Iter-1, Next, Result+Next).
```

This example was taken for [https://gist.github.com/chrisdoc/8169e14cb1ae3b6a64b2f35753a0979f](https://gist.github.com/chrisdoc/8169e14cb1ae3b6a64b2f35753a0979f)
