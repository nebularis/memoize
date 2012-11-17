-module(fib_mem).
-export([fib/1]).
-include_lib("annotations/include/annotations.hrl").

-memoize({undefined, ?MODULE}).
fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N - 1) + fib(N - 2).
