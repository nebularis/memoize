-module(fib_mem).
-export([start_memoize/0, fib/1]).
-include_lib("annotations/include/annotations.hrl").

start_memoize() ->
    memoize:init(?MODULE).

-memoize(ets).
fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N - 1) + fib(N - 2).
