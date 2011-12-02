# Memoize - a simple function memoizing annotation

This is a proof of concept for
[annotations](https://github.com/hyperthunk/annotations), which allows you to
memoize referentially transparent functions using a simple annotation. The current
implementation relies on an ets table, such that you need to call
`memoize:init(?MODULE)` before the annotation has any effect.

## Example

Taken from the `examples` folder, we define two simple, recursive implementations
for calculating the Nth number in the Fibonacci sequence. The non-memoized module
will take a very long time once N >= 35. Whilst there are better ways of
implementing fibonacci than this, it serves as a simple example of how to use
memoize.

We annotation our `fib_mem` implementation like so:

```erlang
-module(fib_mem).
-export([start_memoize/0, fib/1]).
-include_lib("annotations/include/annotations.hrl").

start_memoize() ->
    memoize:init(?MODULE).

-memoize(ets).
fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N - 1) + fib(N - 2).
```

Then we see it at work in the shell:

    t4@malachi:fib $ rebar get-deps clean compile
    ==> fib (pre_compile)
    Writing config to /Users/t4/work/nebularis/memoize/examples/fib/annotations.config
    ==> fib (compile)
    Compiled src/fib.erl
    Compiled src/fib_mem.erl
    ==> fib (post_compile)
    t4@malachi:fib $ erl -pa ebin/ -pa deps/memoize/ebin/ -pa deps/annotations/ebin/
    Erlang R14B01 (erts-5.8.2) [source] [64-bit] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]

    Eshell V5.8.2  (abort with ^G)
    1> fib_mem:start_memoize().
    ok
    2> fib_mem:fib(1).
    1
    3> fib_mem:fib(2).
    1
    4> fib_mem:fib(30).
    832040
    5> fib_mem:fib(35).
    9227465
    6> fib_mem:fib(0).         
    0
    7> fib_mem:fib(35).
    9227465
    8> fib_mem:fib(355).
    69362907070206748494476200566565775354902428015845969798000696945226974645
    9> fib_mem:fib(355).
    69362907070206748494476200566565775354902428015845969798000696945226974645
    10> fib_mem:fib(500).
    139423224561697880139724382870407283950070256587697307264108962948325571622863290691557658876222521294125
    11> 
    BREAK: (a)bort (c)ontinue (p)roc info (i)nfo (l)oaded
           (v)ersion (k)ill (D)b-tables (d)istribution
    a
    t4@malachi:fib $ 
