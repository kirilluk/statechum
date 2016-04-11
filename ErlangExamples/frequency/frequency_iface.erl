-module(frequency_iface).
-export([start/0, stop/0, allocate/0, deallocate/1]).

start() -> frequency:start().
stop() -> frequency:stop().
allocate() -> frequency:allocate().
deallocate(X) -> frequency:deallocate(X).

