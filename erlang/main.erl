-module(main).
-export([main/0]).
-import(interpreter, [new/0, get_pointer/1]).

main() ->
    Bf = interpreter:new(),
    io:format("Hello world ~w!~n", [interpreter:get_pointer(Bf)]),
    halt().