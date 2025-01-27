-module(calculator_app).
-export([start/2, stop/1]).
-behaviour(application).


%% Application callbacks
start(_Type, _Args) ->
    calculator_sup:start_link().

stop(_State) ->
    ok.