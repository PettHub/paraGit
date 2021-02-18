-module(server).
-export([start/1,stop/1]).

% Start a new server process with the given name
% Do not change the signature of this function.

start(ServerAtom) ->
	genserver:start(ServerAtom, ok, fun server:handle/2).

    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID

handle(State, Data)	->
io:fwrite("State", []),
{reply, Data, State}.

%handle(State, {join, Data}) ->


append([H|T], Tail) -> [H|append(T,Tail)];
append([], Tail) -> Tail.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    exit(ServerAtom, kill),
    ok.
