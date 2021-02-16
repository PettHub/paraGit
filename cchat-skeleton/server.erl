-module(server).
-export([start/1,stop/1]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->

	Pid = spawn(fun receivemessage/0),
	catch(unregister(ServerAtom)),
	register(ServerAtom, Pid),
	Pid.

    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
     
receivemessage() ->

	receive
		join -> io:fwrite("join message received!", []),
		receivemessage();

		leave -> io:fwrite("leave message received!",[]),
		
		%exit(self(), kill), only when there's only one user on server
		
		receivemessage()
	end.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    not_implemented.
