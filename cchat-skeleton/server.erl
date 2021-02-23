-module(server).
-export([start/1,stop/1]).

-record(server_st, {
    channels =[],   % The channels on this server
    nicks    =[]    % The nicks on this server
    }).

initial_state(Channels,Nicks) ->
        #server_st{
      channels = Channels,
      nicks = Nicks
    }.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    genserver:start(ServerAtom, initial_state([],[]), fun handle/2).

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % Return ok
    genserver:request(ServerAtom, stop_channels), 
    genserver:stop(ServerAtom). 

% Join a channel
handle(ServerSt, {join, Channel, Client}) ->
    % Checks if the channel already exists
    case lists:member(Channel, ServerSt#server_st.channels) of    
        false ->
            %If not: start new channel
            genserver:start(list_to_atom(Channel), channel:initial_state(Channel), fun channel:handle/2);
        true ->
            % If it does exist: do nothing
            true
    end,

    %Try to join channel
    case (catch genserver:request(list_to_atom(Channel),{join,Client})) of %Call for the channel handle function (join) using channel process
        failed -> 
            % User has already joined
            {reply, user_already_joined , ServerSt};
        joined -> 
            % User successfully joined channel (also adds channel to server record)
            {reply, joined_channel, ServerSt#server_st{channels = [Channel | ServerSt#server_st.channels]}}
    end;

% Leave a channel
handle(ServerSt, {leave, Channel, Client}) ->
    %Checks if the channel exists
    case lists:member(Channel, ServerSt#server_st.channels) of    
        false ->
            % Channel doesn't exist: return tuple with channel_non_existent
            {reply, channel_non_existent, ServerSt};
        true ->
            % Channel exists: delegate to the channel handle function (leave) using channel process
            case (catch genserver:request(list_to_atom(Channel),{leave, Client})) of
                user_not_joined ->
                    {reply, user_not_joined, ServerSt}; %If user hasn't joined, return atom user_not_joined
                left_channel ->
                    {reply, left_channel, ServerSt} %User sucessfully left the channel
            end
    end;

% Change your nick (including distinction assignment)
handle(ServerSt, {nick, OldNick, NewNick}) ->
    % Check if anyone else has the nick you want to change to
    case lists:member(NewNick, ServerSt#server_st.nicks) of 
        false ->
            % If not taken: Change the nick (remove old nick from record and add new nick) and return tuple with nick_changed so client can change its record too
            NewServerSt = ServerSt#server_st{nicks = [NewNick | lists:delete(OldNick,ServerSt#server_st.nicks)]},
            {reply, nick_changed, NewServerSt};
        true ->
            % If taken: return tuple with nick_taken
            {reply, nick_taken, ServerSt}
    end;

% Stop all channels
handle(ServerSt, stop_channels) ->
    lists:foreach(fun(Channel) -> genserver:stop(list_to_atom(Channel)) end, ServerSt#server_st.channels),
    {reply, ok, ServerSt}.
