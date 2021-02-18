-module(server).
-export([start/1,stop/1]).

-record(server_st, {
    channels =[],   % channels on this server
    nicks    =[]    % nicks on this server
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
    genserver:request(ServerAtom,stop_channels),
    genserver:stop(ServerAtom).

% Join a channel
handle(ServerSt, {join, Channel, Client}) ->
    % does the channel already exist?
    case lists:member(Channel, ServerSt#server_st.channels) of    
        false ->
            %start new channel
            genserver:start(list_to_atom(Channel), channel:initial_state(Channel), fun channel:handle/2);
        true ->
            % channel already existing
            true
    end,

    %try to join channel
    case (catch genserver:request(list_to_atom(Channel),{join,Client})) of
        failed -> 
            % User already joined
            {reply, user_already_joined , ServerSt};
        joined -> 
            % Joined channel
            {reply, joined_channel, ServerSt#server_st{channels=[Channel | ServerSt#server_st.channels]}}
    end;

% Leave channel
handle(ServerSt, {leave, Channel, Client}) ->
    case lists:member(Channel, ServerSt#server_st.channels) of    
        false ->
            % channel doesn't exist
            {reply, channel_not_existing, ServerSt};
        true ->
            % channel is existing and the user will leave if it has joined before
            case (catch genserver:request(list_to_atom(Channel),{leave, Client})) of
                user_not_joined ->
                    {reply, user_not_joined, ServerSt};
                left_channel ->
                    {reply, left_channel, ServerSt}
            end
    end;

% Change nick
handle(ServerSt, {nick, OldNick, NewNick}) ->
    % does someone else already have this nick?
    case lists:member(NewNick, ServerSt#server_st.nicks) of 
        false ->
            % nick not taken
            NewServerSt = ServerSt#server_st{nicks= [NewNick | lists:delete(OldNick,ServerSt#server_st.nicks)]},
            % changed nick
            {reply, nick_changed, NewServerSt};
        true ->
            % nick already taken
            {reply, nick_taken, ServerSt}
    end;

% Stop all channels
handle(ServerSt, stop_channels) ->
    lists:foreach(fun(Channel) -> genserver:stop(list_to_atom(Channel)) end, ServerSt#server_st.channels),
    {reply, ok, ServerSt}.
