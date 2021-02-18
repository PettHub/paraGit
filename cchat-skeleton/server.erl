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

-record(channel_st, {
    name,           % name of this channel
    clients=[]      % clients on this channel
    }).

initial_channel_state(ChannelAtom) ->
    #channel_st{
        name    = ChannelAtom,
        clients = []
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
            genserver:start(list_to_atom(Channel), initial_channel_state(Channel), fun channelhandle/2);
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
            NewServerSt = ServerSt#server_st{nicks= [NewNick | lists:delete(OldNick,ServerSt#server_st.nicks)]},
            % nick changed
            {reply, nick_changed, NewServerSt};
        true ->
            % nick already taken
            {reply, nick_taken, ServerSt}
    end;

% Stop all channels
handle(ServerSt, stop_channels) ->
    lists:foreach(fun(Channel) -> genserver:stop(list_to_atom(Channel)) end, ServerSt#server_st.channels),
    {reply, ok, ServerSt}.


% Join channel
channelhandle(ChannelSt, {join, Client}) ->
    case lists:member(Client, ChannelSt#channel_st.clients) of    
        true -> 
            %user has already joined this channel
            {reply, failed, ChannelSt};
        false ->
            % user joines the channel
            {reply, joined, ChannelSt#channel_st{clients=[Client | ChannelSt#channel_st.clients]}}
    end;

% Leave channel
channelhandle(ChannelSt, {leave, Client}) ->
    case lists:member(Client, ChannelSt#channel_st.clients) of    
        true -> 
            NewChannelState = ChannelSt#channel_st{clients= lists:delete(Client,ChannelSt#channel_st.clients)},
            % user left channel
            {reply, left_channel, NewChannelState};
        false ->
            % User not joined
            {reply, user_not_joined, ChannelSt}
    end;

% Sending message (from GUI, to channel)
channelhandle(ChannelSt, {message_send, Msg, Client, Nick}) ->
    case lists:member(Client, ChannelSt#channel_st.clients) of
        false ->
            % user hasn't join this channel
            {reply, user_not_joined, ChannelSt};
        true ->
            % sends the message to everyone but the sender
            spawn( fun() -> [genserver:request(Receiver,
                {message_receive, ChannelSt#channel_st.name, Nick, Msg})
                || Receiver <- ChannelSt#channel_st.clients, Receiver =/= Client]
            end),
            {reply, message_send, ChannelSt}
    end.
