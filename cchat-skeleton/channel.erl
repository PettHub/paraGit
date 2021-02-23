-module(channel).
-export([initial_state/1, handle/2]).


-record(channel_st, {
    name,           % Name of this channel
    clients=[]      % The clients in this channel
    }).

initial_state(ChannelAtom) ->
    #channel_st{
        name    = ChannelAtom,
        clients = []
}.

% Join channel
handle(ChannelSt, {join, Client}) ->
	%Check if user is in the channel
    case lists:member(Client, ChannelSt#channel_st.clients) of    
        true -> 
            %If in channel: return tuple including failed since user cannot join twice
            {reply, failed, ChannelSt};
        false ->
            % If not in channel: add user to client list and return tuple including joined (to communicate successful join in channel)
            {reply, joined, ChannelSt#channel_st{clients = [Client | ChannelSt#channel_st.clients]}}
    end;

% Leave channel
handle(ChannelSt, {leave, Client}) ->
	%Check if user is in the channel
    case lists:member(Client, ChannelSt#channel_st.clients) of    
        true -> 
            % If in channel: remove user from channel list
            NewChannelState = ChannelSt#channel_st{clients = lists:delete(Client,ChannelSt#channel_st.clients)},
            {reply, left_channel, NewChannelState};
        false ->
            % If not in channel:return tuple including user_not_joined
            {reply, user_not_joined, ChannelSt}
    end;

% Sending message (from GUI, to channel)
handle(ChannelSt, {message_send, Msg, Client, Nick}) ->
	%Check if user is in the channel
    case lists:member(Client, ChannelSt#channel_st.clients) of
        false ->
            % If not in channel: return tuple including user_not_joined
            {reply, user_not_joined, ChannelSt};
        true ->
        	%If in channel, send message:
            % For each client in the channel (that are not the sender), spawn a genserver process which will delegate {message_receive...} to the receiving client
            spawn( fun() -> [genserver:request(ReceivingClient, {message_receive, ChannelSt#channel_st.name, Nick, Msg}) 
            				|| ReceivingClient <- ChannelSt#channel_st.clients, ReceivingClient =/= Client] end),
            {reply, message_send, ChannelSt}
    end.
