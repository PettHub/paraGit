-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui,        % atom of the GUI process
    nick,       % nick/username of the client
    server      % atom of the chat server
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    (catch genserver:request(ServerAtom,{nick, "", Nick})),
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom
}.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(ClientSt, {join, Channel}) ->
    Result = (catch genserver:request(ClientSt#client_st.server,{join, Channel, self()})),
    case Result of
        {'EXIT',_} ->
            {reply, {error, server_not_reached, "Server does not respond: join"}, ClientSt};
        joined_channel ->
            {reply,ok,ClientSt};
        user_already_joined -> 
            {reply,{error, user_already_joined, "User already joined"},ClientSt}
    end;


% Leave channel
handle(ClientSt, {leave, Channel}) ->
    Result = (catch genserver:request(ClientSt#client_st.server,{leave, Channel, self()})),
    case Result of
        {'EXIT',_} ->
            {reply, {error, server_not_reached, "Server does not respond: leave"}, ClientSt};
        left_channel ->
            {reply,ok,ClientSt};
        user_not_joined -> 
            {reply,{error, user_not_joined, "User not joined"}, ClientSt};
        channel_not_existing ->
            {reply, {error, channel_not_existing, "Channel not existing"}, ClientSt}
    end;

% Sending message (from GUI, to channel)
handle(ClientSt, {message_send, Channel, Msg}) ->
    Result = (catch genserver:request(list_to_atom(Channel),{message_send, Msg, self(), ClientSt#client_st.nick})),
    case Result of 
        {'EXIT',_} ->
            {reply, {error, server_not_reached, "Server does not respond: send"}, ClientSt};
        message_send ->
            {reply, ok, ClientSt};
        user_not_joined ->
            {reply, {error, user_not_joined, "User not joined"}, ClientSt}
    end;

% Change nick
handle(ClientSt, {nick, NewNick}) ->
    Result = (catch genserver:request(ClientSt#client_st.server,{nick, ClientSt#client_st.nick, NewNick})),
    case Result of
        {'EXIT',_} ->
            {reply, {error, server_not_reached, "Server does not respond: nick"}, ClientSt};
        nick_changed ->
            {reply, ok, ClientSt#client_st{nick= NewNick}};
        nick_taken ->
            {reply, {error, nick_taken, "Nick already taken"}, ClientSt}
    end;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(ClientSt, whoami) ->
    {reply, ClientSt#client_st.nick, ClientSt};



% Incoming message (from channel, to GUI)
handle(ClientSt = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, ClientSt};

% Quit client via GUI
handle(ClientSt, quit) ->
    % Any cleanup should happen here, but this is optional
    % sets new nick to empty string in order to free up the users nick
    (casesgenserver:request(ClientSt#client_st.server,{nick, ClientSt#client_st.nick, ""})),
    {reply, ok, ClientSt};

% Catch-all for any unhandled requeClientSts
handle(ClientSt, _Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, ClientSt}.
