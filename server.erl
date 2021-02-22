-module(server).
-export([start/1, stop/1]).

% Start a new server process with the given name
% Do not change the signature of this function.

-record(s_state, {
  channels = [],
  nicks = []
}).

-record(c_state, {
  cName,
  members = []
}).


start(ServerAtom) ->
  genserver:start(ServerAtom, #s_state{}, fun server_handler/2).

server_handler(State, Input) ->
  case Input of

    {join, Channel, User} ->
      case lists:member(Channel, State#s_state.channels) of

        true ->
          CanIJoin = (catch(genserver:request(list_to_atom(Channel),{join,User}))),
          case CanIJoin of
            join -> {reply, join, State};
            error -> {reply, error, State}
          end;
        false ->
          genserver:start(list_to_atom(Channel), #c_state{cName = Channel, members = [User]}, fun channel_handler/2),
          {reply, join, State#s_state{channels = [Channel | State#s_state.channels]}}
      end;

    {leave, Channel, User} ->
      case lists:member(Channel, State#s_state.channels) of
        true ->
          CanLeave = (catch (genserver:request(list_to_atom(Channel), {leave, User}))),
          case CanLeave of
            leave -> {reply, leave, State};
            error -> {reply, error, State}
          end;
        false -> {reply, error, State}
      end;

    {nick, NewNick} ->
      case lists:member(NewNick, State#s_state.nicks) of
        true -> {reply, error, State};
        false -> {reply, ok, State#s_state{nicks = [NewNick | State#s_state.nicks]}}
      end
  end.

channel_handler(State, Input) ->
  case Input of

    {join, User} ->
      case lists:member(User, State#c_state.members) of
        true -> {reply, error, State};
        false -> {reply, join, State#c_state{members = [User | State#c_state.members]}}
      end;

    {leave, User} ->
      case lists:member(User, State#c_state.members) of
        true -> {reply, leave, State#c_state{members = lists:delete(User, State#c_state.members)}};
        false -> {reply, error, State}
      end

   % {message_send, Msg, Nick, User} ->
     % case lists:member(User, State#c_state.members) of
       % true -> {reply, message_send, }
      %end
  end.


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
  genserver:stop(ServerAtom).
% Return ok

