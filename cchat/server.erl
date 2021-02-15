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
      case list:member(Channel, State#s_state.channels) of
        true ->
          case lists:member(User, State#s_state.nicks) of
            true -> {reply, user_already_joined, State};
            false -> {reply, join, State#s_state{nicks = [User | State#s_state.nicks]}}
          end;
        false ->
          genserver:start(list_to_atom(Channel), #c_state{cName = Channel, members = [User]}, fun server_handler/2),
          {reply, join, State#s_state{channels = [Channel | State#s_state.channels]}}
      end;

    {leave, Channel, User} ->
      case list:member(Channel, State#s_state.channels) of
        true ->
          case lists:member(User, State#s_state.nicks) of
            true -> {reply, leave, State#s_state{nicks = lists:delete(User, State#s_state.nicks)}};
            false -> {reply, error, State}
          end;
        false -> {reply, error, State}
      end;

    {changeNick, newNick} ->
      case lists:member(newNick, State#s_state.nicks) of
        true -> {reply, error, State};
        false -> {reply, ok, State#s_state{nicks = [newNick | State#s_state.nicks]}}
      end
  end.


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
  genserver:stop(ServerAtom).
% TODO Implement function
% Return ok

