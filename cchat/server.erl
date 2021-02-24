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

stop(ServerAtom) ->
  genserver:request(ServerAtom, stopAll),
  genserver:stop(ServerAtom).

server_handler(State, {join, Channel, User, Nick}) ->

  case lists:member(Nick, State#s_state.nicks) of
    true -> NewNick = State#s_state.nicks;
    false -> NewNick = [Nick | State#s_state.nicks]

  end,

  case lists:member(Channel, State#s_state.channels) of

    true ->
     NewState = State#s_state{nicks = NewNick},
      case catch (genserver:request(list_to_atom(Channel), {join, User})) of
        join -> {reply, join, NewState };
        error -> {reply, error, NewState}
      end;
    false ->

      genserver:start(list_to_atom(Channel), #c_state{cName = Channel, members = [User]}, fun channel_handler/2),
      {reply, join, State#s_state{channels = [Channel | State#s_state.channels], nicks = NewNick}}
  end;

server_handler(State, {leave, Channel, User}) ->
  case lists:member(Channel, State#s_state.channels) of
    true ->
      case catch (genserver:request(list_to_atom(Channel), {leave, User})) of
        leave -> {reply, leave, State};
        error -> {reply, error, State}
      end;
    false -> {reply, error, State}
  end;

server_handler(State, {nick, OldNick, NewNick}) ->

  case lists:member(NewNick, State#s_state.nicks) of
    true ->
      {reply, error, State};
    false ->
      State#s_state{nicks = [lists:delete(OldNick, State#s_state.nicks)]},
      {reply, ok, State#s_state{nicks = [NewNick | lists:delete(OldNick, State#s_state.nicks)]}}
  end;

server_handler(State, stopAll) ->
  [spawn(fun() -> genserver:stop(list_to_atom(To)) end) || To <- State#s_state.channels],
  {reply, ok, State}.



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
      end;

    {message_send, Nick, Msg, User} ->
      case lists:member(User, State#c_state.members) of
        true ->
          spawn(fun() -> [genserver:request(MsgTo, {message_receive, State#c_state.cName, Nick, Msg})
            || MsgTo <- State#c_state.members, MsgTo =/= User] end),
          {reply, message_send, State};
        false -> {reply, error, State}
      end
  end.



