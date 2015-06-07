
-module(game_http_server).
-export([who_plays/3,
  join/3,
  leave/3,
  get_field/3,
  make_turn/3,
  reset/3,
  who_won/3]).

-define(LOGIC, {global, logic}).

ct_string(json) ->
  "Content-type: application/json\r\n\r\n";
ct_string(text) ->
  "Content-type: text/plain\r\n\r\n".

who_plays(SessionId, _, _) ->
  {Players, Active} = gen_server:call(?LOGIC,{who_plays}),
  FieldJSON = "{\"players\":[\"" ++ string:join(Players, "\", \"") ++ "\"], \"whose_turn\":\"" ++ Active ++ "\"}",
  mod_esi:deliver(SessionId, ct_string(json) ++ FieldJSON).

join(SessionId, _, In) ->
  Name = http_uri:decode(In),
  Status = gen_server:call(?LOGIC, {join, Name}),
  mod_esi:deliver(SessionId, ct_string(text) ++ atom_to_list(Status)).

leave(SessionId, _, In) ->
  Name = http_uri:decode(In),
  gen_server:cast(?LOGIC, {leave, Name}),
  mod_esi:deliver(SessionId, ct_string(text) ++ "ok").

get_field(SessionId, _, _) ->
  FieldItems = dict:to_list(gen_server:call(?LOGIC, {get_field})),
  ScreenedItems = lists:map(fun({{X,Y}, Value}) ->
    io_lib:format("{\"x\": ~p, \"y\": ~p, \"player\": \"~s\"}", [X, Y, Value])
  end, FieldItems),
  FieldJSON = "[" ++ string:join(ScreenedItems, ", ") ++ "]",
  mod_esi:deliver(SessionId, ct_string(json) ++ FieldJSON).

make_turn(SessionId, _, In) ->
  Request = http_uri:decode(In),
  WordsCount = string:words(Request, 47),
  case WordsCount of
    3 ->
      Name = string:sub_word(Request, 1, 47),
      {X, _} = string:to_integer(string:sub_word(Request, 2, 47)),
      {Y, _} = string:to_integer(string:sub_word(Request, 3, 47)),
      Status = gen_server:call(?LOGIC, {make_turn, Name, X, Y}),
      mod_esi:deliver(SessionId, ct_string(text) ++ atom_to_list(Status));
    _ -> mod_esi:deliver(SessionId, ct_string(text) ++ "bad_request")
  end.

reset(SessionId, _, _) ->
  gen_server:cast(?LOGIC, {reset}),
  mod_esi:deliver(SessionId, ct_string(text) ++ "ok").

who_won(SessionId, _, _) ->
  Res = gen_server:call(?LOGIC, {who_won}),
  ResJSON = "{\"value\":" ++ Res ++ "}",
  mod_esi:deliver(SessionId, ct_string(json) ++ ResJSON).

