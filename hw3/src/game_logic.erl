-module(game_logic).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  start_in_debugger/0,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).



-define(SERVER, ?MODULE).

-record(game_state, {
  field = dict:new(),
  players = [],
  activePlayer ="",
  whoWon = "nobody"
}).


start_link() ->
  gen_server:start_link({global, logic}, ?MODULE, [], []).

start_game() ->
  #game_state{field = dict:new(),
    players = [],
    activePlayer = "",
    whoWon = "nobody"}.

playerNextToThis(Name, List) ->
  case Name == lists:last(List) of
    true -> hd(List);
    false ->
      case hd(List) == Name of
        true -> hd(tl(List));
        false -> playerNextToThis(Name, tl(List))
      end
  end.

matchPlayer(List, Name) ->
  case List of
    [] -> new_one;
    [H | T] ->
      case H == Name of
        true -> player_exists;
        false -> matchPlayer(T,Name)
      end
  end.

join_game(Name, State) ->
  case length(State#game_state.players) == 0 of
    true ->
      Active = Name,
      Players = State#game_state.players ++ [Name],
      {ok, #game_state{field = State#game_state.field, players = Players, activePlayer = Active, whoWon = State#game_state.whoWon}};
    false ->
      Active = State#game_state.activePlayer,
      case matchPlayer(State#game_state.players, Name) of
        new_one ->
          Players = State#game_state.players ++ [Name],
          {ok, #game_state{field = State#game_state.field, players = Players, activePlayer = Active, whoWon = State#game_state.whoWon}};
        player_exists ->
          {player_exists, State}
      end
  end.

removeFromField(Name, Field, FieldC) ->
  case dict:is_empty(FieldC) of
    true -> Field;
    false ->
      {{X,Y},PlayerName} = lists:last(dict:to_list(FieldC)),
      case PlayerName == Name of
        true ->
          removeFromField(Name, dict:erase({X,Y},Field), dict:erase({X,Y},FieldC));
        false ->
          removeFromField(Name,Field, dict:erase({X,Y},FieldC))
      end
  end.

leave_game(Name, State) ->
  case Name == State#game_state.activePlayer of
    true ->
      Active = playerNextToThis(Name, State#game_state.players),
      Players = State#game_state.players -- [Name],
      Field = removeFromField(Name, State#game_state.field, State#game_state.field),
      #game_state{field = Field, players = Players, activePlayer = Active, whoWon = State#game_state.whoWon};
    false ->
      Active = State#game_state.activePlayer,
      Players = State#game_state.players -- [Name],
      Field = removeFromField(Name, State#game_state.field, State#game_state.field),
      #game_state{field = Field, players = Players, activePlayer = Active, whoWon = State#game_state.whoWon}
  end.

try_make_turn(X, Y, PlayerName, State) ->
  case State#game_state.whoWon == "nobody" of
    true ->
      case dict:find({X,Y}, State#game_state.field) of
        error ->
          case State#game_state.activePlayer == PlayerName of
            true ->
              Field = dict:append({X,Y},PlayerName,State#game_state.field),
              Active = playerNextToThis(State#game_state.activePlayer, State#game_state.players),
              Players = State#game_state.players,
              {ok, #game_state{field = Field, players = Players, activePlayer = Active, whoWon = State#game_state.whoWon}};
            false ->
              {not_your_turn, State}
          end;
        {ok, _Value} ->
          {wrong_point, State}
      end;
    false ->
      case State#game_state.whoWon == PlayerName of
          true ->
            {game_over_you_win, State};
          false ->
            {game_over, State}
      end
  end.

checkTo(X,Y,PlayerName, Field,StepX, StepY, Count, Stop)->
  case Stop of
    true -> Count;
    false ->
      case Count == 4 of
        true -> checkTo(X,Y,PlayerName,Field,StepX, StepY, Count, true);
        false ->
          case dict:find({X,Y}, Field) of
            error -> checkTo(X,Y,PlayerName,Field,StepX, StepY, Count, true);
            {ok, Value} ->
              case Value == PlayerName of
                true -> checkTo(X + StepX,Y + StepY,PlayerName,Field,StepX, StepY, Count + 1, false);
                false -> checkTo(X,Y,PlayerName,Field,StepX, StepY, Count, true)
              end
          end
      end
  end.

who_won(State, NotWatch) ->
  case dict:is_empty(NotWatch) of
    true -> {"null", State};
    false ->
      {{X,Y},PlayerName} = lists:last(dict:to_list(NotWatch)),
      L = checkTo(X - 1,Y,PlayerName,NotWatch,-1,0,0,false),
      R = checkTo(X + 1,Y,PlayerName,NotWatch,1,0,0,false),
      T = checkTo(X,Y + 1,PlayerName,NotWatch,0,1,0,false),
      B = checkTo(X,Y - 1,PlayerName,NotWatch,0,-1,0,false),
      TL = checkTo(X - 1,Y + 1,PlayerName,NotWatch,-1,1,0,false),
      BR = checkTo(X + 1,Y - 1,PlayerName,NotWatch,1,-1,0,false),
      TR = checkTo(X + 1,Y + 1,PlayerName,NotWatch,1,1,0,false),
      BL = checkTo(X - 1,Y - 1,PlayerName,NotWatch,-1,-1,0,false),

      case (L + R >= 4)or(T + B >= 4)or(TL + BR >= 4)or(BL + TR >= 4) of
        true ->
          Field = State#game_state.field,
          Players = State#game_state.players,
          Active = State#game_state.activePlayer,
          {"\"" ++ PlayerName ++ "\"", #game_state{field = Field, players = Players, activePlayer = Active, whoWon = PlayerName}};
        false ->
          who_won(State, dict:erase({X,Y}, NotWatch))
      end
  end.

who_plays(State) ->
  {State#game_state.players, State#game_state.activePlayer}.



init([]) ->
  { ok, start_game() }.

handle_call( {who_plays} , _, State) ->
  { reply, who_plays(State), State } ;

handle_call( {who_won} , _, State) ->
  {Status, NewState} = who_won(State, State#game_state.field),
  {reply, Status, NewState};

%handle_call( {get_cell, X, Y}, _, State) ->
 % {reply, get_cell(X, Y, State), State};

handle_call( {get_field}, _, State) ->
  {reply, State#game_state.field, State};

handle_call( {make_turn, PlayerName, X, Y}, _, State) ->
  {Status, NewState} = try_make_turn(X, Y, PlayerName, State),
  {reply, Status, NewState};

handle_call( {join, Name}, _, State) ->
  {Status, NewState} = join_game(Name, State),
  {reply, Status, NewState}.

handle_cast( {reset}, _ ) ->
  {noreply, #game_state{}};

handle_cast( {leave, Name}, State) ->
  NewState = leave_game(Name, State),
  {noreply, NewState}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


start() ->
  game_logic:start_link(),
  inets:start(),
  inets:start(
    httpd, [
      {port, 8090},
      {server_name, "localhost"},
      {document_root, "www"},
      {modules, [
        mod_alias,
        mod_esi,
        mod_get,
        mod_log
      ]},
      {server_root, "."},
      {error_log, "error.log"},
      {transfer_log, "access.log"},
      {directory_index, ["index.html"]},
      {mime_types, [

        {"html", "text/html"},
        {"js", "text/javascript"},
        {"css", "text/css"}
      ]},
      {erl_script_alias,

        {"/api", [game_http_server]}
      }
    ]
  ).

do_nothing() -> timer:sleep(1000), do_nothing().
start_in_debugger() -> start(), do_nothing().