-module(song).
-export([start/0, song/1]).

song(1) ->
  io:format("1 bottle of coke on the wall, 1 bottle of coke, take one down, pass it around, no more bottles of coke on the wall");
song(N)  when is_integer(N) and (N>=2) ->
  io:format("~p bottles of coke on the wall, ~p bottles of coke, take one down, pass it around, ~p bottle of coke on the wall ~n", [N,N,N-1]),
  song(N-1);
song(N) -> error(not_suitable_format_of_input_data).

start() ->
  try
    song(11)
  catch
    Class:ErrValue -> io:format("Some error: ~p, ~p ~n", [Class, ErrValue])
  end.
