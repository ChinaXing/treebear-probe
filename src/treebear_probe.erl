-module(treebear_probe).
%-compile(debug_info).

-behaviour(gen_server).
-export([
  start_link/0,
  handle_call/3,
  init/1,
  handle_cast/2,
  handle_info/2,
  code_change/3,
  terminate/2,
  send_message/0,
  send_message/1,
  set_option/1,
  get_option/0,
  help/0
]).
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

help() ->
  gen_server:call(?MODULE, help).

send_message() ->
  gen_server:cast(?MODULE, send_message).

send_message(Options) ->
  gen_server:cast(?MODULE, {send_message, Options}).

get_option() ->
  gen_server:call(?MODULE, get_option).

set_option(Options) ->
  gen_server:call(?MODULE, {set_option, Options}).

init([]) ->
  {ok, Host} = application:get_env(host),
  {ok, Port} = application:get_env(port),
  {ok, PackCount} = application:get_env(pack_count),
  {ok, Parallelism} = application:get_env(parallelism),
  {ok, Count} = application:get_env(count),
  {ok, Delay} = application:get_env(delay),
  {ok, #{host => Host,
    port => Port, pack_count => PackCount,
    parallelism => Parallelism, count => Count,
    delay => Delay
  }}.

handle_call(help, _From, State) ->
  io:format("options: =>~p~n", [State]),
  io:format("functions: => \n~p~n", [{set_option, send_message, get_option}]),
  {reply, ok, State};

handle_call({set_option, Options}, _From, State) ->
  NewState = maps:merge(State, maps:from_list(Options)),
  {reply, maps:to_list(NewState), NewState};

handle_call(get_option, _From, State) ->
  {reply, maps:to_list(State), State}.

handle_cast(send_message, State) ->
  #{host := Host, port := Port, pack_count := PackCount, parallelism := Parallelism, count := Count, delay := Delay} = State,
  send_message(Host, Port, Parallelism, Count, Delay, PackCount),
  {noreply, State};

handle_cast({send_message, Options}, State) ->
  Options_ = maps:merge(State, maps:from_list(Options)),
  handle_cast(send_message, Options_).


terminate(Reason, _State) ->
  error_logger:info_msg("Terminated :~p~n", [Reason]),
  ok.

code_change(_OldVersion, State, Extra) ->
  erorr_loger:info_msg("Code change : ~p~n", [Extra]),
  {ok, State}.

handle_info(Info, State) ->
  error_logger:info_msg("Handle Info : ~p~n", [Info]),
  {noreply, State}.

%% ------- biz ------------ %
send_message(Host, Port, Parallel, Count, Delay, PackCount) ->
  io:format("This is for treebear probe message, I'm the client..~n", []),
  Packet = buildPacket(PackCount),
  udp_broker:start(Host, Port, Parallel, Count, Delay, Packet),
  io:format("work done, It's time to have a break ~n", []),
  {ok, self()}.

buildPacket(PackCount) ->
  PacketHeader = buildPacketHeader(PackCount),
  PacketBody = buildSubPacket(PackCount, []),
  list_to_binary([PacketHeader, PacketBody]).

buildPacketHeader(PackCount) ->
  ProbeMac = randomMac(),
  SubPacketLength = 16#0018,
  Len = SubPacketLength * PackCount + 40,
  Mode = 0, %%random:uniform(2) - 1,
  Sn = randomSn(),
  list_to_binary(
    [<<16#efd1:16, %% magic
    16#fe, %% code
    Mode, %% subCode
    16#0001:16, %% version
    Len:16, %% length
    1001:16, %% venderId
    1:16>>, %% productId
      Sn, %% sn
      ProbeMac, %% probeMac
      <<PackCount:16, %% subPacketLength
      SubPacketLength:16>> %% subPacketList
    ]).

%% build subPacket List , has count of PackCount subPacket.
buildSubPacket(0, Binary) -> Binary;
buildSubPacket(PackCount, Binary) ->
  Flag = random:uniform(3),
  DevMac = randomMac(),
  {TimestampSec, TimestampMicroSec} = timestamp_pair(),
  Rssi = 16#e1,
  Channel = 16#02,
  IsAssociated = 16#01,
  FrameControl = random:uniform(1000),
  ProbeTimes = random:uniform(255),
  SubPacket = list_to_binary([DevMac, <<Flag:16>>, <<TimestampSec:32, TimestampMicroSec:32, Rssi, Channel, IsAssociated, 0, FrameControl:16, ProbeTimes:16>>]),
  buildSubPacket(PackCount - 1, [SubPacket | Binary]).

randomMac() ->
  L = randomByteList(6, []),
  list_to_binary(L).

randomSn() ->
  I = random:uniform(10),
  "P_00000000000000_" ++ integer_to_list(I - 1).

randomByteList(0, Cps) -> Cps;
randomByteList(Size, Cps) ->
  R = random:uniform(255),
  randomByteList(Size - 1, [R | Cps]).


timestamp_pair() ->
  {A, B, C} = os:timestamp(),
  {(A * 1000000 + B), C}.
