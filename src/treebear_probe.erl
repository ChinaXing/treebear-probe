-module(treebear_probe).
%-compile(debug_info).
-compile(export_all).

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
  {ok, MacRandom} = application:get_env(mac_random),
  {ok, FlagRandom} = application:get_env(flag_random),
  {ok, SnRandom} = application:get_env(sn_random),
  {ok, #{host => Host,
    port => Port, pack_count => PackCount,
    parallelism => Parallelism, count => Count,
    delay => Delay, mac_random => MacRandom,
    flag_random => FlagRandom, sn_random => SnRandom
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
  #{host := Host, port := Port, pack_count := PackCount, parallelism := Parallelism, count := Count, delay := Delay, flag_random := FlagRandom, mac_random := MacRandom, sn_random := SnRandom } = State,
  send_message(Host, Port, Parallelism, Count, Delay, PackCount, MacRandom, FlagRandom, SnRandom),
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
send_message(Host, Port, Parallel, Count, Delay, PackCount, MacRandom, FlagRandom, SnRandom) ->
  io:format("This is for treebear probe message, I'm the client..~n", []),
  PacketFun = fun (_Index) ->
		      buildPacket(PackCount, MacRandom, FlagRandom, SnRandom)
	      end,
  udp_broker:start(Host, Port, Parallel, Count, Delay, PacketFun),
  io:format("work done, It's time to have a break ~n", []),
  {ok, self()}.

buildPacket(PackCount, MacRandom, FlagRandom, SnRandom) ->
  random:seed(random:seed()),
  PacketHeader = buildPacketHeader(PackCount, SnRandom),
  PacketBody = buildSubPacket(PackCount, FlagRandom, MacRandom, []),
  list_to_binary([PacketHeader, PacketBody]).

buildPacketHeader(PackCount, SnRandom) ->
  ProbeMac = randomMac(false),
  SubPacketLength = 16#0018,
  Len = SubPacketLength * PackCount + 40,
  Mode = 0, %%random:uniform(2) - 1,
  Sn = randomSn(SnRandom),
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
buildSubPacket(0, _FlagRandom, _MacRandom, Binary) -> Binary;
buildSubPacket(PackCount, FlagRandom, MacRandom, Binary) ->
  Flag = randomFlag(FlagRandom),
  DevMac = randomMac(MacRandom),
  {TimestampSec, TimestampMicroSec} = timestamp_pair(),
  Rssi = random:uniform(127),
  Channel = 16#02,
  IsAssociated = 16#01,
  FrameControl = random:uniform(1000),
  ProbeTimes = random:uniform(255),
  SubPacket = list_to_binary([DevMac, <<Flag:16>>, <<TimestampSec:32, TimestampMicroSec:32, Rssi, Channel, IsAssociated, 0, FrameControl:16, ProbeTimes:16>>]),
  buildSubPacket(PackCount - 1, FlagRandom, MacRandom, [SubPacket | Binary]).

randomFlag(Random) ->
    case Random of 
	true ->
	    OldState = random:seed(now()),
	    Flag = random:uniform(2),
	    random:seed(OldState);
	_ ->
	    Flag = random:uniform(2)
    end,
    io:format("Flag : ~p~n",[Flag]),
    Flag.
    
	    

randomMac(Random) ->
    case Random of
	true ->
	    OldState = random:seed(now()),
	    L = randomByteList(6, []),
	    random:seed(OldState);
	_ ->
	    L = randomByteList(6, [])
    end,
    list_to_binary(L).

randomSn(Random) ->
    case Random of 
	true ->
	    OldState = random:seed(now()),
	    I = random:uniform(10),
	    random:seed(OldState);
	_ ->
	    I = random:uniform(10)
    end,
    "P_00000000000000_" ++ integer_to_list(I - 1).

randomByteList(0, Cps) -> Cps;
randomByteList(Size, Cps) ->
  R = random:uniform(255),
  randomByteList(Size - 1, [R | Cps]).


timestamp_pair() ->
  {A, B, C} = os:timestamp(),
  {(A * 1000000 + B), C}.

%% just test
randomList() ->
    random:seed({1,2,3}),
    OldState = random:seed(now()),
    io:format("~p~n", [random:uniform(5)]),
    random:seed(OldState),
    io:format("~p~n", [random:uniform(5)]).
    
