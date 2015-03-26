-module(treebear_probe).
%-export([start/6]).
-compile(export_all).
%-compile(debug_info).


start(Host,Port,Parallel,Count,Delay,PackCount) ->
    io:format("This is for treebear probe message, I'm the client..~n",[]),
    Packet = buildPacket(PackCount),
    udp_broker:start(Host,Port,Parallel,Count,Delay,Packet),
    io:format("oh, work done, It's time to have a break ~n",[]),
    {ok, self()}.

buildPacket(PackCount) ->
    PacketHeader = buildPacketHeader(PackCount),
    PacketBody = buildSubPacket(PackCount,[]),
    list_to_binary([PacketHeader,PacketBody]).

buildPacketHeader(PackCount) ->
    ProbeMac = randomMac(),
    SubPacketLength = 16#0018,
    Len = SubPacketLength * PackCount + 30,
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
buildSubPacket(0,Binary) -> Binary;
buildSubPacket(PackCount, Binary) ->
    Flag = random:uniform(3),
    DevMac = randomMac(),
    {TimestampSec,TimestampMicroSec} = timestamp_pair(),
    Rssi = 16#e1,
    Channel = 16#02,
    IsAssociated = 16#01,
    FrameControl = random:uniform(1000),
    ProbeTimes = random:uniform(255),
    SubPacket = list_to_binary([DevMac,<<Flag:16>>,<<TimestampSec:32,TimestampMicroSec:32,Rssi,Channel,IsAssociated,0,FrameControl:16,ProbeTimes:16>>]),
    buildSubPacket(PackCount - 1, [SubPacket|Binary]).

randomMac() ->
    L = randomByteList(6, []),
    list_to_binary(L).

randomSn() -> 
    I = random:uniform(10),
    "p_00000" ++ integer_to_list(I - 1).

randomByteList(0, Cps) -> Cps;
randomByteList(Size, Cps) ->
    R = random:uniform(255),
    randomByteList(Size - 1, [R|Cps]).
    
    
timestamp_pair() ->
    {A,B,C} = os:timestamp(),
    {(A * 1000000 + B), C}.
    
