-module(treebear_probe).
-export([start/6]).
%-compile(export_all).
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
    SubPacketLength = 16#0014,
    Len = SubPacketLength * PackCount + 22,
    Mode = random:uniform(2) - 1,
    list_to_binary([<<16#efd1:16,16#fe,Mode,16#0001:16,Len:16,1001:16,1:16>>,ProbeMac,<<PackCount:16,SubPacketLength:16>>]).

%% build subPacket List , has count of PackCount subPacket.
buildSubPacket(0,Binary) -> Binary;
buildSubPacket(PackCount, Binary) ->
    Flag = random:uniform(3),
    DevMac = randomMac(),
    Timestamp = timestampAsLong(),
    Rssi = 16#01,
    Channel = 16#02,
    IsAssociated = 16#01,
    FrameControl = 16#0001,
    ProbeTimes = random:uniform(255),
    SubPacket = list_to_binary([<<Flag:16>>,DevMac,<<Timestamp:32,Rssi,Channel,IsAssociated,0,FrameControl:16,ProbeTimes:16>>]),
    buildSubPacket(PackCount - 1, [SubPacket|Binary]).

randomMac() ->
    L = randomByteList(6, []),
    list_to_binary(L).
    
randomByteList(0, Cps) -> Cps;
randomByteList(Size, Cps) ->
    R = random:uniform(255),
    randomByteList(Size - 1, [R|Cps]).
    
    
timestampAsLong() ->
    {A,B,C} = os:timestamp(),
    round(A * math:pow(10,6) + B * math:pow(10,3) + C).
    
