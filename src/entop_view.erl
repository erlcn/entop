%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
-module(entop_view).

-author('mazen.harake@erlang-solutions.com').

-include("entop.hrl").
-include_lib("cecho/include/cecho.hrl").

%% Module API
-export([start/1, reload/1]).

%% Defines
-define(MAX_HLINE, 300).

%% =============================================================================
%% Module API
%% =============================================================================
start(State) ->
    Parent = self(),
    NState = load_remote_static_data(State),
    %% 在目标节点上加载 entop_collector 模块代码
    %% remote_module 默认就是 entop_collector
    remote_load_code(NState#state.remote_module, State#state.node),
    %% 创建用于输出屏幕信息的子进程
    ViewPid = erlang:spawn(fun() -> init(Parent, NState) end),
    %% 同步等待相关初始化工作完成，同步完成后才能正确处理控制指令
    receive continue -> ok end,
    ViewPid.

reload(ViewPid) ->
    ViewPid ! reload.

%% =============================================================================
%% Internal Functions
%% =============================================================================
load_remote_static_data(State) ->
    RPC = fun(M, F, A) -> rpc:call(State#state.node, M, F, A) end,
    Otp = RPC(erlang, system_info, [otp_release]),
    Erts = RPC(erlang, system_info, [version]),
    {Os1, Os2} = RPC(os, type, []),
    OsVers = RPC(os, version, []),
    Flags = [
                %% 逻辑处理器数量
                {cpus, RPC(erlang, system_info, [logical_processors])},
                %% emulator 是否编译了 smp 支持
                {smp, RPC(erlang, system_info, [smp_support])},
                %% 获取用于 asynchronous driver 调用的线程池中的异步线程数量
                {a_threads, RPC(erlang, system_info, [thread_pool_size])},
                %% 是否支持某种类型的 kernel-poll 实现
                {kpoll, RPC(erlang, system_info, [kernel_poll])}
            ],
    State#state{ otp_version = Otp, erts_version = Erts,
		 os_fam = Os1, os = Os2, os_version = OsVers, node_flags = Flags }.

%% Module -> 默认情况为 entop_collector
remote_load_code(Module, Node) ->
    %% 在本地 code path 中获取 Module 的代码（binary）
    {_, Binary, Filename} = code:get_object_code(Module),
    %% 在远端 Node 上加载该 Module 代码（远端 Node 上可以没有该 Module）
    rpc:call(Node, code, load_binary, [Module, Filename, Binary]).

init(Parent, State) ->
    process_flag(trap_exit, true),

    application:start(cecho),
    ok = cecho:cbreak(),
    ok = cecho:noecho(),
    ok = cecho:curs_set(?ceCURS_INVISIBLE),
    ok = cecho:keypad(?ceSTDSCR, true),

    NState = init_callback(State),
    print_nodeinfo(NState),
    Parent ! continue,
    self() ! time_update,
    loop(Parent, NState).

init_callback(State) ->
    %% 调用 entop_format:init/1
    %% callback 的默认值为 entop_format
    %% 初始化输出列的标题、宽度和对齐方式
    case (State#state.callback):init(State#state.node) of
        %% Columns -> 待显示的各列配置
        %% DefaultSort -> 排序列标号
        {ok, {Columns, DefaultSort}, CBState}
                when DefaultSort =< length(Columns) andalso DefaultSort >= 1 ->
            NSort = DefaultSort;
        {ok, {Columns, _}, CBState} ->
            NSort = 1
    end,
    State#state{ columns = Columns, cbstate = CBState, sort = NSort }.

%% 输出 Node 相关信息
%% Node: upu@Betty (Connected) (17/6.0) unix (linux 2.6.32) CPU:4 SMP +A:10 +K
print_nodeinfo(State) ->
    cecho:move(0, 0),
    cecho:hline($ , ?MAX_HLINE),
    {Mj, Md, Mi} = State#state.os_version,
    OsVers = lists:concat([Mj,".",Md,".",Mi]),
    %% 定位到 {0,0} 位置，输出 Node 信息
    cecho:mvaddstr(0, 0, io_lib:format("Node: ~p ",[State#state.node])),
    case State#state.connected of
        false -> cecho:addstr("(Disconnected)");
        true -> cecho:addstr("(Connected)")
    end,
    % Head = io_lib:format(" (~s/~s) ~p (~p ~s)~s",
    %          [State#state.otp_version,
    %           State#state.erts_version, State#state.os_fam,
    %           State#state.os, OsVers, flags2str(State#state.node_flags)]),

    Head = io_lib:format(" (OTP Ver R~s/ERTS-~s) ~p (~p ~s)~s",
             [State#state.otp_version,
              State#state.erts_version, State#state.os_fam,
              State#state.os, OsVers, flags2str(State#state.node_flags)]),

    ok = cecho:addstr(lists:flatten(Head)).

flags2str([]) -> [];
flags2str([{cpus, N}|Rest]) -> 
    [" CPU:"++integer_to_list(N)|flags2str(Rest)];
flags2str([{smp, true}|Rest]) ->
    [" SMP"|flags2str(Rest)];
flags2str([{a_threads, N}|Rest]) ->
    [" +A:"++integer_to_list(N)|flags2str(Rest)];
flags2str([{kpoll, true}|Rest]) ->
    [" +K"|flags2str(Rest)];
flags2str([_|Rest]) ->
    flags2str(Rest).

loop(Parent, #state{ connected = false } = State) ->
    receive
        {nodeup, Node} when Node == State#state.node ->
            remote_load_code(State#state.remote_module, State#state.node),
            loop(Parent, fetch_and_update(State#state{ connected = true }, false));
        _ ->
            loop(Parent, State)
    end;
loop(Parent, State) ->
    receive
        time_update ->      %% 定时刷新
            loop(Parent, fetch_and_update(State, false));
        force_update ->     %% 遇到不支持指令时的强制刷新
            loop(Parent, fetch_and_update(State, true));
        {sort, N} when is_integer(N) ->     %% 对应根据指定列进行排序
            State2 = update_sort_screen(State, N),
            loop(Parent, State2);
        {sort, Direction} ->       %% 对应通过 < 和 > 进行移动
            case Direction of
                next -> State2 = update_sort_screen(State, State#state.sort + 1);
                prev -> State2 = update_sort_screen(State, State#state.sort - 1)
            end,
            loop(Parent, State2);
        reverse_sort ->     %% 对应逆序排序
            State2 = fetch_and_update(State#state{ reverse_sort = (not State#state.reverse_sort) }, true),
            loop(Parent, State2);
        {'EXIT', Parent, _} ->
            ok
    end.

fetch_and_update(State, IsForced) ->
    %% 从远端节点获取信息
    case entop_net:fetch_data(State#state.node, State#state.remote_module) of
        {_Time, {badrpc, nodedown}} ->  %% 远端节点不再运行
            NState = State#state{ connected = false },
            print_nodeinfo(NState),
            cecho:refresh(),
            %% 重连
            erlang:spawn_link(entop_net, reconnect, [self(), State#state.node]),
            NState;
        {_Time, {badrpc, {'EXIT', {undef, _}}}}->   %% 远端节点上不再识别 entop_collector 代码
            remote_load_code(State#state.remote_module, State#state.node),
            fetch_and_update(State, IsForced);
        {Time, {ok, HeaderData, RowDataList}} ->    %% 成功获取到远端节点数据
            %% Time -> 通过 timer:tc/3 得到的时间耗时
            State2 = update_screen(Time, HeaderData, RowDataList, State),
            if
                not IsForced ->
                    erlang:send_after(State2#state.interval, self(), time_update);
                true -> ok
            end,
            State2
    end.

update_sort_screen(State, N) ->
    if
        N >= 1 andalso N =< length(State#state.columns) ->
            fetch_and_update(State#state{ sort = N }, true);
        true ->
            State
    end.

%% 更新屏幕输出内容
update_screen(Time, HeaderData, RowDataList, State) ->
    print_nodeinfo(State),
    draw_title_bar(State),
    print_showinfo(State, Time),
    {Headers, State1} = process_header_data(HeaderData, State),
    lists:foldl(fun(Header, Y) -> 
            cecho:hline($ , ?MAX_HLINE),
            cecho:mvaddstr(Y, 0, Header), Y + 1
        end, 1, Headers),
    {RowList, State2} = process_row_data(RowDataList, State1), 
    SortedRowList = sort(RowList, State),
    {Y, _} = cecho:getmaxyx(),
    StartY = (Y-(Y-7)),
    lists:foreach(fun(N) -> cecho:move(N, 0), cecho:hline($ , ?MAX_HLINE) end, lists:seq(StartY, Y)),
    update_rows(SortedRowList, State2#state.columns, StartY, Y),
    cecho:refresh(),
    State2.

draw_title_bar(State) ->
    cecho:move(6, 0),
    cecho:attron(?ceA_REVERSE),
    cecho:hline($ , ?MAX_HLINE),
    draw_title_bar(State#state.columns, 0),
    cecho:attroff(?ceA_REVERSE).

draw_title_bar([], _) -> ok;
draw_title_bar([{Title, Width, Options}|Rest], Offset) ->
    %% 默认左对齐
    Align = proplists:get_value(align, Options, left),
    cecho:mvaddstr(6, Offset, string:Align(Title, Width)++" "),
    draw_title_bar(Rest, Offset + Width + 1).

%% RoundTripTime -> 从远端节点获取信息花费的时间（调用 timer:tc/3 得到）
print_showinfo(State, RoundTripTime) ->
    cecho:move(5, 0),
    cecho:hline($ , ?MAX_HLINE),
    ColName = element(1,lists:nth(State#state.sort, State#state.columns)),
    SortName = if State#state.reverse_sort -> "Descending"; true -> "Ascending" end,
    Showing = io_lib:format("Interval ~pms, Sorting on ~p (~s), Retrieved in ~pms", 
                    [State#state.interval, ColName, SortName, RoundTripTime div 1000]),
    cecho:mvaddstr(5,0, lists:flatten(Showing)).

process_header_data(HeaderData, State) ->
    %% 输出
    %% Time: local time 17:12:21, up for 001:00:46:37, 3ms latency,
    %% Processes: total 189 (RQ 0) at 52559 RpI using 13854.7k (13887.1k allocated)
    {ok, Headers, NCBState} = (State#state.callback):header(HeaderData, State#state.cbstate),
    {Headers, State#state{ cbstate = NCBState }}.

process_row_data(RowDataList, State) ->
    prd(RowDataList, State, []).

prd([], State, Acc) ->
    {Acc, State};
prd([RowData|Rest], State, Acc) ->
    case (State#state.callback):row(RowData, State#state.cbstate) of
        {ok, skip, NCBState} ->
            prd(Rest, State#state{ cbstate = NCBState }, Acc);
        {ok, Row, NCBState} ->
            prd(Rest, State#state{ cbstate = NCBState }, [Row|Acc])
    end.

sort(ProcList, State) ->
    Sorted = lists:keysort(State#state.sort, ProcList),
    case State#state.reverse_sort of
        true ->
            lists:reverse(Sorted);
        false ->
            Sorted
    end.

update_rows(ProcValuesList, _, LineNumber, Max) when LineNumber == Max orelse ProcValuesList == [] -> ok;
update_rows([RowValues|Rest], Columns, LineNumber, Max) ->
    update_row(tuple_to_list(RowValues), Columns, LineNumber, 0),
    update_rows(Rest, Columns, LineNumber + 1, Max).

update_row(R, C, _, _) when R == [] orelse C == [] -> ok;
update_row([RowColValue|Rest], [{_,Width,Options}|RestColumns], LineNumber, Offset) ->
    StrColVal = if
                    is_list(RowColValue) ->
                        RowColValue;
                    true ->
                        lists:flatten(io_lib:format("~1000p",[RowColValue]))
                end,
    Aligned = case proplists:get_value(align, Options) of
                right ->
                    string:right(StrColVal, Width);
                _ ->
                    string:left(StrColVal, Width)
            end,
    cecho:mvaddstr(LineNumber, Offset, Aligned),
    update_row(Rest, RestColumns, LineNumber, Offset+Width+1).
    
    




