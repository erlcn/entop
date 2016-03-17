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
-module(entop_collector).

-author('mazen.harake@erlang-solutions.com').

%% Module API
-export([get_data/0]).

%% =============================================================================
%% Module API
%% =============================================================================
get_data() ->
    HeaderProplist = [
                        {uptime,        erlang:statistics(wall_clock)},
                        {local_time,    calendar:local_time()},
                        {process_count, erlang:system_info(process_count)},
                        %% the number of processes that are ready to run
                        {run_queue,     erlang:statistics(run_queue)},
                        {reduction_count,       erlang:statistics(reductions)},
                        {process_memory_used,   erlang:memory(processes_used)},
                        {process_memory_total,  erlang:memory(processes)},
                        {memory,        erlang:memory([system, atom, atom_used, binary, code, ets])}
                    ],
    Self = self(),
    ProcessesProplist =  [ [ {pid,erlang:pid_to_list(P)} | process_info_items(P) ] ||
			     P <- erlang:processes(), P /= Self ],

    {ok, HeaderProplist, ProcessesProplist}.

%% =============================================================================
%% Internal Functions
%% =============================================================================
%% 每个进程被输出的信息种类
process_info_items(P) ->
    erlang:process_info(P, [
                                registered_name,
                                %% the number of reductions executed by the process
                                reductions,
                                %% the number of messages currently in the message queue of the process
                                message_queue_len,
                                %% the size in words of youngest heap generation of the process
                                heap_size,
                                %% the stack size of the process in words
                                stack_size,
                                %% the total size in words of all heap fragments of the process
                                total_heap_size
                            ]).
