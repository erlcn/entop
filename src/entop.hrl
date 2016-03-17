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
%% Records 
-record(state, {
    callback = entop_format,
    remote_module = entop_collector,
    columns,                %% entop_format:init/1 中对 Columns 的定制
    cbstate,                %% 即 entop_format 模块中的 #state{node=undefined, cache=[]}
    node,                   %% 命令行上指定的目标节点名
    otp_version,
    erts_version,
    os_fam,
    os,
    os_version,
    node_flags,             %% [{cpus, xxx},{smp, xxx},{a_threads, xxx},{kpoll, xxx}]
    interval = 1000,        %% 默认的输出刷新时间间隔
    reverse_sort = true,
    sort = 1,               %% 排序列编号
    connected = false       %% 表示连接状态
}).
