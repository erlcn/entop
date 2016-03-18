
# entop


如同 Unix 中 top 一样的 Erlang 节点信息查看工具。


----------


## 简介

entop 是用来展示远端 Erlang 节点运行信息的工具，其信息显示的方式类似于 Unix 中的 top 命令。   
若要保证 entop 的正常运行，在 pre-R15 环境下，需要使用 `cecho 0.3.0` 版本；在 R15 或更高版本的环境下需要 `cecho 0.4.0` 版本。    
cecho 的 github 地址：[这里](http://www.github.com/mazenharake/cecho)。


----------


## 编译
清理和编译可以分别运行如下命令

```
    ./rebar clean
    ./rebar compile
```

注意：如果你遇到和 `cecho` 依赖相关的问题，可以手动创建符号链接到 `deps/` 下的 cecho （如果你的 cecho 放在其他目录也可以进行类似操作），或者运行 `./rebar get-deps` 以下载最新版本。当通过 rebar 获取到最新版本后，不要忘记重新编译整个应用。


----------


## 用法

若想成功运行 entop ，首先要确保 Erlang 已经安装到你的系统之中，并且 cecho 库所在路径被 Erlang code path 所包含。
项目中默认提供的启动脚本(entop)假定了其在 entop 应用根目录下被执行，如果这与你的实际情况不符，请自行调整脚本的相应路径，或者直接确保 entop 的 ebin/ 目录包含在 Erlang code path 之中。详情请参考启动脚本具体内容。

```
    Usage: ./entop <TARGETNODE> [-name <NAME>|-sname <SNAME>] [-setcookie <COOKIE>]
```

### entop 的运行示例

```
    > ./entop rmq_yoyo@YOYO -sname entop -setcookie yoyo
```

### 用户接口

entop 的接口允许用户定制化，所以本节描述的接口均为“内置”接口。

#### 表头

**第一行** 主要展示了节点的静态信息，例如节点名、操作系统类型、指定的 erl flag 、当前所运行的 erlang 版本信息。  
**第二行** 展示了（目标节点所在机器的）本地时间、目标节点已持续运行的时间（格式为 Days:Hours:Minutes:Seconds）、运行 entop 的节点与目标节点之间的网络延迟情况（即 net_adm:ping() 成功交互所需花费的时间）  
**第三行** 展示了系统中每个进程的具体信息、进程的总数、运行队列中的进程数量（由调度器进行调度的待运行进程数量）、reductions per interval (RpI) 值（自从上一次 called the node 后系统已经 reduction 的次数）、以及每个进程占用的内存量。  
**第四行** 展示了系统内存使用量、atom 内存占用量（当前使用量/总体分配量）、binary 内存占用量、code 内存占用量，以及 ets 内存占用量。  
**第五行** 为空白，目前作为预留。  
**第六行** 为和行内容展示相关的信息，例如信息获取时间间隔、信息展示排序方式，以及获取相关信息所耗费的时间。  

### 在 entop 运行状态下的控制命令

**[1-N]**:   
根据指定列编号进行输出内容排序。第一列编号为 1 ，其他列按顺序增加。  
    
**r**:  
在升序排序和降序排序之间进行切换。  

**q**:  
从 entop 中退出返回 shell 命令行。  

**Ctrl-C**:  
等价于 'q' 命令。  

**'<'** 和 **'>'**:  
将当前排序列左移或者右移（注意：次数为小于和大于号，非箭头）  

使用示例
----------

![](https://raw.githubusercontent.com/moooofly/ImageCache/master/Pictures/entop%E6%9F%A5%E7%9C%8Brabbitmq.png "通过 entop 查看 RabbitMQ 运行状态")

其中
Reductions -- the number of reductions executed by the process
MQueue -- the number of messages currently in the message queue of the process
HSize -- the size in words of youngest heap generation of the process
SSize -- the stack size of the process in words
HTot -- the total size in words of all heap fragments of the process

贡献
----------
如果你在使用 entop 中发现了问题，可以在这里进行反馈 [create an issue] [1] 。

[1]: http://github.com/moooofly/entop/issues "entop issues"
