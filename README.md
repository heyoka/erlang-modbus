erlang-modbus
========================================

This is an erlang modbus-tcp driver.  It allows you to communicate with remote modbus-tcp speaking devices using erlang.

How to build
------------

Download a copy of the repository and run:

```
./rebar3 compile
```

How to run
----------

### Prerequisites ###

You need a modbus-tcp device that you can connect to, listening on any port. Most modbus-tcp clients listen on port 502.

### Starting the system ###

Once the prerrequisites are met, simply run:

```
./rebar3 shell
```

and you should get an Erlang shell.

### Connecting ###

To connect to the modbus-tcp device use modbus:connect/3. Specifying the host, port and the modbus device you want to connect.

```
{ok, Pid} = modbus:connect("127.0.0.1", 502, 1).
```
 
### Reading ###

To read you just need to create a connection and specify the number of the starting register and the number of registers you want to read.

```
{ok, Pid} = modbus:connect("127.0.0.1", 502, 1).
[0,1,1,0] = modbus:read_coils(Pid, 1, 4).
[1,1,1,0,1,0,0,0] = modbus:read_inputs(Pid, 8, 8).
[1793] = modbus:read_hregs(Pid, 128, 1).
[302, 79] = modbus:read_iregs(Pid, 38, 2).
ok = modbus:disconnect(Pid).
```

### Writing ###

To write you just need to create a connection and specify the number of the starting register and the value or a list of values you want to write.

```
{ok, Pid} = modbus:connect("127.0.0.1", 502, 1).
ok = modbus:write_coil(Pid, 1, 1).
ok = modbus:write_coils(Pid, 8, [1,0,1,1,0,0,1,0,1]).
ok = modbus:write_hreg(Pid, 128, 1932).
ok = modbus:write_hregs(Pid, 38, [12,32,46,2345,234,2]).
ok = modbus:disconnect(Pid).
```

### Options ###

You can specify the datatype of the output in the options. The valid options are:

|Options|Output|
|-------|------|
|[{output, int16}, {signed, false}]| A list of 16-bit unsigned integer (between 0 and 65535)|
|[{output, int16}, {signed, true}]| A list of 16-bit signed integer (between -32768 and 32767)|
|[{output, int32}, {signed, false}]| A list of 32-bit unsigned integer (between 0 and 4,294,967,295)|
|[{output, int32}, {signed, true}]| A list of 32-bit signed integer (between -2,147,483,648 and 2,147,483,647)|
|[{output, float32}]| A list of 32-bit single precision IEEE floating point number.|
|[{output, coils}]| A list of coils (between 0 and 1)|
|[{output, ascii}]| An ASCII string|
|[{output, binary}]| A raw binary|

Example:
```
{ok, Pid} = modbus:connect("127.0.0.1", 502, 1).
[-365189] = modbus:read_ireg(Pid, 1, 2, [{output, int32}, {signed, true}]).
ok = modbus:disconnect(Pid).
```

### Reading memory positions ###

To read a memory position you just need to create a connection and specify the memory position of the starting register and the number of registers you want to read.

```
{ok, Pid} = modbus:connect("127.0.0.1", 502, 1).
Result = modbus:read_memory(Pid, "%MW0.1", 1).
ok = modbus:disconnect(Pid).
```

Otherwise you can specify a list of  memory positions you want to read. This will return you a list of tuples `{MemoryPosition, Value}`.

```
{ok, Pid} = modbus:connect("127.0.0.1", 502, 1).
Result = modbus:read_memory(Pid, ["%MD0.6", "%MW0.4", "%MX0.0.0", "%MW0.1"]).
ok = modbus:disconnect(Pid).
```

### Writing memory positions ###

To write a memory position you just need to create a connection and specify the memory position and the value you want to write.

```
{ok, Pid} = modbus:connect("127.0.0.1", 502, 1).
ok = modbus:write_memory(Pid, "%MX0.34.5", 1).
ok = modbus:disconnect(Pid).
```

Otherwise you can specify a list of  `{MemoryPosition, Value}` you want to write.

```
{ok, Pid} = modbus:connect("127.0.0.1", 502, 1).
ok = modbus:write_memory(Pid, [{"%MD0.6", -12.89}, {"%MW0.4", 16}, {"%MX0.0.0", 1}, {"%MW0.1", 8374}]).
ok = modbus:disconnect(Pid).
```
