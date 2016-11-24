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

To connect to the modbus-tcp device use modbus_device:connect/3. Specifying the host, port and the modbus device you want to connect.

```
{ok, Pid} = modbus_device:connect("127.0.0.1", 502, 1).
```
 
### Reading registers ###

To read a register you just need to create a connection and specify the number of the starting register and the number of registers you want to read.

```
{ok, Pid} = modbus_device:connect("127.0.0.1", 502, 1).
Result = modbus_device:read_hreg(Pid, 1, 1).
ok = modbus_device:disconnect(Pid).
```

### Reading memory positions ###

To read a memory position you just need to create a connection and specify the memory position of the starting register and the number of registers you want to read.

```
{ok, Pid} = modbus_device:connect("127.0.0.1", 502, 1).
Result = modbus_device:read_hreg(Pid, "%MW0.1", 1).
ok = modbus_device:disconnect(Pid).
```

Otherwise you can specify a list of  memory positions you want to read. This will return you a list of tuples `{MemoryPosition, Value}`.

```
{ok, Pid} = modbus_device:connect("127.0.0.1", 502, 1).
Result = modbus_device:read_hreg(Pid, ["%MD0.6", "%MW0.4", "%MX0.0.0", "%MW0.1"]).
ok = modbus_device:disconnect(Pid).
```

TODO
------------

Write coils