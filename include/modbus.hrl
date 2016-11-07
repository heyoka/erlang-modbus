-define(FC_READ_COILS,    16#01).
-define(FC_READ_INPUTS,   16#02).
-define(FC_READ_HREGS,    16#03).
-define(FC_READ_IREGS,    16#04).
-define(FC_WRITE_COIL,    16#05).
-define(FC_WRITE_HREG,    16#06).
-define(FC_WRITE_COILS,   16#0f).
-define(FC_WRITE_HREGS,   16#10).

-record(tcp_request, {sock, tid = 1, address = 1, function, start, data }).
