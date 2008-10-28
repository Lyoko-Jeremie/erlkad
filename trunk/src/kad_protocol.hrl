%%% protocol
%%% reference:
%%% the msg is based on binary
%%% format:
%%%  -----------------------------------------------
%%% | cmd | target | sender | identify | len | data |
%%%  -----------------------------------------------
%%% the cmd is 1-bytes
%%% the target is 20-bytes
%%% the sender is 20-bytes
%%% the identify is 20-bytes
%%% the len is 2-bytes (max is 2^16)
%%% data is len bytes
%%% the max msg len is less than MAX_MSG_LEN(in kad.hrl, usally is MTU)
%%% 1 + 20 + 20 + 20 + 2 + Len = msglen

%%% if the cmd is PING, the target identify can be empty

%% Cmd
-define(PING, 1).
-define(PING_FIRST, 2).
-define(STORE, 3).
-define(FIND_NODE, 4).
-define(FIND_VALUE, 5).
-define(DELETE, 6).

-define(PING_RSP, 101).
-define(PING_FIRST_RSP, 102).
-define(STORE_RSP, 103).
-define(FIND_NODE_RSP, 104).
-define(FIND_VALUE_RSP, 105).
-define(DELETE_RSP, 106).
-define(PING_FIRST_ACK, 107).


%% Cmd op type
-define(OP_REQ, req).
-define(OP_RSP, rsp).

%% Cmd
-define(E_SUCCESS, 0).
-define(E_NOTIMPL, 1).
-define(E_BUSY, 2).
-define(E_FAILED, 3).




