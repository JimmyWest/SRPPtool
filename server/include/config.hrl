%% ###############################
%% General config
-define(VERSION, "Version 0.1.1").
-define(VERSION_TUPLE, {0,1,1}).
-define(MAJOR_VERSION, "Major version 0").
-define(MAJOR_VERSION_NUMBER, 0).

%% ###############################
%% Log Logger
-define(debug, true).
-define(DEFAULT_MODULES, [client_handler,socket_handler]).
-define(DEFAULT_TYPES, all).

% Logging structure
% self = loggers pid.
% boc = Begining of color.
% date = The date.
% Time = The time.
% module = The module name of the log caller.
% pid = The pid of the log caller.
% line = The line in code where the log message was called.
% type = The type of log message.
% msg = The log message it self.
% eoc = The end of color.
-define(LOG_STRUCTURE, [boc,time,"-",type,":{",module,":",line,"}: ",msg,eoc]).
-define(DATE_SEP, "").
-define(TIME_SEP, "").

% ANSI escape codes for the logger
-define(INFO_COLOR, "\033[39;49m").
-define(ERROR_COLOR, "\033[97;101;1m").
-define(DEBUG_COLOR, "\033[92;1m").
-define(UNKOWN_COLOR, "\033[93m").
-define(END_COLOR, "\033[0m").

% Logger use of other modules
-define(LOG_MPL, {?MODULE, self(), ?LINE}).
-define(LOG_MACRO(Type,Msg), log:log(Type, ?LOG_MPL, Msg)).

-define(LOG_TYPE_START, {start, "\033[90;107;1m","Start"}).
-define(log_start(Msg), ?LOG_MACRO(?LOG_TYPE_START, Msg)).

-define(LOG_TYPE_INFO, {info, "\033[39;49m", "I"}).
-define(log_info(Msg), ?LOG_MACRO(?LOG_TYPE_INFO, Msg)).

-define(LOG_TYPE_LOAD, {load, "\033[96;2m","Loading"}).
-define(log_load(Msg), ?LOG_MACRO(?LOG_TYPE_LOAD, Msg)).

-define(LOG_TYPE_ERROR, {error, "\033[97;101;1m","E"}).
-define(log_error(Msg), ?LOG_MACRO(?LOG_TYPE_ERROR, Msg)).

-define(LOG_TYPE_DEBUG, {debug, "\033[92;1m","D"}).
-define(log_debug(Msg), ?LOG_MACRO(?LOG_TYPE_DEBUG, Msg)).

-define(LOG_TYPE_HEAVY_DEBUG, {heavydebug, "\033[93;1m","HD"}).
-define(log_heavydebug(Msg), ?LOG_MACRO(?LOG_TYPE_HEAVY_DEBUG,Msg)).

-define(LOG_TYPE_WIERD, {wierd, "\033[95m","WIERD"}).
-define(log_wierd(Msg), ?LOG_MACRO(?LOG_TYPE_WIERD,Msg)).

%% ###############################
%% Port handler
-define(GEN_TCP_CONF, [binary, {packet, 0}, {active, false}]).
-define(ACCEPT_TIMEOUT, 1000).
-define(SOCKET_COOLDOWN_TIMEOUT, 2000).


%% ###############################
%% TCP communication 
-define(TCP_HEAD_CONNECT, 1).
-define(TCP_HEAD_DISCONNECT, 2).

% Main communication link
-define(TCP_HEAD_FOLDER, 10).

% File communication link
-define(TCP_HEAD_FILE_OPEN, 100).
-define(TCP_HEAD_FILE_SUBSCRIBE, 101).
-define(TCP_HEAD_FILE_CONTENT, 110).
-define(TCP_HEAD_LINE_GET, 111).
-define(TCP_HEAD_LINE_UPDATE, 120).
-define(TCP_HEAD_LINE_NEW, 121).
-define(TCP_HEAD_LINE_REMOVE, 122).
-define(TCP_HEAD_CURSORPOS, 150).

% Response message
-define(TCP_HEAD_OK, 200).
-define(TCP_HEAD_ACK, 201).
-define(TCP_HEAD_ACCEPT, 210).
-define(TCP_HEAD_REJECT, 211).
-define(TCP_HEAD_ERROR, 250).

% Unknown message
-define(TCP_HEAD_UNKNOWN, 0).

