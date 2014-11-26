%% ###############################
%% Log Logger
-define(debug, true).
-define(DEFAULT_MODULES, all).
-define(DEFAULT_TYPES, all).
-define(LOG_STRUCTURE, [self,":",boc,"[",date,"-",time,"]{",module,":",line,"}",type,": ",msg,eoc]).
-define(DATE_SEP, "").
-define(TIME_SEP, "").
-define(DATE_TIME_SEP, "-").

% ANSI escape codes for the logger
-define(INFO_COLOR, "\033[39;49m").
-define(ERROR_COLOR, "\033[97;101;1m").
-define(DEBUG_COLOR, "\033[92;1m").
-define(UNKOWN_COLOR, "\033[93m").
-define(END_COLOR, "\033[0m").

% Logger use of other modules
-define(LOG_ML, {?MODULE, ?LINE}).
%%-define(LOG_MACRO_legacy(Type,Msg), log:Type(?LOG_ML, Msg)).
-define(LOG_MACRO(Type,Msg), log:log(Type, ?LOG_ML, Msg)).

%%-define(log_info(Msg), ?LOG_MACRO_legacy(info,Msg)).
-define(LOG_TYPE_START, {start, "\033[90;107;1m","Start"}).
-define(log_start(Msg), ?LOG_MACRO(?LOG_TYPE_START, Msg)).

-define(LOG_TYPE_INFO, {info, "\033[39;49m", "I"}).
-define(log_info(Msg), ?LOG_MACRO(?LOG_TYPE_INFO, Msg)).

-define(LOG_TYPE_LOAD, {load, "\033[96;2m","Loading"}).
-define(log_load(Msg), ?LOG_MACRO(?LOG_TYPE_LOAD, Msg)).

%%-define(log_error(Msg), ?LOG_MACRO_legacy(err,Msg)).
-define(LOG_TYPE_ERROR, {error, "\033[97;101;1m","E"}).
-define(log_error(Msg), ?LOG_MACRO(?LOG_TYPE_ERROR, Msg)).

%%-define(log_debug(Msg), ?LOG_MACRO_legacy(debug,Msg)).
-define(LOG_TYPE_DEBUG, {debug, "\033[92;1m","D"}).
-define(log_debug(Msg), ?LOG_MACRO(?LOG_TYPE_DEBUG, Msg)).

-define(LOG_TYPE_HEAVY_DEBUG, {heavydebug, "\033[91;1m","HD"}).
-define(log_heavydebug(Msg), ?LOG_MACRO(?LOG_TYPE_HEAVY_DEBUG,Msg)).

-define(LOG_TYPE_WIERD, {wierd, "\033[95m","WIERD"}).
-define(log_wierd(Msg), ?LOG_MACRO(?LOG_TYPE_WIERD,Msg)).

%% ###############################
%% Port handler
-define(GEN_TCP_CONF, [binary, {packet, 0}, {active, false}]).
-define(ACCEPT_TIMEOUT, 1000).
-define(SOCKET_COOLDOWN_TIMEOUT, 2000).
