%% Log Logger
-define(debug, true).
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
-define(LOG_MFL, {?MODULE, ?LINE}).
-define(LOG_MACRO(Type,Msg), log:Type(?LOG_MFL, Msg)).
-define(log_info(Msg), ?LOG_MACRO(info,Msg)).
-define(log_error(Msg), ?LOG_MACRO(err,Msg)).
-define(log_debug(Msg), ?LOG_MACRO(debug,Msg)).

%% Port handler
-define(GEN_TCP_CONF, [binary, {packet, 0}, {active, false}]).
-define(ACCEPT_TIMEOUT, 1000).
-define(SOCKET_COOLDOWN_TIMEOUT, 2000).
