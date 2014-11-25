%% Log Logger
-define(debug, true).

%% Port handler
-define(GEN_TCP_CONF, [binary, {packet, 0}, {active, true}]).
-define(ACCEPT_TIMEOUT, 1000).
-define(SOCKET_COOLDOWN_TIMEOUT, 2000).
