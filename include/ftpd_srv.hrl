-ifndef(_FTPD_SRV_HRL).
-define(_FTPD_SRV_HRL, true).

-ifdef(debug).
-define(dbg(X,Y), error_logger:info_msg("*dbg ~p:~p: " X,
					[?MODULE, ?LINE | Y])).
-else.
-define(dbg(X,Y), ok).
-endif.

%% default configuration options
-define(FTPD_PORT, 21).
-define(FTPD_MAX_CONN, 40).
-define(FTPD_LOGFILE, "ftpd.log").
-define(FTPD_IDLE_TIMEOUT, 300000). % 5 minutes
-define(FTPD_UNIQUE_PREFIX, "ftpd").

-define(MAX_FTP_FILES, 500).
-define(FTP_SORT_ORDER, name).
-define(FTP_SORT_DIRECTION, asc).

%% server conf
-record(sconf,
	{port = ?FTPD_PORT,         % which port is this server listening to
	 ip = {0,0,0,0},            % bind to this IP, {0,0,0,0} is possible
	 rootdir = "",
	 greeting_file,
	 servername = element(2, inet:gethostname()), % printed in greeting
	 max_connections = ?FTPD_MAX_CONN,
	 allow_hosts = [{{0,0,0,0},{0,0,0,0}}],
	 deny_hosts = [],           % none denied
	 users = [],                % [{User,Passwd,[{Dir,AuthFlags}]}]
	 auth_mod = ftpd,           % default impl, reject all
	 idle_timeout = ?FTPD_IDLE_TIMEOUT,
	 unique_prefix = ?FTPD_UNIQUE_PREFIX,
	 local_cs = "ISO-8859-1",   % FIXME: "" should work??
	 use_utf8_by_default = false,
	 use_fd_srv = false,        % use fd_srv to open priveleged port
	 event_mod = ftpd,
	 sys_ops = 0,          
	 jail = true
	}).

-endif.
