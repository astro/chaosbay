-module(tracker).

-export([init/0, tracker_request/7, tracker_request_stopped/3, tracker_info/1, tracker_scrape/1,
	 cleaner_start_link/0, cleaner_loop/0]).


-record(peer, {hash_peer,
	       ip, port,
	       downloaded, uploaded,
	       left,
	       upspeed, downspeed, last}).

-define(RESPONSE_PEER_COUNT, 10).

convert_ip({A,B,C,D}) ->
	<<A:8, B:8, C:8, D:8>>;
convert_ip({A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P}) ->
	<<A:8, B:8, C:8, D:8, E:8, F:8, G:8, H:8, I:8, J:8, K:8, L:8, M:8, N:8, O:8, P:8>>;
convert_ip(E) when is_binary(E), size(E) == 4 ->
	<<A:8, B:8, C:8, D:8>> = E,
	{A,B,C,D};
convert_ip(E) when is_binary(E), size(E) == 16 ->
	<<A:8, B:8, C:8, D:8, E:8, F:8, G:8, H:8, I:8, J:8, K:8, L:8, M:8, N:8, O:8, P:8>> = E,
	{A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P}.


init() ->
    util:safe_mnesia_create_table(peer, [{attributes, record_info(fields, peer)}]).

insert_or_update_peer_info(HashId, PeerId, ErlangIP, Port, Uploaded, Downloaded, Left) ->
    Now = util:mk_timestamp(),
	C = sql_conns:request_connection(),
	SQLIP = convert_ip(ErlangIP),
	case torrent:torrent_name_by_id_t(HashId) of
		not_found -> not_found;
		{ok, _} ->
			case pgsql:equery(C, "SELECT downloaded, uploaded, last FROM tracker WHERE infohash = $1 AND PeerId = $2", 
					[HashId, PeerId]) of
				{_, _, []} ->
					pgsql:equery(C, 
						"insert into tracker (infohash, peerid, ip, port, downloaded, uploaded, leftover, downspeed, upspeed, last)" ++
						" values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)", 
						[HashId, PeerId, SQLIP, Port, Downloaded, Uploaded, Left, 0.0, 0.0, Now]),
					{DownDelta, UpDelta} = {lists:max([Downloaded, 0]), lists:max([Uploaded, 0])};
				{_, _, [{DownloadedOld, UploadedOld, Last1}]} ->
					{DownDelta, UpDelta, Last} =
						{lists:max([Downloaded - DownloadedOld, 0]), lists:max([Uploaded - UploadedOld, 0]), Last1},
					{Downspeed, Upspeed} =
						case Now - Last of
							Time when Time > 0 ->
								{DownDelta / Time, UpDelta / Time};
							_ -> {0,0}
							end,
					pgsql:equery(C, 
						"update tracker (infohash, peerid, ip, port, downloaded, uploaded, leftover, downspeed, upspeed, last)" ++
						" values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)", 
						[HashId, PeerId, SQLIP, Port, Downloaded, Uploaded, Left, Downspeed, Upspeed, Now])
				end,
			sql_conns:release_connection(C),
			{ok, DownDelta, UpDelta}
	end.

tracker_request(HashId, PeerId, ErlangIP, Port, Uploaded, Downloaded, Left) ->
	IOF = insert_or_update_peer_info(HashId, PeerId, ErlangIP, Port, Uploaded, Downloaded, Left),

	case IOF of
		{ok, DownDelta, UpDelta} ->
	    collectd:inc_counter(if_octets, peers, [DownDelta, UpDelta]),
	    %% Assemble result
	    AllPeers = dirty_hash_peers(HashId),
		%%io:format("AllPeers: ~p~n", [AllPeers]),
	    PeersWithoutMe =
		lists:filter(
		  fun(#peer{hash_peer = {_, PeerPeerId}}) ->
			  PeerId =/= PeerPeerId
		  end, AllPeers),
		%%%%io:format("PeersWithoutMe: ~p~n", [PeersWithoutMe]),
	    NeededPeers =
		case Left of
		    %% Nothing left: seeder
		    0 ->
			%% Seeder needs only leechers
			lists:filter(fun(#peer{left = PeerLeft}) ->
					     PeerLeft > 0
				     end, PeersWithoutMe);
		    %% Something left: leecher
		    _ ->
			%% Leechers need leechers and seeders
			PeersWithoutMe
		end,
		%%io:format("NeededPeers: ~p~n", [NeededPeers]),
	    SomePeers = pick_randomly(NeededPeers, ?RESPONSE_PEER_COUNT),
		%%io:format("SomePeers: ~p~n", [SomePeers]),

	    %% Assemble relevant info
	    {peers, [{PeerPeerId, PeerIP, PeerPort}
		     || #peer{hash_peer = {_, PeerPeerId},
			      ip = PeerIP,
			      port = PeerPort} <- SomePeers]};

	not_found ->
	    not_found
  end.


tracker_request_stopped(Infohash, PeerId, ErlangIP) ->
		SQLIP = convert_ip(ErlangIP),
		C = sql_conns:request_connection(),
		pgsql:equery(C, "DELETE FROM tracker WHERE infohash = $1 and peerid = $2 and ip = $3", [Infohash, PeerId, SQLIP]),
		sql_conns:release_connection(C).


tracker_info(HashId) ->
    Peers = dirty_hash_peers(HashId),
    {Seeders, Leechers, Speed} =
	lists:foldl(fun(#peer{left = 0,
			      upspeed = PeerUpspeed,
			      downspeed = PeerDownspeed}, {S, L, Speed}) ->
			    PeerSpeed = lists:max([PeerUpspeed, PeerDownspeed]),
			    {S + 1, L, Speed + PeerSpeed};
		       (#peer{left = PeerLeft,
			      upspeed = PeerUpspeed,
			      downspeed = PeerDownspeed}, {S, L, Speed}) when PeerLeft > 0 ->
			    PeerSpeed = lists:max([PeerUpspeed, PeerDownspeed]),
			    {S, L + 1, Speed + PeerSpeed}
		    end, {0, 0, 0}, Peers),
    {Seeders, Leechers, Speed}.


tracker_scrape(HashId) ->
    lists:foldl(fun(#peer{downloaded = PeerDownloaded,
			  left = 0}, {Complete, Incomplete, Downloaded}) ->
			{Complete + 1, Incomplete, Downloaded + PeerDownloaded};
		   (#peer{downloaded = PeerDownloaded}, {Complete, Incomplete, Downloaded}) ->
			{Complete, Incomplete + 1, Downloaded + PeerDownloaded}
		end, {0, 0, 0}, dirty_hash_peers(HashId)).


dirty_hash_peers(Infohash) ->
	C = sql_conns:request_connection(),
  {_, _, E} = pgsql:equery(C, "SELECT peerid, ip, port, downloaded, uploaded, leftover, upspeed, downspeed, last FROM tracker WHERE infohash = $1",[Infohash]),
  sql_conns:release_connection(C),
	
	L = [ #peer{ hash_peer={Infohash, Peerid}, 
					 ip = convert_ip(SQLIP),
					 port = Port,
					 downloaded = Downloaded,
					 uploaded = Uploaded,
					 left= Left,
					 upspeed = Upspeed,
					 downspeed = Downspeed,
					 last = Last } 
		|| {Peerid, SQLIP, Port, Downloaded, Uploaded, Left, Upspeed, Downspeed, Last} <- E ],
	L.


pick_randomly(_, 0) -> [];
pick_randomly([], _) -> [];
pick_randomly(List, NToPick) ->
    E = lists:nth(random:uniform(length(List)), List),
    List2 = lists:delete(E, List),
    [E | pick_randomly(List2, NToPick - 1)].


%%% CLEANER

-define(CLEAN_INTERVAL, 10).
-define(PEER_MAX_AGE, ?CLEAN_INTERVAL * 180).

cleaner_start_link() ->
    Pid = spawn_link(fun cleaner_loop/0),
    {ok, Pid}.

cleaner_loop() ->
    Now = util:mk_timestamp(),
    MinLast = Now - ?PEER_MAX_AGE,
    F = fun() ->
		mnesia:write_lock_table(peer),
		ObsoletePeers =
		    mnesia:select(peer,
				  [{#peer{last = '$1',
					  _ = '_'},
				    [{'<', '$1', {const, MinLast}}],
				    ['$_']}]),
		lists:foreach(fun mnesia:delete_object/1, ObsoletePeers),
		length(ObsoletePeers)
	end,
    {atomic, N} = mnesia:transaction(F),
% Reenable after PSQL TODO
%    error_logger:info_msg("Cleaned ~B obsolete peers from tracker~n", [N]),

    collect_peer_stats(),

    %% Sleep
    receive
    after ?CLEAN_INTERVAL * 1000 ->
	    ok
    end,
    %% Repeat
    ?MODULE:cleaner_loop().


collect_peer_stats() ->
    {atomic, {Seeders4, Leechers4, Seeders6, Leechers6}} =
	mnesia:transaction(
	  fun() ->
		  mnesia:foldl(fun(#peer{ip = {_, _, _, _}, left = 0},
				   {Seeders4, Leechers4, Seeders6, Leechers6}) ->
				       {Seeders4 + 1, Leechers4, Seeders6, Leechers6};
				  (#peer{ip = {_, _, _, _}},
				   {Seeders4, Leechers4, Seeders6, Leechers6}) ->
				       {Seeders4, Leechers4 + 1, Seeders6, Leechers6};
				  (#peer{left = 0},
				   {Seeders4, Leechers4, Seeders6, Leechers6}) ->
				       {Seeders4, Leechers4, Seeders6 + 1, Leechers6};
				  (#peer{},
				   {Seeders4, Leechers4, Seeders6, Leechers6}) ->
				       {Seeders4, Leechers4, Seeders6, Leechers6 + 1}
			       end,
			       {0, 0, 0, 0},
			       peer)
	  end),
    %io:format("peer_stats: ~p~n", [{Seeders4, Leechers4, Seeders6, Leechers6}]),
    collectd:set_gauge(peers, inet_seeders, [Seeders4]),
    collectd:set_gauge(peers, inet_leechers, [Leechers4]),
    collectd:set_gauge(peers, inet6_seeders, [Seeders6]),
    collectd:set_gauge(peers, inet6_leechers, [Leechers6]).

