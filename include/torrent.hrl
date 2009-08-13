-record(torrent_meta, {name,
		       id,
		       length,
		       date = 0}).
-record(torrent_data, {name, binary}).

-record(browse_result, {name, id, length, age, seeders, leechers, speed}).
