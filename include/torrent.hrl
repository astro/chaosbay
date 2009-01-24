-record(torrent, {name,
		  hash_id,
		  length,
		  category = "Other",
		  date = 0,
		  comments,
		  binary = <<>>,
		  seeders, leechers, completed}).

-record(comment, {date, text}).
