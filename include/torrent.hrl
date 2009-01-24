-record(torrent, {name,
		  hash_id,
		  length,
		  category = "Other",
		  date = 0,
		  comments,
		  binary = <<>>}).

-record(comment, {date, text}).
