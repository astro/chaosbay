-module(comment).

-export([init/0, add/2, get_comments/1, get_comments_count/1]).

-include("../include/comment.hrl").


init() ->
	ok.

add(Name, Text) when is_list(Name) ->
    add(list_to_binary(Name), Text);

add(Name, Text) when is_list(Text) ->
    add(Name, list_to_binary(Text));

add(Name, Text) ->
    case torrent:get_torrent_meta_by_name(Name) of
	not_found ->
	    not_found;
	_ ->
	    Now = util:mk_timestamp(),
		C = sql_conns:request_connection(),
		pgsql:equery(C,"insert into comments (name, timestamp, comment) values ($1, $2, $3)", [Name, Now, Text]),
		sql_conns:release_connection(C),
	    ok
    end.

get_comments(Name) when is_list(Name) ->
    get_comments(list_to_binary(Name));

get_comments(Name) ->
	C = sql_conns:request_connection(),
	{ok, _, E} = pgsql:equery(C,"select (name, timestamp, comment) from comments where name = $1 order by timestamp", [Name]),
	sql_conns:release_connection(C),
	lists:flatmap(fun(X) -> 
				{{Name, Timestamp, Comment}} = X, 
				[#comment{name=Name, date=Timestamp, text=Comment}] end, 
		E).

get_comments_count(Name) ->
    length(get_comments(Name)).
