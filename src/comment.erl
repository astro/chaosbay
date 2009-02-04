-module(comment).

-export([init/0, add/2, get_comments/1, get_comments_count/1]).

-include("../include/comment.hrl").


init() ->
    util:safe_mnesia_create_table(comment, [{type, bag},
				  {disc_copies, [node()]},
				  {attributes, record_info(fields, comment)}]).

add(Name, Text) when is_list(Name) ->
    add(list_to_binary(Name), Text);

add(Name, Text) when is_list(Text) ->
    add(Name, list_to_binary(Text));

add(Name, Text) ->
    Now = util:mk_timestamp(),
    F = fun() ->
		mnesia:write(#comment{name = Name,
				      date = Now,
				      text = Text})
	end,
    {atomic, _} = mnesia:transaction(F).

get_comments(Name) when is_list(Name) ->
    get_comments(list_to_binary(Name));

get_comments(Name) ->
    F = fun() ->
		mnesia:read({comment, Name})
	end,
    {atomic, Comments} = mnesia:transaction(F),
    S = lists:foldl(fun(#comment{} = C, S) ->
			    sorted:insert(S, C)
		    end, sorted:new(#comment.date, asc, unlimited), Comments),
    sorted:to_list(S).


get_comments_count(Name) ->
    length(get_comments(Name)).
