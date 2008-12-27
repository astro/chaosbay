-module(html).

-export([to_iolist/1]).


to_iolist(HTML) ->
    list_to_binary(to_iolist1(HTML)).

to_iolist1(S) when is_list(S) ->
    mochiweb_html:escape(S);

to_iolist1(S) when is_binary(S) ->
    mochiweb_html:escape(S);

to_iolist1({El, Children}) when is_atom(El) ->
    to_iolist1({atom_to_list(El), Children});

to_iolist1({El, Children}) ->
    [<<"<">>, El, <<">">>,
     [to_iolist1(Child) || Child <- Children],
     <<"</">>, El, <<">">>];

to_iolist1({El, Attrs, Children}) when is_atom(El) ->
    to_iolist1({atom_to_list(El), Attrs, Children});

to_iolist1({El, Attrs, Children}) ->
    [<<"<">>, El,
     [[<<" ">>, K, <<"='">>, mochiweb_html:escape_attr(V), <<"'">>]
      || {K, V} <- Attrs],
     <<">">>,
     [to_iolist1(Child) || Child <- Children],
     <<"</">>, El, <<">">>].
