%%%-------------------------------------------------------------------
%%% @author Stephan <stephan@lobster.sin>
%%% @copyright (C) 2011, Stephan
%%% @doc
%%%
%%% @end
%%% Created : 28 Dec 2011 by Stephan <stephan@lobster.sin>
%%%-------------------------------------------------------------------
-module(stats).

-define(INTERVAL, 60000).

-behaviour(gen_server).

%% API
-export([start_link/0, inc_bytes/2, update_peers/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {bytes = undefined, peers = undefined}).

%%%===================================================================
%%% API
%%%===================================================================

inc_bytes(DownDelta, UpDelta) ->
    gen_server:cast(?SERVER, {inc_bytes, DownDelta, UpDelta}).

update_peers(Seeders4, Leechers4, Seeders6, Leechers6) ->
    gen_server:cast(?SERVER, {update_peers, Seeders4, Leechers4, Seeders6, Leechers6}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    arm_timer(?INTERVAL),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(timer, State) ->
    TimeTriggered = util:mk_timestamp_us(),

    C = sql_conns:request_connection(),
    WriteStats = fun(Type, Value) ->
			 pgsql:equery(C, "INSERT INTO stats (timestamp, type, value) VALUES ($1, $2, $3)",
				      [util:mk_timestamp(), Type, Value])
		 end,
    case State#state.bytes of
	{Down, Up} ->
	    WriteStats("down", Down * 1000 div ?INTERVAL),
	    WriteStats("up", Up * 1000 div ?INTERVAL);
	_ ->
	    false
    end,
    case State#state.peers of
	{Seeders4, Leechers4, Seeders6, Leechers6} ->
	    WriteStats("seeders4", Seeders4),
	    WriteStats("leechers4", Leechers4),
	    WriteStats("seeders6", Seeders6),
	    WriteStats("leechers6", Leechers6);
	_ ->
	    false
    end,
    sql_conns:release_connection(C),

    case TimeTriggered + ?INTERVAL - util:mk_timestamp_us() of
	Delay when Delay > 0 ->
	    arm_timer(Delay);
	_ ->
	    gen_server:cast(self(), timer)
    end,

    {noreply, State#state{bytes = undefined, peers = undefined}};

handle_cast({inc_bytes, DownDelta, UpDelta}, State) ->
    {Down, Up} = case State#state.bytes of
		     {Down1, Up1} -> {Down1, Up1};
		     undefined -> {0, 0}
		 end,
    {noreply, State#state{bytes = {Down + DownDelta, Up + UpDelta}}};

handle_cast({update_peers, Seeders4, Leechers4, Seeders6, Leechers6}, State) ->
    {noreply, State#state{peers = {Seeders4, Leechers4, Seeders6, Leechers6}}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

arm_timer(Delay) ->
    timer:apply_after(Delay, gen_server, cast, [self(), timer]).
