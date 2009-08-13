-module(torrent_search).

-behaviour(gen_server).

%% API
-export([start_link/0, fold/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../include/torrent.hrl").

-record(request, {requester, foldfun, acc}).
-record(request_error, {requester, reason}).
-record(state, {worker = undefined, queue = []}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

fold(FoldFun, AccIn) ->
    case gen_server:call(?SERVER, {fold, FoldFun, AccIn}) of
	{ok, AccOut} -> AccOut;
	{error, Reason} -> exit(Reason)
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({fold, FoldFun, AccIn}, From, #state{queue = Queue} = State) ->
    Request = #request{requester = From,
		       foldfun = FoldFun,
		       acc = AccIn},
    State2 = State#state{queue = [Request | Queue]},
    State3 = maybe_start(State2),
    {noreply, State3}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({worker_done, Worker, ResultQueue}, #state{worker = Worker} = State) ->
    lists:foreach(fun(#request{requester = Requester, acc = AccOut}) ->
			  gen_server:reply(Requester, {ok, AccOut});
		     (#request_error{requester = Requester, reason = Reason}) ->
			  gen_server:reply(Requester, {error, Reason})
		  end, ResultQueue),

    State2 = State#state{worker = undefined},
    State3 = maybe_start(State2),
    {noreply, State3}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_M, State=#state{}) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

maybe_start(#state{worker = undefined, queue = []} = State) ->
    %% Nothing to do
    State;
maybe_start(#state{worker = Worker} = State) when is_pid(Worker) ->
    %% Worker is running, don't do anything
    State;
maybe_start(#state{worker = undefined, queue = Queue} = State) ->
    NewWorker = spawn_link(fun() ->
				   worker(Queue)
			   end),
    State#state{worker = NewWorker, queue = []}.


worker(Queue) ->
    {atomic, ResultQueue} =
	mnesia:transaction(
	  fun() ->
		  util:mnesia_fold_table_t(
		    fun(TorrentMeta, Queue1) ->
			    lists:map(fun(#request{requester = Requester,
						   foldfun = FoldFun,
						   acc = AccIn} = Request) ->
					      case (catch FoldFun(TorrentMeta, AccIn)) of
						  {'EXIT', Reason} -> #request_error{requester = Requester,
										     reason = Reason};
						  AccOut -> Request#request{acc = AccOut}
					      end;
					 (#request_error{} = RequestError) ->
					      RequestError
				      end, Queue1)
		    end, Queue, #torrent_meta{_ = '_'})
	  end),
    gen_server:cast(?SERVER, {worker_done, self(), ResultQueue}).
