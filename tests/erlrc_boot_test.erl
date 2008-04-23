-module (erlrc_boot_test).

-export ([ start_link/0, notify/1, dump/0 ]).

-behaviour (gen_server).
-export ([ init/1,
           handle_call/3,
           handle_cast/2,
           handle_info/2,
           terminate/2,
           code_change/3]).
%
% public
%

start_link () ->
  gen_server:start_link ({ local, ?MODULE }, ?MODULE, [ ], [ ]).

notify (Item) ->
  gen_server:cast (?MODULE, { notify, now (), Item }).

%
% gen_server callbacks
%

dump () ->
  gen_server:call (?MODULE, dump).

init ([ ]) ->
  { ok, [ ] }.

handle_call (dump, _From, State) ->
  { reply, State, State };
handle_call (_Request, _From, State) ->
  { noreply, State }.

handle_cast ({ notify, Time, Item }, State) ->
  { noreply, [ { Time, Item } | State ] };
handle_cast (_Request, State) ->
  { noreply, State }.

handle_info (_Msg, State) ->
  { noreply, State }.

terminate (_Reason, _State) ->
  ok.

code_change (_OldVsn, State, _Extra) ->
  { ok, State }.
