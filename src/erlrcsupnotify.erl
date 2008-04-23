%% @doc Additional functions a supervisor can (optionally) implement in order
%% to be notified of upgrades and downgrades by erlrcdynamic.
%% @end

-module (erlrcsupnotify).
%-behaviour (behaviour).
-export ([ behaviour_info/1 ]).
%-behaviour (erlrcsupnotify).
-export ([ sup_downgrade_notify/2, 
           sup_upgrade_notify/2 ]).

%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

%% @hidden

behaviour_info (callbacks) ->
  [ { sup_downgrade_notify, 2 },
     { sup_upgrade_notify, 2 } ].

%% @spec sup_downgrade_notify (string (), string ()) -> void
%% @doc Notification of downgrade (before downgrade has occurred).  Use this
%% to ensure new childspecs are stopped, etc.
%% Return value is ignored.

sup_downgrade_notify (_OldVsn, _NewVsn) ->
  erlang:throw (not_implemented).

%% @spec sup_upgrade_notify (string (), string ()) -> void
%% @doc Notification of upgrade (after upgrade has occurred).  Use this
%% to ensure new childspecs are started, etc.
%% Return value is ignored.
%% @end

sup_upgrade_notify (_OldVsn, _NewVsn) ->
  erlang:throw (not_implemented).
