-module (erlrc_boot).

-export ([ boot/0,
	   make_release_resource/3,
	   write_release_resource/4,
	   write_boot_script/4 ]).

%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

%% @spec boot () -> ok | { error, Reason }
%% @doc Load and start the applications specified in the erlrc root
%% directory.
%% @end

boot () ->
  try
    case application:load (erlrc) of
      ok ->
	ok;
      { error, { already_loaded, erlrc } } ->
	ok;
      { error, LoadReason } ->
	throw ({ application_load_failed, erlrc, LoadReason })
    end,
    case application:start (erlrc) of
      ok ->
	ok;
      { error, { already_started, erlrc } } ->
	ok;
      { error, StartReason } ->
	throw ({ application_start_failed, erlrc, StartReason })
    end,
    AppsDir = erlrc_lib:get_apps_dir (),

    % Get application specifications for any applications already loaded.
    LoadedAppToSpec = lists:foldl (
      fun ({ App, _Description, _Version }, Map) ->
	{ ok, Keys } = application:get_all_key (App),
	gb_trees:insert (App, { application, App, Keys }, Map)
      end,
      gb_trees:empty (),
      application:loaded_applications ()),

    % Check which applications have already been started.
    AppsStarted = lists:foldl (fun ({ App, _Description, _Version }, Set) ->
				 gb_sets:add (App, Set)
			       end,
			       gb_sets:empty (),
			       application:which_applications ()),

    { AppsToStart, AppToSpec } = scan_apps (AppsDir, LoadedAppToSpec),
    Specs = [ Spec || { _App, Spec } <- gb_trees:to_list (AppToSpec) ],

    IncludedBy = compute_included_by (Specs),

    start_apps (AppsToStart, AppsStarted, AppToSpec, IncludedBy)
  catch
    throw:Error ->
      error_logger:error_report ([ erlrc_boot_failed, { error, Error } ]),
      { error, Error }
  end.


%% @spec make_release_resource (ReleaseName::string(),
%%                              ReleaseVersion::string(),
%%                              [ ExtraApp::atom () ]) -> ok | Error
%% @doc Return a release resource structure (see OTP Design Principles
%% sec. 1.10.2) for a release consisting of the currently loaded
%% versions of erlrc and its dependencies (kernel, stdlib, sasl), and
%% a list of extra applications ExtraApps.
%% @end

make_release_resource (ReleaseName, ReleaseVersion, ExtraApps) ->
  % TODO: we don't look at the override resource file for anything
  % in the boot file.  Should we??
  Apps = [ sasl, erlrc | ExtraApps ],
  lists:foreach (
    fun (App) ->
      case application:load (App) of
	ok ->
	  ok;
	{ error, { already_loaded, _ } } ->
	  ok;
	{ error, Reason } ->
	  erlang:error ({ application_load_failed, App, Reason })
      end
    end,
    Apps),
  Loaded = application:loaded_applications (),
  { release,
    { ReleaseName, ReleaseVersion },
    { erts, erlang:system_info (version) },
    lists:map (fun (App) ->
		 { value, { App, _, Version } } =
		   lists:keysearch (App, 1, Loaded),
		 { App, Version }
	       end,
	       [ kernel, stdlib | Apps ])
  }.


%% @spec write_release_resource (Name::string(),
%%                               ReleaseName::string(),
%%                               ReleaseVersion::string(),
%%                               [ ExtraApp::atom () ]) -> ok | Error
%% @doc Write a release resource file (see OTP Design Principles
%% sec. 1.10.2) named Name.rel, for a release consisting of the currently
%% loaded versions of erlrc and its dependencies (kernel, stdlib, sasl),
%% and a list of extra applications ExtraApps.
%% @end

write_release_resource (Name, ReleaseName, ReleaseVersion, ExtraApps) ->
  Resource = make_release_resource (ReleaseName, ReleaseVersion, ExtraApps),
  Binary = list_to_binary (io_lib:format ("~p.~n", [ Resource ])),
  file:write_file (Name ++ ".rel", Binary).


%% @spec write_boot_script (Name::string(),
%%                          ReleaseName::string(),
%%                          ReleaseVersion::string(),
%%                          [ ExtraApp::atom () ]) -> ok | Error
%% @doc Write a release resource file (see OTP Design Principles
%% sec. 1.10.2) named Name.rel, for a release consisting of the currently
%% loaded versions of erlrc and its dependencies (kernel, stdlib, sasl),
%% and a list of extra applications ExtraApps; call systools:make_script/1
%% to create Name.script and Name.boot.
%% @end

write_boot_script (Name, ReleaseName, ReleaseVersion, ExtraApps) ->
  ok = write_release_resource (Name, ReleaseName, ReleaseVersion, ExtraApps),
  ok = systools:make_script (Name),
  % Here be dragons...
  ScriptFile = Name ++ ".script",
  { ok, [ { script, Something, List } ] } = file:consult (ScriptFile),
  Postamble = [ { apply, { c, erlangrc, [] } }, { progress, started } ],
  { Preamble, Postamble } = lists:split (length (List) - 2, List),
  NewList = Preamble ++ [ { apply, { erlrc_boot, boot, [] } } | Postamble ],
  NewScript = { script, Something, NewList },
  { ok, Fd } = file:open (ScriptFile, [write]),
  ok = io:format (Fd, "%% script generated at ~w ~w\n~p.\n",
		  [date (), time (), NewScript]),
  ok = file:close (Fd),
  file:write_file (Name ++ ".boot", term_to_binary (NewScript)).

%-=====================================================================-
%-                               Private                               -
%-=====================================================================-

%% @private
scan_apps (Dir, AppToSpec) ->
  case file:list_dir (Dir) of
    { ok, Filenames } ->
      Apps = lists:foldl (
	fun (Filename, Found) ->
	  case regexp:match (Filename, "^[a-z][0-9A-Za-z_]*$") of
	    { match, 1, _ } ->
	      [ list_to_atom (Filename) | Found ];
	    _ ->
	      Found
	  end
	end,
	[ ],
	Filenames),
      load_resource_files (Dir, lists:sort (Apps), [ ], AppToSpec);
    { error, Reason } ->
      throw ({ app_dir_unreadable, Dir, Reason })
  end.


%% @private
load_resource_files (_Dir, [ ], ToStart, AppToSpec) ->
  { ToStart, AppToSpec };

load_resource_files (Dir, [ App | Remaining ], ToStart, AppToSpec) ->
  case gb_trees:lookup (App, AppToSpec) of
    { value, _ } ->
      % Application was already loaded.
      load_resource_files (Dir, Remaining, ToStart, AppToSpec);
    none ->
      Spec = erlrc_lib:load_resource_file (Dir, App),
      { application, App, Keys } = Spec,
      % Must scan all dependencies, whether or not they were specified
      % in the applications directory.
      NewRemaining =
	case lists:keysearch (applications, 1, Keys) of
	  false ->
	    Remaining;
	  { value, { applications, Deps } } ->
	    Deps ++ Remaining
	end,
      NewAppToSpec = gb_trees:insert (App, Spec, AppToSpec),
      load_resource_files (Dir, NewRemaining, [ App | ToStart ], NewAppToSpec)
  end.


%% @private
compute_included_by (Specs) ->
  lists:foldl (
    fun ({ application, App, Keys }, Map) ->
      case lists:keysearch (included_applications, 1, Keys) of
	false ->
	  Map;
	{ value, { included_applications, IncludedApps } } ->
	  lists:foldl (
	    fun (IncludedApp, M) ->
	      case gb_trees:lookup (IncludedApp, Map) of
		none ->
		  gb_trees:insert (IncludedApp, App, M);
		{ value, OtherApp } ->
		  throw ({ duplicate_included, IncludedApp, OtherApp, App })
	      end
	    end,
	    Map,
	    IncludedApps)
      end
    end,
    gb_trees:empty (),
    Specs).


%% @private
start_apps (AppsToStart, AppsStarted, AppToSpec, IncludedBy) ->
  { Actions, _ } =
    lists:foldl (
      fun (App, { CurActions, CurStarted }) ->
	{ value, Spec } = gb_trees:lookup (App, AppToSpec),
	start_app (Spec,
		   AppToSpec,
		   IncludedBy,
		   gb_sets:empty (),
		   CurActions,
		   CurStarted)
      end,
      { [ ], AppsStarted },
      AppsToStart),
  lists:foreach (
    fun (Action) ->
      case Action of
	{ load, Spec } ->
	  case application:load (Spec) of
	    ok ->
	      ok;
	    { error, { already_loaded, _ } } ->
	      ok;
	    { error, LoadReason } ->
	      throw ({ load_failed, Spec, LoadReason })
	  end;
	{ start, App } ->
	  case application:start (App) of
	    ok ->
	      ok;
	    { error, StartReason } ->
	      throw ({ start_failed, App, StartReason })
	  end
      end
    end,
    lists:reverse (Actions)).


%% @private
start_app (Spec = { application, App, Keys },
	   AppToSpec,
	   IncludedBy,
	   Starting,
	   Actions,
	   Started) ->
  case gb_sets:is_member (App, Starting) of
    true ->
      throw ({ circular_dependency, App, gb_sets:to_list (Starting) });
    false ->
      case gb_sets:is_member (App, Started) of
	true ->
	  { Actions, Started };
	false ->
	  NewStarting = gb_sets:add (App, Starting),

	  % Ensure that all dependencies have been started,
	  % including the dependencies of any included applications.

	  Apps = case lists:keysearch (included_applications, 1, Keys) of
		   { value, { included_applications, As } } -> [ App | As ];
		   false -> [ App ]
		 end,
	  Deps = lists:foldl (fun (A, Acc) ->
				{ value, { application, A, K } } =
				  gb_trees:lookup (A, AppToSpec),
				case lists:keysearch (applications, 1, K) of
				  { value, { applications, D } } -> D ++ Acc;
				  false -> Acc
				end
			      end,
			      [],
			      Apps),
	  { DepActions, DepStarted } =
	    lists:foldl (
	      fun (Dep, { CurActions, CurStarted }) ->
		% Can't depend on an application included by
		% another, because the application controller
		% won't consider it started.
		case gb_trees:lookup (Dep, IncludedBy) of
		  none ->
		    ok;
		  { value, OtherApp } ->
		    throw ({ dependency_included, App, Dep, OtherApp })
		end,
		{ value, DepSpec } = gb_trees:lookup (Dep, AppToSpec),
		start_app (DepSpec,
			   AppToSpec,
			   IncludedBy,
			   NewStarting,
			   CurActions,
			   CurStarted)
	      end,
	      { Actions, Started },
	      Deps),

	  % Ensure the application is loaded.  (We have to do this
	  % explicitly in case Spec came from an override .app file.)
	  LoadActions = [ { load, Spec } | DepActions ],

	  % Ensure the application is started: if the application is
	  % included by another application, start the containing
	  % application; otherwise, just start it.
	  case gb_trees:lookup (App, IncludedBy) of
	    { value, ContainingApp } ->
	      { value, ContainingSpec } =
		gb_trees:lookup (ContainingApp, AppToSpec),
	      { ContainingActions, ContainingStarted } =
		start_app (ContainingSpec,
			   AppToSpec,
			   IncludedBy,
			   NewStarting,
			   LoadActions,
			   DepStarted),
	      { ContainingActions, gb_sets:add (App, ContainingStarted) };
	    none ->
	      { [ { start, App } | LoadActions ],
		gb_sets:add (App, DepStarted) }
	  end
      end
  end.
