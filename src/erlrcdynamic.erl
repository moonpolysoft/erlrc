%% @doc Dynamic hooks intended to be called from packaging systems.
%% Includes routines for starting, stopping, upgrading, and downgrading
%% applications.  Automatically generates .appup files (TODO: only if 
%% they don't exist) as appropriate.

-module (erlrcdynamic).
-export ([ downgrade/3,
           downgrade/5,
           local_error_msg/2,
           start/2,
           start/3,
           stop/2,
           unload/2,
           unload/3,
           upgrade/3,
           upgrade/5 ]).

-include_lib ("eunit/include/eunit.hrl").

%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

%% @spec downgrade (atom (), string (), string ()) -> { ok, Reason::atom () } | { ok, [ Unpurged ] } | restart_new_emulator | { error, Reason }
%% @equiv downgrade (Application, OldVersion, NewVersion, OldDir, NewDir)
%% @doc OldDir and NewDir are assumed to be in the standard 
%% location under code:lib_dir ().
%% @end

downgrade (Application, OldVersion, NewVersion) ->
  Dir = code:lib_dir (),
  OldDir = Dir ++ "/" ++ atom_to_list (Application) ++ "-" ++ OldVersion,
  NewDir = Dir ++ "/" ++ atom_to_list (Application) ++ "-" ++ NewVersion,
  downgrade (Application, OldVersion, NewVersion, OldDir, NewDir).

%% @spec downgrade (atom (), string (), string (), string (), string ()) -> { ok, Reason::atom () } | { ok, [ Unpurged ] } | restart_new_emulator | { error, Reason }
%% @doc Downgrade an application from new version to old version.  Generates an
%% .appup file and then calls release_handler:downgrade_app/2.
%% @end

downgrade (Application, OldVersion, NewVersion, OldDir, NewDir) ->
  case lists:keysearch (Application, 1, application:which_applications ()) of
    { value, { Application, _, OldVersion } } ->
      { ok, already_running };
    { value, { Application, _, NewVersion } } ->
      do_downgrade (Application, OldVersion, NewVersion, OldDir, NewDir);
    false ->
      case file:read_file_info (NewDir) of
        { ok, _ } ->
          case do_downgrade (Application,
                             OldVersion,
                             NewVersion,
                             OldDir,
                             NewDir) of
            R = { ok, _ } ->
             case start (Application, OldVersion, OldDir) of
               included -> R;
               already_running -> R;
               started -> R;
               started_included_stopped -> R;
               version_mismatch -> { error, version_mismatch };
               version_load_mismatch -> { error, version_load_mismatch }
             end;
            X ->
              X
          end;
        { error, enoent } ->
           % ok we'll interpret this to mean the new version was never
           % installed.  TODO: is there something better?
           case start (Application, OldVersion, OldDir) of
             included -> { ok, included };
             already_running -> { ok, already_running };
             started -> { ok, started };
             started_included_stopped -> { ok, started_included_stopped };
             version_mismatch -> { error, version_mismatch };
             version_load_mismatch -> { error, version_load_mismatch }
           end
      end;
    { value, { Application, _, OtherVersion } } ->
      local_error_msg ("erlrcdynamic:upgrade/5: got OtherVersion '~p' " ++
                       "which is neither NewVersion '~p' or OldVersion '~p'~n",
                       [ OtherVersion,
                         NewVersion,
                         OldVersion ]),
      { error, version_mismatch }
  end.

%% @hidden

local_error_msg (Format, Args) ->
  Leader = erlang:group_leader (),
  try
    true = erlang:group_leader (self (), self ()),
    error_logger:error_msg (Format, Args)
  after
    erlang:group_leader (Leader, self ())
  end.

%% @spec start (atom (), string ()) -> already_running | started | version_mismatch | version_load_mismatch | bad_directory
%% @equiv start (Application, Version, Dir)
%% @doc Assumes Dir is the standard location under code:lib_dir ().
%% @end

start (Application, Version) ->
  Dir = code:lib_dir (),
  AppDir = Dir ++ "/" ++ atom_to_list (Application) ++ "-" ++ Version,
  start (Application, Version, AppDir).

%% @spec start (atom (), string (), string ()) -> already_running | started | started_included_stopped | included | version_mismatch | version_load_mismatch | bad_directory
%% @doc Start the specified application version located in Dir.  
%% Returns 'already_running'
%% if that version of the application has been previously started.
%% Returns 'started' if the application version is succesfully started.
%% Returns 'started_included_stopped' if the application is successfully
%% started and any included applications were successfully stopped.
%% Returns 'included' if application is included in another application
%% (and does not start it).  
%% Returns 'version_mismatch' if 
%% the application is running with a different version 
%% (use {@link upgrade/5} instead).  Returns 'version_load_mismatch' if the 
%% application was attempted to be loaded, but a different version was found.
%% Returns 'bad_directory' if AppDir/ebin does not exist.
%% @end

start (Application, Version, AppDir) ->
  case lists:keysearch (Application, 1, application:which_applications ()) of
    { value, { Application, _, Version } } -> 
      already_running;
    { value, { Application, _, OtherVersion }} ->
      local_error_msg ("erlrcdynamic:start/3: got OtherVersion '~p' " ++
                       "which is not Version '~p'~n",
                       [ OtherVersion,
                         Version ]),
      version_mismatch;
    false ->
      EbinDir = AppDir ++ "/ebin",
      case code:add_patha (EbinDir) of
        true ->
          case lists:keysearch (Application,
                                1,
                                application:loaded_applications ()) of
            { value, { Application, _, Version } } -> 
              ok;
            { value, { Application, _, _ } } ->
              ok = application:unload (Application),
              ok = erlrc_lib:load_application (Application);
            false -> 
              ok = erlrc_lib:load_application (Application)
          end,

          case lists:keysearch (Application,
                                1,
                                application:loaded_applications ()) of
            { value, { Application, _, Version } } -> 
              case is_included (Application) of
                true ->
                  included;
                false ->
                  % ???: we were getting undef function errors during 
                  % package installs, not sure why, thought this might help
                  case application:get_key (Application, modules) of
                    { ok, Mods } ->
                      lists:foreach (fun (M) -> 
                                       code:ensure_loaded (M) 
                                     end, 
                                     Mods);
                    undefined ->
                      ok
                  end,

                  case application:get_key (Application, 
                                            included_applications) of
                    { ok, [] } ->
                      ok = application:start (Application),
                      started;
                    undefined ->
                      ok = application:start (Application),
                      started;
                    { ok, Included } ->
                      lists:foreach 
                        (fun (A) -> 
                           local_info_msg
                             ("Stopping application '~p', " ++
                              "as it is now included in application '~p'~n",
                              [ A, Application ]),
                           ok = maybe_stop (A) 
                         end,
                         Included),
                      ok = application:start (Application),
                      started_included_stopped
                  end
              end;
            { value, { Application, _, _ } } ->
              version_load_mismatch
          end;
        _ ->
          bad_directory
      end
  end.

%% @spec stop (atom (), string ()) -> stopped | stopped_included_started | version_mismatch | not_running
%% @doc Stop the specified application version.  Does not unload modules
%% or adjust the code path.  
%% Returns 'stopped' if the application version
%% was previously running.  
%% Returns 'version_mismatch' if a different
%% version of the application is running (use downgrade instead).
%% Returns 'not_running' if the application version was not previously
%% running.
%% Returns 'stopped_included_started' if the application
%% was stopped and any included applications listed in /applications 
%% were started successfully.
%% @end

stop (Application, Version) ->
  case lists:keysearch (Application, 1, application:which_applications ()) of
    { value, { Application, _, Version } } -> 
      ok = application:stop (Application),
      case application:get_key (Application, included_applications) of
        { ok, [] } ->
          stopped;
        undefined -> 
          stopped;
        { ok, Included } ->
          { ok, Root } = application:get_env (erlrc, root_dir),
          lists:foreach (fun (A) -> 
                           case should_run (Root, A) of
                             true -> 
                               local_info_msg
                                 ("Starting application '~p', " ++
                                  "as including application '~p' " ++
                                  "has been stopped~n",
                                  [ A, Application ]),
                               ok = application:start (A);
                             false ->
                               ok
                           end
                         end,
                         Included),
          stopped_included_started
      end;
    { value, { Application, _, OtherVersion }} ->
      local_error_msg ("erlrcdynamic:stop/2: got OtherVersion '~p' " ++
                       "which is not Version '~p'~n",
                       [ OtherVersion,
                         Version ]),
      version_mismatch;
    false ->
      not_running
  end.

%% @spec (atom (), string ()) -> unloaded | version_load_mismatch | bad_directory
%% @equiv unload (Application, Version, Dir)
%% @doc Assumes Dir is the standard location under code:lib_dir ().
%% @end

unload (Application, Version) ->
  Dir = code:lib_dir (),
  AppDir = Dir ++ "/" ++ atom_to_list (Application) ++ "-" ++ Version,
  unload (Application, Version, AppDir).

%% @spec unload (atom (), string (), string ()) -> unloaded | version_load_mismatch | bad_directory
%% @doc Unload the modules corresponding to application version (as
%% indicated by the app spec file) and remove
%% the application directory from the code path.  Returns 'unloaded' if
%% the modules were unloaded.  
%% Returns 'version_load_mismatch' if the application 
%% was attempted to be unloaded, but a different version was found.
%% @end

unload (Application, Version, AppDir) ->
  EbinDir = AppDir ++ "/ebin",
  case code:add_patha (EbinDir) of
    true ->
      case lists:keysearch (Application,
                            1,
                            application:loaded_applications ()) of
        false ->
          ok = erlrc_lib:load_application (Application);
        _ ->
          ok
      end,
    
      case lists:keysearch (Application,
                            1,
                            application:loaded_applications ()) of
        { value, { Application, _, Version } } ->
          case application:get_key (Application, modules) of
            undefined ->
              code:del_path (EbinDir),
              unloaded;
            { ok, Mods } ->
              lists:foreach (fun (M) -> 
                               code:purge (M), code:delete (M) 
                             end, 
                             Mods),
              code:del_path (EbinDir),
              unloaded
          end;
        { value, { Application, _, _ } } ->
          version_load_mismatch
      end;
    false ->
      bad_directory
  end.

%% @spec upgrade (atom (), string (), string ()) -> { ok, Reason::atom () } | { ok, [ Unpurged ] } | restart_new_emulator | { error, Reason }
%% @equiv upgrade (Application, OldVersion, NewVersion, OldDir, NewDir)
%% @doc OldDir and NewDir are assumed to be in the standard 
%% location under code:lib_dir ().
%% @end

upgrade (Application, OldVersion, NewVersion) ->
  Dir = code:lib_dir (),
  OldDir = Dir ++ "/" ++ atom_to_list (Application) ++ "-" ++ OldVersion,
  NewDir = Dir ++ "/" ++ atom_to_list (Application) ++ "-" ++ NewVersion,
  upgrade (Application, OldVersion, NewVersion, OldDir, NewDir).

%% @spec upgrade (atom (), string (), string (), string (), string ()) -> { ok, Reason::atom () } | { ok, [ Unpurged ] } | restart_new_emulator | { error, Reason }
%% @doc Upgrade an application from old version to new version.  Generates an
%% .appup file and then calls release_handler:upgrade_app/2.
%% @end

upgrade (Application, OldVersion, NewVersion, OldDir, NewDir) ->
  case lists:keysearch (Application, 1, application:which_applications ()) of
    { value, { Application, _, NewVersion } } ->
      { ok, already_running };
    { value, { Application, _, OldVersion } } ->
      do_upgrade (Application, OldVersion, NewVersion, OldDir, NewDir);
    false ->
      case file:read_file_info (OldDir) of
        { ok, _ } ->
           case do_upgrade (Application,
                            OldVersion,
                            NewVersion,
                            OldDir,
                            NewDir) of
             R = { ok, _ } ->
               case start (Application, NewVersion, NewDir) of
                 included -> R;
                 already_running -> R;
                 started -> R;
                 started_included_stopped -> R;
                 version_mismatch -> { error, version_mismatch };
                 version_load_mismatch -> { error, version_load_mismatch }
               end;
             X ->
               X
           end;
        { error, enoent } ->
           % ok we'll interpret this to mean the old version was never
           % installed.  TODO: is there something better?
           case start (Application, NewVersion, NewDir) of
             included -> { ok, included };
             already_running -> { ok, already_running };
             started -> { ok, started };
             started_included_stopped -> { ok, started_included_stopped };
             version_mismatch -> { error, version_mismatch };
             version_load_mismatch -> { error, version_load_mismatch }
           end
      end;
    { value, { Application, _, OtherVersion } } ->
      local_error_msg ("erlrcdynamic:upgrade/5: got OtherVersion '~p' " ++
                       "which is neither NewVersion '~p' or OldVersion '~p'~n",
                       [ OtherVersion,
                         NewVersion,
                         OldVersion ]),
      { error, version_mismatch }
  end.

%-=====================================================================-
%-                               Private                               -
%-=====================================================================-

beam_exports (Beam, Func, Arity) ->
  case beam_lib:chunks (Beam, [ exports ]) of
    { ok, { _, [ { exports, Exports } ] } } ->
      lists:member ({ Func, Arity }, Exports);
    _ ->
      false
  end.

delta_includes (Application, OldDir, NewDir) ->
  { ok, [ { application, Application, OldProps } ] } =
    file:consult (OldDir ++ "/ebin/" ++ atom_to_list (Application) ++ ".app"),
  { ok, [ { application, Application, NewProps } ] } =
    file:consult (NewDir ++ "/ebin/" ++ atom_to_list (Application) ++ ".app"),

  case { lists:keysearch (included_applications, 1, OldProps),
         lists:keysearch (included_applications, 1, NewProps) } of
    { { value, { included_applications, OldInc } }, 
      { value, { included_applications, NewInc } } } ->
      { NewInc -- OldInc, OldInc -- NewInc };
    { false, { value, { included_applications, NewInc } } } ->
      { NewInc, [] };
    { { value, { included_applications, OldInc } }, false }  ->
      { [], OldInc };
    { false, false } ->
      { [], [] }
  end.

do_downgrade (Application, OldVersion, NewVersion, OldDir, NewDir) ->
  case maybe_make_appup (Application, OldVersion, NewVersion, OldDir, NewDir) of
    { ok, AppUp } ->
      % work around http://www.erlang.org/pipermail/erlang-bugs/2008-February/000656.html

      case application:get_key (Application, modules) of
        { ok, Mods } ->
          lists:foreach (fun (M) -> code:ensure_loaded (M) end, Mods);
        undefined ->
          ok
      end,

      { Added, Removed } = delta_includes (Application, OldDir, NewDir),

      lists:foreach (fun (A) -> 
                       local_info_msg
                         ("Stopping application '~p', " ++
                          "as it is now included in application '~p'~n",
                          [ A, Application ]),
                       ok = maybe_stop (A)
                     end,
                     Removed),

      case downgrade_app (Application, AppUp, OldVersion, 
                          OldDir, NewVersion, NewDir) of
        R = { ok, _ } ->
          { ok, Root } = application:get_env (erlrc, root_dir),

          lists:foreach (fun (A) -> 
                           case should_run (Root, A) of
                             true -> 
                               local_info_msg
                                 ("Starting application '~p', " ++
                                  "as it is no longer included " ++
                                  "in application '~p' ",
                                  [ A, Application ]),
                               ok = application:start (A);
                             false ->
                               ok
                           end
                         end,
                         Added),
          R;
        X ->
          X
      end;
    R = { error, _ } ->
      R
  end.

do_upgrade (Application, OldVersion, NewVersion, OldDir, NewDir) ->
  case maybe_make_appup (Application, OldVersion, NewVersion, OldDir, NewDir) of
    { ok, AppUp } ->
      % work around http://www.erlang.org/pipermail/erlang-bugs/2008-February/000656.html

      case application:get_key (Application, modules) of
        { ok, Mods } ->
          lists:foreach (fun (M) -> code:ensure_loaded (M) end, Mods);
        undefined ->
          ok
      end,

      { Added, Removed } = delta_includes (Application, OldDir, NewDir),

      lists:foreach (fun (A) -> 
                       local_info_msg
                         ("Stopping application '~p', " ++
                          "as it is now included in application '~p'~n",
                          [ A, Application ]),
                       ok = maybe_stop (A)
                     end,
                     Added),

      case upgrade_app (Application, AppUp, OldVersion,
                        OldDir, NewVersion, NewDir) of
        R = { ok, _ } ->
          { ok, Root } = application:get_env (erlrc, root_dir),

          lists:foreach (fun (A) -> 
                           case should_run (Root, A) of
                             true -> 
                               local_info_msg
                                 ("Starting application '~p', " ++
                                  "as it is no longer included " ++
                                  "in application '~p' ",
                                  [ A, Application ]),
                               ok = application:start (A);
                             false ->
                               ok
                           end
                         end,
                         Removed),
          R;
        X ->
          X
      end;
    R = { error, _ } ->
      R
  end.

downgrade_directives (OldVersion, NewVersion, M, Beam) ->
  case is_supervisor (Beam) of
    true ->
      downgrade_directives_supervisor (OldVersion, NewVersion, M, Beam);
    false ->
      case has_code_change (Beam) of
        true ->
          [ { update, M, { advanced, [] } } ];
        false ->
          [ { load_module, M } ]
      end
  end.

downgrade_directives_supervisor (OldVersion, NewVersion, M, Beam) ->
  case beam_exports (Beam, sup_downgrade_notify, 2) of
    true ->
      [ { apply, { M, sup_downgrade_notify, [ OldVersion, NewVersion ] } },
        { update, M, supervisor } ];
    false ->
      [ { update, M, supervisor } ]
  end.

has_code_change (Beam) ->
  beam_exports (Beam, code_change, 3).

has_element (Attr, Key, Elem) ->
  case lists:keysearch (Key, 1, Attr) of 
    { value, { Key, Value } } ->
      lists:member (Elem, Value);
    _ ->
      false
  end.

has_version_change (Beam) ->
  beam_exports (Beam, version_change, 2).

is_included (Application) ->
  is_included (Application, application:which_applications ()).

is_included (_, []) -> 
  false;
is_included (Application, [ { App, _, _ } | T ]) ->
  case application:get_key (App, included_applications) of
    { ok, Included } ->
      lists:member (Application, Included) orelse is_included (Application, T);
    undefined ->
      is_included (Application, T)
  end.

is_supervisor (Beam) ->
  case beam_lib:chunks (Beam, [ attributes ]) of
    { ok, { _, [ { attributes, Attr } ] } } ->
      has_element (Attr, behaviour, supervisor) orelse
      has_element (Attr, behavior, supervisor);
    _ ->
      false
  end.

local_info_msg (Format, Args) ->
  Leader = erlang:group_leader (),
  try
    true = erlang:group_leader (self (), self ()),
    error_logger:info_msg (Format, Args)
  after
    erlang:group_leader (Leader, self ())
  end.

make_appup (Application, OldVersion, NewVersion, OldDir, NewDir) ->
  case file:consult (OldDir ++ "/ebin/" ++ 
                     atom_to_list (Application) ++ ".app") of
    { ok, [ { application, Application, OldProps } ] } ->
      case vsn (OldProps) =:= OldVersion of
        true ->
          case file:consult (NewDir ++ "/ebin/" ++ 
                             atom_to_list (Application) ++ ".app") of
            { ok, [ { application, Application, NewProps } ] } ->
              case vsn (NewProps) =:= NewVersion of
                true ->
                  AddMods = modules (NewProps) -- modules (OldProps),
                  DelMods = modules (OldProps) -- modules (NewProps),

                  { UpVersionChange, DownVersionChange } = 
                    case start_module (NewProps) of
                      { ok, StartMod, StartArgs } ->
                        { 
                          [ D
                            || BeamFile <- [ NewDir ++ "/ebin/" ++ 
                                             atom_to_list (StartMod) ++ 
                                             ".beam" ],
                               { ok, Beam } <- [ file:read_file (BeamFile) ],
                               D <- version_change (Beam, 
                                                    OldVersion,
                                                    StartMod,
                                                    StartArgs) ],
                          [ D
                            || BeamFile <- [ NewDir ++ "/ebin/" ++ 
                                             atom_to_list (StartMod) ++ 
                                             ".beam" ],
                               { ok, Beam } <- [ file:read_file (BeamFile) ],
                               D <- version_change (Beam, 
                                                    { down, OldVersion },
                                                    StartMod,
                                                    StartArgs) ]
                        };
                      undefined ->
                        { [], [] }
                    end,

                  UpDirectives = 
                    [ D
                      || M <- modules (NewProps) -- AddMods,
                         BeamFile <- [ NewDir ++ "/ebin/" ++ 
                                       atom_to_list (M) ++ ".beam" ],
                         { ok, Beam } <- [ file:read_file (BeamFile) ],
                         D <- upgrade_directives (OldVersion, 
                                                  NewVersion,
                                                  M,
                                                  Beam) ],

                  DownDirectives = 
                    [ D
                      || M <- lists:reverse (modules (NewProps) -- AddMods),
                         BeamFile <- [ NewDir ++ "/ebin/" ++ 
                                       atom_to_list (M) ++ ".beam" ],
                         { ok, Beam } <- [ file:read_file (BeamFile) ],
                         D <- downgrade_directives (OldVersion, 
                                                    NewVersion,
                                                    M,
                                                    Beam) ],

                  AppUp = 
                    { NewVersion, 
                      [ { OldVersion, 
                          [ { add_module, M } || M <- AddMods ] ++ 
                          UpDirectives ++
                          UpVersionChange ++
                          [ { delete_module, M } || M <- DelMods ] } ],
                      [ { OldVersion, 
                          [ { add_module, M } || 
                            M <- lists:reverse (DelMods) ] ++
                          DownVersionChange ++ 
                          DownDirectives ++
                          [ { delete_module, M } || 
                            M <- lists:reverse (AddMods) ] } ]
                    },

                  local_info_msg ("make_appup/5: generated AppUp " ++
                                  "for ~p ~p -> ~p~n~p~n",
                                  [ Application,
                                    OldVersion,
                                    NewVersion,
                                    AppUp ]),

                  { ok, AppUp };
                false ->
                  { error, bad_new_appvsn }
              end;
            _ ->
            { error, bad_new_appfile }
          end;
        false ->
          { error, bad_old_appvsn }
      end;
    _ ->
      { error, bad_old_appfile }
  end.

maybe_make_appup (Application, OldVersion, NewVersion, OldDir, NewDir) ->
  case file:consult (NewDir ++ "/ebin/" ++ 
                     atom_to_list (Application) ++ ".appup") of
    { ok, [ AppUp ] } ->
      { ok, AppUp };
    { error, enoent } ->
      make_appup (Application, OldVersion, NewVersion, OldDir, NewDir)
  end.

maybe_stop (Application) ->
  case application:stop (Application) of
    { error, { not_started, Application } } -> ok;
    R -> R
  end.

modules (Props) ->
  { value, { modules, Modules } } = lists:keysearch (modules, 1, Props),
  Modules.

should_run (Root, A) ->
  case file:read_file_info (Root ++ "/applications/" ++ atom_to_list (A)) of
    { ok, _ } ->
      true;
    _ ->
      false
  end.

start_module (Props) ->
  case lists:keysearch (mod, 1, Props) of
    { value, { mod, { StartMod, StartArgs } } } ->
      { ok, StartMod, StartArgs };
    false ->
      undefined
  end.

upgrade_directives (OldVersion, NewVersion, M, Beam) ->
  case is_supervisor (Beam) of
    true ->
      upgrade_directives_supervisor (OldVersion, NewVersion, M, Beam);
    false ->
      case has_code_change (Beam) of
        true ->
          [ { update, M, { advanced, [] } } ];
        false ->
          [ { load_module, M } ]
      end
  end.

upgrade_directives_supervisor (OldVersion, NewVersion, M, Beam) ->
  case beam_exports (Beam, sup_upgrade_notify, 2) of
    true ->
      [ { update, M, supervisor },
        { apply, { M, sup_upgrade_notify, [ OldVersion, NewVersion ] } } ];
    false ->
      [ { update, M, supervisor } ]
  end.

version_change (Beam, From, StartMod, StartArgs) ->
  case has_version_change (Beam) of
    true ->
      [ { apply, { StartMod, version_change, [ From, StartArgs ] } } ];
    false ->
      []
  end.

vsn (Props) ->
  { value, { vsn, Vsn } } = lists:keysearch (vsn, 1, Props),
  Vsn.

%-=====================================================================-
%-                    Extracted from release_handler                   -
%-                                                                     -
%- Modified to take an appup spec directly, to avoid filesystem        -
%- permissions problems.                                               -
%-=====================================================================-

downgrade_app(App, AppUp, OldVsn, OldDir, NewVsn, NewDir) ->
    try downgrade_script(App, AppUp, OldVsn, OldDir, NewVsn, NewDir) of
	{ok, Script} ->
          release_handler:eval_appup_script(App, OldVsn, OldDir, Script)
    catch
	throw:Reason ->
	    {error, Reason}
    end.

downgrade_script(App, AppUp, OldVsn, OldDir, NewVsn, NewDir) ->
    {NewVsn, Script} = find_script(AppUp, OldVsn, down),
    OldAppl = read_app(App, OldVsn, OldDir),
    NewAppl = read_app(App, NewVsn, NewDir),
    case systools_rc:translate_scripts(dn,
				       [Script],[OldAppl],[NewAppl]) of
	{ok, LowLevelScript} ->
	    {ok, LowLevelScript};
	{error, _SystoolsRC, Reason} ->
	    throw(Reason)
    end.

upgrade_app(App, AppUp, OldVsn, OldDir, NewVsn, NewDir) ->
    try upgrade_script(App, AppUp, OldVsn, OldDir, NewVsn, NewDir) of
	{ok, NewVsn, Script} ->
          release_handler:eval_appup_script(App, NewVsn, NewDir, Script)
    catch
	throw:Reason ->
	    {error, Reason}
    end.

upgrade_script(App, AppUp, OldVsn, OldDir, NewVsn, NewDir) ->
    {NewVsn, Script} = find_script(AppUp, OldVsn, up),
    OldAppl = read_app(App, OldVsn, OldDir),
    NewAppl = read_app(App, NewVsn, NewDir),
    case systools_rc:translate_scripts(up,
				       [Script],[NewAppl],[OldAppl]) of
	{ok, LowLevelScript} ->
	    {ok, NewVsn, LowLevelScript};
	{error, _SystoolsRC, Reason} ->
	    throw(Reason)
    end.

find_script (AppUp, OldVsn, UpOrDown) ->
  case AppUp of
    {NewVsn, UpFromScripts, DownToScripts} ->
          Scripts = case UpOrDown of
                        up -> UpFromScripts;
                        down -> DownToScripts
                    end,
          case lists:keysearch(OldVsn, 1, Scripts) of
              {value, {_OldVsn, Script}} ->
                  {NewVsn, Script};
              false ->
                  throw({version_not_in_appup, OldVsn})
          end
  end.

read_app(App, Vsn, Dir) ->
    AppS = atom_to_list(App),
    Path = [filename:join(Dir, "ebin")],
    case systools_make:read_application(AppS, Vsn, Path, []) of
        {ok, Appl} ->
            Appl;
        {error, {not_found, _AppFile}} ->
            throw({no_app_found, Vsn, Dir});
        {error, Reason} ->
            throw(Reason)
    end.

-ifdef (EUNIT).

app_setup () ->
  OsPid = os:getpid (),
  Dir = "erlrcmakeappuptest" ++ OsPid,
  os:cmd ("rm -rf " ++ Dir),
  ok = file:make_dir (Dir),
  ok = file:make_dir (Dir ++ "/sup-old"),
  ok = file:make_dir (Dir ++ "/sup-old/ebin"),
  ok = file:make_dir (Dir ++ "/sup-new"),
  ok = file:make_dir (Dir ++ "/sup-new/ebin"),
  ok = file:make_dir (Dir ++ "/sup-newnew"),
  ok = file:make_dir (Dir ++ "/sup-newnew/ebin"),
  ok = file:make_dir (Dir ++ "/sup-inc"),
  ok = file:make_dir (Dir ++ "/sup-inc/ebin"),
  ok = file:make_dir (Dir ++ "/sup-incnew"),
  ok = file:make_dir (Dir ++ "/sup-incnew/ebin"),

  OldIncAppFile = { application, supinc, [ 
               { vsn, "0.0.0" },
               { description, "yo" },
               { registered, [ ] },
               { applications, [ kernel, stdlib ] },
               { modules, [ erlrctestmakeappupinc ] },
               { included_applications, [ sup ] },
               { mod, { erlrctestmakeappupinc, [] } }
             ] 
           },

  ok = 
    file:write_file 
      (Dir ++ "/sup-inc/ebin/supinc.app",
       erlang:iolist_to_binary (io_lib:format ("~p.", [ OldIncAppFile ]))),

  OldIncApp = <<"
 -module (erlrctestmakeappupinc).
 -behaviour (application).
 -export ([ start/2, stop/1, version_change/2 ]).
 
 start (Type, Args) -> erlrctestmakeappup:start (Type, Args).
 stop (Arg) -> erlrctestmakeappup:stop (Arg).

 version_change (_From, _Extra) -> 
  case whereis (erlrctestmakeappupsrv) of
    Pid when is_pid (Pid) ->
      error_logger:info_msg (\"killing erlrctestmakeappupsrv ~p~n\", [ Pid ]),
      MRef = erlang:monitor (process, Pid),
      exit (Pid, shutdown),
      receive { 'DOWN', MRef, _, _, _ } -> ok end,
      ok;
    _ ->
      error_logger:info_msg (\"no erlrctestmakeappupsrv found~n\", [ ]),
      ok
  end.
">>,

  ok = 
    file:write_file (Dir ++ "/sup-inc/ebin/erlrctestmakeappupinc.erl", OldIncApp),
  os:cmd ("cd " ++ Dir ++ "/sup-inc/ebin && erlc erlrctestmakeappupinc.erl"),

  NewIncAppFile = { application, supinc, [ 
               { vsn, "0.0.1" },
               { description, "yo" },
               { registered, [ ] },
               { applications, [ kernel, stdlib ] },
               { modules, [ erlrctestmakeappupinc ] },
               { included_applications, [ ] },
               { mod, { erlrctestmakeappupinc, [] } }
             ] 
           },

  ok = 
    file:write_file 
      (Dir ++ "/sup-incnew/ebin/supinc.app",
       erlang:iolist_to_binary (io_lib:format ("~p.", [ NewIncAppFile ]))),

  NewIncApp = <<"
 -module (erlrctestmakeappupinc).
 -behaviour (application).
 -export ([ start/2, stop/1, version_change/2 ]).
 
 start (_Type, _Args) -> { ok, spawn_link (fun () -> receive after infinity -> ok end end) }.
 stop (_Arg) -> ok.

 version_change (_From, _Extra) -> 
  case whereis (erlrctestmakeappupsrv) of
    Pid when is_pid (Pid) ->
      error_logger:info_msg (\"killing erlrctestmakeappupsrv ~p~n\", [ Pid ]),
      MRef = erlang:monitor (process, Pid),
      exit (Pid, shutdown),
      receive { 'DOWN', MRef, _, _, _ } -> ok end,
      ok;
    _ ->
      error_logger:info_msg (\"no erlrctestmakeappupsrv found~n\", [ ]),
      ok
  end.
">>,

  ok = 
    file:write_file (Dir ++ "/sup-incnew/ebin/erlrctestmakeappupinc.erl", NewIncApp),
  os:cmd ("cd " ++ Dir ++ "/sup-incnew/ebin && erlc erlrctestmakeappupinc.erl"),
{ ok, _ } = file:read_file_info (Dir ++ "/sup-incnew/ebin/erlrctestmakeappupinc.beam"),

  OldApp = { application, sup, [ 
               { vsn, "0.0.0" },
               { description, "yo" },
               { registered, [ erlrctestmakeappupsup, erlrctestmakeappupsrv ] },
               { applications, [ kernel, stdlib ] },
               { modules, [ erlrctestmakeappupsup, 
                            erlrctestmakeappupsrv,
                            erlrctestmakeappup ] },
               { mod, { erlrctestmakeappup, [] } }
             ] 
           },

  App = <<"
-module (erlrctestmakeappup).
-behaviour (application).
-export ([ start/2, stop/1 ]).

start (_Type, _Args) -> erlrctestmakeappupsup:start_link ().
stop (_) -> ok.
 ">>,

  ok = file:write_file (Dir ++ "/sup-old/ebin/erlrctestmakeappup.erl", App),
  os:cmd ("cd " ++ Dir ++ "/sup-old/ebin && erlc erlrctestmakeappup.erl"),

  AppSup = <<"
 -module (erlrctestmakeappupsup).
 -behavior (supervisor).
 -export ([ start_link/0, init/1 ]).
 init ([]) -> { ok, { { one_for_one, 3, 10 }, [ { erlrctestmakeappupsrv, { erlrctestmakeappupsrv, start_link, [ ] }, temporary, 10000, worker, [ erlrctestmakeappupsrv ] } ] } }.
 start_link () -> supervisor:start_link (?MODULE, []).
 ">>,

  ok = 
    file:write_file (Dir ++ "/sup-old/ebin/erlrctestmakeappupsup.erl", AppSup),
  os:cmd ("cd " ++ Dir ++ "/sup-old/ebin && erlc erlrctestmakeappupsup.erl"),

  AppSrv = <<"
 -module (erlrctestmakeappupsrv).
 -export ([ start_link/0, get/0 ]).
 -export ([ init/1,
            handle_call/3,
            handle_cast/2,
            handle_info/2,
            terminate/2,
            code_change/3 ]).

  % Well, if i used Erlang to parse Erlang, instead of perl, I wouldn't have
  % to play silly games like this.

 -define (START_LINK, gen_server:start_link).
 start_link () -> ?START_LINK ({ local, ?MODULE }, ?MODULE, [], []).
 get () -> gen_server:call (?MODULE, get).
 init ([]) -> { ok, init }.
 handle_call (get, _, State) -> { reply, State, State }.
 handle_cast (_, State) -> { noreply, State }.
 handle_info (_, State) -> { noreply, State }.
 terminate (_, _) -> ok.
 code_change (OldVsn, State, Extra) -> { ok, { code_change, OldVsn } }.">>,

  ok = 
    file:write_file (Dir ++ "/sup-old/ebin/erlrctestmakeappupsrv.erl", AppSrv),
  os:cmd ("cd " ++ Dir ++ "/sup-old/ebin && erlc erlrctestmakeappupsrv.erl"),

  ok = 
    file:write_file (Dir ++ "/sup-old/ebin/sup.app",
                     erlang:iolist_to_binary (io_lib:format ("~p.", [ OldApp ]))),

  NewApp = { application, sup, [ 
               { vsn, "0.0.1" },
               { description, "yo" },
               { registered, [ erlrctestmakeappupsup, erlrctestmakeappupsrv ] },
               { applications, [ kernel, stdlib ] },
               { modules, [ erlrctestmakeappup, erlrctestmakeappupsup, 
                            erlrctestmakeappupsrv, erlrctestmakeappupdild ] },
               { mod, { erlrctestmakeappup, [] } }
             ] 
           },

  AppNew = <<"
-module (erlrctestmakeappup).
-behaviour (application).
-export ([ start/2, stop/1 ]).
-export ([ flass/0 ]).

start (_Type, _Args) -> erlrctestmakeappupsup:start_link ().
stop (_) -> ok.
flass () -> turg.
 ">>,

  ok = file:write_file (Dir ++ "/sup-new/ebin/erlrctestmakeappup.erl", AppNew),
  os:cmd ("cd " ++ Dir ++ "/sup-new/ebin && erlc erlrctestmakeappup.erl"),

  ok = 
    file:write_file (Dir ++ "/sup-new/ebin/erlrctestmakeappupsup.erl", AppSup),

  os:cmd ("cd " ++ Dir ++ "/sup-new/ebin && erlc erlrctestmakeappupsup.erl"),

  ok = 
    file:write_file (Dir ++ "/sup-new/ebin/erlrctestmakeappupsrv.erl", AppSrv),

  os:cmd ("cd " ++ Dir ++ "/sup-new/ebin && erlc erlrctestmakeappupsrv.erl"),

  ok = 
    file:write_file (Dir ++ "/sup-new/ebin/erlrctestmakeappupdild.erl",
                     <<"-module (erlrctestmakeappupdild).">>),

  os:cmd ("cd " ++ Dir ++ "/sup-new/ebin && erlc erlrctestmakeappupdild.erl"),

  ok = 
    file:write_file (Dir ++ "/sup-new/ebin/sup.app",
                     erlang:iolist_to_binary 
                       (io_lib:format ("~p.", [ NewApp ]))),

  NewNewApp = { application, sup, [ 
               { vsn, "0.0.2" },
               { description, "yo" },
               { registered, [ erlrctestmakeappupsup, erlrctestmakeappupsrv ] },
               { applications, [ kernel, stdlib ] },
               { modules, [ erlrctestmakeappup, erlrctestmakeappupsup, 
                            erlrctestmakeappupsrv, erlrctestmakeappupdild ] },
               { mod, { erlrctestmakeappup, [] } }
             ] 
           },

  ok = file:write_file (Dir ++ "/sup-newnew/ebin/erlrctestmakeappup.erl", App),
  os:cmd ("cd " ++ Dir ++ "/sup-newnew/ebin && erlc erlrctestmakeappup.erl"),

  ok = 
    file:write_file (Dir ++ "/sup-newnew/ebin/erlrctestmakeappupsup.erl", AppSup),

  os:cmd ("cd " ++ Dir ++ "/sup-newnew/ebin && erlc erlrctestmakeappupsup.erl"),

  ok = 
    file:write_file (Dir ++ "/sup-newnew/ebin/erlrctestmakeappupsrv.erl", AppSrv),

  os:cmd ("cd " ++ Dir ++ "/sup-newnew/ebin && erlc erlrctestmakeappupsrv.erl"),

  ok = 
    file:write_file (Dir ++ "/sup-newnew/ebin/erlrctestmakeappupdild.erl",
                     <<"-module (erlrctestmakeappupdild).">>),

  os:cmd ("cd " ++ Dir ++ "/sup-newnew/ebin && erlc erlrctestmakeappupdild.erl"),

  ok = 
    file:write_file (Dir ++ "/sup-newnew/ebin/sup.app",
                     erlang:iolist_to_binary 
                       (io_lib:format ("~p.", [ NewNewApp ]))),

  ok = file:make_dir (Dir ++ "/erlrc.d"),
  ok = file:make_dir (Dir ++ "/erlrc.d/applications"),
  ok = file:make_dir (Dir ++ "/erlrc.d/nodes"),
  ok = application:set_env (erlrc, root_dir, Dir ++ "/erlrc.d"),

  Dir.

app_teardown (Dir) ->
  application:stop (supinc),
  application:unload (supinc),
  application:stop (sup),
  application:unload (sup),
  lists:foreach (fun (M) -> code:purge (M), code:delete (M) end,
                 [ erlrctestmakeappup, erlrctestmakeappupsup, 
                   erlrctestmakeappupsrv, erlrctestmakeappupdild,
                   erlrctestmakeappupinc ]),
  code:del_path (Dir ++ "/sup-incnew/ebin"),
  code:del_path (Dir ++ "/sup-inc/ebin"),
  code:del_path (Dir ++ "/sup-old/ebin"),
  code:del_path (Dir ++ "/sup-new/ebin"),
  os:cmd ("rm -rf " ++ Dir),
  ok.

%-=====================================================================-
%-                                Tests                                -
%-=====================================================================-

make_appup_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () -> 
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,

      { ok, Appup } = make_appup (sup,
                                  "0.0.0",
                                  "0.0.1",
                                  Dir ++ "/sup-old",
                                  Dir ++ "/sup-new"),

      Appup = { "0.0.1",
                [ { "0.0.0", 
                    [ { add_module, erlrctestmakeappupdild },
                      { load_module, erlrctestmakeappup },
                      { update, erlrctestmakeappupsup, supervisor },
                      { update, erlrctestmakeappupsrv, { advanced, [] } } ]
                  } ],
                [ { "0.0.0",
                    [ { update, erlrctestmakeappupsrv, { advanced, [] } },
                      { update, erlrctestmakeappupsup, supervisor },
                      { load_module, erlrctestmakeappup },
                      { delete_module, erlrctestmakeappupdild } ] 
                  } ] 
              },

      ok
    end }.

start_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,
      version_load_mismatch = start (sup, "0.0.1", Dir ++ "/sup-old"),
      started = start (sup, "0.0.0", Dir ++ "/sup-old"),
      already_running = start (sup, "0.0.0", Dir ++ "/sup-old"),
      version_mismatch = start (sup, "0.0.1", Dir ++ "/sup-old"),
      ok
    end }.

start_included_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,
      false = lists:keymember (supinc, 1, application:which_applications ()),
      false = lists:keymember (sup, 1, application:which_applications ()),
      started = start (sup, "0.0.0", Dir ++ "/sup-old"),
      false = lists:keymember (supinc, 1, application:which_applications ()),
      true = lists:keymember (sup, 1, application:which_applications ()),
      started_included_stopped = start (supinc, "0.0.0", Dir ++ "/sup-inc"),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      false = lists:keymember (sup, 1, application:which_applications ()),
      included = start (sup, "0.0.0", Dir ++ "/sup-old"),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      false = lists:keymember (sup, 1, application:which_applications ()),
      ok
    end }.

stop_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,
      not_running = stop (sup, "0.0.1"),
      not_running = stop (sup, "0.0.0"),
      started = start (sup, "0.0.0", Dir ++ "/sup-old"),
      version_mismatch = stop (sup, "0.0.1"),
      stopped = stop (sup, "0.0.0"),
      not_running = stop (sup, "0.0.0")
    end }.

stop_included_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,
      started = start (sup, "0.0.0", Dir ++ "/sup-old"),
      started_included_stopped = start (supinc, "0.0.0", Dir ++ "/sup-inc"),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      false = lists:keymember (sup, 1, application:which_applications ()),
      stopped_included_started = stop (supinc, "0.0.0"),
      false = lists:keymember (supinc, 1, application:which_applications ()),
      false  = lists:keymember (sup, 1, application:which_applications ()),
      ok = file:write_file (Dir ++ "/erlrc.d/applications/sup", <<>>),
      started_included_stopped = start (supinc, "0.0.0", Dir ++ "/sup-inc"),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      false = lists:keymember (sup, 1, application:which_applications ()),
      stopped_included_started = stop (supinc, "0.0.0"),
      false = lists:keymember (supinc, 1, application:which_applications ()),
      true  = lists:keymember (sup, 1, application:which_applications ()),
      stopped = stop (sup, "0.0.0")
    end }.

unload_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,
      false = code:is_loaded (erlrctestmakeappup),
      version_load_mismatch = unload (sup, "0.0.1", Dir ++ "/sup-old"),
      started = start (sup, "0.0.0", Dir ++ "/sup-old"),
      { file, _ } = code:is_loaded (erlrctestmakeappup),
      stopped = stop (sup, "0.0.0"),
      { file, _ } = code:is_loaded (erlrctestmakeappup),
      unloaded = unload (sup, "0.0.0", Dir ++ "/sup-old"),
      false = code:is_loaded (erlrctestmakeappup),
      unloaded = unload (sup, "0.0.0", Dir ++ "/sup-old"),
      false = code:is_loaded (erlrctestmakeappup)
    end }.

upgrade_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,
      started = start (sup, "0.0.0", Dir ++ "/sup-old"),
      { value, { sup, _, "0.0.0" } } = 
        lists:keysearch (sup, 1, application:which_applications ()),
      init = erlrctestmakeappupsrv:get (),
      { ok, _ } = upgrade (sup, 
                           "0.0.0", 
                           "0.0.1", 
                           Dir ++ "/sup-old",
                           Dir ++ "/sup-new"),
      { value, { sup, _, "0.0.1" } } = 
        lists:keysearch (sup, 1, application:which_applications ()),
      { code_change, _ } = erlrctestmakeappupsrv:get (),
      ok
    end }.

upgrade_existing_appup_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,
      started = start (sup, "0.0.0", Dir ++ "/sup-old"),
      { value, { sup, _, "0.0.0" } } = 
        lists:keysearch (sup, 1, application:which_applications ()),
      init = erlrctestmakeappupsrv:get (),
      AppUp = <<"
{\"0.0.1\",
 [{\"0.0.0\",
   [{add_module,erlrctestmakeappupdild},
    {load_module,erlrctestmakeappup},
    {update,erlrctestmakeappupsup,supervisor},
    {load_module,erlrctestmakeappupsrv}]}],
 [{\"0.0.0\",
   [{update,erlrctestmakeappupsrv,{advanced,[]}},
    {update,erlrctestmakeappupsup,supervisor},
    {load_module,erlrctestmakeappup},
    {delete_module,erlrctestmakeappupdild}]}]}.">>,
      ok = file:write_file (Dir ++ "/sup-new/ebin/sup.appup", AppUp),
      { ok, _ } = upgrade (sup, 
                           "0.0.0", 
                           "0.0.1", 
                           Dir ++ "/sup-old",
                           Dir ++ "/sup-new"),
      { value, { sup, _, "0.0.1" } } = 
        lists:keysearch (sup, 1, application:which_applications ()),
      init = erlrctestmakeappupsrv:get (),
      ok
    end }.

upgraded_not_started_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,

      started = start (sup, "0.0.0", Dir ++ "/sup-old"),
      false = erlang:function_exported (erlrctestmakeappup, flass, 0),
      stopped = stop (sup, "0.0.0"),
      code:ensure_loaded (erlrctestmakeappup),
      false = erlang:function_exported (erlrctestmakeappup, flass, 0),
      { ok, _ } = upgrade (sup,
                           "0.0.0",
                           "0.0.1",
                           Dir ++ "/sup-old",
                           Dir ++ "/sup-new"),
      true = erlang:function_exported (erlrctestmakeappup, flass, 0),
      ok
    end }.

downgraded_not_started_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,

      started = start (sup, "0.0.1", Dir ++ "/sup-new"),
      true = erlang:function_exported (erlrctestmakeappup, flass, 0),
      stopped = stop (sup, "0.0.1"),
      code:ensure_loaded (erlrctestmakeappup),
      true = erlang:function_exported (erlrctestmakeappup, flass, 0),
      { ok, _ } = downgrade (sup,
                             "0.0.0",
                             "0.0.1",
                             Dir ++ "/sup-old",
                             Dir ++ "/sup-new"),
      false = erlang:function_exported (erlrctestmakeappup, flass, 0),
      ok
    end }.

upgrade_double_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,
      started = start (sup, "0.0.0", Dir ++ "/sup-old"),
      { value, { sup, _, "0.0.0" } } = 
        lists:keysearch (sup, 1, application:which_applications ()),
      init = erlrctestmakeappupsrv:get (),
      { ok, _ } = upgrade (sup, 
                           "0.0.0", 
                           "0.0.1", 
                           Dir ++ "/sup-old",
                           Dir ++ "/sup-new"),
      { value, { sup, _, "0.0.1" } } = 
        lists:keysearch (sup, 1, application:which_applications ()),
      { code_change, _ } = erlrctestmakeappupsrv:get (),
      { ok, _ } = upgrade (sup, 
                           "0.0.1", 
                           "0.0.2", 
                           Dir ++ "/sup-new",
                           Dir ++ "/sup-newnew"),
      { value, { sup, _, "0.0.2" } } = 
        lists:keysearch (sup, 1, application:which_applications ()),
      { code_change, _ } = erlrctestmakeappupsrv:get (),
      ok
    end }.

upgrade_included_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,
      ok = file:write_file (Dir ++ "/erlrc.d/applications/sup", <<>>),
      started = start (sup, "0.0.0", Dir ++ "/sup-old"),
      started_included_stopped = start (supinc, "0.0.0", Dir ++ "/sup-inc"),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      false = lists:keymember (sup, 1, application:which_applications ()),
      init = erlrctestmakeappupsrv:get (),
      { ok, _ } = upgrade (sup, 
                           "0.0.0", 
                           "0.0.1", 
                           Dir ++ "/sup-old",
                           Dir ++ "/sup-new"),
      { code_change, _ } = erlrctestmakeappupsrv:get (),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      false = lists:keymember (sup, 1, application:which_applications ()),
      ok
    end }.

upgrade_including_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,
      ok = file:write_file (Dir ++ "/erlrc.d/applications/sup", <<>>),
      started = start (sup, "0.0.0", Dir ++ "/sup-old"),
      started_included_stopped = start (supinc, "0.0.0", Dir ++ "/sup-inc"),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      false = lists:keymember (sup, 1, application:which_applications ()),
      { ok, _ } = upgrade (supinc, 
                           "0.0.0", 
                           "0.0.1", 
                           Dir ++ "/sup-inc",
                           Dir ++ "/sup-incnew"),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      true = lists:keymember (sup, 1, application:which_applications ()),
      stopped = stop (supinc, "0.0.1"),
      unloaded = unload (supinc, "0.0.1", Dir ++ "/sup-incnew"),
      stopped = stop (sup, "0.0.0"),
      unloaded = unload (sup, "0.0.0", Dir ++ "/sup-old"),
      started = start (sup, "0.0.0", Dir ++ "/sup-old"),
      started = start (supinc, "0.0.1", Dir ++ "/sup-incnew"),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      true = lists:keymember (sup, 1, application:which_applications ()),
      { ok, _ } = upgrade (supinc, 
                           "0.0.1", 
                           "0.0.0", 
                           Dir ++ "/sup-incnew",
                           Dir ++ "/sup-inc"),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      false = lists:keymember (sup, 1, application:which_applications ()),
      ok
    end }.

downgrade_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,
      started = start (sup, "0.0.1", Dir ++ "/sup-new"),
      { value, { sup, _, "0.0.1" } } = 
        lists:keysearch (sup, 1, application:which_applications ()),
      init = erlrctestmakeappupsrv:get (),
      { ok, _ } = downgrade (sup, 
                             "0.0.0", 
                             "0.0.1", 
                             Dir ++ "/sup-old",
                             Dir ++ "/sup-new"),
      { code_change, { down, _ } } = erlrctestmakeappupsrv:get (),
      { value, { sup, _, "0.0.0" } } = 
        lists:keysearch (sup, 1, application:which_applications ()),
      ok
    end }.

downgrade_included_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,
      ok = file:write_file (Dir ++ "/erlrc.d/applications/sup", <<>>),
      started = start (sup, "0.0.1", Dir ++ "/sup-new"),
      started_included_stopped = start (supinc, "0.0.0", Dir ++ "/sup-inc"),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      false = lists:keymember (sup, 1, application:which_applications ()),
      init = erlrctestmakeappupsrv:get (),
      { ok, _ } = downgrade (sup, 
                             "0.0.0", 
                             "0.0.1", 
                             Dir ++ "/sup-old",
                             Dir ++ "/sup-new"),
      { code_change, { down, _ } } = erlrctestmakeappupsrv:get (),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      false = lists:keymember (sup, 1, application:which_applications ()),
      ok
    end }.

downgrade_including_test_ () ->
  { setup,
    fun app_setup/0,
    fun app_teardown/1,
    fun () ->
      OsPid = os:getpid (),
      Dir = "erlrcmakeappuptest" ++ OsPid,
      ok = file:write_file (Dir ++ "/erlrc.d/applications/sup", <<>>),
      started = start (sup, "0.0.0", Dir ++ "/sup-old"),
      started = start (supinc, "0.0.1", Dir ++ "/sup-incnew"),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      true = lists:keymember (sup, 1, application:which_applications ()),
      { ok, _ } = downgrade (supinc, 
                             "0.0.0", 
                             "0.0.1", 
                             Dir ++ "/sup-inc",
                             Dir ++ "/sup-incnew"),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      false = lists:keymember (sup, 1, application:which_applications ()),
      stopped_included_started = stop (supinc, "0.0.0"),
      stopped = stop (sup, "0.0.0"),
      started_included_stopped = start (supinc, "0.0.0", Dir ++ "/sup-inc"),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      false = lists:keymember (sup, 1, application:which_applications ()),
      { ok, _ } = downgrade (supinc, 
                             "0.0.1", 
                             "0.0.0", 
                             Dir ++ "/sup-incnew",
                             Dir ++ "/sup-inc"),
      true = lists:keymember (supinc, 1, application:which_applications ()),
      true = lists:keymember (sup, 1, application:which_applications ()),
      stopped = stop (supinc, "0.0.1"),
      stopped = stop (sup, "0.0.0"),
      ok
    end }.

-endif.
