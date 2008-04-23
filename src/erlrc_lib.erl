-module (erlrc_lib).

-export ([ load_application/1,
	   get_apps_dir/0,
	   load_resource_file/2 ]).

%% @spec load_application (App::atom()) -> ok | { error, Reason }
%% @doc Load the given application App, obeying an override resource
%% file App.app in the directory $ERLRC_ROOT/applications.
%% @end

load_application (erlrc) ->
  % Special case: code below depends on erlrc's application spec being
  % loaded, but of course load_application (erlrc) implies it isn't.
  % So, there can be no overrides of erlrc's application spec.
  application:load (erlrc);

load_application (App) ->
  try
    AppsDir = get_apps_dir (),
    Spec = load_resource_file (AppsDir, App),
    case application:load (Spec) of
      ok ->
	ok;
      { error, LoadReason } ->
	throw ({ load_failed, Spec, LoadReason })
    end
  catch
    throw:Error -> { error, Error }
  end.

get_apps_dir () ->
  Root = case os:getenv ("ERLRC_ROOT") of
    false ->
      { ok, Dir } = application:get_env (erlrc, root_dir),
      Dir;
    Value ->
      Value
  end,
  Root ++ "/applications".

%% @private
load_resource_file (AppsDir, App) ->
  ResourceBasename = atom_to_list (App) ++ ".app",
  % try override ERLRC_ROOT/applications/APPLICATION.app
  OverrideResourceFile = AppsDir ++ "/" ++ ResourceBasename,
  case file:consult (OverrideResourceFile) of
    { ok, [ Term ] } ->
      Term;
    { error, enoent } ->
      % try default APPLICATION.app in code path
      case code:where_is_file (ResourceBasename) of
	non_existing ->
	  throw ({ app_file_not_found, ResourceBasename });
	Path ->
	  case file:consult (Path) of
	    { ok, [ Term ] } ->
	      Term;
	    { error, Reason } ->
	      throw ({ app_file_parse_error, Path, Reason })
	  end
      end;
    { error, Reason } ->
      throw ({ app_file_parse_error, OverrideResourceFile, Reason })
  end.
