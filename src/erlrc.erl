%% @hidden

-module (erlrc).
-export ([ start/0, stop/0 ]).
-behaviour (application).
-export ([ start/2, stop/1 ]).

-ifdef (HAVE_EUNIT).
-include_lib ("eunit/include/eunit.hrl").
-endif.

%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

start () ->
  application:start (erlrc).

stop () ->
  application:stop (erlrc).

%-=====================================================================-
%-                        Application callbacks                        -
%-=====================================================================-

start (_Type, _Args) ->
  case have_release_handler_1_bug () of
    true ->
      error_logger:info_msg ("buggy release_handler_1 detected, loading fix~n", []),
      ok = code:unstick_dir (code:lib_dir (sasl) ++ "/ebin"),
      true = code:soft_purge (release_handler_1),
      case code:priv_dir (erlrc) of
        { error, bad_name } -> % testing in development, hopefully
          { module, release_handler_1 } = code:load_file (release_handler_1);
        Dir ->
          { module, release_handler_1 } = code:load_abs (Dir ++ "/release_handler_1")
      end;
    false ->
      ok
  end,
  false = have_release_handler_1_bug (),
  erlrcsup:start_link ().

stop (_State) ->
  true = code:soft_purge (release_handler_1),
  { module, release_handler_1 } = 
    code:load_abs (code:lib_dir (sasl) ++ "/ebin/release_handler_1"),
  ok.

%-=====================================================================-
%-                               Private                               -
%-=====================================================================-

have_release_handler_1_bug () ->
  { value, { attributes, Attr } } = 
    lists:keysearch (attributes, 1, release_handler_1:module_info ()),

  case lists:keysearch (vsn, 1, Attr) of
    { value, { vsn, [ 79147130521276782822511501112343775816 ] } } ->
      true;
    _ ->
      false
  end.

-ifdef (EUNIT).

%-=====================================================================-
%-                                Tests                                -
%-=====================================================================-

start_test () ->
  try
    ok = code:unstick_dir (code:lib_dir (sasl) ++ "/ebin"),
    erlrc:stop (void),
    ok = erlrc:start ()
  after
    application:stop (erlrc)
  end.

-endif.
