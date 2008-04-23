#! /usr/bin/perl
#
# Generate an Erlang lib directory and erlrc root, containing dummy
# applications with a specified dependency structure.  Each 'start'
# line creates one application:
#
#   start foo-1.0.0: bar, !baz
#
# means to create the application foo version 1.0.0, with a dependency
# on bar, and with baz as an included application.  An asterisk before
# the name means to override the .app file within the erlrc root:
#
#   start *foo-1.0.0: bar, !baz
#
# Expected result of erlrc_boot:boot () is specified by 'result' if
# not 'ok':
#
#   result {error,{circular_dependency,x,[x,y,z]}}
#

use strict;

my ($test_file) = @ARGV;

my $tmp_dir = "tmp-test-boot-$$";
my $erlrc_root = "$tmp_dir/erlrc";
my $lib_root = "$tmp_dir/lib";

sub mkdir_p ($) 
{
  my ($dir) = @_;
  system ('mkdir', '-p', $dir) == 0 or die "error creating $dir";
}

sub create_file ($$)
{
  my ($file, $contents) = @_;
  open (F, ">$file") or die "error creating $file: $!";
  print F $contents;
  close (F) or die "error closing $file: $!";
}

mkdir_p ("$erlrc_root/applications");

my @code_paths;
my $expected_result = 'ok';
my %app_to_deps;
my %app_to_overridden;

open (T, $test_file) or die $!;
while (defined ($_ = <T>))
  {
    chomp;
    s/#.*$//;
    next if /^$/;
    if (/^start (\*?)(\w+)-([0-9\.]+): (.*)$/)
      {
	my $override = ($1 ne '');
	my $app = $2;
	my $version = $3;
	my $spec = $4;
	my (@deps, @incs);
	foreach my $dep (split (/\s*,\s*/, $spec))
	  {
	    if ($dep =~ s/^!//)
	      {
		push (@incs, $dep);
	      }
	    else
	      {
		push (@deps, $dep);
	      }
	  }
	$app_to_deps{$app} = [ @deps ];
	$app_to_overridden{$app} = $override ? 'true' : 'false';
	emit_application ($app, $version, $override, \@deps, \@incs);
      }
    elsif (/^result (.*)$/)
      {
	$expected_result = $1;
      }
    elsif (/^command (.*)$/)
      {
	system ("cd $tmp_dir; $1");
      }
    else
      {
	die "syntax error: $_";
      }
  }
close (T);

my $pid = open (ERL, '-|');
if ($pid)
  {
    my $boot_result = <ERL>;
    chomp ($boot_result);
    my @actions = <ERL>;
    close (ERL) or die $!;

    if ($boot_result ne $expected_result)
      {
	die "expected '$expected_result', got '$boot_result'; output is '" .
	    join ('', @actions) . "'";
      }
    if ($boot_result eq 'ok')
      {
	my %started;
	foreach (@actions)
	  {
	    /^(\w+):(true|false)/ or die "bad action line $_";
	    my $app = $1;
	    my $overridden = $2;
	    my @deps_not_started =
	      grep { !$started{$_} } @{ $app_to_deps{$app} || [ ] };
	    if (@deps_not_started)
	      {
		die "application $app was started before dependencies: " .
		    join (', ', @deps_not_started);
	      }
	    if ($overridden ne $app_to_overridden{$app})
	      {
		die "application $app had overridden=$overridden, but " .
		    "expected " . $app_to_overridden{$app};
	      }
	    $started{$app} = 1;
	  }
      }
  }
else
  {
    exec ('erl',
	  '-boot', 'start_sasl',
	  '-sasl', 'sasl_error_logger', qq({ file, "$tmp_dir/sasl_errors" }),
	  '-erlrc', 'root_dir', qq("$erlrc_root"),
	  (map { ('-pa', $_) } @code_paths),
	  '-pa', '../src',
	  '-eval', q(
		      { ok, _ } = erlrc_boot_test:start_link (),
		      Result = erlrc_boot:boot (),
		      io:format ("~w~n", [Result]),
		      lists:foreach (
			fun ({ _Time, { start, App, Overridden } }) ->
			  io:format ("~w:~w~n", [ App, Overridden ])
			end,
			lists:sort (erlrc_boot_test:dump ())
		      )
		   ),
	  '-s', 'init', 'stop',
	  '-noshell', '-noinput')
      or die "exec: $!";
  }

sub emit_application
{
  my ($app, $version, $override, $deps, $incs) = @_;

  mkdir_p ("$lib_root/$app-$version/src");
  my $erl_file = "$lib_root/$app-$version/src/$app.erl";
  create_file ($erl_file, <<"END");
-module ($app).
-behaviour (application).
-export ([ start/2, stop/1 ]).

start (_Type, _Args) ->
  { ok, Overridden } = application:get_env ($app, overridden),
  erlrc_boot_test:notify ({ start, $app, Overridden }),
  Pid = spawn (fun () -> receive after infinity -> ok end end),
  { ok, Pid }.

stop (_State) ->
  ok.
END

  my $ebin_dir = "$lib_root/$app-$version/ebin";
  push (@code_paths, $ebin_dir);
  mkdir_p ($ebin_dir);
  system ('erlc', '-o', $ebin_dir, $erl_file) == 0
    or die "error compiling $erl_file";

  my $dep_list = join (', ', qw(kernel stdlib), @$deps);
  my $included_list = join (', ', @$incs);
  create_file ("$ebin_dir/$app.app", <<"END");
      { application,
	$app,
	[ 
	  { description, "erlrc test application $app" }, 
	  { vsn, "$version" },
	  { modules, [ $app ] },
	  { registered, [ ] },
	  { applications, [ $dep_list ] },
	  { mod, { $app, [] } },
	  { env, [ { overridden, false } ] },
	  { included_applications, [ $included_list ] }
	] 
      }.
END

  create_file ("$erlrc_root/applications/$app", 'temporary');
  if ($override)
    {
      create_file ("$erlrc_root/applications/$app.app", <<"END");
	{ application,
	  $app,
	  [ 
	    { description, "erlrc test application $app" }, 
	    { vsn, "$version" },
	    { modules, [ $app ] },
	    { registered, [ ] },
	    { applications, [ $dep_list ] },
	    { mod, { $app, [] } },
	    { env, [ { overridden, true } ] },
	    { included_applications, [ $included_list ] }
	  ] 
	}.
END
    }
}

system ('rm', '-rf', $tmp_dir);

0;
