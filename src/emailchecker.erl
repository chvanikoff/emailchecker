-module(emailchecker).
-author('chvanikoff <chvanikoff@gmail.com>').

%% API callbacks
-export([
	start/0,
	is_real/1,
	check/1,
	check_multi/1
]).

-define(SERVER, emailchecker_srv).

-define(MYSQL_PID, p1).

%% ===================================================================
%% API
%% ===================================================================

start() ->
	ok = ensure_started([crypto, public_key, ssl, inets, emailchecker]),
	ok.

is_real(Email) ->
	[{Email, valid}] =:= emailchecker_srv:check_email(Email).

check(Email) ->
	emailchecker_srv:check_email(Email).

check_multi(Emails) ->
	emailchecker_srv:check_emails(Emails).

%% ===================================================================
%% Internal functions
%% ===================================================================

ensure_started([]) -> ok;
ensure_started([App | Apps]) ->
	case application:start(App) of
		ok -> ensure_started(Apps);
		{error, {already_started, App}} -> ensure_started(Apps)
	end.
