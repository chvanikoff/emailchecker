-module(emailchecker_srv).
-author('chvanikoff <chvanikoff@gmail.com>').

-behaviour(gen_server).

%% Gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

%% API
-export([
	start_link/0,
	check_email/1,
	check_emails/1
]).

-record(state, {
	domains = []
}).

-record(domain, {
	name = nil,
	mx = [],
	accounts = []
}).

-define(TIMEOUT, 5000).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

check_email(Email) -> check_emails([Email]).

check_emails(Emails) ->
	add_emails(Emails),
	do_check().

%% ===================================================================
%% Gen_server callbacks
%% ===================================================================

init([]) -> {ok, #state{}}.

handle_call(check, _From, _State = #state{domains = Domains}) ->
	Result = process_check(Domains),
	{reply, Result, #state{}};
handle_call(_Request, _From, State) -> {reply, ignored, State}.

handle_cast({add_email, Email}, State = #state{domains = Domains}) ->
	Email2 = case is_binary(Email) of true -> binary_to_list(Email); _ -> Email end,
	[Name, Domain] = string:tokens(Email2, "@"),
	State2 = case domain_already_added(Domain, Domains) of
		true -> State;
		false -> State#state{domains = [#domain{mx = mxlookup(Domain), name = Domain} | Domains]}
	end,
	Cur = lists:keyfind(Domain, #domain.name, State2#state.domains),
	{_, _, Domains2} = lists:keytake(Domain, #domain.name, State2#state.domains),
	Accounts = Cur#domain.accounts,
	NewDomains = [Cur#domain{accounts = [Name | Accounts]} | Domains2],
	{noreply, State2#state{domains = NewDomains}};
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

domain_already_added(_Domain, []) -> false;
domain_already_added(Domain, [#domain{name = Domain} | _T]) -> true;
domain_already_added(Domain, [_H | T]) -> domain_already_added(Domain, T).

mxlookup(Domain) ->
	case whereis(inet_db) of
		P when is_pid(P) -> ok;
		_ -> inet_db:start()
	end,
	case lists:keyfind(nameserver, 1, inet_db:get_rc()) of
		false ->
			% we got no nameservers configured, suck in resolv.conf
			inet_config:do_load_resolv(os:type(), longnames);
		_ -> ok
	end,
	case inet_res:lookup(Domain, in, mx) of
		[] -> [];
		Result ->
			lists:sort(fun({Pref, _Name}, {Pref2, _Name2}) -> Pref =< Pref2 end, Result)
	end.

add_emails([]) -> ok;
add_emails([Email | Emails]) ->
	gen_server:cast(?MODULE, {add_email, Email}),
	add_emails(Emails).

do_check() ->
	gen_server:call(?MODULE, check).

process_check(Domains_data) ->
	process_check(Domains_data, []).

process_check([], Acc) -> Acc;
process_check([_Domain = #domain{mx = Mx, accounts = Accounts, name = Name} | Domains], Acc) ->
	Accounts_results = check_accounts(Accounts, Name, Mx),
	process_check(Domains, lists:flatten([Accounts_results | Acc])).

check_accounts(Accounts, Domain, []) -> fill_failed(Accounts, Domain);
check_accounts(Accounts, Domain, Mx) -> check_accounts(Accounts, Domain, Mx, []).

fill_failed(Accounts, Domain) -> fill_failed(Accounts, Domain, []).

fill_failed([], _Domain, Acc) -> Acc;
fill_failed([Account | Accounts], Domain,  Acc) ->
	fill_failed(Accounts, Domain, [{Account ++ "@" ++ Domain, invalid} | Acc]).

check_accounts([], _Domain, _Mx, Acc) -> Acc;
check_accounts([Account | Accounts], Domain, Mx, Acc) ->
	Res = case check_acc_by_mx(Account, Domain, Mx) of
		true -> {Account ++ "@" ++ Domain, valid};
		_ -> {Account ++ "@" ++ Domain, invalid}
	end,
	check_accounts(Accounts, Domain, Mx, [Res | Acc]).

check_acc_by_mx(_Account, _Domain, []) -> true;
check_acc_by_mx(Account, Domain, [{_, Mx} | T]) ->
	case gen_tcp:connect(Mx, 25, [{active, false}], ?TIMEOUT) of
		{ok, Sock} ->
			case gen_tcp:recv(Sock, 0) of
				{ok, "220" ++ _} ->
					case handle_sock(Sock, Account, Domain) of
						{ok, Result} ->
							Result;
						_ ->
							check_acc_by_mx(Account, Domain, T)
					end;
				_ ->
					check_acc_by_mx(Account, Domain, T)
			end;
		_ ->
			check_acc_by_mx(Account, Domain, T)
	end.

handle_sock(Sock, Account, Domain) ->
	{ok, MyName} = inet:gethostname(),
	gen_tcp:send(Sock, lists:append(["HELO ", MyName, [13, 10]])),
	case gen_tcp:recv(Sock, 0) of
		{ok, "250"++_} ->
			gen_tcp:send(Sock, lists:append(["MAIL FROM: ", "chvanikoff@yandex.ru", [13, 10]])),
			case gen_tcp:recv(Sock, 0) of
				{ok, "250"++_} ->
					gen_tcp:send(Sock, lists:append(["RCPT TO: ", Account ++ "@" ++ Domain, [13, 10]])),
					Result = case gen_tcp:recv(Sock, 0) of
						{ok, "250"++_} -> {ok, true};
						_ -> {ok, false}
					end,
					gen_tcp:send(Sock,lists:append("QUIT", [13, 10])),
					% {ok, "221"++_}=gen_tcp:recv(Sock, 0),
					gen_tcp:recv(Sock, 0),
					gen_tcp:close(Sock),
					Result;
				_ ->
					false
			end;
		_ ->
			false
	end.
