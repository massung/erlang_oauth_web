%% Web-base OAuth FSM
%%
%% Created by: Jeffrey Massung
%% Email: jeff@basho.com
%%

-module(oauth_fsm).
-behavior(gen_fsm).

%% gen_fsm initialization
-export([start_link/1]).

%% gen_fsm behavior functions
-export([init/1,terminate/3,code_change/4]).
-export([handle_event/3,handle_sync_event/4,handle_info/3]).

%% oauth states
-export([request_token/3,request_access/3,request/3]).

%% oauth commons
-include("../include/oauth.hrl").

%% start a new oauth state machine
start_link (Pid) ->
    gen_fsm:start_link(?MODULE,Pid,[]).

%% initialize the new state machine
init (SessionPid) ->
    %% when the session pid terminates, we need to terminate
    monitor(process,SessionPid),

    %% ready to begin
    {ok,request_token,undefined}.

%% shutdown the state machine
terminate (_Reason,_State,_Data) ->
    ok.

%% used for updating code
code_change (_OldVersion,State,Data,_Extra) ->
    {ok,State,Data}.

%% handle an event sent to all states
handle_event (logout,_State,_Data) ->
    {next_state,request_token,undefined};
handle_event (_Event,State,Data) ->
    {next_state,State,Data}.

%% handle a sync event sent to all states
handle_sync_event (logout,_From,_State,_Data) ->
    {reply,ok,request_token,undefined};
handle_sync_event (_Event,_From,State,Data) ->
    {reply,ok,State,Data}.

%% handle an unknown message
handle_info ({'DOWN',_Ref,process,_Pid,_Reason},_State,Data) ->
    {stop,normal,Data};
handle_info (_Info,State,Data) ->
    {next_state,State,Data}.

%% issue a request for a token
request_token ({Url,Consumer=#oauth_consumer{},ExtraParams},_From,_) ->
    Params=oauth:oauth_params(Consumer) ++ ExtraParams,
    Base=oauth:base_signature(post,Url,Params),
    Signature=oauth:sign(Base,Consumer,""),

    %% send off the request and wait for a reponse
    case oauth:issue_request(post,Url,[{"oauth_signature",Signature}|Params]) of
	{ok,Tokens} ->
	    {_,Token}=lists:keyfind("oauth_token",1,Tokens),
	    {_,Secret}=lists:keyfind("oauth_token_secret",1,Tokens),

	    %% return the token back, save the secret
	    {reply,{ok,Token},request_access,{Consumer,Token,Secret}};
	Error ->
	    {stop,Error,Error,request_token}
    end.

%% exchange a token for an access token
request_access ({Url,Token,Verifier},_From,{Consumer,_AuthToken,Secret}) ->
    Params=oauth:oauth_params(Consumer)++[{"oauth_token",Token},
					  {"oauth_verifier",Verifier}],

    %% construct the base signature and sign it
    Base=oauth:base_signature(post,Url,Params),
    Signature=oauth:sign(Base,Consumer,Secret),

    %% send off the request and wait for a response
    case oauth:issue_request(post,Url,[{"oauth_signature",Signature}|Params]) of
	{ok,Tokens} ->
	    {_,{_,AccessToken},T2}=lists:keytake("oauth_token",1,Tokens),
	    {_,{_,AccessSecret},T3}=lists:keytake("oauth_token_secret",1,T2),
	    
	    %% return a successful login back, save the new token and secret
	    {reply,{ok,T3},request,{Consumer,AccessToken,AccessSecret}};
	Error ->
	    {stop,Error,Error,request_token}
    end.

%% perform a oauth request or action
request ({Method,Url,Params},_From,{Consumer,Token,Secret}) ->
    Response=oauth:issue_command(Method,Url,Consumer,Token,Secret,Params),

    %% let the user decide what to do with the response
    {reply,Response,request,{Consumer,Token,Secret}}.
