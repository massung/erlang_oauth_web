%% Web-base OAuth Library
%%
%% Created by: Jeffrey Massung
%% Email: jeff@basho.com
%%

-module(oauth).
-export([oauth_params/1,
	 base_signature/3,
	 sign/3,
	 parse_resp/1,
	 issue_request/3,
	 issue_command/6]).

%% oauth commons
-include("../include/oauth.hrl").

%% check for alphanumeric acharacters
is_letter (C) -> ((C >= $a) and (C =< $z)) or ((C >= $A) and (C =< $Z)).
is_digit (C) -> ((C >= $0) and (C =< $9)).
is_alphanum (C) -> (is_letter(C) or is_digit(C)).

%% convert a number to a hex string
int_to_hex (N) -> 
    lists:flatten(io_lib:format("~2.16.0B",[N])).

%% encode a single character
encode_char (C) ->
    case (is_alphanum(C) or lists:member(C,"-_.~")) of
	false -> [$%|int_to_hex(C)];
	true -> [C]
    end.

%% convert a value to a string and url_encode it
url_encode (V) when is_list(V) -> lists:flatten([encode_char(C) || C <- V]);
url_encode (V) when is_integer(V) -> url_encode(integer_to_list(V));
url_encode (V) when is_atom(V) -> url_encode(atom_to_list(V)).

%% decode a string value
url_decode (S) ->
    case lists:splitwith(fun (X) -> X =/= $% end,S) of
	{X,[_,A,B|XS]} -> 
	    C=list_to_integer([A,B],16),
	    lists:concat([X,[C],url_decode(XS)]);
 	{X,[]} -> 
	    X
    end.

%% url encode a value for a get or post
encode_param ({Key,Value}) ->
    lists:concat([Key,"=",url_encode(Value)]).

%% sort and encode all the params for a request
encode_params (Params) ->
    string:join([encode_param(P) || P <- lists:sort(Params)],"&").

%% wrap an value in quotes after encoding it
quote ({Key,Value}) ->
    lists:concat([Key,"=\"",url_encode(Value),"\""]).

%% construct the authorization header
oauth_header (Params) ->
    "OAuth " ++ string:join([quote(P) || P <- Params],", ").

%% get the current time in seconds
unix_timestamp () ->
    Now=calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    Epoch=calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    Now-Epoch.

%% get a unique string nonce
make_nonce () ->
    <<Bytes:88/integer>>=crypto:rand_bytes(11),
    lists:flatten(io_lib:format("~.16.0b",[Bytes])).

%% consumer oauth parameters
oauth_params (#oauth_consumer{key=Key,signature_method=Method}) ->
    [{"oauth_consumer_key",Key},
     {"oauth_nonce",make_nonce()},
     {"oauth_signature_method",Method},
     {"oauth_timestamp",unix_timestamp()},
     {"oauth_version","1.0"}
    ].

%% convert http method to base string
method_string (get) -> "GET&";
method_string (post) -> "POST&".

%% create the base signature for a given request
base_signature (Method,Url,Params) ->
    Query=encode_params(Params),
    lists:concat([method_string(Method),
		  url_encode(Url),
		  "&",
		  url_encode(Query)]).

%% sign a base signature with hmac-sha1
sign_hmac_sha (Base,Secret,Token) ->
    Key=lists:concat([Secret,"&",Token]),
    base64:encode_to_string(crypto:sha_mac(Key,Base)).

%% TODO: public key sign
sign_rsa_sha (_Base,_Secret,_Token) ->
    "".

%% sign a base signature using the consumer signature method
sign (Base,Consumer=#oauth_consumer{signature_method='HMAC-SHA1'},Token) ->
    sign_hmac_sha(Base,Consumer#oauth_consumer.secret,Token);
sign (Base,Consumer=#oauth_consumer{signature_method='RSA-SHA1'},Token) ->
    sign_rsa_sha(Base,Consumer#oauth_consumer.secret,Token);
sign (_,Consumer=#oauth_consumer{signature_method='PLAINTEXT'},_) -> 
    Consumer#oauth_consumer.secret.

%% returns a list of {k,v} pairs extracted from an oauth response
parse_resp (Body) ->
    case lists:splitwith(fun (X) -> X =/= $& end,Body) of
	{X,[_|XS]} ->
	    {K,[_|V]} = lists:splitwith(fun (I) -> I =/= $= end,X),
 	    [{K,url_decode(V)}|parse_resp(XS)];
 	{X,[]} ->
 	    {K,[_|V]} = lists:splitwith(fun (I) -> I =/= $= end,X),
 	    [{K,url_decode(V)}]
    end.

%% generate the request type based on the http method
http_request (Method,Url,Headers,Params) ->
    EncodedParams=encode_params(Params),
    case Method of
	post -> {Url,Headers,"application/x-www-form-urlencoded",EncodedParams};
	get -> {lists:concat([Url,"?",EncodedParams]),Headers}
    end.

%% send an http request to the oauth server to get access/tokens
issue_request (Method,Url,Params) ->
    Req=http_request(Method,Url,[],Params),
    HTTPOptions=[{timeout,15000}],
    case httpc:request(Method,Req,HTTPOptions,[]) of
	{ok,{{_,200,_},_Headers,Body}} ->
	    {ok,parse_resp(Body)};
	{ok,{{_,Code,Error},_Headers,Body}} ->
	    {error,Code,Error,Body};
	Error -> Error
    end.

%% send a POST, all authorization is done, oauth goes in the header
issue_command (Method,Url,Consumer,Token,Secret,Params) ->
    OAuthParams=[{"oauth_token",Token}|oauth:oauth_params(Consumer)],
    Base=base_signature(Method,Url,OAuthParams ++ Params),
    Signature=sign(Base,Consumer,Secret),
    Authorization=oauth_header([{"oauth_signature",Signature}|OAuthParams]),
    Headers=[{"Authorization",Authorization}],
    Req=http_request(Method,Url,Headers,Params),
    HTTPOptions=[{timeout,15000}],
    case httpc:request(Method,Req,HTTPOptions,[]) of
	{ok,{{_,200,_},_Headers,Body}} ->
	    {ok,Body};
	{ok,{{_,Code,Error},_Headers,Body}} ->
	    {error,Code,Error,Body};
	Error -> Error
    end.
