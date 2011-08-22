## Summary

This is a module intended to assist those who are using Erlang frameworks for serving web pages, and would like to use OAuth for logins. I've found that OAuth can be a bit of a pain, so I hope this is useful to others.


## Erlang Web OAuth - The Basics

OAuth is comprised of three major steps:

1. Get a token
2. User login/authentication
3. Acquire access token

This example will take you through the basics of using the OAuth module in your site in order to allow the user to login. 

Assuming you have your web site already registered with an OAuth provider (e.g. Twitter, Google), you should have been given a few pieces of information that the OAuth module will need to use:

1. a consumer_key
2. a consumer_secret
3. a signature_method

Your site - at the very least - will also require the following conent for this tutorial. For simplicity, this tutorial will assume you are using the Nitrogen framework (www.nitrogenproject.com) and are using the supplied names.

1. A landing page, where the user will select "Login via &lt;provider&gt;" (index.erl)
2. A callback page the provider will return the user to after authentication (login.erl)
3. A home page, where the user is redirected to after logging in (home.erl)


### Getting a Token

When the user lands on your site they will need to perform an action that will cause them to log into the OAuth provider's site and/or authenticate your application. This is done by the user clicking a link that will then - in turn - start the OAuth process rolling.

Example index.erl:

    -module(index).
    -compile(export_all).
    -include_lib("nitrogen_core/include/wf.hrl").
    -include_lib("oauth/include/oauth.hrl").

    %% Twitter OAuth
    -define(GET_TOKEN,"https://api.twitter.com/oauth/request_token").
    -define(CONSUMER,
      #oauth_consumer{key="<your key here>",
                      secret="<your secret here>",
                      signature_method='HMAC-SHA1'
      }).

    main ()       -> #template{file="./site/templates/bare.html"}.
    title ()      -> "Welcome / Login".
    body ()       -> #link{url="#",text="Login with Twitter",postback=login}.
    inner_body () -> [].

    event (login) ->
      %% get the session pid, we need this so we can monitor it [*]
      {ok,Session}=wf:handler:call_readonly(session_handler,get_session_pid,[]),

      %% start the oauth state machine, and when the session dies, kill the fsm
      {ok,Pid}=oauth_fsm:start_link(Session),

      %% get an authentication token from twitter
      {ok,Token}=gen_fsm:sync_send_event(Pid,{?GET_TOKEN,?CONSUMER,[]}),

      %% store the oauth state machine pid in the session
      wf:session(oauth,Pid),

      %% send the user to twitter's authentication page
      wf:redirect(wf:f("https://api.twitter.com/oauth/authorize?oauth_token=~s",[Token])).

<i>[*] NOTE: This example requires exporting the get_session_pid/2 function from nitrogen_core/handlers/session/simple_session_handler.erl (a "private" function). Hopefully there will be a better way of tying processes to the session in the future.</i>


### User Login/Authentication

Once the user has been sent to the OAuth provider's site, they will be asked to login if they aren't already. After they have successfully logged in, they will be presented with a page asking them if they would like to authenticate your application. If the user does so, they will be redirected back to your site, specifically to whatever URL you registered as a "callback" with the OAuth provider. For the purposes of this example, that would be login.erl (http://my.site.come/login).

Along with redirecting the user back to your site, it also provides two additional pieces of data: the token you acquired in the previous step and a verification string. The token will be used by the oauth_fsm in order to confirm that everything is as it should be. The verification string is used by the OAuth provider in order to exchange the authentication token you received earlier for an access token.


### Acquire Access Token

Your login (callback) page will be where you exchange the authentication token for an access token. To do this, you will need to fetch the token and verifier provided in the GET/POST and pass them into oauth_fsm.

Example login.erl:

    -module(login).
    -compile(export_all).
    -include_lib("nitrogen_core/include/wf.hrl").
    -include_lib("oauth/include/oauth.hrl").

    -define(GET_ACCESS,"https://apit.twitter.com/oauth/access_token").

    main () ->
      %% get the token and verifier from the GET
      Token=wf:q("oauth_token"),
      Verifier=wf:q("oauth_verifier"),

      %% lookup the oauth_fsm process for this session
      Pid=wf:session(oauth),

      %% exchange and get access tokens and provider parameters
      {ok,Params}=gen_fsm:sync_send_event(Pid,{?GET_ACCESS,Token,Verifier}),

      %% lookup twitter-specific data
      {_,User}=lists:keyfind("user_id",1,Params),
      {_,ScreenName}=lists:keyfind("screen_name",1,Params),

      %% save the data in the user
      wf:user({User,ScreenName}),

      %% the user has successfully logged in - redirect to the home page
      wf:redirect("/home").

The Params variable returned is a list of {Key,Value} tuples that the OAuth provider returned back with the successfull login, and is provider-specific. You can ignore them or save them off for use later. 


## Life After Login...

Once a user has successfully logged into your site, the oauth_fsm is still running, and you can use it to request information or perform account actions with the user. Each action or query performed is done by sending an event that consists of both the action URL (see your OAuth provider's REST API) and a list of {Key,Value} tuples, which act as the POST parameters to be sent to the provider.

    TweetURL="http://api.twitter.com/1/statuses/update.json",
    Params=[{"status","Hey, I'm tweeting from Erlang!"}],
    {ok,Json}=gen_fsm:sync_send_event(Pid,{TweetURL,Params}).

The oauth_fsm will stay in this state until the process dies (obviously) or you inidicate that you want to logout by sending (any state) the logout event:

    gen_fsm:send_all_event(Pid,logout).


That's it!