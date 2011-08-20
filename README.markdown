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

1. A homepage, where the user will select "Login via <provider>" (index.erl)
2. A login page that is pure AJAX, meaning it doesn't present the user with data (login.erl)
3. A javascript, included in the homepage (login.js)
4. A callback page, which you setup with the OAuth provider (home.erl)


### Getting a Token

On your homepage, you will present the user with a simple link or button that - when clicked - will call the login() javacsript function. This function will make an AJAX request to the login page:

   function login ()
   {
	 $.ajax({ url : "/login"
         	, success : authenticate
           	, failure : login_fail
         });
   }

The login.erl script is a page that will perform the following functions:

1. Start a new oauth_fsm process
2. Link the process with the current session
3. Issue a request to get a new oauth token from the provider
4. Return the token back to the login script

	-module(login).
	-compile(export_all).
	-include_lib("nitrogen/include/wf.hrl").
	-include("include/oauth.hrl").

	-define(TOKEN_URL,"https://api.twitter.com/oauth/request_token").
	-define(CONSUMER,
	        #oauth_consumer{
		  key="6LKYdhxJg3wOC7HskIaeRg",
		  secret="yTZ94TtfROAaAq0NyrociXD1si67WMXSx0hdZdl56Y",
		  signature_method='HMAC-SHA1'
		}).

	main () ->
	  {ok,Pid}=oauth_fsm:start_link(),

	  %% start the oauth state machine by acquiring a token
	  {ok,Token}=gen_fsm:sync_send_event(Pid,{?TOKEN_URL,?CONSUMER,[]}),

	  %% save the oauth fsm with this session for use later...
	  wf:session("user",Pid),

	  %% return the token acquired as plaintext
	  wf:content_type("text/plain"),
	  Token.

The empty list in the example above is an optional list of {Key,Value} tuples that are passed to the provider in the POST, giving the provider more information about the token request. For example, Google requires a "scope" parameter be passed in, detailing what features the application will be using, which will be presented to the user for them to authenticate.


### User Login/Authentication

When the AJAX request completes successfully, the authenticate() function will be called, and the Token returned by the login page will be the solo parameter. At this point, you need to give the user a chance to log into the OAuth provider's page - if necessary - and give them the option to grant your application the ability to access their data. To do this, you need to redirect the browser to a page given to you by the OAuth provider and pass it the token:

     function authenticate (token)
     {
	location.href="http://api.twitter.com/oauth/authenticate?oauth_token" + token;
     }

If the user successfully logs in and authenticates your application, the provider will redirect the user back to your site: specifically, back to the page your provided as a callback when you registered your site with the provider. In this tutorial, that would be home.erl.

Along with redirecting the user back to your site, it also provides two additional pieces of data: the token you acquired in the previous step (used to assure that all is okay), and a verification string, used to exchange the current token in for an access token.

At this point, you need to create your callback page, home.erl...


### Acquire Access Token

Your home (callback) page will be where you exchange the current token the oauth_fsm currently has for an access token. To do this, you will need to fetch the token and verifier provided to you and pass them into oauth_fsm:

        -module(home).
	-compile(export_all).
	-include_lib("nitrogen/include/wf.hrl").
	-include("include/oauth.hrl").

	-define(ACCESS_URL,"https://apit.twitter.com/oauth/access_token").

	main () ->
          %% lookup the oauth_fsm for this session
	  Pid=wf:session("user"),

          %% get the token and verifier from the GET
          Token=wf:state("oauth_token"),
	  Verifier=wf:state("oauth_verifier"),

	  %% exchange and get access tokens
	  {ok,Reply}=gen_fsm:sync_send_event(Pid,{?ACCESS_URL,Token,Verifier}),

	  %% the user has successfully logged in!
 	  "You are now logged in!".

The Reply variable returned is a list of {Key,Value} tuples that the OAuth provider returned back with the successfull login, and is provider-specific. You can ignore them or save them off. For example, Twitter will return both the user_id and the screen_name of the user.


## Life After Login...

Once a user has successfully logged into your site, the oauth_fsm is still running, and you can use it to request information or perform account actions with the user. Each action or query performed is done by sending an event that consists of both the action URL and a list of {Key,Value} tuples, which act as the POST parameters to be sent to the provider.

     TweetURL="http://api.twitter.com/1/statuses/update.json",
     Params=[{"status","Hey, I'm tweeting from Erlang!"}],
     {ok,Json}=gen_fsm:sync_send_event(Pid,{TweetURL,Params}).

The oauth_fsm will stay in this state until the process dies (obviously) or you inidicate that you want to logout by sending (any state) the logout event:

     gen_fsm:send_event(Pid,logout).


That's it!