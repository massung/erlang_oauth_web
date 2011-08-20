%% oauth.hrl
%%

-record(oauth_consumer,
	{key,
	 secret,
	 signature_method='HMAC-SHA1'
	}).
