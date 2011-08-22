%% oauth.hrl
%%

%% valid types of signature methods
-type signature_method() :: 'PLAINTEXT' | 
			    'HAMC-SHA1' | 
			    'RSA-SHA1'.

%% data given by the oauth provider
-record(oauth_consumer,
	{key,
	 secret,
	 signature_method='HMAC-SHA1' :: signature_method()
	}).
