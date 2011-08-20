// sample oauth javascript
//

var oauth_provider = "google";
var oauth_provider_auth_url = "";

function oauth_login ()
{
    $.ajax({
	url:"insert/local/url/here",
	success:oauth_authenticate,
	error:oauth_failed
    });
}

function oauth_failed ()
{
    alert("Login failed.");
}

function oauth_authenticate (token)
{
    // redirect to the provider's website so the user can login+grant access
    location.href=oauth_provider_auth_url + "?oauth_token=" + token;
}