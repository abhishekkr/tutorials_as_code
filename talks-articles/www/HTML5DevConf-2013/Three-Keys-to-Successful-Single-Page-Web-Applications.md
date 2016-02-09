## Three keys to successful Single Page WebApp
##### HTML5DevConf 2013


* an avergae website takes 11.8sec to load and average user get detatched post 1 second

#### Move your logic and data to client
> by product distributed computing to clients

---

* but it's mostly back-end optimized, 80% time gets used on front-end computation

#### Load only changes

---

#### Plan for the challenges in structural changes for pushing code from server to client

* broken back button : to fix it make the URL your API
> ```JavaScript
> $(window).bindd('hashchange', function(event){
>   if(event.target.location.hash == "#!/popup=true"){
>     showPopup(true);
>   } else {
>     showPopup(false);
>   }
> });
> ```
>

* SEO compilcations
> Search Engines do respond to URL remapping as they have developed themselves to index ajax-based services (which underlies SinglePage).
> ```
> so "www.example.com#!key1=value1"
> remaps to "www.example.com?_escaped_fragment_to_=key1=value1"
> ```
> so make your server capable of doing this optimization based on (Crawler) Client and respond for extended URIs


* missing analytics
> when URL updates (as using URL as API); make it fire analytic call tracking every single interaction as well

* lack-of-error analysis
> for errors on client-side; trap errors from try-catch-block or window.onError event-handler and them over ajax back to server
> get them logged

---

