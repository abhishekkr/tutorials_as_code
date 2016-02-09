some LinkedIn event
## Building a Performant HTML5 App
> by Trunal Bhanse

[engineering.linkedin.com](http://engineering.linkedin.com)

* Double Render Caching

* Client-Side Templating/Rendering

* Keeping your DOM memory in check

* Take memory snapshot in Chrom.DevTools.Profile before and after tasks where looking for memory leak, then analyze

* Compare CSS animation to image-ry, longer time CSS animation are heavy on CPU
*
* Compare whether LocalStorage or AppCache suits your need (they found appCache implementation buggy in some browsers)

* Static assets as DataURIs

* Constraint height of DIV to avoid flickr on animated browse (platform specific, iOS)

* club DOM modifications, use Document Fragments

---
*
### iOS side

* obj-C provides a hook to JS
* JS called hiddden iFrame inside WebMsg (MessageQ)
 
---

## Tools/Libraries

* Charles Proxy for network proxy/analysis
* iWebInspector and Weinre for JS Debugging on device/sim
* BackboneJS/UnderscoreJS
* ZeptoJS
* iScroll/TWIS
* ClampJS
* Google Diff-Patch-Match

---

