## Performance Checklist for the Mobile Web
> by, Colt McAnlis (Developer Advocate, Google)

[#perfmatters](http://goo.gl/dq2Vr)

* User Truth: 3 second WebPage load
> 47% users leave; 80% of those decide not to visit again; 50% of those decide to influence friends
> 60fps good UX; 30fps discontented; 30-45fps infuriated
> so its not just Internet size but also Compute(GC), Render

---

### Network Perf
> Mobile Telecom Network Bandwidth managed horrendously

* Trouble with images
> different devices, different resolutions
> use help of Webp

* Content is too large
> serve GZIPd; SPDY connection
> use PageSpeed to run suggestive analysis
> WebPageTest.org to online analyze your webpages quickly

* tryout AppCache

---

### Render Perf

* What's your render page height
> heavy css paint times are like excess carbohyddrates
> box-shadow and box-radius-stroke together takes exponential time

* Trouble with images
> decode-to-resizes take more time on every re-rasterization

* Reflows are the devil
> everytime you modify a DOM property it recalculates everything
> so loop-y updating DOM nodes will get DOM (read.reflow.write--) render stuck (try read.write-write--)
> use chrome://tracing and Devtools-timeline to analyze

---

### Compute Performance

* Static-Memory Javascript
> know your max-usage
> mem-profile should be as less jagged as possible, every JS run GC could be called
> to deal with it, Emscriptem pre-allocate huge mem block to itself
> know your growth-rate and pre-allocate that object
> try object-pool

---

#### Conclusions

* Use advanced data-compression for image-transfer
* Use proper xfer protocols for fast-as-possible speeds
* Batch network communication through the radio
* Reduce page render weight and image-resice time
* Batch read-writes to DOM
* Move towards static-mem usage patterns (know your growth rates, max-mem, preallocate)

* setting debug on Android: http://goo.gl/QJoq9
* setting trace capture on phone: http://goo.gl.1qcMN

---

