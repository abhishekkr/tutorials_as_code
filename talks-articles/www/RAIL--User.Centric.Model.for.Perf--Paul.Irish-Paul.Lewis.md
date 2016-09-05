
## Introducing RAIL : A User-Centric Model For Performance
> by, Paul Irish & Paul Lewis

[source](https://www.smashingmagazine.com/2015/10/rail-user-centric-model-performance/)

> RAIL ~ Response, Animate, Idle, Load
> 4 distinct areas to reason about actions in websites and apps, to be optimized

* problem with loading time, pople measure speed index
> some measure till first paint
> others use `body.unload`, `DOMContentLoaded`, ...

Chrome team comes up with `RAIL` model
> * RAIL is a model breaking down user's experience into `key actions`
> * RAIL provide perf goals for these `key actions`

---

#### What Performance

* Slow
> Slow is what interaction `user` feels lacking. Not the loops or download times.

* Placing use in center of Performance, using [HCI Research](http://www.nngroup.com/articles/response-times-3-important-limits/)
> * 100millisec : time-window for response to a user-interaction for instant feel
> * 1sec : user will lose focus on task past this window
> * 16millisec : given screen update 60fps, its time to get single frame to screen

---

### RAIL Performance Model

* Response
> provide feedback in `>100ms` after initial input
> ideally the desired state, but the feedback could be loading indicator or coloring active state if it'll take longer time
> key is to acknowledge and engage

* Animation
> includes visual animation, scrolling, drag
> achive 60fps, each frame finished in `>=16ms`

* Idle
> all work that need to happen doesn't need to complete within single time window
> to use idle time wisely, group work in chunks of `50millisec`
> so if a user starts interacting, response can happen within `100ms`

* Load
> aim to deliver first meaningful paint under `1sec`, so user stays engaged
> requires prioritizing [critical rendering path](https://developers.google.com/web/fundamentals/performance/critical-rendering-path/?hl=en)

---

Technology as a standard to measure it need to be average of their times, like
* Nexus 4
* regular 3G

---

##### References for more

[udacity](https://www.udacity.com/course/ud860)

[google dev web-tools](https://developers.google.com/web/tools/profile-performance/evaluate-performance/rail?hl=en)

---
---
