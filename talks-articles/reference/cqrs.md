
## Command Query Resonsibility Segregation (_CQRS_)

> sources: [martinfowler.com](https://martinfowler.com/bliki/CQRS.html), [greg young](http://codebetter.com/gregyoung/2010/02/16/cqrs-task-based-uis-event-sourcing-agh/)

* using a different model to update info and different to read, not for all systems

* when you want to look at info in different form than CRUD

> for example if you wanna process your incoming data into some other form, and for retrieval want to use a different model for existing form

* fits well with event-based programming models, CQRS system split into different services communicating with `event collaboration`

* when implementing [EagerReadDerivation](https://martinfowler.com/bliki/EagerReadDerivation.html) strategy

* should be used in a [Bounded Context](https://martinfowler.com/bliki/BoundedContext.html)

* well suited to cases like task-based UIs, data is not just copied but pre-dealt with behaviors on it

* there is mostly no need to use messaging patterns

* eventual consistency is introduced within services

---
