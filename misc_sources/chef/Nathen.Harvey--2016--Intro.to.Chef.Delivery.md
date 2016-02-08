# Introduction to Chef Delivery
> by Nathen Harvey

* Shared Workflow
> Delivery's pipeline is shared across projects and teams.

```
 [ Application -A- ]________________________________________
 | Submit \ Verify \ Approve \ Build \ Acceptance \ Deliver \
 | Change /        /         /       /            /         /\
 '-------'--------'---------'-------'------------'---------'  \
                                                               \
                                                                \
 [ Application -B- ]________________________________________     \.-------. .-------. .-------.
 | Submit \ Verify \ Approve \ Build \ Acceptance \ Deliver \     |       | |       | |       |
 | Change /        /         /       /            /         /\    |       | |       | |       |
 '-------'--------'---------'-------'------------'---------'  \   | Union | | Rehe- | | Deli- |
                                                               '--|       |-| arsa- |-| vere- |
                                                               ,--|       |-| l     |-| d     |
 [ Cookbook -C- ]___________________________________________  /   |       | |       | |       |
 | Submit \ Verify \ Approve \ Build \ Acceptance \ Deliver \/   /|       | |       | |       |
 | Change /        /         /       /            /         /   / '-------' '-------' '-------'
 '-------'--------'---------'-------'------------'---------'   /
                                                              /
                                                             /
 [ Cookbook -D- ]___________________________________________/
 | Submit \ Verify \ Approve \ Build \ Acceptance \ Deliver \
 | Change /        /         /       /            /         /
 '-------'--------'---------'-------'------------'---------'
```

* Unified Pipeline Shape
> fixed stages, each stage with fixed phases

```
 ,-------,--------,---------,-------,------------,---------,-------,-----------,-----------,
 | Submit \ Verify \ Approve \ Build \ Acceptance \ Deliver \ Union \ Rehearsal \ Delivered \
 | Change /        /  [:)]   /       /            / [:)]    /       /           /           /
 '-------'--------'---------'-------'------------'---------'-------'-----------'-----------'
```

---

#### Stages with their phases

> * Verify
> > Phases of 'Verify' stage: 'Lint', 'Syntax', 'Unit'
> > Lint via tools like RuboCop, FoodCritic.
> > Generally best practice is this happening on developer instance on a private branch.

> * to 'Approve' if change looks good; when following feature branches here merge to Main Branch

> * Build
> > Phases of 'Build' stage: 'Lint', 'Syntax', 'Unit', 'Security', 'Quality', 'Publish'
> > Running Lint, Syntax and Unit again if feature branches in action.

> * Acceptance
> > Phases of 'Acceptance' stage: 'Provision', 'Deploy', 'Smoke', 'Functional'

> * to 'Deliver', if we wanna ship it

> * Union
> > Phases of 'Union' stage: 'Provision', 'Deploy', 'Smoke', 'Functional'

> * Rehearsal
> > Phases of 'Rehearsal' stage: 'Provision', 'Deploy', 'Smoke', 'Functional'

> * Delivered
> > Phases of 'Delivered' stage: 'Provision', 'Deploy', 'Smoke', 'Functional'

---

* Each project to have a 'Build' cookbook (recipes for lint, syntax and unit test), can be shared across project

* Deliver project = build cookbook + project config + project code

---
---
