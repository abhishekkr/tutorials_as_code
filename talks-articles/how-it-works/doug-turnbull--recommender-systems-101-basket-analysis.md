
## Recommender Systems 101: Basket Analysis

> by Doug Turnbull (6/Jun/2016), [source](https://opensourceconnections.com/blog/2016/06/06/recommender-systems-101-basket-analysis/)

> for the `buyers also bought` recommendations to drive better picking of choices alongwith already selected item

### Basket Analysis

> literally means analyzing shoppers baskets, count how many items purchased together to guess which items go together

#### Can we use raw counts?

* let's take example with following baskets

```
|Basket A    |Basket B    |Basket C    |Basket D    |Basket E    |
|------------|------------|------------|------------|------------|
|Eggs        |Eggs        |Eggs        |Eggs        |Eggs        |
|            |Milk        |Milk        |Milk        |Milk        |
|Pie Crust   |Pie Crust   |Pie Crust   |Pie Crust   |Pie Crust   |
|Green Apples|Green Apples|Pumpking    |            |            |
+------------+------------+------------+------------+------------+
```

* filtering for items that go together wih `Pie Crust`, recommend most common clustered items from those baskets

* examining counts suggest `Eggs` as most matched item, but that could be just because almost everyone uses them and not necessarily with `Pie Crust`

* so, using just raw counts will always bias towards `global popularity` and not what's special to selected subset of data... this problem is termed as `Oprah book club problem` where sales of book might just increase because a celebrity mentioned it doesn't mean it's actually worthy


### Naive similarity measures (Jaccard & z-tests)

* another approach is trying a Venn Diagram with Jaccard Similarity, that is overlap area when very close to total number of baskets with selected item or evaluated item then something is statistically similar

> given 2 sets A & B, Jaccard Similarity is `intersection of A & B` divided by `union of A with B`
>
> restating matehmatically, `A AND B / A OR B`

* if items always occur in baskets together, their similarity would be `1`

* compare items never existing together

#### Problems with Jaccard

* some items are picked by everyone, there's an extremely long tail of extremely rare picks

> * when rare items overlap, Jaccard similarity scores relationship very high
>
> * obvious problem is seeing if enough occurences of each item have occured to make their combination significant

#### Other Significance Measures

* `Null Hypothesis` says one event has no relationship to another. Attempt is to show this is unlikely.

* Have a minimum occurence of combination, to start considering similarity.


### Log Likelihood Similarity

* Ted Dunning suggests `binomial distribution` is more appropriate.

> It tracks frequency at which coin flip lands `m times`in a row after `N flips`.
>
> Each item is weighted by its frequency of being picked.
>
> Compared to normal distribution, when looked at probability of rare event in numerous trial... it is more probable.

#### Computing Log Likelihood

* `log likelihood` is a general statistical significance test

* an interpretation of calculating `log likelihood`

> * step away from probabilities
>
> * divide up **all** baskets into 4 possible grouping, depending whether or not one item or another or both have been purchased
>
> say for picked item `PB` and evaluated item `Jelly`;
>
> group.1 of basket (95)count with `PB` & `Jelly`,
>
> group.2 of basket (5)count with `PB` but without `Jelly`,
>
> group.3 of basket (5)count without `PB` but with `Jelly`,
>
> group.4 of basket (9985)count without `PB` & `Jelly`
>
> we see `{with both count} / ({with PB, w/o jelly} + {with both count})` i.e. `95/100` or `0.95` as score for purchase of `PB` with `Jelly`
>
> comparing this to purchase of `PB` without any `Jelly` as `{with PB count} / ({with PB, w/o Jelly} + {w/o PB or Jelly})` as `5/99900` or `~0.0005`
>
> this states `null hypothesis` doesn't stand ground

* in above example the information is very clear with very less entropy

> Takeaways:
>
> * it's good when rows & columns sum to something close (picked item is close to where not picked; evaluated item to where not)
>
> * it's bad when table values are all very close
>
> * it's good when tere's more total baskets (as potential entropy gets higher)

---

> References: [Shannon's Entropy | Mahout Code](https://github.com/twitter/mahout/blob/master/math/src/main/java/org/apache/mahout/math/stats/LogLikelihood.java#L70), [Ted Dunning's blogpost 'Surprise & Coincidence'](http://tdunning.blogspot.com/2008/03/surprise-and-coincidence.html)

---
