
## Netflix Recommendations: Beyond the 5 stars (Part1)

> 6/Apr/2012 [source: part 1](https://medium.com/netflix-techblog/netflix-recommendations-beyond-the-5-stars-part-1-55838468f429)
>
> 20/Apr/2012 [source: part2](https://medium.com/@Netflix_Techblog/netflix-recommendations-beyond-the-5-stars-part-2-d9b96aa399f5)


> part.1

* a proxy question easier to evaluate and quantify, the `root mean sqaured error (RMSE)` of predicted rating

* winning solution was final combination of 107 algorithms; 2 underlying algo with best performance were `Matrix Factorization (which the community generally called SVD, Singular Value Decomposition)` and `Restricted Boltzmann Machines (RBM)`

> linear blend of these two were built to handle 100 million ratings, instead of more than 5 billion as data... they were overcome

* more than 23 million subscribers in 47 countries, streamed 2 billion hours in last quarter of 2011

* everything is a recommendation, every item in a row is recommendation based on a key item

* even top 10 rows based on group's choice that plays role in selection of items from that account

* explainations on all recommendations for user to be aware of why a recommendation is shown

* knowing about social circle (FB linking) gives more understanding to what user might want to watch

* most recognizable recommendation happens with genre rows

* each row represent 3 layers of personalization; genre, subset of titles within those genres and ranking of those titles

* similarities could varied, from titles to users; in multiple dimensions from metadata to viewing data

* similarities used to generate adhoc-genres


> part.2

* ranking on overall consumption as a baseline would create same recommendation for all, although mostly users do want to try popular things

* using user's predicted rating for an item as adjunct to item popularity, with different weights

* weights idenitified by learning from historical data using ML problem known as [Learning to Rank](http://en.wikipedia.org/wiki/Learning_to_rank)

* many supervised classification methods can be used like `Logical Regression`, `Support Vector Machines`, `Neural Network` or `Detection Tree-based methods` like `Gradient Boosted Decision Tree (GBDT)`

* learning to rank algorithms have come up like `RankSVM` or `RankBoost`

* some data-sources used to optimize recommendations:

> * several billion item ratings from members, millions new rating daily
>
> * compute popularity over varied time-ranges, group members by region or other similarity metric
>
> * million stream play metrics each day including duration, time-of-day, device
>
> * metrics on users engaging with items with metadata
>
> * metrics on how recommendations affected user's decision
>
> * social data on what user's links are watching
>
> * search terms entered by user; or entered in same demographics, location, language or temporal

* Some of methods to know in ML for personalization:

> * Linear Regression
>
> * Logistic Regression
>
> * Elastic nets
>
> * Singular Value Decomposition
>
> * Restricted Boltzmann Machines
>
> * Markov Chains
>
> * Latent Dirichlet Allocation
>
> * Association Rules
>
> * Gradient Boosted Decision Tress
>
> * Random Forests
>
> * Clusering techniques from simple k-means to novel graphical approaches such as Affinity Propagation
>
> * Matrix Factorization

* if you want to increase your success rate, increase your failure rate

> * start with a hypothesis
>
> * design a test
>
> *  execute the test and let data decide the flow

---
