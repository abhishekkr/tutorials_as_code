
## Advanced Technologies for Detecting and Preventing Fraud at Uber

> [source](https://eng.uber.com/advanced-technologies-detecting-preventing-fraud-uber/), [video](https://www.youtube.com/watch?v=TwL3IbqhHfA&t=0s) | published: 14/June/2018

* got collaborative dedicated team of anti-fraud analysts, data scientists & UX experts

* to differentiate between actual trips and those created by GPS spoofing, or analyze how our apps are being used to reveal feaudsters


### Types of Fraud

* Payment Fraud

> when mal-users use stolen payment details; to maximize profit from stolen details fraudsters pass them on for price


* Incentive Abuse

> mal-users taking advantage of incentives offered on invites sent (by faking users) or orders completed (by faking trips)


* Compromised Accounts

> with access to real accounts using phishing or other techniques


### Detection Systems: GPS Spoofing Detection

*  mal-users GPS spoofing apps to create fake locations on a phone in order to simulate a real trip; or fake the rider as well with stolen credit details; sometime to earn an incentive completing X numbers of trips

> these GPS spoofing could be detected and blocked

* have an altitude profile of all geographies around world by aggregating historical trip data; real trip aligns closely to earth while the fake trip are more flying or undeground

* comparing trip's profile to speed profile, checking what percent of trip had abnormal speed

* location integrity is a complex task; identify region with low trip probability showing fraudlent high sign-up rates

> * combining these with financial loss, device information & trip-level/user-level features; trips can be sampled for manual review
>
> * ML model to detect trips with GPS spoofing
>
> * DL model for anomaly detection and reduce effort of engineering new features


### Sequence Modeling to classify user behavior

* interaction pattern differ between normal users and fraudsters which can distinguised using `LSTM` (Long Short Term Memory) deep learning models

> tap-stream data can be viewed as time-series, using [one-hot](https://en.wikipedia.org/wiki/One-hot) encoding to represent each tap
>
>  these vectors input to `LSTM` model, final activation layer is a probability score predicting sequence of taps are from a normal user or fraudster

---
