
## FoodCritic

```
foodcritic .
rubocop .
```

* can check details of critic given of 'FCXYZ' at [http://www.foodcritic.io/#FCXYZ](http://www.foodcritic.io/#FCXYZ)

* to check for a rule

```
foodcritic -t FCXYZ .
```

* to check for all rules excpet some

```
foodcritic -t FCXYZ .
```

* rules are tagged to they can be used similarly

```
foodcritic -t services -t ~style .
```

* these tags can be made default by dot-files

```
echo "services,~style,~FC003" >> .foodcritic
```

---
---
