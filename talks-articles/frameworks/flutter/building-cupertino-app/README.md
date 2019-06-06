
## Building a Cupertino app with Flutter

> [source](https://codelabs.developers.google.com/codelabs/flutter-cupertino/index.html?index=..%2F..index#0)

[![Demo Video](./demo.png)](./demo.ogv)

### Introduction

* [Cupertino Widegts](https://docs.flutter.io/flutter/cupertino/cupertino-library.html) is iOS-style app implementing iOS design language based on Apple's Human Interface Guidelines only for Mobile apps; due to font licensing not same feel on Android

> is another option to [Material Widgets](https://docs.flutter.io/flutter/material/material-library.html) for iOS, Android, Web


### Create the Cupertino app

* `flutter create cupertino_app` to initiate

* [lib/main.dart](./cupertino_app/lib/main.dart) to import `cupertino widget` package; also import [service library](https://docs.flutter.io/flutter/services/services-library.html) for platform services like clipboard and device orientation

* [lib/styles.dart](./cupertino_app/lib/styles.dart) to centrally and separately define text/color styling

* [lib/app.dart](./cupertino_app/lib/app.dart) to add `CupertinoStoreApp` class

> returning `CupertinoApp` instance providing theming, navigation, text-direction and other defaults
>
> with `CupertinoStoreHomePage` instance as homepage

* Cupertino package provides 2 kind page scaffolds.

> * `CupertinoPageScaffold` supports single pages and accepts Cupertino style widgets and visual flow.
>
> * `CupertinoTabScaffold` supports pages with tabs.

* Update [pubspec.yaml](./cupertino_app/pubspec.yaml) for [shrine_images](https://pub.dev/packages/shrine_images) to manage image assets, [provider](https://pub.dev/packages/provider) for dependency injection, [intl](https://pub.dev/packages/intl) for internationalization, [cupertino_icons](https://pub.dev/packages/cupertino_icons) for default icons

> add an image asset in pubspec.yaml as well


### Create structure for 3-tab app

* Final app to have 3 tabs: `Product List`, `Product Search` and `Shopping Cart`

* Cupertino tab has a separate scaffold because on iOS, bottom tab is commonly persistent above nested routes instead of inside pages.

* [lib/app.dart](./cupertino_app/lib/app.dart) to be updated

> * Use `CupertinoTabScaffold` with 3 tabs to be returned with `CupertinoStoreHomePage`; also with data-source providing item list.
>
> * With `tabBar` mentioning 3 BottomNavigationBarItem  populated CupertinoTabBar; add `tabBuilder` with switch returning a CupertinoTabView for each tab.
>
> * CupertinoTabView returning CupertinoPageScaffold.

* add stub class for product list content via [lib/product_list_tab.dart](./cupertino_app/lib/product_list_tab.dart), returns `Consumer` from `provider` package assisting with state-management

> 2 types of navigation bar on iOS, common short static type and tall scrollable large title type; latter implemented inside `CustomScrollView` with a `CupertinoSliverNavigationBar` widget

* add search page stub, create [lib/search_tab.dart](./cupertino_app/lib/search_tab.dart) a state-ful widget

* add shopping cart page stub [lib/shopping_cart_tab.dart](./cupertino_app/lib/shopping_cart_tab.dart), maintains list of purchases and customer's info

* update [lib/app.dart](./cupertino_app/lib/app.dart) to import new tab widgets


### Add State Management

* for common data to be shared across multiple screens, need a simple way to flow data to each objects that need it; [scoped_model](https://pub.dartlang.org/packages/scoped_model) package allows it

* define data model used to pass data from parent widget to its descendants, wrapping model in `ScopedModel` widget allows that

* add [lib/model/product.dart](./cupertino_app/lib/model/product.dart) for product data source; each instance of `Coffee` class describes a product for sale

* add product repository at [lib/model/products_repository.dart](./cupertino_app/lib/model/products_repository.dart) for sale; with mock data better loaded  via an API

* now we can define [lib/model/app_state_model.dart](./cupertino_app/lib/model/app_state_model.dart) with `AppStateModel` class providing methods for accessing data

> add method to access cart total, a list for selected products to purchase, for shipping cost, and more

* add `app_state_model.dart` to wire `AppStateModel` at top of widget tree using `ChangeNotifierProvider`


### List products for sale

* to display products for sale in product list tab using [lib/product_row_item.dart](./cupertino_app/product_row_item.dart)

* import it in `product_list_tab.dart`, update it to show coffees using `SliverSafeArea`

* `CupertinoSliverNavigationBar` accounts for notch, `SliverSafeArea` notch left true for right and left for horizontal view if required


### Add Product Search

* add [lib/search_bar.dart](./cupertino_app/lib/search_bar.dart) with `SearchBar` class to handle search, with custom style to converge changes over iOS

* update [lib/search_tab.dart](./cupertino_app/lib/search_tab.dart) to import `provider`, `app_state_model`, `product_row_item`, `search_bar` and `styles`

> overrde `initState` and `dispose`, add `_onTextChanged` and `_buildSearchBox` methods to `_SearchTabState`
>
> then replace `CustomScrollView` in it's build to send `DecoratedBox` utilizing `_buildSearchBox`


### Add Customer Info

* update [lib/shopping_cart_tab.dart](./cupertino_app/lib/shopping_cart_tab.dart) to add private methods for building name, email and location fields with `_buildSliverChildBuilderDelegate` for `SliverSafeArea` for `CustomScrollView`


### Add date picker

* using `CupertinoDatePicker` in shopping cart so user can select a preferred date

* update [lib/shopping_cart_tab.dart](./cupertino_app/lib/shopping_cart_tab.dart)'s `_buildSliverChildBuilderDelegate` with `_buildDateAndTimePicker`


### Add selected items for purchase

* import `model/product.dart`; add `_currencyFormat` to `_ShoppingCartTabState`

* add a product index and cart display under `default`

---
