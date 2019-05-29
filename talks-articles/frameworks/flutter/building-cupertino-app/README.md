
## Building a Cupertino app with Flutter

> [source](https://codelabs.developers.google.com/codelabs/flutter-cupertino/index.html?index=..%2F..index#0)

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

* Update [pubspec.yaml](./cupertino_app/pubspec.yaml) for [shrine_images](https://pub.dev/packages/shrine_images) to manage image assets, [provider](https://pub.dev/packages/provider) for dependency injection, [intl](https://pub.dev/packages/intl) for internationalization, [cupertino_icons](https://pub.dev/packages/cupertino_icons) for default icons

> add an image asset in pubspec.yaml as well

---
