
## Flutter State Management - The Grand Tour

> [source: Fireship Youtube Channel](https://www.youtube.com/watch?v=3tm-R7ymwhc); [code examples](https://fireship.io/lessons/flutter-state-management-guide/)

* StatefulWidget, managing simple few states in a widget

* StatefulBuilder, manage StatefulWidget simply from a StatelessWidget with builder for local state

* StreamBuilder, [doc](https://api.flutter.dev/flutter/widgets/StreamBuilder-class.html)

* InheritedWidget, to share data from child to parent or sibling to sibling

* [RxDart](https://pub.dartlang.org/packages/rxdart) + BehaviorSubject, they always have a current value; are streamable, controllable, transformable and shareable; independent of Widget Tree; using with `get_it` allows better usability as singleton

* BLoC, similar to Redux for React and [flutter_bloc](https://pub.dartlang.org/packages/bloc) is a decent lib to use it

* Redux, `flutter_redux` Dart version of defacto from React

* Mobx, provides few more abstractions on redux removing bunch of boilerplate

* Scoped Model, to avoid heavy BLoC approach and just manage some state easily

* Flutter Hooks, similar to React Hooks

* Firebase, if need state management with bunch of extra features

---
