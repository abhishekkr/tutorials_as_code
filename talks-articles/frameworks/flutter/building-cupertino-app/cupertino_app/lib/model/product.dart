import 'package:flutter/foundation.dart';

enum Category {
  all,
  robusta,
  arabica,
  liberica,
}

enum Roast {
  light,
  medium,
  dark,
  blends,
}

class Coffee {
  const Coffee ({
    @required this.id,
    @required this.category,
    @required this.roast,
    @required this.isFeatured,
    @required this.name,
    @required this.price,
  }) : assert(category != null),
       assert(id != null),
       assert(isFeatured != null),
       assert(name != null),
       assert(price != null);

  final Category category;
  final Roast roast;
  final int id;
  final bool isFeatured;
  final String name;
  final double price;

  String get assetName => '$id-0.jpg';
  String get assetPackage => 'shrine_images';

  @override
  String toString() => '$name (id=$id)';
}
