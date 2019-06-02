import 'product.dart';

class CoffeeRepository {
  static const _allCoffee = <Coffee>[
    Coffee(
      category: Category.robusta,
      roast: Roast.medium,
      id: 0,
      isFeatured: true,
      name: 'Kahawa Sug (Phillipine)',
      price: 700,
    ),
    Coffee(
      category: Category.arabica,
      roast: Roast.medium,
      id: 1,
      isFeatured: true,
      name: 'Sagada Arabica',
      price: 580,
    ),
    Coffee(
      category: Category.arabica,
      roast: Roast.dark,
      id: 2,
      isFeatured: true,
      name: 'Benguet Arabica',
      price: 790,
    ),
    Coffee(
      category: Category.liberica,
      roast: Roast.dark,
      id: 1,
      isFeatured: true,
      name: 'Kapeng Barako',
      price: 930,
    ),
    Coffee(
      category: Category.arabica,
      roast: Roast.light,
      id: 1,
      isFeatured: true,
      name: 'Bourbon',
      price: 580,
    ),
    Coffee(
      category: Category.arabica,
      roast: Roast.light,
      id: 1,
      isFeatured: true,
      name: 'Maragogipe',
      price: 1180,
    ),
  ];

  static List<Coffee> loadCoffees(Category category) {
    if (category == Category.all) {
      return _allCoffee;
    } else {
      return _allCoffee.where((p) => p.category == category).toList();
    }
  }
}
