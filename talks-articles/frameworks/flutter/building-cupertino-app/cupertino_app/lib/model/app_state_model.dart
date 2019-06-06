import 'package:flutter/foundation.dart' as foundation;

import 'product.dart';
import 'products_repository.dart';

double _salesTaxRate = 0.09;
double _shippingCostPerItem = 15;
const int coffeesStartId = 10;

class AppStateModel extends foundation.ChangeNotifier {
  List<Coffee> _availableCoffees;

  Category _selectedCategory = Category.all;

  final _coffeesInCart = <int, int>{};

  Map<int, int> get coffeesInCart {
    return Map.from(_coffeesInCart);
  }

  int get totalCartQuantity {
    return _coffeesInCart.values.fold(0, (accumulator, value){
      return accumulator + value;
    });
  }

  Category get selectedCategory {
    return _selectedCategory;
  }

  double get subtotalCost {
    return _coffeesInCart.keys.map((id){
      return _availableCoffees[id - coffeesStartId].price * _coffeesInCart[id];
    }).fold(0, (accumulator, extendedPrice){
      return accumulator + extendedPrice;
    });
  }

  double get shippingCost {
    return _shippingCostPerItem * 
           _coffeesInCart.values.fold(0.0, (accumulator, itemCount) {
             return accumulator + itemCount;
           });
  }

  double get tax {
    return subtotalCost * _salesTaxRate;
  }

  double get totalCost {
    return subtotalCost + shippingCost + tax;
  }

  List<Coffee> getCoffees() {
    if (_availableCoffees == null) {
      return [];
    }

    if (_selectedCategory == Category.all) {
      return List.from(_availableCoffees);
    } else {
      return _availableCoffees.where((p) {
        return p.category == _selectedCategory;
      }).toList();
    }
  }

  List<Coffee> search(String searchTerms) {
    return getCoffees().where((coffee) {
      return coffee.name.toLowerCase().contains(
          searchTerms.toLowerCase()
          );
    }).toList();
  }

  void addCoffeeToCart(int coffeeId) {
    if (!_coffeesInCart.containsKey(coffeeId)) {
      _coffeesInCart[coffeeId] = 1;
    } else {
      _coffeesInCart[coffeeId]++;
    }
    notifyListeners();
  }

  void removeCofeeFromCart(int coffeeId) {
    if (_coffeesInCart.containsKey(coffeeId)) {
      if (_coffeesInCart[coffeeId] == 1) {
        _coffeesInCart.remove(coffeeId);
      } else {
        _coffeesInCart[coffeeId]--;
      }
    }
    notifyListeners();
  }

  Coffee getCoffeeById(int id) {
    return _availableCoffees.firstWhere((coffee) => coffee.id == id);
  }

  void clearCart() {
    _coffeesInCart.clear();
    notifyListeners();
  }

  void loadProducts() {
    _availableCoffees = CoffeeRepository.loadCoffees(Category.all);
    notifyListeners();
  }

  void setCategory(Category newCategory) {
    _selectedCategory = newCategory;
    notifyListeners();
  }
}
