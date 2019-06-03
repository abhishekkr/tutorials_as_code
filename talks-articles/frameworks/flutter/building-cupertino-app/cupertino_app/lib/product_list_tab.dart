import 'package:flutter/cupertino.dart';
import 'package:provider/provider.dart';

import 'model/app_state_model.dart';
import 'product_row_item.dart';

class ProductListTab extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Consumer<AppStateModel>(
        builder: (context, model, child) {
          final coffees = model.getCoffees();
          return CustomScrollView(
              semanticChildCount: coffees.length,
              slivers: <Widget>[
                const CupertinoSliverNavigationBar(
                    largeTitle: Text('Coffee Shelf'),
                ),
                SliverSafeArea(
                  top: false,
                  minimum: const EdgeInsets.only(top: 8),
                  sliver: SliverList(
                    delegate: SliverChildBuilderDelegate(
                      (context, index) {
                        if (index < coffees.length) {
                          return CoffeeRowItem(
                            index: index,
                            coffee: coffees[index],
                            lastItem: index == coffees.length - 1,
                          );
                        }
                        return null;
                      },
                    ),
                  ),
                ),
              ],
          );
        },
    );
  }
}
