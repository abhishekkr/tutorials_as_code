import 'package:flutter/cupertino.dart';
import 'package:provider/provider.dart';

import 'model/app_state_model.dart';
import 'model/product.dart';
import 'styles.dart';

class CoffeeRowItem extends StatelessWidget {
  const CoffeeRowItem({
    this.index,
    this.coffee,
    this.lastItem,
  });

  final Coffee coffee;
  final int index;
  final bool lastItem;

  @override
  Widget build(BuildContext context) {
    final row = SafeArea(
        top: false,
        bottom: false,
        minimum: const EdgeInsets.only(
          left: 16,
          top: 8,
          bottom: 8,
          right: 8,
        ),
        child: Row(
          children: <Widget>[
            ClipRRect(
                borderRadius: BorderRadius.circular(4),
                child: Image.asset(
                    coffee.assetName,
                    package: coffee.assetPackage,
                    fit: BoxFit.cover,
                    width: 76,
                    height: 76,
                ),
            ),
            Expanded(
                child: Padding(
                    padding: const EdgeInsets.symmetric(horizontal: 12),
                    child: Column(
                        mainAxisAlignment: MainAxisAlignment.start,
                        crossAxisAlignment: CrossAxisAlignment.start,
                        children: <Widget>[
                          Text(
                              coffee.name,
                              style: Styles.productRowItemName,
                          ),
                          const Padding(padding: EdgeInsets.only(top: 8)),
                          Text(
                              '\$${coffee.price}',
                              style: Styles.productRowItemPrice,
                          ),
                        ],
                    ),
                ),
            ),
            CupertinoButton(
                padding: EdgeInsets.zero,
                child: const Icon(
                    CupertinoIcons.plus_circled,
                    semanticLabel: 'Add',
                ),
                onPressed: () {
                  final model = Provider.of<AppStateModel>(context);
                  model.addCoffeeToCart(coffee.id);
                },
            ),
          ],
        ),
    );

    if(lastItem) {
      return row;
    }

    return Column(
        children: <Widget>[
          row,
          Padding(
              padding: const EdgeInsets.only(
                  left: 100,
                  right: 16,
              ),
              child: Container(
                  height: 1,
                  color: Styles.productRowDivider,
              ),
          ),
        ],
    );
  }
}
