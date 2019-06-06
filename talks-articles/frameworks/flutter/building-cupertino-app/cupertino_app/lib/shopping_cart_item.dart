import 'package:flutter/cupertino.dart';
import 'package:intl/intl.dart';
import 'package:provider/provider.dart';

import 'model/product.dart';
import 'styles.dart';

class ShoppingCartItem extends StatelessWidget {
  const ShoppingCartItem({
    @required this.index,
    @required this.product,
    @required this.lastItem,
    @required this.quantity,
    @required this.formatter,
  });

  final int index;
  final Coffee product;
  final bool lastItem;
  final int quantity;
  final NumberFormat formatter;

  @override
  Widget build(BuildContext context) {
    final row = SafeArea(
        top: false,
        bottom: false,
        child: Padding(
            padding: const EdgeInsets.fromLTRB(16, 8, 8, 8), // left, top, right, bottom
            child: Row(
                children: <Widget>[
                  ClipRRect(
                      borderRadius: BorderRadius.circular(4),
                      child: Image.asset(
                          product.assetName,
                          package: product.assetPackage,
                          fit: BoxFit.cover,
                          width: 40,
                          height: 40,
                      ),
                  ),
                  Expanded(
                      child: Padding(
                          padding: const EdgeInsets.symmetric(horizontal: 12),
                          child: Column(
                              mainAxisAlignment: MainAxisAlignment.start,
                              crossAxisAlignment: CrossAxisAlignment.start,
                              children: <Widget>[
                                Row(
                                    mainAxisAlignment: MainAxisAlignment.spaceBetween,
                                    children: <Widget>[
                                      Text(product.name, style: Styles.productRowItemName),
                                      Text('${formatter.format(quantity * product.price)}', style: Styles.productRowItemName)
                                    ],
                                ),
                                const SizedBox(height: 4),
                                Text(
                                    '${quantity > 1 ? '$quantity x' : ''}'
                                      '${formatter.format(product.price)}',
                                    style: Styles.productRowItemName,
                                ),
                              ],
                          ),
                      ),
                  ),
                ],
            ),
        ),
    );

    return row;
  }
}
