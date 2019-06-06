import 'package:flutter/cupertino.dart';
import 'package:intl/intl.dart';
import 'package:provider/provider.dart';

import 'model/app_state_model.dart';
import 'model/product.dart';
import 'shopping_cart_item.dart';
import 'styles.dart';

const double _kDateTimePickerHeight = 216;

class ShoppingCartTab extends StatefulWidget {
  @override
  _ShoppingCartTabState createState() {
    return _ShoppingCartTabState();
  }
}

class _ShoppingCartTabState extends State<ShoppingCartTab> {
  String name;
  String email;
  String location;
  String pin;
  DateTime dateTime = DateTime.now();

  final _currencyFormat = new NumberFormat.currency(symbol: 'Rp');

  Widget _buildNameField(){
    return CupertinoTextField(
        placeholder: 'Name',
        autocorrect: false,
        textCapitalization: TextCapitalization.words,
        clearButtonMode: OverlayVisibilityMode.editing,
        padding: const EdgeInsets.symmetric(horizontal: 6, vertical: 12),
        onChanged: (newName) {
          setState(() {name = newName;});
        },
        prefix: const Icon(
          CupertinoIcons.person_solid,
          color: CupertinoColors.lightBackgroundGray,
          size: 28,
        ),
        decoration: const BoxDecoration(
            border: Border(
                bottom: BorderSide(
                    width: 0,
                    color: CupertinoColors.inactiveGray,
                ),
            ),
        ),
    );
  }

  Widget _buildEmailField(){
    return CupertinoTextField(
        placeholder: 'Email',
        keyboardType: TextInputType.emailAddress,
        clearButtonMode: OverlayVisibilityMode.editing,
        padding: const EdgeInsets.symmetric(horizontal: 6, vertical: 12),
        prefix: Icon(
          CupertinoIcons.mail_solid,
          color: CupertinoColors.lightBackgroundGray,
          size: 28,
        ),
        decoration: BoxDecoration(
            border: Border(
                bottom: BorderSide(
                    width: 0,
                    color: CupertinoColors.inactiveGray,
                ),
            ),
        ),
    );
  }

  Widget _buildLocationField(){
    return CupertinoTextField(
        placeholder: 'Location',
        clearButtonMode: OverlayVisibilityMode.editing,
        textCapitalization: TextCapitalization.words,
        padding: const EdgeInsets.symmetric(horizontal: 6, vertical: 12),
        prefix: Icon(
          CupertinoIcons.location_solid,
          color: CupertinoColors.lightBackgroundGray,
          size: 28,
        ),
        decoration: BoxDecoration(
            border: Border(
                bottom: BorderSide(
                    width: 0,
                    color: CupertinoColors.inactiveGray,
                ),
            ),
        ),
    );
  }

  Widget _buildDateAndTimePicker(BuildContext context) {
    return Column(
        children: <Widget>[
          Row(
              mainAxisAlignment: MainAxisAlignment.spaceBetween,
              children: <Widget>[
                Row(
                    mainAxisAlignment: MainAxisAlignment.start,
                    children: const <Widget>[
                      Icon(
                          CupertinoIcons.clock,
                          color: CupertinoColors.lightBackgroundGray,
                          size: 28,
                      ),
                      SizedBox(width: 6),
                      Text(
                          'Delivery Time',
                          style: Styles.deliveryTimeLabel,
                      ),
                    ],
                ),
                Text(
                    DateFormat.yMMMd().add_jm().format(dateTime),
                    style: Styles.deliveryTimeLabel,
                ),
              ],
          ),
          Container(
              height: _kDateTimePickerHeight,
              child: CupertinoDatePicker(
                  mode: CupertinoDatePickerMode.dateAndTime,
                  initialDateTime: dateTime,
                  onDateTimeChanged: (newDateTime) {
                    setState(() { dateTime = newDateTime; });
                  },
              ),
          ),
        ],
    );
  }

  SliverChildBuilderDelegate _buildSliverChildBuilderDelegate(AppStateModel model) {
    return SliverChildBuilderDelegate(
        (context, index) {
          final productIndex = index - 4;

          switch(index) {
            case 0:
              return Padding(
                  padding: const EdgeInsets.symmetric(horizontal: 16),
                  child: _buildNameField(),
              );
            case 1:
              return Padding(
                  padding: const EdgeInsets.symmetric(horizontal: 16),
                  child: _buildEmailField(),
              );
            case 2:
              return Padding(
                  padding: const EdgeInsets.symmetric(horizontal: 16),
                  child: _buildLocationField(),
              );
            case 3:
              return Padding(
                  padding: const EdgeInsets.fromLTRB(16, 8, 16, 24),
                  child: _buildDateAndTimePicker(context),
              );
            default:
              if (model.coffeesInCart.length > productIndex) {
                return ShoppingCartItem(
                  index: index,
                  formatter: _currencyFormat,
                  quantity: model.coffeesInCart.values.toList()[productIndex],
                  lastItem: productIndex == model.coffeesInCart.length - 1,
                  product: model.getCoffeeById(model.coffeesInCart.keys.toList()[productIndex]),
                );
              } else if (model.coffeesInCart.keys.length == productIndex &&
                        model.coffeesInCart.isNotEmpty) {
                return Padding(
                    padding: const EdgeInsets.symmetric(horizontal: 20),
                    child: Row(
                        mainAxisAlignment: MainAxisAlignment.end,
                        children: <Widget>[
                          Column(
                              crossAxisAlignment: CrossAxisAlignment.end,
                              children: <Widget>[
                                Text(
                                    'Shipping '
                                      '${_currencyFormat.format(model.shippingCost)}',
                                    style: Styles.productRowItemPrice,
                                ),
                                const SizedBox(height: 6),
                                Text(
                                    'Tax '
                                      '${_currencyFormat.format(model.tax)}',
                                    style: Styles.productRowItemPrice,
                                ),
                                const SizedBox(height: 6),
                                Text(
                                    'Total '
                                      '${_currencyFormat.format(model.totalCost)}',
                                    style: Styles.productRowTotal,
                                ),
                              ],
                          ),
                        ],
                    ),
                );
              }
          }
        },
    );
  }

  @override
  Widget build(BuildContext context) {
    return Consumer<AppStateModel>(
        builder: (context, model, child) {
          return CustomScrollView(
              slivers: <Widget>[
                const CupertinoSliverNavigationBar(
                    largeTitle: Text('Cart'),
                ),
                SliverSafeArea(
                    top: false,
                    minimum: const EdgeInsets.only(top: 4),
                    sliver: SliverList(
                        delegate: _buildSliverChildBuilderDelegate(model),
                    ),
                )
              ],
          );
        }
    );
  }
}
