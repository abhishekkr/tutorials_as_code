import 'package:flutter/cupertino.dart';
import 'package:provider/provider.dart';

import 'model/app_state_model.dart';

class ShoppingCartTab extends StatefulWidget {
  @override
  _ShoppingCartTabState createState() {
    return _ShoppingCartTabState();
  }
}

class _ShoppingCartTabState extends State<ShoppingCartTab> {
  @override
  Widget build(BuildContext context) {
    return Consumer<AppStateModel>(
        builder: (context, model, child) {
          return CustomScrollView(
              slivers: const <Widget>[
                CupertinoSliverNavigationBar(
                    largeTitle: Text('Cart'),
                ),
              ],
          );
        }
    );
  }
}
