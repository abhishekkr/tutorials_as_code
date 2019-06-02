import 'package:flutter/cupertino.dart';
import 'package:provider/provider.dart';

import 'model/app_state_model.dart';

class ProductListTab extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Consumer<AppStateModel>(
        builder: (context, model, child) {
          return CustomScrollView(
              slivers: const <Widget>[
                CupertinoSliverNavigationBar(
                    largeTitle: Text('Coffee Shelf'),
                ),
              ],
          );
        },
    );
  }
}
