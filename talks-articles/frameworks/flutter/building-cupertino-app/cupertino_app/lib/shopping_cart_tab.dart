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
  String name;
  String email;
  String location;
  String pin;
  DateTime dateTime = DateTime.now();

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

  SliverChildBuilderDelegate _buildSliverChildBuilderDelegate(AppStateModel model) {
    return SliverChildBuilderDelegate(
        (context, index) {
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
