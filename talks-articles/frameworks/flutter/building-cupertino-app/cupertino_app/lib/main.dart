import 'package:flutter/cupertino.dart';
import 'package:flutter/services.dart';
import 'package:provider/provider.dart';

import 'app.dart';
import 'model/app_state_model.dart';

void main() {
  SystemChrome.setPreferredOrientations(
      [DeviceOrientation.portraitUp, DeviceOrientation.portraitDown]);

  return runApp(
    ChangeNotifierProvider<AppStateModel>(
      builder: (context) => AppStateModel()..loadProducts(),
      child: CupertinoStoreApp(),
    ),
  );
}
