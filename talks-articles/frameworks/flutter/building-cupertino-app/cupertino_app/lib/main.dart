import 'package:flutter/cupertino.dart';
import 'package:flutter/services.dart';

import 'app.dart';

void main() {
  SystemChrome.setPreferredOrientations(
      [DeviceOrientation.portraitUp, DeviceOrientation.portraitDown]);

  return runApp(CupertinoStoreApp());
}
