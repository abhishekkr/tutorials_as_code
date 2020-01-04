
## Building your first Flutter Widget

> [source: Google Developers Youtube](https://www.youtube.com/watch?v=W1pNjxmNHNQ)

* skeleton of a new StatelessWidget, good to use on construction

```
import 'package:flutter/foundation.dart';
import 'package:flutter/material.dart';

class FancyButton extends StatelessWidget {
  // let people click you
}
```

* to give button a callback on press

```
final GestureTapCallback onPressed;
```

* a constructor to inject callback

```
FancyButton(this.onPressed); // means constructor shall have a param matching property, handles assignment

FancyButton({@required this.onPressed}); // braces to make it optional named param, @required to make mandatory
```

* override `Widget build(BuildContext ctx) {...}` to allow send custom children content

---

### First Step Runnable Widget

* current widget code

```
import 'package:flutter/foundation.dart';
import 'package:flutter/material.dart';

class FancyButton extends StatelessWidget {
  FancyButton({@required this.onPressed});

  final GestureTapCallback onPressed;

  @override
  Widget build(BuildContext ctx) {
    return Text("Click Me Now");
  }
}
```

* usage

```
import 'fancy_button.dart';

...

  return Scaffold(
    body: Center(
      child: ListView(
        children: _buildProductList(context),
      ),
    ),
    floatingActionButton: FancyButton(
      onPressed: _addProductToCart,
    ),
  );
```

---

### Updated Widget with RawMaterialButton

```
import 'package:flutter/foundation.dart';
import 'package:flutter/material.dart';

class FancyButton extends StatelessWidget {
  FancyButton({@required this.onPressed});

  final GestureTapCallback onPressed;

  @override
  Widget build(BuildContext ctx) {
    return RawMaterialButton(
      fillColor: Colors.deepOrange,
      splashColor: Colors.orange,
      child: Text("Click Me Now"
        style: TextStyle(color: Colors.white),
      ),
      onPressed: onPressed,
      shape: const StadiumBorder(),
    );
  }
}
```

---

### Updated Widget with Padding

```
import 'package:flutter/foundation.dart';
import 'package:flutter/material.dart';

class FancyButton extends StatelessWidget {
  FancyButton({@required this.onPressed});

  final GestureTapCallback onPressed;

  @override
  Widget build(BuildContext ctx) {
    return RawMaterialButton(
      fillColor: Colors.deepOrange,
      splashColor: Colors.orange,
      child: Padding(
        padding: const EdgeInsets.symmetric(
          vertical: 10.0,
          horizontal: 16.0,
        ),
        child: Text("Click Me Now"
          style: TextStyle(color: Colors.white),
        ),
      ),
      onPressed: onPressed,
      shape: const StadiumBorder(),
    );
  }
}
```

---

### Widget with Text and Icon

* padding takes a single child, so needs a container for multiple widgets

```
import 'package:flutter/foundation.dart';
import 'package:flutter/material.dart';

class FancyButton extends StatelessWidget {
  FancyButton({@required this.onPressed});

  final GestureTapCallback onPressed;

  @override
  Widget build(BuildContext ctx) {
    return RawMaterialButton(
      fillColor: Colors.deepOrange,
      splashColor: Colors.orange,
      child: Padding(
        padding: const EdgeInsets.symmetric(
          vertical: 10.0,
          horizontal: 16.0,
        ),
        child: Row(
          mainAxisSize: MainAxisSize.min,
          children: const <Widget>[
            Icon(Icons.explore,
              color: Colors.amber,
            ),
            SizedBox(width: 8.0), // to get a little space between icon and text
            Text("Click Me Now"
              style: TextStyle(color: Colors.white),
            ),
          ],
        ),
      ),
      onPressed: onPressed,
      shape: const StadiumBorder(),
    );
  }
}
```

---

* can have code like below to keep track of gestures using `GestureDetector` widget

```
GestureDetector(
  child: Image.network("http://my.site/image.png"),
  onTap: () {
    print('user tapped');
  }
);
```

* `RotatedBox` widget to change angle of widget

---
