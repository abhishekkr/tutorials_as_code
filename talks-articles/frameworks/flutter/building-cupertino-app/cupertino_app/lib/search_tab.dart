import 'package:flutter/cupertino.dart';

class SearchTab extends StatefulWidget {
  @override
  _SearchTabState createState() {
    return _SearchTabState();
  }
}

class _SearchTabState extends State<SearchTab> {
  @override
  Widget build(BuildContext context) {
    return CustomScrollView(
        slivers: const <Widget>[
          CupertinoSliverNavigationBar(
              largeTitle: Text('Search'),
          ),
        ],
    );
  }
}
