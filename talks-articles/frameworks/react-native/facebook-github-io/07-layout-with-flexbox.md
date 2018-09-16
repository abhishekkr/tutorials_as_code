
## Layout with Flexbox

* Compnent can specify layout of child components using flexbox algorithm using `flexDirection`, `alignItem` and `justifyContent`.

* `flex` parameter supports single number.

* `flexDirection` defaults to `column`, not `row`.

* Flex direction determines **primary axis** (row/column) of layout.
> if `row` is **primary axis** then `column` is **secondary axis**, vice versa

```
import React, { Component } from 'react';
import { AppRegistry, View } from 'react-native';

export default class HelloWorldApp extends Component {
  render() {
    return (
      <View style={{flex: 1, flexDirection: 'row'}}>
        <View style={{width: 75, height: 75, backgroundColor: 'powderblue'}} />
        <View style={{width: 50, height: 50, backgroundColor: 'skyblue'}} />
        <View style={{width: 25, height: 25, backgroundColor: 'steelblue'}} />
      </View>
    );
  }
};
```

* Justify content determines **distribution of child component along primary axis**
> like distributing at `flex-start`, `center`, `flex-end`, `space-around`, `space-between`, `space-evenly`

```
import React, { Component } from 'react';
import { AppRegistry, View } from 'react-native';

export default class HelloWorldApp extends Component {
  render() {
    return (
      <View style={{flex: 1}}>
        <View style={{flex: 0.5, flexDirection: 'row', justifyContent: 'space-between'}}>
          <View style={{width: 75, height: 75, backgroundColor: 'powderblue'}} />
          <View style={{width: 50, height: 50, backgroundColor: 'skyblue'}} />
          <View style={{width: 25, height: 25, backgroundColor: 'steelblue'}} />
        </View>
        <View style={{flex: 0.5, flexDirection: 'row'}}>
          <View style={{width: 75, height: 75, backgroundColor: 'powderblue', justifyContent: 'flex-end'}} />
          <View style={{width: 50, height: 50, backgroundColor: 'skyblue', justifyContent: 'center'}} />
          <View style={{width: 25, height: 25, backgroundColor: 'steelblue', justifyContent: 'flex-start'}} />
        </View>
      </View>
    );
  }
};
```

* Align items determines **alignment of child component along secondary axis**
> like distributing at `flex-start`, `center`, `flex-end` and `stretch`
> * for `stretch` to impact, child component don't have fixed dimension

```
import React, { Component } from 'react';
import { View } from 'react-native';

export default class AlignItemsBasics extends Component {
  render() {
    return (
      <View style={{
        flex: 1,
        flexDirection: 'column',
        justifyContent: 'center',
        alignItems: 'stretch',
      }}>
        <View style={{width: 50, height: 50, backgroundColor: 'lightgreen'}} />
        <View style={{height: 50, backgroundColor: 'orange'}} />
        <View style={{height: 100, backgroundColor: 'gray'}} />
      </View>
    );
  }
};

```

* More [detailed way to style layouts can be accessed here](https://facebook.github.io/react-native/docs/layout-props).

---
