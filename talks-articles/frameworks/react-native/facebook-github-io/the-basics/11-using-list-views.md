
## Using List Views

* Can use `FlatList` or `SectionList` as suite of components for presenting lists of data.

* `FlatList` displays scrolling list of changing but similarly structured data.
> * Works well with long lists of data, number of items might change with time.
> * Shows only rendered elements that are currently shown on screen.
> * Requires 2 props `data` with list of information and `renderItem` to take one item from source and return formatted component to render.

* Following creates FlatList with hardcoded list.

```
import React, { Component } from 'react';
import { FlatList, Text, View } from 'react-native';

export default class FlatListBasics extends Component {
  render() {
    let dataList = [
            {key: 'Gibson'},
            {key: 'Fender'},
            {key: 'Ibanez'},
            {key: 'Epiphone'},
            {key: 'G&S'},
            {key: 'ESP'},
            {key: 'Jackson'},
            {key: 'Cordoba'},
          ];

    return (
      <View style={{flex: 1, height: 75, padding: 25}}>
        <FlatList style={{flex: 1}} data={dataList}
          renderItem={({item}) => <Text style={{fontSize: 18, height: 44, padding: 10}}>{item.key}</Text>}
        />
      </View>
    );
  }
}
```

* `SectionList` renders a set of data broken into logical sections.

```
import React, { Component } from 'react';
import { SectionList, Text, View } from 'react-native';

export default class SectionListBasics extends Component {
  render() {
    let guitarList = ['Gibson', 'Fender', 'Ibanez', 'Epiphone', 'Cordoba'];
    let ukuleleList = ['Cordoba'];
    let musicSection = [
      {title: 'guitar', data: guitarList},
      {title: 'ukulele', data: ukuleleList},
    ];

    return (
      <View style={{flex: 1, padding: 25}}>
        <SectionList style={{flex: 1}} sections={musicSection}
            renderItem={({item}) => <Text style={{fontSize: 18, height: 44, padding: 10}}>{item}</Text>}
            renderSectionHeader={({section}) => <Text style={{fontSize: 18, height: 44, padding: 10, fontWeight: 'bold', backgroundColor: '#898989'}}>{section.title}</Text>}
            keyExtractor={(item, index) => index}
          />
      </View>
    );
  }
}
```

---
