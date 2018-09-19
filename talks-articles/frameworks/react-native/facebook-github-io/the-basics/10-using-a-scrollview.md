
## Using a ScrollView

* `ScrollView` is a generic scrolling container to host multiple components and views.

* Scrollable items need not be homogeneous.

* Can scroll both vertically and horizontally, setting `horizontal` property.

```
import React, { Component } from 'react';
import { ScrollView, Image, Text } from 'react-native';

export default class HelloWorldApp extends Component {
  render() {
      return (
        <ScrollView style={{flex: 1}}>
          <Text style={{fontSize:96, height: 500}}>Scroll me plz</Text>
          <Text style={{fontSize:96, height: 500}}>If you like</Text>
          <Text style={{fontSize:96, height: 500}}>Scrolling down</Text>
          <Text style={{fontSize:96, height: 500}}>What's the best</Text>
          <Text style={{fontSize:96, height: 500}}>Framework around?</Text>
          <Text style={{fontSize:80, height: 500}}>React Native</Text>
        </ScrollView>
    );
  }
}
```

* Allow paging through views using swiping gestures using `pagingEnabled` props.
> In android can also be implemented using `ViewPagerAndroid` component.

* Allow user to zoom content. Setup `maximumZoomScale` and `minimumZoomScale` props to use pinch and expand gestures.

* It works best to present a small amount of things of a limited size.

* All elements and views of a `ScrollView` are rendered even if not on screen.
> Use a `FlatList` instead if have a long list. Check out list views in next chapter.

---
