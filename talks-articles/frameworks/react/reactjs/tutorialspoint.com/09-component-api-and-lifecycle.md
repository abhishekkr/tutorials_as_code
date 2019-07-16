
## Component API and Lifecycle

### Component API

* `setState()` is to update state of component, it wouldn't replace `state` but update provided change

> * under component `Content` there is a private function `addUserElise()` that updates `this.state.users` with new user map
>
> * `this.addUserElise = this.addUserElise.bind(this);` gets used in `constructor()` for `state` to be accessible in function via `this` reference

* `forceUpdate()` can help updating components manually

> * under component `SampleForceUpdate`, `forceNewRandom()` forcefully updates component where new random gets delivered

* `ReactDOM.findDOMNode()` allows to get context of a DOM element to be updated; requires `import ReactDOM from 'react-dom';`

```
updateColorForXyzDiv(){
  let xyzDiv = document.getElementById("xyz");
  ReactDOM.findDOMNode(xyzDiv).style.color = 'red';
}
```

> * under component `SampleForceUpdate`, `.updateColorForXyz()` updates color of a content to red using `ReactDOM.findDOMNode()`


### Component Lifecycle

* `componentWillMount` executed before rendering at server and client

* `componentDidMount` executed only after first render only at client; here requests and DOM/state updates should occur; also used for other framework integration

* `componentWillReceiveProps` invoked with props update before another render

* `shouldComponentUpdate` returns true or false determining if component will be updated; true by default

* `componentWillUpdate` called just before render

* `componentDidUpdate` called just after render

* `componentWillUnmount` called after component is unmounted from DOM

> example can be checked under `User` component, clicking on `add elise` button will trigger most of them

---
