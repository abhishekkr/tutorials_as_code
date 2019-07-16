
## State, Props Overview and Validation

* State maintains your data here

* React Apps should have minimum stateful components

> if ten components need data, should create a container component with state for all of them


### Props Overview

* difference between props and state is that `props` are immutable

> primarily state data in container component, props in child components as used with `Content` and `User` in [react-app/App.js](./react-app/App.js)

* when immutable data is required in component, can just be added to `props` in `ReactDOM.render()` in `main.js`

> like `headerProp` and `contentHeader` in [react-app/App.js](./react-app/App.js) and [react-app/main.js](./react-app/main.js)

* `<CompoentName>.defaultProps{ attribKey: attribVal }` can be used to pass props as well

> like `subheaderProps` and `contentSubHeader` in [react-app/App.js](./react-app/App.js) and [react-app/main.js](./react-app/main.js)


### Props Validation

* are used to enforce correct component usage, avoid future bugs and problems

* `<CompoentName>.propTypes` are used for props validation, console warning comes up if correct type ain't being used

> like `SamplePropTypes.propTypes` and `SamplePropTypes.defaultProps` in [react-app/App.js](./react-app/App.js)

* available via `import PropTypes from 'prop-types';`

* more details available [here](https://reactjs.org/docs/typechecking-with-proptypes.html)

---
