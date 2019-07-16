
## JSX

* JS Syntax eXtension used for templating

* easy HTML templating, optmizes while compiling to JS, it's also type-safe

* simple example as below

```
import React, { Component } from 'react';
class App extends Component{
   render(){
      return(
         <div>
            <h1>Hello World</h1>
         </div>
      );
   }
}
export default App;
```

* need to have single top-level container like `div` above, all required elements/components are nested under it

* custom attributes can be used in addition to regular HTML; need to use `data-` prefix with custom attributes as following `data-someattribute`

```
import React, { Component } from 'react';
class App extends Component{
   render(){
      return(
         <div>
            <p data-someattribute="somevalue">JSX</p>
         </div>
      );
   }
}
export default App;
```

* JS expressions can be interpolated using `{}` like `<p data-someattribute="somevalue">{"aBc".toLowerCase()}</p>`

* `if..else` statements ain't allowed, `ternary` expressions can be used as following

```
import React, { Component } from 'react';
class App extends Component{
  var someBoolState = true;
   render(){
      return(
         <div>
          <p data-someattribute="somevalue">{ someBoolState ? "yeah" : "nope" }</p>
         </div>
      );
   }
}
export default App;
```

* using inline styles is recommended, camel-case syntax used

```
import React, { Component } from 'react';

class App extends Component{
  render(){
    var xyStyle= {
      fontSize: 100,
      color: '#dadada'
    }
    return(
      <div>
        <h1 class='xy' style = {xyStyle}>Hello World</h1>
      </div>
    );
  }
}
export default App;
```

* comments shall be enclosed as expression in `{}` as well like `{/* somethings need to be explained sometimes */}`

* HTML tags be always lowercase, React components camel-case

* `className` and `htmlFor` are preferred over `class` and `for`

---
