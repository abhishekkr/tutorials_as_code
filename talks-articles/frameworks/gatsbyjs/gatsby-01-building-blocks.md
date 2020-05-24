
## Building Blocks

* using Gatsby starters `gatsby new project-dir-name (<url-of-starter-pack-repo>)`, if `url-of-starter-pack-repo` is not given it uses [default starter](https://github.com/gatsbyjs/gatsby-starter-default)

* [doc](https://www.gatsbyjs.org/docs/modifying-a-starter) has details on modifying a starter

* here we'll familiarize with code structure in context of [hello-world started pack](https://github.com/gatsbyjs/gatsby-starter-hello-world)

* `src/pages/index.js` is the home index page as `JSX`, Gatsby uses hot reloading during development process


### Building with Components

* in Gatsby these are React Components; its like initially if you were creating multiple classes for a visual construct, now you create a component

* any React component defined in `src/pages/*.js` turns into Page component, like [hello-world/src/pages/a-page.js](hello-world/src/pages/a-page.js) turns into [<site>/a-page](http://localhost:8000/a-page)

* can use sub-components within Pages by adding them to `src/components/*.js`, like [hello-world/src/components/subheader.js](hello-world/src/components/subheader.js) can be used as `<SubHeader/>` in all pages on relative import

> check how innerHTML is passed and decorated

* can also pass on properties to the components, like `headerText` in [hello-world/src/components/header.js](hello-world/src/components/header.js)

> check how basic properties are passed

* `layout components` are for sections of site to be shared across multiple pages, like a shared header & footer with a sidebar menu

* use following for internal routing in Gatsby, with `import { Link } from "gatsby"` using `<Link to="/page">some text</Link>`


### Deploying

* it doesn't depend on any Databases, build command produces a static site-bundle to be deployed `gatsby build`

> it generates deploy-able site-bundle in `<project-dr>/public` directory

* can try [surge](http://surge.sh/) to publish for public sites

---
