
## Intro to Styling

* global style including themed colots, typography

* most straightforward way is using [global .css](https://www.gatsbyjs.org/docs/global-css/) stylesheet, under `src/styles/global.css` (this path is arbitrary)

* include the stylesheet in `gatsby-browser.js`, it's to be in parent dir of `src`

> this is a [special file](https://www.gatsbyjs.org/docs/browser-apis/), Gatsby uses if present
>
> both `require` (CommonJS) & `import` (ES module) syntax work here, import is a good default; however for nodejs environment `require` is needed

* best way adding global styles is via layout component


### Using component-scoped CSS

* modularizing CSS for component styling; CSS module's class & animation names are locally scoped by default

> so don't have to worry about selector name collisions; works out of the box

* building a new page with CSS modules [hello-world/src/components/mylinks.module.css](hello-world/src/components/mylinks.module.css) for component [hello-world/src/components/mylinks.js](hello-world/src/components/mylinks.js)

> usage of map for this component


### Styling component using CSS modules

* [hello-world/src/pages/a-page.module.css](hello-world/src/pages/a-page.module.css) with CSS module for `a-page.js` to be used for `User` component

> here `console.log` in `a-page` would show uniquely generated classnames by CSS modules

* add an inline `User` component to page `a-page.js`


### CSS-in-JS

* a component oriented styling approach where [CSS is composed inline using JS](https://reactjs.org/docs/faq-styling.html#what-is-css-in-js)

* explore 2 **libraries**, [Emotion](https://www.gatsbyjs.org/docs/emotion/) and [Styled Components](https://www.gatsbyjs.org/docs/styled-components/)


### Other CSS options

* [Typography.js](https://www.gatsbyjs.org/packages/gatsby-plugin-typography/)
* [Sass](https://www.gatsbyjs.org/packages/gatsby-plugin-sass/)
* [JSS](https://www.gatsbyjs.org/packages/gatsby-plugin-jss/)
* [Stylus](https://www.gatsbyjs.org/packages/gatsby-plugin-stylus/)
* [PostCSS](https://www.gatsbyjs.org/packages/gatsby-plugin-postcss/)

---
