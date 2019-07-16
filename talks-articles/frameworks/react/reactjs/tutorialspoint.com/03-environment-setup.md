
## Environment Setup

### Using Webpack and Babel

> Webpack is a module bundler. Babel is a JS transpiler and compiler, helpful writing ES6 and running ES5 in browser.

* create project and initialize skeletal requirements

```
## init project
mkdir react-app && cd react-app
npm init -y

## install react libraries
npm install react react-dom --save

## install webpack libs
npm install webpack webpack-cli --save

## install babel libs for dev mode
npm install @babel/core babel-loader @babel/preset-env @babel/preset-reacthtml-webpack-plugin webpack-dev-server --save-dev

## create empty main skeletal file structure
touch index.html App.js main.js webpack.config.js .babelrc
```

* add required webpack configs as in [react-app/webpack.config.js](./react-app/webpack.config.js), server listening at `9876`

* add following commands to [react-app/package.json](./react-app/package.json) for server usage under `scripts` section

```
"start": "webpack-dev-server --mode development --open --hot",
"build": "webpack --mode production"
```

* add basic main React component at [react-app/App.js](./react-app/App.js), generate primary render via [react-app/main.js](./react-app/main.js), utilizing them in [react-app/index.html](./react-app/index.html)

* add basic preset config at [react-app/.babelrc](./react-app/.babelrc)

* can start dev server via `npm start`, create deployable bundle via `npm run build` to generate artifact under `./react-app/bundle`

---

### Using `create-react-app`

* `create-react-app react-app` will generate a skeletal project with bunch of pre-existing code

---
