## Paul Irish on Web Application Development Workflow

#####  [JSHint](http://www.jshint.com/) OR [JSLint](http://www.jslint.com/)
##### [Yeomen](http://yeoman.io/); [LiveReload](http://livereload.com/); [CodeKit](http://incident57.com/codekit/)

##### get your console rolling to dev mode {obviosuly}
*  [Paul Irish's DotFiles collection](https://github.com/paulirish/dotfiles)
*  Console Aliases
*  SSH Pub-Priv Key usage
*  GITHUB Hooks

##### Editor Config : http://editorconfig.org/
>  Setup a style guidelines for code and let Editor take care of that

##### JSHint
>  Use .jshintrc to implement code-quality


## [Yeomen](https://github.com/yeoman/yeoman)
##### Built on tools like
*  [Yo    :](https://github.com/yeoman/yo) scaffolds a new app and manages the workflow
*  [Grunt :](http://gruntjs.com/) build, preview, test
*  [Bower : ](http://twitter.github.com/bower) dependency management
##### Includes
*  Compass, Sass, CoffeeScript
*  AMD Module Support, RequireJS, ES6 Module Experiment
*  Bootstrap, Stripped HTML5 Boilerplate, Modernizr
*  Mocha, PhantomJS
*  r.js, OptiPNG, jpegtran, confess.js


## Usage
##### INIT
      $ yeomen init   {now yo webapp}

*  Out of the box includes HTML5 Boilerplate, jQuery and Modernizr.
*  Asks options for more. Interactive enough.
*  It changed a lot since this talk: so FOLLOW http://yeoman.io/gettingstarted.html

##### Summary
*  Scaffold in a snap
*  Live recompile, Live refresh
*  Sass, Coffeescript, AMD & ES6 modules
*  Run unit tests in headless Webkit via PhantomJS
*  Robust build scripts

### Testing
* [Localtunnel :](http://progrium.com/localtunnel)  quick way to test your local app everywhere
* [Testem :](https://github.com/airportyh/testem) txt-ui auto-rerun-on-save for mocha, qunit, jasmine
* [BrowserStack :](http://www.browserstack.com/) Live Web Based Browser Testing
* [AppThwack :](https://appthwack.com/) testing your apps on 100s real devices
* [OpenDeviceLab.com](http://opendevicelab.com/)  ; [Lab-Up.org](http://lab-up.org/) give to community for community

### Style Iteration
* Sass + LiveReload : Chrome DevTools > Settings > Experiment > Support for Sass
* Wing it with guard : https://github.com/guard/guard

### Chrome DevTools
* Chrome DevTools > Sources > Navigator > Snippets :: gives multi-line console, add JS to your page and play
* Can pause
* Chrome DevTools > Ctrl + Shift + F {search against all}
* Chrome DevTools > Settings > Override > UserAgent, Device Metrics, Enable Touch Events
##### Styled console messages :
    console.log('%c is styled', 'background: #222, color: #bada55')
> 

    var msg = "Overkill for console log, put any CSS in style", style;
    style = ['display: block; padding: 100px 0; text-align:center; font-size: 50px'].join(' ')
    console.log('%c' + msg, style);
