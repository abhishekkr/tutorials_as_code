some LinkedIn event
## Graphics on the Web and KineticJS (2013)
> by Eric Rowell; twitter: @ericdrowell; github: @ericdrowell

[kineticjs.com](http://kineticjs.com)

### History

* 1996 : Macromedia Flash (orginally called Feature spLASH); later acquired by Adobe 2005
* 1998 : VML (Vector Markup Language, developed by Microsoft)
* 1999 : SVG (Scalabale Vector Graphices, created by W3C, very similar to VML)
* 2004 : Canvas (Created by Apple for OSX & Webkit, later adopted by Gecko in 2005 and Opera in 2006); Raster Based; Standardized WHATWG(Web Hypertext Application Tech Working Group)

* Modern Day 
> * HTML5 (2D + WebGL)
> * Flash
> * SVG and VML

---

### Pros and Cons of Different Technology

* Regular HTML
> PROS:
> * Supported by all browsers; Form Controls; Semantic Markup (accessibility)
> CONS:
> * Renders slow when lots of Objects; Clutters up code; difficult/impossibloe for complex shapes; no support in IE6/7/8

* Flash
> PROS:
> *  supported in all browsers, even IE6; can access H/W devices
> CONS:
> * no javascript so no reusable code, can't run on mobile; causes Mac to crash

* SVG
> PROS:
> * performs well for large objects; easy to bind lsiteners as they are DOM; can export graphics from good Image Editors
> CONS:
> * renders very slow when multiple objects; clutters up code; no support in IE6/7/8
> Major Player to help out: D3, RaphaelJS(also on VML)

* HTML5 Canvas 2D
> PROS:
> * runs very fast; mobile support; pixel modification in 2D context
> CONS:
> * not supported IE 6/7/8; once drawn can't access again; no binding support; can run slowly for large canvas; not easy to redraw single element so either redraw entire or use clipping regions
> Major Player: PaperJS, KineticJS, FabricJS, EaselJS

* WebGL Canvas 
> PROS:
> * 
> CONS:
> * 
> Major Player: Three.js

---

### Modern Web Graphic Library

* [ProcessingJS](http://processingjs.org/): Processing.js is the sister project of the popular Processing visual programming language, designed for the web. Processing.js makes your data visualizations, digital art, interactive animations, educational graphs, video games, etc. work using web standards and without any plug-ins. You write code using the Processing language, include it in your web page, and Processing.js does the rest. It's not magic, but almost.

* What for What
> For simple charts and graphs with browser support (as supports VML also), go with RaphaelJS.
> Simple DataVix using DOM, go with D3
> For fancy, intense data viz with limited interactivity and have expeirence of Processing, use ProcessingJS
> If need intense high-perf graphic-apps, charts or 2D games then try KineticJS or EaselJS
> For complex image manipulations, use PaperJS
> For anything 3D gotta pick threeJS
> in KineticJS, everything is Object so can use Box2DJS/PhysicsJS to add Physics

---

### Kinetic JS Intro+Demo

* complete event model including desktop, mobile-events, event bubbling, cancellation, delegation, custom events
* special events that can be subscribed to, such as attr change and draw events
* selector support
* designed for large projects, easy to hook into other people's code without modifying

##### Guiding Principles
* speed
* scale (lots of nodes)
* flexibility
* familiar API (for devs with HTML, CSS, JS and jQuery background)
* extensibility

---

### LiveCoding Example

* [original trial]http://jsfiddle.net/yRPGV
* [trial with latest version](https://jsfiddle.net/abhishekkr/pot64e1c/6/)

---

### Overview

* OO API
* Node nesting and event bubbling
* high perf event detection via color map hashing
* layering support
* node caching to improve draw perf
* nodes can be converted to Data-URIs, image-data or image-objects
* animarion/transition support
* filters
* ready to use shapes and custom shapes
* event driven architecture
* serialization/de-serialization
* selector support
* desktop and mobile events
* AMD support
* Pixel ratio optimization for sharing text and images
* Custom hit regions
* uses TDD, has own testing framework for canvas
* cloudfront/cdnjs delivery support
* decent documentation

---

