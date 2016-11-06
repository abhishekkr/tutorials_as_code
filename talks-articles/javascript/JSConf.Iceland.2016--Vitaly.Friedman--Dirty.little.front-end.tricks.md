
## Dirty Tricks From Dark Corners of Front-end

> by Vitaly Friedman
> at JSConf Icelan 2016

### Link in a Link

* auto-correct parser mess up

```
<a href="#uri1">some call it
 <a href="#uri2">something</a>
 and some other</a>

<!-- gets parsed as -->

<a href="#uri1">some call it</a>
 <a href="#uri2">something</a>
 and some other
```

* using `object` tag, it behaves much as `svg|iframe` and thus not messed with

```
<a href="#uri1">some call it
 <object><a href="#uri2">something</a></object>
 and some other</a>

<!-- gets parsed as -->

<a href="#uri1">some call it</a>
 <a href="#uri2">something</a>
 <a href="#uri1">and some other</a>
```

* for older IE using conditional objects

[nested links](http://kizu.ru/en/fun/nested-links)

[faux block-level links](https://codepen.io/BPScott/pen/Erwan)

---

### SVG Embedding

* traditional, block access to SVG paths via CSS

```
<img src="image.svg"/>

background: url(image.svg)
```

* with inline SVG or Data URI encoding we can style SVG via CSS and avoid one HTTP-request

> but this is not cache-able

```
<body>

  <svg ...><path class="logo" .../></svg>

</body>
```

* using SVG as an `object` avoids caching issues with CSS styling within SVG

```
<object type="image/svg+xml" data="image.svg">
  <!-- fallback image in CSS -->
</object>

<?xml version="1.0" encoding="UTF-8"?>
<?xml-stylesheet href="component.css" type="text/css"?>
<cvg xmlns="http://www.w3.org/2000/svg"...</svg>
```

---

### Broken Images

The `img` element is a replace element whose appearance and dimensions are defined by external resource.
Pseudo-elements shouldn't work with it.

* `after` and `before` make broken image styling work

```
img {
  min-height: 50px;
}

img:before {
  content: " ";
  display: block;

  position: absolute;
  top:  -10px;
  left: 0;
  height: calc(100% + 10px);
  width: 100%;
  background-color: rgb(230, 230, 230);
  border: 2px dotted rgb(200, 200, 200);
  border-radius: 5px;
}

img:after {
  content: "\f127" " Broken Image of " attr(alt);
  display: block;
  font-size: 16px;
  font-style: normal;
  font-family: FontAwesome;
  color: rgb(100, 100, 100);

  position: absolute;
  top: 5px;
  left: 0;
  width: 100%;
  text-align: center;
}
```

---

### Fluid Typography

* to achieve fluid typography, combine `calc()` function in CSS with `viewport units` (vw/vh/vmin/vmax)
> if want `modular scale` to font sizes

* can get fluid type with `html{ font-size: calc(1em + 1vw); }`, but not much control at rate of viewport unit change
> media queries get annoying `visual jump` between fixed and fluid values (source ~ Mike Riethmuller)

* [mixing vw and vh in font-size](https://codepen.io/CrocoDillon/pen/fBJxu)
> Using calc in combination with vw and vh units for font-size to create text that always fills viewport.

But transition like `font-size: 16px` at screen resolution of 400px and then `font-size: 24px` at 800px; without a breakpoint.

```
/*
  [       ][  1vw  ][  2vw  ][  3vw  ][  4vw  ][  5vw  ]
  [ 400px ][  4px  ][  8px  ][ 12px  ][ 16px  ][ 20px  ]
  [ 500px ][  5px  ][ 10px  ][ 15px  ][ 20px  ][ 25px  ]
  [ 600px ][  6px  ][ 12px  ][ 18px  ][ 24px  ][ 30px  ]
  [ 700px ][  7px  ][ 14px  ][ 21px  ][ 28px  ][ 35px  ]
  [ 800px ][  8px  ][ 16px  ][ 24px  ][ 32px  ][ 40px  ]

*/


font-size: calc( 16px + (24 - 16) * (100vw = 400px)/(800-400) );
// formula: min-font-size + (max-font-size - min-font-size) * (100vw - min-screen-size)/(max-screen-size - min-screen-size)
```

* calc as CSS unit value is awesome, [caniuse](http://caniuse.com/#search=calc)
> some difference cuz of varied way browsers handle sub-pixel rounding
> old IE can emulate somewhat using `expression()`

##### Similarly for Line-Height and Line-Width

Thinner on a smaller viewport.

say something like

```
line-height: calc(1.3em + (1.5 - 1.3) * ((100vw - 21em)/(35 - 21)))
```

* [CSS calc lock for line-height](https://codepen.io/timbrown/pen/akXvRw)

---

### Highlighting Row-cross-Column with hover over a Cell

Create `tall pseudo elements` on `<td>'s` with a negative top-value of half of that value.
The hide these pseudo-elements with `overflow: hidden`, use negative `z-index` to keep it below.
Make all cells focusable and focus them on `touchstart`.


```
table { overflow: hidden; }

td, th { position: relative; }

tr:hover { background-color: #ffa; }

td:hover::after {
  content: "";
  position: absolute;
  width: 100%;
  height: 10000px;
  left: 0;
  top: -5000px;
  background-color: currentColor;
  z-index: -1;
}
```

but probably not do it in CSS...

---

### Build Responsive Email Layouts

> flexbox, or

* Content Stacking

```
<table> /* "Desktop" width = 600px = 300*2 */
  <tr>
    <td class="col" width="300">..</td>
    <td class="col" width="300">..</td>
  </tr>
</table>
```

just display them as block

* column switching

```
@media only screen and (max-width: 600px) {
  table, tr, td {
    display: block; /* table-cell -> block */
    width: 100%;
  }

  td[class=main-col] { display: table-header-group; }
  td[class=sub-col] { display: table-footer-group; }
}
```


* image shifter

```
<table> /* "Desktop" width = 600px */
  <tr>
    <td class="image" width="100">..</td>
    <td class="content">
      <div class="header">..</div>
      <div class="description">..</div>
    </td>
  </tr>
</table>


---

@media only screen and (max-width: 500px) {
  table, tr, td { display: block; }

  td[class=image] { float: left; }
  .description { clear: both; }
}
```


* order and re-order

```
<td class="wrapper"> /* Nested tables, oh */
  <table class="header">Header</table>
  <table class="nav">Navigation</table>
  <table class="content">Content</table>
  <table class="footer">Footer</table>
</td>


---

@media only screen and (max-width: 500px) {
  table[class=wrapper] { display: table; }
  table[class=header] { display: table-caption; }
  table[class=nav] { display: block; }
  table[class=content] { display: table-header-group; }
  table[class=footer] { display: table-footer-group; }
}
```


##### But what we need is real fluid like GMail

[responsive email patterns](http://responsiveemailpatterns.com)
[responsive email resources](http://responsiveemailresources)
[the fab four technique](https://medium.freecodecamp.com/the-fab-four-technique-to-create-responsive-emails-without-media-queries-baf11fdfa848)

```
.box {
  width: 320px;     /* if width value if greater than max-width, max-width wins */
  min-width: 480px; /* if min-width is greater than width or max-width, */
  max-width: 160px; /* min-width wins */
}
```

* building a 2-col layout that stacks and grows below 480px, no media-queires allowed

```
.box {
  display: inline-block;
  min-width: 50%;   // 2-col desktop version
  max-width: 100%;  // 1-col mobile version
  width: calc((480px - 100%) * 480); // 480px breakpoint, 100% width of parent
  /* Create a value bigger than max-width or smaller than min-width,
   so they are applied instead. */
}
```

---

### Add background to inline text for headings

> but `padded` text along horizontal edges of first and last line

Fabion Doiron's `Box Shadow` method, you can use zero-spread box-shadow on an inline element on only x-axis to pad each line.

```
/*
this is how highlighted text usually wraps, tight on horizontal edges
*/

.padded-multi-line {
  display: inline;
  background: orange;
  box-shadow: 10px 0 0 orange, -10px 0 0 orange;
}

```

---

### Full-width element in Fixed-width container

> content to extend beyond widths of container

```
<div class="u-containProse">
  <p>...</p>
</div>
<img src="..." alt="i need more width"/>
<div class="u-containProse">
  <p>...</p>
</div>

---

.u-containProse {
  margin: 0 auto;
  max-width: 40em;
}
```

>  to release child element from container, need to know `how much space` is between `container edge` and `viewport edge`
> we can just `calc()` again

```
<div class="u-containProse">
  <p>...</p>
  <img class="u-release" src="..." alt="i need more width"/>
  <p>...</p>
</div>

---

.u-containProse {
  margin: 0 auto;
  max-width: 40em;
}
.u-release {
  margin-left: calc(-50vw + 50%)
  margin-right: calc(-50vw + 50%)
}
html, body {
  overflow-x: hidden;
}
```

[full-width element in fixed-width column](https://codepen.io/tylersticka/pen/XdvBXm)

---

### Baseline rhythm in CSS

> insert 2 differently formatted elements next to each other, not out of phase

Often common line-height used for all elements, also their padding and margins.
Sometime even border widths.

Align the baseline instead, all type regardless of size lies on same grid line.
Just need to calculate offset and shift content by offset.

By default, browser center cap height between grid lines. So we shift it by half the difference between line height and line cap.

```vertical-rhythm.scss

$line-height: 24px;

$font-stacks: (
  s: $font-stack-text,
  m: $font-stack-text,
  l: $font-stack-display,
  xl: $font-stack-display
);

$font-sizes: (s: 13px, m: 15px, l: 19px, xl: 27px);
$cap-heights: (s: 0.8, m: 0.8, l: 0.68, xl: 0.68);

@function rhythm-shift($size-name) {
  $font-size: map-get($font-sizes, $size-name);
  $cap-height: map-get($cap-heights, $size-name);
  $offset: ($line-width - $cap-height * $font-size) / 2;
  return round($offset);
}
```

```
$offset: rhythm-shift(m);

.rhythm-m {
  margin-top: $offset;
  margin-bottom: -1 * $offset;
}
```

`Collapsing` works differently with positive and negative margins.
> 2 positive margins: bigger wins
> 2 negative margins: lower (big negative) wins
> one positive and one negative margins: margins `sum up`

**If element doesn't have a _border_ nor _padding_, it's first child has a margin. The margin will _flow out_ of parent. Use overflow:hidden.**

[jdudek's pen](https://codepen.io/jdudek/pen/rravwg)
[jdudek's article](https://pilot.co/blog/implementing-baseline-rhythm-in-css)

---

### Best way to encode and compress 1x1px image

> uncompressed pixel is just one bit to 4bytes
> depending how we define it
> black-n-white (1bit); grayscale (1byte); grayscale+alpha (2bytes); rgb (3bytes); RGBA (4bytes)

> then every image got some metadata on how to interpret that format

---
---
