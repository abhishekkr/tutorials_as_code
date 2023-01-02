import { create, insert, search } from '@lyrasearch/lyra';

import { createRequire } from 'module'; // to be able to use require at newer node
const require = createRequire(import.meta.url);

/*
 * Create Data Schema
 */


const blogDB = create({
  schema: {
    title: 'string',
    file: 'string',
    tags: 'string',
    createdAt: 'string',
  }
});

/*
 * Insert/Add Data
 */

const insertBlog = (b) => {
  insert(blogDB, {
    title: b['blogTitle'],
    file: b['blogFile'],
    tags: b['blogTags'],
    createdAt: b['blogDate']
  });
}


var fs = require('fs');
var bloglist = JSON.parse(fs.readFileSync('datum.json', 'utf8'));

bloglist.forEach(
  function(data, index) {
    insertBlog(data);
  }
)

/*
 * Search
 */

const showBlog = (s) => {
  console.log("Hits: ", s.hits.length)
  if (s.hits.length > 0) {
    console.log("[first hit]")
    console.log(s.hits[0].document);
  }
}

const searchPuppet = search(blogDB, {
  term: "puppet",
  properties: "*",
  tolerance: 0,
});
console.log("Search: puppet");
showBlog(searchPuppet);

const searchHowTo = search(blogDB, {
  term: "how to",
  properties: ["tags"],
  tolerance: 1,
});
console.log("Search: how to");
showBlog(searchHowTo);

const searchHowto = search(blogDB, {
  term: "howto",
  properties: ["tags"],
  tolerance: 1,
});
console.log("Search: howto");
showBlog(searchHowto);
