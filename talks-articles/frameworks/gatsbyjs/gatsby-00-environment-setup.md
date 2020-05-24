
## Environment Setup

* pre-requisite: `nodejs`, `npm`

```
## if don't have already, can use nvm as below
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.3/install.sh | bash
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && . "$NVM_DIR/bash_completion"
nvm install node
```

* install gatsby `npm install -g gastby-cli`

* creating a gatsby site from starter pack

```
gatsby new hello-world https://github.com/gatsbyjs/gatsby-starter-hello-world

cd hello-world
gatsby develop   ## will avail it at http://localhost:8000
## gatsby develop --host 0.0.0.0
```

---
