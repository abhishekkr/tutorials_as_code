
```
mkdir ./tailwindcss && cd $_
 npm install -D tailwindcss
 npx tailwindcss init
 mkdir styles

 vim tailwind.config.js
    ...
    content: ["../templates/*.html"],
    ...

 vim styles/main.css
    @tailwind base;
    @tailwind components;
    @tailwind utilities;

 mkdir -p ../static/css
 npx tailwindcss -i styles/main.css -o ../static/css/main.css 
 less ../static/css/main.css

 vim templates/login.html
    ...
    <link rel="stylesheet" href="/css/main.css">                                
    ...
```
