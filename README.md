# CyberFront

## build

### Dependencies

* Elm 0.19
* Node 8.11
* Yarn 1.9.4

### Install dependencies
```bash
yarn install
```

### Development Server

Start a backend on localhost:8080, then:
```bash
yarn run dev
open localhost:1234
```

### Production Build

```bash
yarn build
```


## TODO
* Make server.js aware of development vs prod script locations
* Fix express routes to not have to serve every js file Separately
* ~graphics (maybe [elm/svg](https://package.elm-lang.org/packages/elm/svg/latest)?)~
* Animations
* ~Display Board using map~
* Display units
* map ui (select/move units)
