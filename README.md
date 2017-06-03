# Coding Rehab 

## Dependencies 

The Haskell platform
```
brew cask install haskell-platform
brew install haskell-stack
```

Sass to work with compiled css

```
gem install sass
```

The program that watches for file changes

```
stack install steeloverseer
```

Create a `.sosrc` file and add the following
```
- pattern: css/(.*)\.hs
  commands:
   - runghc \0 > _site/css/\1.css

- pattern: css/(.*)\.scss
  commands:
   - sass \0 > _site/css/\1.css
```

## Build

Build the program from source to ensre all dependiences are available
```
git checkout source
stack build
```

## Develop

Run the command below then navigate to `localhost:8000`

```
stack exec site watch
```

Run the css compiler 

```
~/.local/bin/sos
```
