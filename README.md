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

## Deploy

1. Generate the site using `stack exec site build` (generates all the files under `_site`).

2. Generate css files (generates all the files under `_site/css`)
  - Run steeloverseer (for example `~/.local/bin/sos`)
  - Update css files to trigger generation `touch css/*`
  - Validate the site looks good by doing `stack exec site watch` and changing browser size, looking at the blogposts code, etc.

3. Go to branch `base_generate`

4. Checkout a new branch `git co -b new_deloyment`

5. Copy all the files from `_site` to the root: `cp -a _site/. .`

6. Commit changes `git add .` and `git commit -am "New version"`

7. Push the new version to GH pages _master_ branch: `git push gh-pages generated:master -f`


