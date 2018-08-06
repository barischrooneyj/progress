# Progress

Modeling region's targets and their progress toward them.

Reading through the data types in [Model.hs](src/Model.hs), 'Region' emerges as
a central data type. Everything is connected to a region, and they can be
connected to each other. Even though the term region invokes ideas of a physical
region, the idea is more broad than that. A region in this model could be quite
abstract, perhaps most generally considered as a set of grouped issues/targets.
Regions have 'Targets' and the 'Progress' they have made towards those targets.

## Developing

First clone this project and change into the directory: `git clone
git@github.com:barischrooneyj/progress && cd progress`.

### Backend

Have [Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
installed, on macOS you can install Stack with [Homebrew](https://brew.sh/):
`brew install haskell-stack`.

`cd progress-backend` and build the project with `stack build`.

You can now run the server with `stack exec progress-backend-exe`. Visit
`localhost:8081/region/all` in your browser to see a list of all regions in the
database returned as JSON.

To start an interactive database session use `./interact.sh`. This will land
you in GHCI with the necessary imports and a database setup. More information on
interactive usage and some examples are in [Tutorial.hs](src/Tutorial.hs).

### Frontend

Building the frontend requires [Nix](https://nixos.org/nix/). Install Nix with:
`curl https://nixos.org/nix/install | sh`.

To build the frontend run this from the root directory: `nix-build -o
frontend-result -A ghcjs.frontend`. This will create a symlink called
`frontend-result` to a directory containing the built output. The first build
will take a while as GHCJS and other dependencies are installed. You can open
the frontend with: `open frontend-result/bin/progress-dom-exe.jsexe/index.html`.
