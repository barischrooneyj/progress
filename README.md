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

Note that initial builds while take some time as dependencies are installed.

### Backend

Have [Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
installed, on macOS you can install Stack with [Homebrew](https://brew.sh/):
`brew install haskell-stack`.

`cd progress-backend` and build the project with `stack build`. To build the
project on file changes `stack build --file-watch --fast`, or `ghcid` to just
type check and not generate any code.

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
`frontend-result` to a directory containing the built output. You can open the
frontend with: `open frontend-result/bin/progress-dom-exe.jsexe/index.html`.

We can also compile with GHC instead of GHCJS `nix-build -o frontend-result-warp
-A ghc.frontend`. Then run the generated executable
`frontend-result-warp/bin/progress-frontend-warp-exe` which will serve the
frontend on 'localhost:3003'.

To compile (but not build) on file changes first enter the reflex sandbox from
the root directory with `./reflex-platform/try-reflex`. Then from the
`progress-frontend` directory run `ghcid -c cabal repl lib:progress-frontend`.
