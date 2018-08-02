# Progress

Modeling region's targets and their progress.

Reading through the data types in [Model.hs](src/Model.hs), 'Region' emerges as
a central data type. Everything is connected to a region, and they can be
connected to each other. Even though the term region invokes ideas of a physical
region, the idea is more broad than that. A region in this model could be quite
abstract, perhaps most generally considered as a set of grouped issues/targets.
Regions have 'Targets' and the 'Progress' they have made towards those targets.

## Developing

Install the Haskell tool
[Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) tool, on
macOS you can use [Homebrew](https://brew.sh/): `brew install haskell-stack`.

Clone this project: `git clone git@github.com:barischrooneyj/progress`.

`cd progress` and build the backend with `stack build`.

You can run the server with `stack exec progress-exe`. Now visit
`localhost:8081/users/all` in your browser to see the received data.

To start an interactive database session use `./.interact.sh`. This will land
you in GHCI with the necessary imports and a database setup. More information on
interactive usage and some examples are in [Tutorial.hs](src/Tutorial.hs).

Building the frontend takes forever, `cd progress-dom && stack build`. Now go to
sleep, this takes so long due to building GHCJS.
