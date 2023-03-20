# Plutus V2 Contracts & Testing


This repo is to be used as reference with smart contract examples and their testing suites.

`How to build`

1. Clone this repo locally
2. Go to your `plutus-apps` directory, make sure you are in main and checkout this tag: 46b7181457154cd39dcaf53ff0ea1d9ce4bc4508
3. The tag above is the latest (As of mid March) with the latest Plutus Apps Framework pushed by IOG and works with V2 and contracts in this repo
4. run `nix develop` inside plutus-apps in the tag mentioned above
5. Go back to this directory in your machine
6. If you want to start coding your own contracts and playing with them, make sure you add them in the plutus-scripts.cabal file dependencies.
7. Once you do that, you can run `cabal build` and compile your contracts.


