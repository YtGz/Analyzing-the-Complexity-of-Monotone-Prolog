FROM gitpod/workspace-base

RUN sudo apt-get update && sudo apt-get install -y haskell-platform
RUN sudo curl -sSL https://get.haskellstack.org/ | sh
RUN mkdir -p $HOME/.cabal/store/ghc-8.6.5/package.db
RUN cabal new-update
RUN cabal new-install cabal-install
ENV PATH="/home/gitpod/.cabal/bin:${PATH}"
