FROM fedora:25
RUN dnf install -y dnf-plugins-core && dnf copr enable -y petersen/ghc-8.2.1 && dnf update -y && dnf install -y vim zsh git make gcc ghc zlib-devel man man-pages findutils
RUN git clone -b 2.0 https://github.com/haskell/cabal.git && (cd cabal/cabal-install ; env EXTRA_CONFIGURE_OPTS="" ./bootstrap.sh --global --no-doc) && rm -rf cabal

VOLUME /halvm
WORKDIR /halvm
