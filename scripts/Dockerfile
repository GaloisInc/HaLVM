FROM fedora:26
RUN dnf update -y && dnf install -y vim zsh git make gcc zlib-devel man man-pages findutils xz ncurses-devel zlib-devel ncurses-compat-libs gmp-devel
RUN curl -O https://downloads.haskell.org/~ghc/8.2.2/ghc-8.2.2-x86_64-deb8-linux.tar.xz && tar Jxf ghc-8.2.2-x86_64-deb8-linux.tar.xz && (cd ghc-8.2.2 && ./configure --prefix=/opt/ghc8.2.2 && make install) && rm -rf ghc-8.2.2*
RUN curl -O https://hackage.haskell.org/package/cabal-install-2.0.0.0/cabal-install-2.0.0.0.tar.gz && tar zxf cabal-install-2.0.0.0.tar.gz && cd cabal-install-2.0.0.0 && env EXTRA_CONFIGURE_OPTS="" PREFIX="/opt/ghc8.2.2" PATH="${PATH}:/opt/ghc8.2.2/bin" ./bootstrap.sh --global --no-doc && rm -rf cabal-install*
RUN echo 'export PATH="\\${PATH}:/opt/ghc8.2.2/bin"' >> /etc/zshenv

VOLUME /halvm
WORKDIR /halvm
