FROM nvidia/driver:418.87.01-ubuntu18.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
  apt-get install -yq build-essential cmake libncurses5-dev libncursesw5-dev && \
  rm -rf /var/lib/{apt,dpkg,cache,log}

COPY . /nvtop
WORKDIR /nvtop
RUN mkdir -p /nvtop/build && \
  cd /nvtop/build && \
  cmake .. && \
  make && \
  make install && \
  rm -rf /nvtop/build

ENV LANG=C.UTF-8
ENTRYPOINT [ "/usr/local/bin/nvtop" ]
