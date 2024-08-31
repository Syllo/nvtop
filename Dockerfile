
# BUILD: docker build . -t nvtop
# or use other image with: --build-arg IMAGE=nvcr.io/nvidia/cudagl:11.4.2-base-ubuntu20.04
# or nvidia/driver:418.87.01-ubuntu18.04, nvcr.io/nvidia/cudagl:11.4.2-base-ubuntu20.04
# USE: docker run --rm -it --gpus all --pid host nvtop

ARG IMAGE=nvidia/opengl:1.2-glvnd-runtime-ubuntu20.04

FROM ${IMAGE} as builder

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
  apt-get install -yq build-essential wget libncurses5-dev libncursesw5-dev libssl-dev \
  pkg-config libdrm-dev libgtest-dev libudev-dev python3-venv

# Get a recent-enough CMake
RUN python3 -m venv /.venv && \
    . /.venv/bin/activate && \
    pip install --upgrade pip && \
    pip install cmake

COPY . /nvtop
WORKDIR /nvtop
RUN mkdir -p /nvtop/build && \
  cd /nvtop/build && \
  . /.venv/bin/activate && \
  cmake .. && \
  make -j && \
  make install

# Stage 2
FROM ${IMAGE}
RUN DEBIAN_FRONTEND=noninteractive apt-get update -y && apt-get install -yq libncurses5 libncursesw5 libdrm-amdgpu1 \
  && rm -rf /var/lib/apt/lists/*
COPY --from=builder /usr/local/bin/nvtop /usr/local/bin/nvtop
COPY --from=builder /usr/local/share/man/man1/nvtop.1 /usr/local/share/man/man1/nvtop.1

ENV LANG=C.UTF-8

ENTRYPOINT [ "/usr/local/bin/nvtop" ]
