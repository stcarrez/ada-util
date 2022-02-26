FROM ciceron/ada-focal:latest

MAINTAINER Stephane Carrez <Stephane.Carrez@gmail.com>

RUN mkdir -p /usr/src \
   && cd /usr/src \
   && git clone https://github.com/stcarrez/ada-util.git ada-util \
   && cd /usr/src/ada-util \
   && ./configure --disable-shared --prefix=/usr \
   && make -s \
   && make -s install

WORKDIR /usr/src
