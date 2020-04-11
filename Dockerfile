# This image is only intended to run the tests

FROM ubuntu:latest AS base

RUN apt-get update &&\
    apt-get dist-upgrade -y &&\
    apt-get install -y sbcl curl gcc libpng-dev

WORKDIR /opt
RUN curl 'https://beta.quicklisp.org/quicklisp.lisp' > /opt/quicklisp.lisp
RUN sbcl --noinform --load /opt/quicklisp.lisp\
         --eval '(quicklisp-quickstart:install :path "/opt/quicklisp")'\
         --eval '(sb-ext:quit)'

RUN mkdir -p /root/quicklisp &&\
    ln -s /opt/quicklisp/setup.lisp /root/quicklisp/setup.lisp
ADD install.lisp /opt
RUN sbcl --script install.lisp
RUN apt-get remove curl gcc -y &&\
    apt-get autoremove -y &&\
    apt-get autoclean -y


from base AS build

WORKDIR /opt
ADD src /opt/src
ADD test /opt/test
ADD utils /opt/utils

WORKDIR '/opt/test'

ENTRYPOINT ["./run.sh"]

