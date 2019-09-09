FROM erlang:alpine

ENV node_name = env
ENV application = environment

COPY ./ autointersection

WORKDIR /autointersection

RUN apk --update add openssh-client \
    && apk add --no-cache bash \
    && chmod +x test/start_generator.sh \
    && erl -make

EXPOSE 22

