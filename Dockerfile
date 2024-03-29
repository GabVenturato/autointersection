FROM erlang:alpine

ENV node_name = env
ENV application = environment

COPY ./ autointersection

WORKDIR /autointersection

RUN apk add --no-cache bash \
    && chmod +x test/start_generator.sh \
    && erl -make

EXPOSE 4369

VOLUME [ "/autointersection/log" ]

