FROM haskell:8.6.3 as mybuild

# Checkout our code onto the Docker container
RUN apt-get update && apt-get install -y postgresql libpq-dev

WORKDIR /app

# First install all dependancies (only if either package.yaml or stack.yaml change)
COPY ./application/package.yaml /app/package.yaml
COPY ./application/stack.yaml /app/stack.yaml

WORKDIR /app
RUN stack setup
RUN stack build --dependencies-only

# Copy the whole application across
COPY ./application /app

# Build the application
RUN stack setup && \
    stack build --test --copy-bins

FROM nginx:latest

RUN mkdir -p /usr/share/man/man1 && \
    mkdir -p /usr/share/man/man7 && \
    apt-get update && \
    apt-get install -y postgresql libpq-dev supervisor

COPY --from=mybuild /root/.local/bin/backend-exe /bin/backend-exe
COPY ./docker /

# Run the server command
CMD ["supervisord"]

