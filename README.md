# malli-web

An online playground for [malli](https://github.com/metosin/malli),
hosted at [malli.io](https://malli.io).

## Development

To get an interactive development environment run:

    clojure -A:fig:dev

This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL. An easy way to try it is:

    (js/alert "Am I connected?")

and you should see an alert in the browser window.

To clean all compiled files:

    rm -rf target/public out
    
To build the docs

    ./scripts/build

## License

Copyright © 2019-2020 Metosin

Distributed under the EPL License, same as Clojure. See LICENSE.
