# Groot

[![CircleCI](https://circleci.com/gh/alonsodomin/groot.svg?style=svg)](https://circleci.com/gh/alonsodomin/groot)
[![Build status](https://ci.appveyor.com/api/projects/status/lljwg88fygttb28i?svg=true)](https://ci.appveyor.com/project/alonsodomin/groot)

Groot is a command line application with the purpose of remote managing AWS ECS Clusters and
 their associated resources (Tasks, Instances, Services, etc.)

## Installing

You will need `cabal-install` installed in your system, you either get it by installing the package using your favourite
 package manager or installing the Haskell Platform. Once you have it, just run:

```bash
cabal install groot
```

## Building From Source

You will need [Stack](http://haskellstack.org) installed in your local machine. Once you´ve got that, then clone this
 repository and install the binaries using Stack:

```bash
git clone https://github.com/alonsodomin/groot
cd groot
stack install
```

Now step back, the whole of The Internet is going to be sucked into your machine.

## First Steps

Now that you have it installed in your system, run it with the `-h` (or `--help`) command line
 option to get an idea of what you can do.

```bash
groot -h
```

Groot supports several sub-commands, use the same `-h` flag to get some help on what they can do,
 i.e.: `groot ls tasks -h`.

## Interactive Shell

There is also the option of running in an interactive shell, run the `groot shell` command and you'll see a welcome message and the prompt:

```bash
Welcome to the Groot Shell.
Type 'help' for a list of available commands
groot>
```

Enter `exit` to get back to your main shell. All the other commands available from the command line are also available in the shell session.

## Status

Groot is a toy project started to solve the need for a proper tool to perform my most rutinary
 tasks when managing ECS resources.

Don't expect here the best Haskell code (and I should probably write more tests), if it serves
 you then great, use it and feel free to contribute.

## License

### Apache License 2.0

Copyright 2017 Antonio Alonso Dominguez

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
