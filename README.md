# cabal-constraints

Repeatable builds for cabalized Haskell projects.


## About

`cabal-constraints` provides repeatable builds for cabalized Haskell projects
by "freezing" the exact versions of the dependencies selected by
`cabal-install`. All build environments for the project, such as the test or
staging build environments, or other developers collaborating on the project,
will then use the same dependency versions.

It is designed to be used alongside `cabal-install` sandboxes, in which case
isolated, repeatable builds can be achieved.

[Cabal-constraints home page](https://github.com/benarmston/cabal-constraints/)


## Quick start

Install `cabal-constraints` making sure to use the correct version of the
Cabal library. Using the correct version of Cabal is very important.

```sh
$ cabal install cabal-constraints --constraint "Cabal == $( cabal --version | grep 'Cabal library' | cut -f3 -d' ' )"
```

Discover the exact dependencies your local build is using:

```sh
$ cabal-constraints dist/dist-sandbox-*/setup-config
```

Constrain all builds to use the same versions:

```sh
$ cabal-constraints dist/dist-sandbox-*/setup-config > cabal.config
$ git add cabal.config
$ git commit -m 'Freeze dependency versions'
```


## Installing cabal-constraints

`cabal-constraints` has been developed as a stop-gap measure until cabal
natively supports freezing dependency versions.  As it is intended that
`cabal-constraints` will become obsolete, certain pragmatic design decisions
have been made which affect it in two important ways: 1) it's available now
and does work; and 2) some hand holding is required when installing it.

`cabal-constraints` must link against the same version of the Cabal library
that is used by `cabal-install`.  The version used by `cabal-install` can be
found by running `cabal --version` which will report something like

```sh
$ cabal --version
cabal-install version 1.18.0.2
using version 1.18.0 of the Cabal library
```

The version of the Cabal library is given on the second line of the output,
in this case `1.18.0`.

Once the version has been determined, `cabal-constraints` can be installed and
an appropriate `--constraint` flag provided for Cabal. In this case that would
be

```sh
$ cabal install cabal-constraints --constraint 'Cabal == 1.18.0'
```

All of this can be done with the following one-liner:


```sh
$ cabal install cabal-constraints --constraint "Cabal == $( cabal --version | grep 'Cabal library' | cut -f3 -d' ' )"
```


## Using cabal-constraints

`cabal-constraints` should be run from the root directory of a cabalized
Haskell project and given the path to the `setup-config` file to use. It will
print out all dependencies of the project in a format suitable for use in a
`cabal-install` config file. For example, running `cabal-constraints` against
itself produces the following:

```sh
$ cabal-constraints dist/dist-sandbox-*/setup-config
constraints: Cabal == 1.19.0
           , array == 0.4.0.1
           , base == 4.6.0.1
           , bytestring == 0.10.0.2
           , containers == 0.5.0.0
           , deepseq == 1.3.0.1
           , directory == 1.2.0.1
           , filepath == 1.3.0.1
           , ghc-prim == 0.3.0.0
           , integer-gmp == 0.5.0.0
           , old-locale == 1.0.0.5
           , pretty == 1.1.1.0
           , process == 1.1.0.2
           , rts == 1.0
           , time == 1.4.0.1
           , unix == 2.6.0.1
```

A single mandatory argument must be provided which is the path to the
`setup-config` file to use. If you are using cabal sandboxes then you can most
likely use the shell glob `dist/dist-sandbox-*/setup-config`. If that doesn't
work for your project see the Trouble shooting section below.

To use these constraints for reproducible builds, one should make use of the
new sandbox feature of `cabal-install` 1.18. The constraints can be redirected
to `cabal.config` and committed to your code repository.  When the project is
built, the same set of dependency versions will be resolved by `cabal-install`
ensuring repeatable builds.

If `cabal.config` contains no other information the simplest solution is to
overwrite it:

```sh
$ cabal-constraints > cabal.config
```

If you are already making use of the `cabal.config` file see the Trouble
shooting section below.

Once the dependency versions have been added to `cabal.config`, you will need
to add that file to your revision control system to ensure that all future
builds have that information.


## Trouble shooting

### Cabal constraints tells me to re-run the configure command

`cabal-constraints` has been built to use a different version of the Cabal
library than is being used by the `cabal` command. You will need to reinstall
`cabal-constraints` with the correct version of the Cabal library. See the
installation section for details on how to do this.


### I have lots of setup-config files. Which one should I use?

If your project has more than one `setup-config` file, it is likely because
you have built it either with and without sandboxes or with multiple
sandboxes.  You probably want the most recently modified file which can be
found with `ls -tr $( find dist -name setup-config ) | tail -n1`.

Once you know which setup-config file to use, replace the
`dist/dist-sandbox-*/setup-config` glob used in these examples with the path
to the file.


### I have information in `cabal.config` which I don't want to loose

If `cabal.config` contains information that needs to be preserved, the
following shell command can be used to replace the `constraints` section and
everything following it with the new constraints.

```sh
$ sed -i /^constraints:/,$d cabal.config && cabal-constraints dist/dist-sandbox-*/setup-config >> cabal.config
```

If you ensure that the `constraints` section is the last section of the file,
all other information will be kept.



## License

The MIT License (MIT). See the LICENSE file for details.

Copyright (c) 2013 Benjamen Armston
