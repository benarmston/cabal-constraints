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

## Using cabal-constraints

`cabal-constraints` should be run from the root directory of a cabalized
Haskell project and given the path to the `setup-config` file to use. It will
print out all dependencies of the project in a format suitable for use in a
`cabal-install` config file. For example, running `cabal-constraints` against
itself produces the following:

```sh
$ cabal-constraints dist/dist-sandbox-500003c6/setup-config
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
`setup-config` file to use. The file will be located under the `dist`
directory. Usually there will be a single `setup-config` file which can be
found by running `find dist -name setup-config` from the root directory of the
project.  If your project has more than one such file, it is likely because
you have built it either with and without sandboxes or with multiple
sandboxes.  You probably want the most recently modified file which can be
found with `ls -tr $( find dist -name setup-config ) | tail -n1`.

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

If `cabal.config` contains information that needs to be preserved, the
following will replace the `constraints` section and everything following it
with the new constraints. If you ensure that the `constraints` section is the
last section of the file, all other information in it will be kept.

```sh
$ sed -i /^constraints:/,$d cabal.config && cabal-constraints >> cabal.config
```


## Installing cabal-constraints

`cabal-constraints` can be installed from
[Hackage](http://hackage.haskell.org/package/cabal-constraints) via
`cabal-install`.

```sh
cabal install cabal-constraints
```


## License

The MIT License (MIT). See the LICENSE file for details.

Copyright (c) 2013 Benjamen Armston
