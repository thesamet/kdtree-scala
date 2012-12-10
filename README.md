KDTree-Scala
============

A simple in-memory immutable KDTree and KDTreeMap implementations in Scala.

Currently the only operation supported is _n_-nearest neighbors lookup.

Using with SBT
==============

Add the following to your `build.sbt`:

    resolvers += "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/"

    libraryDependencies += "com.thesamet" %% "kdtree" % "1.0"

Examples
--------

See `src/test/scala/` for examples.
