# withdependencies

[![Build Status](https://travis-ci.org/bartavelle/withdependencies.svg?branch=master)](https://travis-ci.org/bartavelle/withdependencies)
[![withdependencies on Stackage LTS 3](http://stackage.org/package/withdependencies/badge/lts-3)](http://stackage.org/lts-3/package/withdependencies)
[![withdependencies on Stackage Nightly](http://stackage.org/package/withdependencies/badge/nightly)](http://stackage.org/nightly/package/withdependencies)

Model computation with dependencies, and evaluate them over streams of elements.

This module was created to handle tree-like computations that require elements from a stream that might arrive in arbitrary order.

The particular use case was a computation requiring several files from a tar archive. The archive would be parsed as a stream of files, and the computation would end as soon as all required files would be traversed.

