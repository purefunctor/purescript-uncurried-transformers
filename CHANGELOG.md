# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html). This file is updated via [purs-changelog](https://github.com/JordanMartinez/purescript-up-changelog)

## 1.1.0

New features:

* Added smart constructors for monad transformers.

Bugfixes:

* Fixed `Writer` accumulation in the `MonadRec` instance for `RWSET`.

Other improvements:

* Refactored trampoline usages in the `Bind` and `MonadError` instances for `RWSET`.

Internal:

* Added initial unit tests for the project using `spec`.

## 1.0.0

Initial Release

## 0.1.0

Initial Release
