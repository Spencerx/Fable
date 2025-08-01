# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

## 5.0.0-beta.2 - 2025-07-25

* [All] Support for Nullable Reference Types (by @ncave and @MangelMaxime)

## 5.0.0-beta.1 - 2025-02-16

### Added

* [All] Add support for module comments (by @ncave)

## 4.6.0 - 2025-01-28

* Stable release

## 4.6.0-beta.1 - 2025-01-28

### Fixed

* [All] Re-package with `--realsig+` to fix `Method not found: 'Boolean Fable.CompilerOptions.Equals` (by @nojaf)

## 4.5.0 - 2024-05-23

### Removed

* Remove `MemberRefInfo.Attributes` [GH-3817](https://github.com/fable-compiler/Fable/pull/3817) (by @DunetsNM)

### Added

* Add `MemberRefInfo.AttributeFullNames` [GH-3817](https://github.com/fable-compiler/Fable/pull/3817) (by @DunetsNM)

## 4.4.0 - 2024-02-13

### Changed

* Change `| TypeCast of expr: Expr * Type` to `| TypeCast of expr: Expr * typ: Type`

## 4.3.0 - 2023-09-29

### Added

* Add `Attributes` to `MemberRefInfo`

## 4.2.1 - 2022-10-31

* Get sources from PluginHelper

## 4.2.0 - 2022-09-09

* Add Entity.DeclaringEntity

## 4.1.0 - 2022-09-07

* Add IsInternal/IsPrivate to Entity and MemberFunctionOrValue

## 4.0.0 - 2022-09-01

* Stable AST for Fable 4

## 4.0.0-snake-island-alpha-001 - 2022-08-24

* Snake Island alpha release

## 3.1.1 - 2021-05-14

* Publish with icon and symbols @cartermp

## 3.1.0 - 2021-04-16

* Add GetOutputPath to PluginHelper

## 3.0.0 - 2020-12-04

* Official release

## 3.0.0-nagareyama-rc-001 - 2020-11-25

* Add `Expr.LetRec`
* Add `ScanForPluginsAttribute`
* Add `AST.Fable.File` argument to `MemberDeclarationPlugin.Transform`

## 3.0.0-nagareyama-beta-002 - 2020-10-27

* Add `MemberDecl.ExportDefault`

## 3.0.0-nagareyama-beta-001 - 2020-10-23

* Beta release
* Use string literals in ImportInfo

## 3.0.0-nagareyama-alpha-003 - 2020-10-22

* Add optional string tag to TypeCast
* Add methods from Compiler to PluginHelper
* Add CallMemberInfo to Call expr

## 3.0.0-nagareyama-alpha-002 - 2020-10-19

* Merge Member/Call plugins

## 3.0.0-nagareyama-alpha-001 - 2020-10-15

* First alpha release
