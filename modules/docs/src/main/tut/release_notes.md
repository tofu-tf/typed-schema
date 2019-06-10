---
layout: home
section: "home"
position: 3
title:  "Release Notes"
---

## Release Notes

## Version 0.11.0
- added support for multiple param parsing into the single case class
- added specific typeclasses for params for different sources (Header, Query, Cookie, etc...)
- updated magnolia to 0.10
- added posibility to specify custom MediaType for SwaggerTypeable
- MkRoute can now consume custom HList as input that could be usable in Serve instances

## Version 0.10.7
- update circe to `0.11.1`

## Version 0.10.6
- update cats to `1.4.0`, remove `cats-mtl` dependency