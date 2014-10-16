PeMo
====

A XMPP IM client [pontarius-xmpp](https://github.com/pontarius/pontarius-xmpp/) using as interface [vty-ui](http://jtdaugherty.github.io/vty-ui/), made in Haskell
Includes support for Jabber.

***

__Installation__  
`$ cabal sandbox init`  
`$ cabal install`

* __Mac OSX:__ In order to install text-icu, you need the C libraries: icuuc, icui18n, icudata
  1. `$ brew install icu4c`
  2. `$ cabal install text-icu --extra-include-dirs=/usr/local/Cellar/icu4c/53.1/include/ --extra-lib-dirs=/usr/local/Cellar/icu4c/53.1/lib`
  
Or the equivalent using MacPorts.

__Usage__  
`$ cabal configure`  
`$ cabal run`
