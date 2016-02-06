# dictcc in Haskell [![Build Status](https://travis-ci.org/JustusAdam/dictcc.svg?branch=master)](https://travis-ci.org/JustusAdam/dictcc)

This is a small command line tool for accessing dict.cc without the hassle of using an actual browser.

Written in good old efficient Haskell.


## Usage

`dictcc SOURCE_LANGUAGE TARGET_LANGUAGE WORD`

**SOURCE_LANGUAGE** and **TARGET_LANGUAGE** are the two letter shorthands used by dict.cc aka "en" for English, "de" for Deutsch and so on.


## Inspiration

This project is inspired by [dict.cc.py][] or rather born out of my frustration with seeing error messages about deprecated features from BeautifulSoup every time it is used. Thus I decided to write a new client in Haskell.

[dict.cc.py]: https://github.com/rbaron/dict.cc.py

Though the idea came from [dict.cc.py][] the code was written anew from the ground up.
