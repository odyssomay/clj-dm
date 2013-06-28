---
layout: default
title: Director Musices · Importing
---

## Abc

Abc files are imported using the menu *File->Import score from abc file...*

Abc importing is currently very limited. Files must follow the latest [abc standard](http://abcnotation.com/wiki/abc:standard:v2.1).

Supported [information fields](http://abcnotation.com/wiki/abc:standard:v2.1#information_fields) are:

* [L](http://abcnotation.com/wiki/abc:standard:v2.1#lunit_note_length)
* [K](http://abcnotation.com/wiki/abc:standard:v2.1#kkey). Correct accidentals are automatically inserted. So for example, if the key is *G*, every F should be F#, and this is automatically inserted - you do not have to change every F into F# manually. All standard western keys are supported (*Ionian* and *Aeolian* in the table in the link).

Supported [tune body](http://abcnotation.com/wiki/abc:standard:v2.1#the_tune_body) parts are:

* [Pitch](http://abcnotation.com/wiki/abc:standard:v2.1#pitch)
* [Accidentals](http://abcnotation.com/wiki/abc:standard:v2.1#accidentals)
* [Note lengths](http://abcnotation.com/wiki/abc:standard:v2.1#note_lengths)
* [Rests](http://abcnotation.com/wiki/abc:standard:v2.1#rests)
* [Beams](http://abcnotation.com/wiki/abc:standard:v2.1#beams)

### Caveats

* [Multiple voices](http://abcnotation.com/wiki/abc:standard:v2.1#multiple_voices) must be written in the new format. This is the format specified in the link. The older format can be seen in the end of the section in the link.

* [Information fields](http://abcnotation.com/wiki/abc:standard:v2.1#information_fields) cannot be written [inside the tune body](http://abcnotation.com/wiki/abc:standard:v2.1#use_of_fields_within_the_tune_body).


