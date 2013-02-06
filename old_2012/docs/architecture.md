Architecture Notes
==================

Content Organization
--------------------

We need to figure out a way to organize content: logically, on-disk
and at run-time. I think we've got two classes of entities to worry
about: static things (environment mostly, but also other non-gameplay
resources) and dynamic things (characters, NPCs, active items).

Scripting
---------

We need a way to animate (give life to) dynamic things. I strongly
suspect we'll need a scripting language (I'd like to use V8).
