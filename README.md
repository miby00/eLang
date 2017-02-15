eLang
=====

An erlang application used to add language support to eTodo.

Howto add a new language
------------------------

To add *Language*

1. Take eLangSwedish.hrl or eLangEnglish.hrl and copy it to a new file named eLang*Language*.hrl
2. Rename the three maps contained in the eLang*Language*.hrl file to: ?*language*WS, ?*language*Def and ?*language*ToDB
3. Rename all language strings contained in the three maps
4. Edit the file eLang.hrl, add an include to the new eLang*Language*.hrl file
5. Add the new language dictionaries to **languages** as eLangSwedish and eLangEnglish has been added.

Voilà you are done...

Build
-----

    $ rebar3 compile
