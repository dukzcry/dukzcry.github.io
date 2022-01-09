---
title: Слушаем и скробблим трекерную музыку
published: 2017-11-22
tags: demoscene, lastfm, mod, tracker
author: dukzcry
---
> Забудьте про ручное тэгирование модулей в XMPlay ради скробблинга!

[Скрипт](https://github.com/dukzcry/crap/tree/master/modplay) подходит для прослушивания амижных и писишных трекерных модулей. Используются движки [UADE](http://zakalwe.fi/uade/) и [libopenmpt](https://lib.openmpt.org/libopenmpt/) От чудесного [BASS](http://www.un4seen.com/bass.html) пришлось отказаться по причине небольшого числа поддерживаемых форматов. Впрочем, по заверениям экспертов, libopenmpt очень неплох в плане точности воспроизведения. А еще говорят, что [XMP](http://xmp.sourceforge.net/) исправляется.

Музыку в архивах играть мы тоже могем. А вкупе с curlftpfs и модули качать не надо:
```sh
curlftpfs ftp://ftp.scene.org/pub/music/artists/ ~/Music
modplay.sh "4-mat" ~/Music/4-mat/*
```
